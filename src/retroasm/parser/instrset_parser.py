from __future__ import annotations

import re
from collections.abc import Callable, Iterable, Iterator, Mapping, Set
from importlib.resources.abc import Traversable
from logging import WARNING, Logger, getLogger

from ..codeblock import FunctionBody
from ..codeblock_builder import (
    SemanticsCodeBlockBuilder,
    StatelessCodeBlockBuilder,
    returned_bits,
)
from ..decode import Prefix
from ..encoding import (
    Encoding,
    EncodingExpr,
    EncodingItem,
    EncodingMultiMatch,
    ModeMatchReference,
    check_for_missing_placeholders,
)
from ..input import BadInput, DelayedError, ErrorCollector, InputLocation
from ..instrset import InstructionSet, PrefixMappingFactory
from ..mode import Mnemonic, Mode, ModeRow
from ..namespace import (
    ContextNamespace,
    GlobalNamespace,
    LocalNamespace,
    NameExistsError,
    ReadOnlyNamespace,
)
from ..reference import FixedValueReference, Reference, bad_reference
from ..storage import IOChannel, IOStorage, Register
from ..symbol import ImmediateValue
from ..types import IntType, ReferenceType, Width, parse_type, parse_type_decl
from ..utils import bad_type
from .context_parser import parse_placeholders
from .expression_builder import (
    UnknownNameError,
    build_expression,
    build_reference,
    build_statement_eval,
    convert_definition,
)
from .expression_nodes import (
    DefinitionNode,
    FlagTestNode,
    IdentifierNode,
    MultiMatchNode,
    ParseNode,
    VariableDeclarationNode,
)
from .expression_parser import (
    parse_encoding,
    parse_expr,
    parse_expr_list,
    parse_regs,
    parse_statement,
)
from .function_builder import create_func
from .linereader import DefLineReader
from .mnemonic import MnemonicTokenizer, parse_mnemonic

_name_pat = r"[A-Za-z_][A-Za-z0-9_]*'?"
_name_tok = r"\s*(" + _name_pat + r")\s*"
_type_tok = r"\s*(" + _name_pat + r"&?)\s*"

_re_dot_sep = re.compile(r"\s*\.\s*")


def _parse_regs(
    reader: DefLineReader,
    collector: ErrorCollector,
    args: InputLocation,
    global_namespace: GlobalNamespace,
    global_builder: StatelessCodeBlockBuilder,
) -> None:
    if args:
        collector.error("register definition must have no arguments", location=args)

    for line in reader.iter_block():
        try:
            nodes = parse_regs(line)
        except BadInput as ex:
            collector.error(f"bad register definition line: {ex}", location=ex.locations)
            continue

        last_type_location = None
        for node in nodes:
            # Parse type declaration.
            decl = node.decl if isinstance(node, DefinitionNode) else node
            decl_type = decl.type
            type_location = decl_type.location
            try:
                reg_type = parse_type_decl(decl_type.name)
            except ValueError as ex:
                # Avoid reporting the same error twice.
                if type_location != last_type_location:
                    collector.error(f"{ex}", location=type_location)
                continue
            last_type_location = type_location

            match node:
                case VariableDeclarationNode():
                    # Define base register.
                    if isinstance(reg_type, ReferenceType):
                        collector.error(
                            "base register cannot have a reference type",
                            location=decl_type.location,
                        )
                        continue
                    try:
                        global_namespace.add_register(
                            decl.name.name, reg_type, decl.name.location
                        )
                    except NameExistsError as ex:
                        collector.error(
                            f"bad base register definition: {ex}", location=ex.locations
                        )
                case DefinitionNode(decl=decl, value=value):
                    # Define register alias.
                    try:
                        ref = convert_definition(
                            decl, reg_type, value, global_namespace, global_builder
                        )
                    except BadInput as ex:
                        message = f"bad register alias: {ex}"
                        collector.error(message, location=ex.locations)
                        ref = bad_reference(reg_type, message)
                    else:
                        ref = ref.simplify()
                    try:
                        global_namespace.define(decl.name.name, ref, decl.name.location)
                    except NameExistsError as ex:
                        collector.error(
                            f"failed to define register alias: {ex}", location=ex.locations
                        )
                case node:
                    bad_type(node)


_re_comma_sep = re.compile(r"\s*,\s*")
_re_arg_decl = re.compile(_type_tok + r"\s" + _name_tok + r"$")


def _parse_typed_args(
    collector: ErrorCollector, args: InputLocation, description: str
) -> Iterator[tuple[IntType | ReferenceType, InputLocation, InputLocation]]:
    """
    Parses a typed arguments list, yielding a triple for each argument,
    containing the argument type and InputLocations for the type and name.
    Errors are logged on the given reader as they are discovered.
    """
    arg_locs = tuple(args.split(_re_comma_sep))
    if len(arg_locs) == 1:
        # Arg list contains no separators; do we have 0 or 1 argument(s)?
        if len(args) == 0 or args.text.isspace():
            return

    for i, arg_loc in enumerate(arg_locs, 1):
        arg_match = arg_loc.match(_re_arg_decl)
        if arg_match is None:
            collector.error(
                f'{description} {i:d} not of the form "<type> <name>"', location=arg_loc
            )
            continue

        type_loc, name_loc = arg_match.groups
        try:
            arg_type = parse_type_decl(type_loc.text)
        except ValueError as ex:
            collector.error(
                f'bad {description} {i:d} ("{name_loc.text}"): {ex}', location=type_loc
            )
        else:
            yield arg_type, type_loc, name_loc


def _parse_prefix(
    reader: DefLineReader,
    collector: ErrorCollector,
    args: InputLocation,
    namespace: GlobalNamespace,
    factory: PrefixMappingFactory,
) -> None:
    header_location = reader.location

    # Parse header line.
    decode_flags = []
    try:
        with collector.check():
            flag_type = IntType.u(1)
            for arg_type, arg_type_loc, arg_name_loc in _parse_typed_args(
                collector, args, "decode flag"
            ):
                match arg_type:
                    case ReferenceType():
                        collector.error(
                            "decode flag cannot be declared as a reference type",
                            location=arg_type_loc,
                        )
                        continue
                    case IntType() as typ:
                        if typ is not flag_type:
                            # Maybe in the future we'll support other types.
                            collector.error(
                                f'decode flag of type "{arg_type}", expected "{flag_type}"',
                                location=arg_type_loc,
                            )
                    case typ:
                        bad_type(typ)
                arg_name = arg_name_loc.text
                try:
                    namespace.add_register(arg_name, arg_type, arg_name_loc)
                except ValueError as ex:
                    collector.error(f"{ex}", location=reader.location)
                except NameExistsError as ex:
                    collector.error(f"error defining decode flag: {ex}", location=ex.locations)
                else:
                    decode_flags.append(arg_name)
    except DelayedError:
        reader.skip_block()
        return

    # Parse body.
    prefixes = []
    for line in reader.iter_block():
        # Split line into 3 fields.
        fields = tuple(line.split(_re_dot_sep))
        try:
            enc_loc, mnem_loc, sem_loc = fields
        except ValueError:
            collector.error(
                "wrong number of dot-separated fields in prefix line: "
                f"expected 3, got {len(fields):d}",
                location=line,
            )
            continue

        # Parse encoding.
        try:
            with collector.check():
                if len(enc_loc) == 0:
                    collector.error("prefix encoding cannot be empty", location=enc_loc)
                else:
                    try:
                        enc_nodes = parse_expr_list(enc_loc)
                    except BadInput as ex:
                        collector.error(f"bad prefix encoding: {ex}", location=ex.locations)
                    else:
                        enc_items = []
                        for enc_node in enc_nodes:
                            try:
                                enc_items.append(
                                    _parse_encoding_expr(
                                        enc_node, namespace, ContextNamespace(namespace)
                                    )
                                )
                            except BadInput as ex:
                                collector.error(
                                    f"bad prefix encoding: {ex}", location=ex.locations
                                )
            encoding = Encoding.create(enc_items, {}, collector, enc_loc)
        except DelayedError:
            encoding = None

        # Parse mnemonic.
        if len(mnem_loc) != 0:
            collector.warning("prefix mnemonics are not supported yet", location=mnem_loc)

        # Parse semantics.
        semantics: FunctionBody | None
        try:
            with collector.check():
                if len(sem_loc) == 0:
                    collector.error(
                        'prefix semantics cannot be empty; use "nop" instead', location=sem_loc
                    )
                else:
                    sem_builder = SemanticsCodeBlockBuilder()
                    sem_namespace = LocalNamespace(namespace)
                    try:
                        _parse_instr_semantics(collector, sem_loc, sem_namespace, sem_builder)
                    except BadInput as ex:
                        collector.error(f"bad prefix semantics: {ex}", location=ex.locations)
                    else:
                        semantics = sem_builder.create_code_block(
                            returned=(), collector=collector
                        )
        except DelayedError:
            semantics = None

        if encoding is not None and semantics is not None:
            prefixes.append(Prefix(encoding, semantics))

    try:
        factory.add_prefixes(decode_flags, prefixes)
    except ValueError as ex:
        collector.error(f"validation of prefix block failed: {ex}", location=header_location)
    except BadInput as ex:
        collector.error(f"validation of prefix block failed: {ex}", location=ex.locations)


_re_io_line = re.compile(_name_tok + r"\s" + _name_tok + r"\[" + _name_tok + r"\]$")


def _parse_io(
    reader: DefLineReader,
    collector: ErrorCollector,
    args: InputLocation,
    namespace: GlobalNamespace,
) -> None:
    if args:
        collector.error("I/O definition must have no arguments", location=args)

    for line in reader.iter_block():
        match = line.match(_re_io_line)
        if match is None:
            collector.error("invalid I/O definition line", location=line)
        else:
            elem_type_loc, name_loc, addr_type_loc = match.groups

            try:
                elem_type: IntType | None = parse_type(elem_type_loc.text)
            except ValueError as ex:
                collector.error(f"bad I/O element type: {ex}", location=elem_type_loc)
                elem_type = None

            try:
                addr_type: IntType | None = parse_type(addr_type_loc.text)
            except ValueError as ex:
                collector.error(f"bad I/O address type: {ex}", location=addr_type_loc)
                addr_type = None

            if elem_type is None or addr_type is None:
                continue

            name = name_loc.text
            channel = IOChannel(name, elem_type, addr_type)
            try:
                namespace.define(name, channel, name_loc)
            except NameExistsError as ex:
                collector.error(f"error defining I/O channel: {ex}", location=ex.locations)


_re_func_header = re.compile(_name_tok + r"\((.*)\)$")


def _parse_func(
    reader: DefLineReader,
    collector: ErrorCollector,
    header_args: InputLocation,
    namespace: GlobalNamespace,
    want_semantics: bool,
) -> None:
    # Parse header line.
    match = header_args.match(_re_func_header)
    if match is None:
        collector.error("invalid function header line", location=header_args)
        reader.skip_block()
        return

    # Parse arguments.
    args = {}
    try:
        with collector.check():
            name_locations: dict[str, InputLocation] = {}
            for i, (arg_type, _arg_type_loc, arg_name_loc) in enumerate(
                _parse_typed_args(collector, match.group(2), "function argument"), 1
            ):
                arg_name = arg_name_loc.text
                if arg_name == "ret":
                    collector.error(
                        '"ret" is reserved for the return value; '
                        "it cannot be used as an argument name",
                        location=arg_name_loc,
                    )
                elif arg_name in name_locations:
                    collector.error(
                        f"function argument {i:d} has the same name as "
                        f"an earlier argument: {arg_name}",
                        location=(arg_name_loc, name_locations[arg_name]),
                    )
                else:
                    args[arg_name] = arg_type
                    name_locations[arg_name] = arg_name_loc
    except DelayedError:
        reader.skip_block()
        return

    if want_semantics:
        func_name_loc = match.group(1)

        # Parse body lines.
        try:
            func = create_func(
                reader, collector, func_name_loc, args, name_locations, namespace
            )
        except BadInput as ex:
            collector.error(str(ex), location=ex.locations)
        else:
            # Store function in namespace.
            try:
                namespace.define(func_name_loc.text, func, func_name_loc)
            except NameExistsError as ex:
                collector.error(f"error declaring function: {ex}", location=ex.locations)
    else:
        reader.skip_block()


def _parse_encoding_expr(
    enc_node: ParseNode, enc_namespace: ReadOnlyNamespace, ctx_namespace: ContextNamespace
) -> EncodingExpr:
    """
    Parse encoding node that is not a MultiMatchNode.
    Returns the parse result as an EncodingExpr.
    Raises BadInput if the node is invalid.
    """

    # TODO: Maybe TabooReference is a simpler way of explaining why a name is unavailable,
    #       as the decision not to add and the rejection message can be kept together.
    def explain_not_in_namespace(name: str, locations: tuple[InputLocation, ...]) -> None:
        match ctx_namespace.elements.get(name):
            case ModeMatchReference(mode=mode):
                if mode.encoding_width is None:
                    raise BadInput(
                        f'cannot use placeholder "{name}" '
                        f'in encoding field, since mode "{mode.name}" '
                        f"has an empty encoding sequence",
                        *locations,
                        ctx_namespace.locations[name],
                    ) from None
            case FixedValueReference(expr=ImmediateValue(name=imm_name)) if imm_name == name:
                # TODO: The name exists in ctx_namespace but not in enc_namespace.
                #       What does that mean? Is it a bug? User error?
                pass
            case FixedValueReference():
                raise BadInput(
                    f'cannot use placeholder "{name}" in encoding field, '
                    "since its value is computed in the context",
                    *locations,
                    ctx_namespace.locations[name],
                ) from None
            case None:
                # No placeholder with that name exists.
                pass
            case ref:
                bad_type(ref)

    builder = SemanticsCodeBlockBuilder()
    try:
        enc_ref = build_reference(enc_node, enc_namespace, builder)
    except BadInput as ex:
        if isinstance(ex, UnknownNameError):
            explain_not_in_namespace(ex.name, ex.locations)
        raise BadInput(
            f"error in encoding: {ex}", enc_node.tree_location, *ex.locations
        ) from ex

    code = builder.create_code_block((enc_ref.bits,))
    if len(code.nodes) != 0:
        raise BadInput(
            "encoding expression accesses state or performs I/O", enc_node.tree_location
        )
    (enc_bits,) = code.returned
    for storage in enc_bits.iter_storages():
        match storage:
            case Register():
                raise BadInput(
                    "encoding expression references register", enc_node.tree_location
                )
            case IOStorage(channel=channel):
                raise BadInput(
                    f"encoding expression references storage location "
                    f'on I/O channel "{channel.name}"',
                    enc_node.tree_location,
                )
    return EncodingExpr(enc_bits, enc_node.tree_location)


def _parse_multi_match(
    enc_node: MultiMatchNode,
    identifiers: Set[str],
    ctx_namespace: ContextNamespace,
    collector: ErrorCollector,
) -> EncodingMultiMatch:
    """
    Parse an encoding node of type MultiMatchNode.
    Returns the parse result as an EncodingMultiMatch.
    Raises BadInput if the node is invalid.
    """
    name = enc_node.name
    try:
        ref = ctx_namespace.elements[name]
    except KeyError:
        raise BadInput(
            f'placeholder "{name}" does not exist in context', enc_node.tree_location
        ) from None
    if not isinstance(ref, ModeMatchReference):
        raise BadInput(
            f'placeholder "{name}" does not represent a mode match',
            enc_node.tree_location,
            ctx_namespace.locations[name],
        )

    mode = ref.mode
    start = 1 if name in identifiers else 0
    location = enc_node.tree_location

    # Warn about multi-matches that always match zero elements. Technically there is
    # nothing wrong with those, but it is probably not what the user intended.
    if mode.encoding_width is None:
        collector.warning(
            f'mode "{mode.name}" does not contain encoding elements',
            location=(location, ctx_namespace.locations[name]),
        )
    elif start >= 1 and mode.aux_encoding_width is None:
        collector.warning(
            f'mode "{mode.name}" does not match auxiliary encoding units',
            location=(location, ctx_namespace.locations[name]),
        )

    return EncodingMultiMatch(
        name,
        mode.encoding_width if start == 0 else mode.aux_encoding_width,
        mode.aux_encoding_width,
        start,
        None if (length := mode.encoded_length) is None else length - start,
        location,
    )


def _parse_mode_encoding(
    enc_nodes: Iterable[ParseNode], ctx_namespace: ContextNamespace, collector: ErrorCollector
) -> Iterator[EncodingItem]:
    # Define placeholders in encoding namespace.
    # Note: The names in the encoding namespace are a subset of the names in the context
    #       namespace and they share the same parent, so there is no risk of redefining names.
    enc_namespace = ContextNamespace(ctx_namespace.parent)
    for name, ref in ctx_namespace.elements.items():
        match ref:
            case ModeMatchReference(mode=mode):
                enc_width = mode.encoding_width
                if enc_width is None:
                    continue
                imm_type = IntType.u(enc_width)
                imm_expr = ImmediateValue(name, imm_type)
                imm_ref = FixedValueReference(imm_expr, imm_type)
                enc_namespace.define(name, imm_ref, ctx_namespace.locations[name])
            case FixedValueReference(expr=ImmediateValue(name=imm_name)) if imm_name == name:
                enc_namespace.define(name, ref, ctx_namespace.locations[name])

    # Collect the names of all identifiers used in the encoding.
    identifiers = {
        sub_node.name
        for enc_node in enc_nodes
        for sub_node in enc_node
        if isinstance(sub_node, IdentifierNode)
    }

    # Evaluate encoding field.
    for enc_node in enc_nodes:
        try:
            match enc_node:
                case FlagTestNode():
                    pass
                case MultiMatchNode():
                    # Match multiple encoding fields as-is.
                    yield _parse_multi_match(enc_node, identifiers, ctx_namespace, collector)
                case _:
                    # Expression possibly containing single encoding field matches.
                    yield _parse_encoding_expr(enc_node, enc_namespace, ctx_namespace)
        except BadInput as ex:
            collector.add(ex)


def _parse_flags_required(
    enc_nodes: Iterable[ParseNode], prefixes: PrefixMappingFactory, collector: ErrorCollector
) -> Iterator[str]:
    for node in enc_nodes:
        if isinstance(node, FlagTestNode):
            if prefixes.has_flag(name := node.name):
                yield name
            else:
                collector.error(
                    f'there is no decode flag named "{name}"', location=node.location
                )


def _check_aux_encoding_width(
    enc_items: Iterable[EncodingItem], collector: ErrorCollector
) -> None:
    """
    Check whether the encoding widths in the given encoding are the same
    for all auxiliary encoding items.
    Violations are logged as errors on the given logger.
    """
    first_aux_width: Width | None = None
    first_aux_location: InputLocation | None = None

    def check_aux(width: Width | None, location: InputLocation | None) -> None:
        nonlocal first_aux_width, first_aux_location
        if first_aux_width is None:
            first_aux_width = width
            first_aux_location = location
        elif width != first_aux_width:
            collector.error(
                f"encoding item matches width {width}, while first auxiliary "
                f"encoding match has width {first_aux_width}",
                location=(location, first_aux_location),
            )

    first = True
    for enc_item in enc_items:
        match enc_item:
            case EncodingExpr() if first:
                pass
            case EncodingExpr(encoding_width=enc_width, location=loc):
                check_aux(enc_width, loc)
            case EncodingMultiMatch(
                aux_encoding_width=aux_width, encoded_length=enc_len, location=loc
            ) if first:
                if enc_len != 1 and aux_width is not None:
                    check_aux(aux_width, loc)
            case EncodingMultiMatch(encoding_width=enc_width, location=loc):
                check_aux(enc_width, loc)
            case item:
                bad_type(item)
        first = False


def _check_duplicate_multi_matches(
    enc_items: Iterable[EncodingItem], collector: ErrorCollector
) -> None:
    """
    Checks whether more than one multi-matcher exists for the same
    placeholder. If they exist, they are reported as errors on the given logger.
    """
    claimed_multi_matches: dict[str, InputLocation | None] = {}
    for enc_item in enc_items:
        match enc_item:
            case EncodingMultiMatch(name=name, location=location):
                if name in claimed_multi_matches:
                    collector.error(
                        f'duplicate multi-match placeholder "{name}@"',
                        location=(location, claimed_multi_matches[name]),
                    )
                else:
                    claimed_multi_matches[name] = location


def _parse_mode_semantics(
    _collector: ErrorCollector,
    sem_loc: InputLocation,
    sem_namespace: LocalNamespace,
    sem_builder: SemanticsCodeBlockBuilder,
    mode_type: None | IntType | ReferenceType,
) -> Reference | None:
    semantics = parse_expr(sem_loc)
    if isinstance(mode_type, ReferenceType):
        ref = build_reference(semantics, sem_namespace, sem_builder)
        if ref.type != mode_type.type:
            raise BadInput(
                f"semantics type {ref.type} does not match mode type {mode_type.type}", sem_loc
            )
        sem_namespace.define("ret", ref, sem_loc)
        return ref
    else:
        expr = build_expression(semantics, sem_namespace, sem_builder)
        # Note that modeType can be None because of earlier errors.
        if mode_type is None:
            return None
        ref = sem_namespace.add_variable("ret", mode_type, sem_loc)
        ref.emit_store(sem_builder, expr, sem_loc)
        return ref


def _parse_instr_semantics(
    collector: ErrorCollector,
    sem_loc: InputLocation,
    namespace: LocalNamespace,
    builder: SemanticsCodeBlockBuilder,
    mode_type: None | IntType | ReferenceType = None,
) -> None:
    assert mode_type is None, mode_type
    node = parse_statement(sem_loc)
    build_statement_eval(collector, "semantics field", namespace, builder, node)


def _parse_mode_rows(
    reader: DefLineReader,
    collector: ErrorCollector,
    global_namespace: GlobalNamespace,
    prefixes: PrefixMappingFactory,
    modes: Mapping[str, Mode],
    mode_type: None | IntType | ReferenceType,
    mnem_base_tokens: MnemonicTokenizer,
    parse_sem: Callable[
        [
            ErrorCollector,
            InputLocation,
            LocalNamespace,
            SemanticsCodeBlockBuilder,
            None | IntType | ReferenceType,
        ],
        Reference | None,
    ],
    want_semantics: bool,
) -> Iterator[ModeRow]:
    for line in reader.iter_block():
        # Split mode line into 4 fields.
        fields = list(line.split(_re_dot_sep))
        if len(fields) < 2:
            collector.error('field separator "." missing in mode line', location=line)
            continue
        if len(fields) > 4:
            collector.error(f"too many fields ({len(fields):d}) in mode line", location=line)
            continue
        fields += [line.end_location] * (4 - len(fields))
        enc_loc, mnem_loc, sem_loc, ctx_loc = fields

        # Parse context.
        if ctx_loc:
            try:
                with collector.check():
                    ctx_namespace = parse_placeholders(
                        ctx_loc, modes, global_namespace, collector
                    )
            except DelayedError:
                # To avoid error spam, skip this line.
                continue
        else:
            ctx_namespace = ContextNamespace(global_namespace)

        try:
            with collector.check():
                # Parse encoding.
                enc_nodes: Iterable[ParseNode] | None
                if len(enc_loc) != 0:
                    try:
                        # Parse encoding field.
                        enc_nodes = parse_encoding(enc_loc)
                    except BadInput as ex:
                        collector.error(f"error in encoding: {ex}", location=ex.locations)
                        enc_nodes = None
                else:
                    enc_nodes = ()
                if enc_nodes is None:
                    encoding = None
                else:
                    try:
                        with collector.check():
                            enc_items = tuple(
                                _parse_mode_encoding(enc_nodes, ctx_namespace, collector)
                            )
                        with collector.check():
                            _check_aux_encoding_width(enc_items, collector)
                            _check_duplicate_multi_matches(enc_items, collector)
                        # Parse required decode flags.
                        with collector.check():
                            flags_required = set(
                                _parse_flags_required(enc_nodes, prefixes, collector)
                            )
                            encoding = Encoding.create(
                                enc_items,
                                flags_required,
                                collector,
                                enc_loc,
                            )
                    except DelayedError:
                        encoding = None

                # Parse mnemonic.
                # Additional placeholders may be inserted by the mnemonic parser.
                mnem_tokens = mnem_base_tokens + MnemonicTokenizer.scan(mnem_loc)
                mnem_items = tuple(parse_mnemonic(mnem_tokens, ctx_namespace, collector))
                if mode_type is None and (not mnem_items or not isinstance(mnem_items[0], str)):
                    collector.error("missing instruction name", location=mnem_loc)
                mnemonic = Mnemonic(mnem_items)

                # Parse semantics.
                if want_semantics:
                    if len(sem_loc) == 0:
                        # Parse mnemonic field as semantics.
                        sem_loc = mnem_loc

                    sem_builder = SemanticsCodeBlockBuilder()
                    sem_namespace = LocalNamespace(global_namespace)
                    try:
                        # Define placeholders in semantics builder.
                        for name, ref in ctx_namespace.elements.items():
                            location = ctx_namespace.locations[name]
                            match ref:
                                case ModeMatchReference(mode=mode):
                                    sem_type = mode.semantics_type
                                    arg_type = (
                                        sem_type.type
                                        if isinstance(sem_type, ReferenceType)
                                        else sem_type
                                    )
                                    sem_namespace.add_argument(name, arg_type, location)
                                case FixedValueReference(expr=expr, type=arg_type):
                                    sem_namespace.add_variable(
                                        name, arg_type, location
                                    ).emit_store(sem_builder, expr, None)
                                case _:
                                    bad_type(ref)

                        sem_ref = parse_sem(
                            collector, sem_loc, sem_namespace, sem_builder, mode_type
                        )
                    except BadInput as ex:
                        collector.error(f"error in semantics: {ex}", location=ex.locations)
                        # This is the last field.
                        continue
                    semantics: FunctionBody | None
                    semantics = sem_builder.create_code_block(
                        returned=returned_bits(sem_ref), collector=collector
                    )
                else:
                    semantics = None

            if encoding is not None:
                with collector.check():
                    check_for_missing_placeholders(encoding, ctx_namespace.elements, collector)
                yield ModeRow(
                    encoding,
                    # If mnemonic was not defined, DelayedError will have been raised.
                    mnemonic,  # pylint: disable=possibly-used-before-assignment
                    semantics,
                    {
                        name: ref.mode
                        for name, ref in ctx_namespace.elements.items()
                        if isinstance(ref, ModeMatchReference)
                    },
                    {
                        name: ref
                        for name, ref in ctx_namespace.elements.items()
                        if not isinstance(ref, ModeMatchReference)
                    },
                )
        except DelayedError:
            pass


_re_mode_args = re.compile(_type_tok + r"\s" + _name_tok + r"$")


def _parse_mode(
    reader: DefLineReader,
    collector: ErrorCollector,
    args: InputLocation,
    global_namespace: GlobalNamespace,
    prefixes: PrefixMappingFactory,
    modes: dict[str, Mode],
    want_semantics: bool,
) -> None:
    # Is its safe to add this mode to 'modes'?
    add_mode = True

    # Parse header line.
    match = args.match(_re_mode_args)
    if match is None:
        collector.error('invalid mode arguments, expected "mode <type> <name>"', location=args)
        reader.skip_block()
        return
    mode_type_loc, mode_name_loc = match.groups
    sem_type: IntType | ReferenceType
    try:
        sem_type = parse_type_decl(mode_type_loc.text)
    except ValueError as ex:
        collector.error(f"bad mode type: {ex}", location=mode_type_loc)
        sem_type = IntType.u(0)
        add_mode = False

    # Check mode name and type.
    mode_name = mode_name_loc.text
    if mode_name in modes:
        lineno = modes[mode_name].location.lineno
        collector.error(
            f'mode "{mode_name}" redefined; first definition was on line {lineno:d}',
            location=mode_name_loc,
        )
        add_mode = False
    else:
        try:
            parse_type(mode_name)
        except ValueError:
            pass
        else:
            collector.error(
                f'mode name "{mode_name}" conflicts with type', location=mode_name_loc
            )
            add_mode = False

    # Parse rows.
    rows = tuple(
        _parse_mode_rows(
            reader,
            collector,
            global_namespace,
            prefixes,
            modes,
            sem_type,
            MnemonicTokenizer.empty(),
            _parse_mode_semantics,
            want_semantics,
        )
    )

    # Create and remember mode object.
    try:
        with collector.check():
            mode = Mode.create(mode_name, sem_type, mode_name_loc, rows, collector)
    except DelayedError:
        # Avoid creating a mode with inconsistent rows, but don't block mode creation
        # altogether as that could cause error spam when parsing the remainder of the file.
        mode = Mode.create(mode_name, sem_type, mode_name_loc, (), collector)
    if add_mode:
        modes[mode_name] = mode


def _parse_instr(
    reader: DefLineReader,
    collector: ErrorCollector,
    args: InputLocation,
    global_namespace: GlobalNamespace,
    prefixes: PrefixMappingFactory,
    modes: Mapping[str, Mode],
    want_semantics: bool,
) -> Iterator[ModeRow]:
    for instr in _parse_mode_rows(
        reader,
        collector,
        global_namespace,
        prefixes,
        modes,
        None,
        MnemonicTokenizer.scan(args),
        _parse_instr_semantics,
        want_semantics,
    ):
        enc_def = instr.encoding
        enc_width = enc_def.encoding_width
        if enc_width is None:
            collector.error(
                "instruction encoding must not be empty", location=enc_def.encoding_location
            )
            # Do not yield the instruction, to avoid this problem from being
            # reporting again as a width inconsistency.
            continue
        aux_encoding_width = enc_def.aux_encoding_width
        if aux_encoding_width not in (enc_width, None):
            collector.error(
                f"auxiliary instruction encoding units are {aux_encoding_width} bits "
                f"wide, while first unit is {enc_width} bits wide",
                location=enc_def.aux_encoding_location,
            )
        yield instr


_re_header = re.compile(_name_tok + r"(?:\s+(.*\S)\s*)?$")


class InstructionSetParser:
    @classmethod
    def parse_file(
        cls, path: Traversable, logger: Logger | None = None, *, want_semantics: bool = True
    ) -> InstructionSet | None:
        """Parse a full instruction set from a single definition file."""

        if logger is None:
            logger = getLogger(__name__)
            logger.setLevel(WARNING)
        collector = ErrorCollector(logger)

        parser = cls(want_semantics=want_semantics)

        with DefLineReader.open(path) as reader:
            parser.parse(reader, collector)
            instr_set = parser.finalize(collector, reader.location)
            collector.summarize(str(path))

        return instr_set

    def __init__(self, *, want_semantics: bool = True):
        self.want_semantics = want_semantics
        self.global_builder = StatelessCodeBlockBuilder()
        self.global_namespace = global_namespace = GlobalNamespace()
        self.prefixes = PrefixMappingFactory(global_namespace)
        self.modes: dict[str, Mode] = {}
        self.instructions: list[ModeRow] = []

        self.attempt_creation = True
        """
        Should `finalize()` attempt to create an `InstructionSet` object?
        This flag will be set to `False` when errors are encountered during parsing,
        to reduce redundant error reporting and to avoid creating an incomplete
        instruction set.
        """

    def parse(self, reader: DefLineReader, collector: ErrorCollector) -> None:
        """Parse the top level of an instruction set definition."""

        num_errors_start = collector.problem_counter.num_errors

        global_namespace = self.global_namespace
        global_builder = self.global_builder
        prefixes = self.prefixes
        modes = self.modes
        instructions = self.instructions
        want_semantics = self.want_semantics

        for header in reader:
            if not header:
                continue
            match = header.match(_re_header)
            if match is None:
                collector.error("malformed line outside block", location=header)
                continue
            keyword = match.group(1)
            args = match.group(2) if match.has_group(2) else header.end_location
            def_type = keyword.text
            if def_type == "reg":
                _parse_regs(reader, collector, args, global_namespace, global_builder)
            elif def_type == "io":
                _parse_io(reader, collector, args, global_namespace)
            elif def_type == "prefix":
                _parse_prefix(reader, collector, args, global_namespace, prefixes)
            elif def_type == "func":
                _parse_func(reader, collector, args, global_namespace, want_semantics)
            elif def_type == "mode":
                _parse_mode(
                    reader,
                    collector,
                    args,
                    global_namespace,
                    prefixes,
                    modes,
                    want_semantics,
                )
            elif def_type == "instr":
                instructions += _parse_instr(
                    reader, collector, args, global_namespace, prefixes, modes, want_semantics
                )
            else:
                collector.error(f'unknown definition type "{def_type}"', location=keyword)
                reader.skip_block()

        if collector.problem_counter.num_errors != num_errors_start:
            self.attempt_creation = False

    def finalize(
        self, collector: ErrorCollector, location: InputLocation | None
    ) -> InstructionSet | None:
        """Perform final consistency checks and create the instruction set."""

        try:
            with collector.check():
                # Check that the program counter was defined.
                pc = self.global_namespace.program_counter
                if pc is None:
                    collector.error(
                        "no program counter defined: "
                        'a register or alias named "pc" is required',
                        location=location,
                    )
                    pc = bad_reference(IntType.int, "undefined program counter")

                if self.attempt_creation:
                    prefix_mapping = self.prefixes.create_mapping()
                    return InstructionSet.create(
                        self.instructions, pc, prefix_mapping, collector
                    )
                else:
                    return None
        except DelayedError:
            return None
