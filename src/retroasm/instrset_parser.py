from __future__ import annotations

from collections import defaultdict
from collections.abc import Callable, Iterable, Iterator, Mapping, Sequence
from importlib.abc import Traversable
from logging import WARNING, Logger, getLogger
from typing import AbstractSet, DefaultDict, cast
import re

from .codeblock import CodeBlock
from .codeblock_builder import SemanticsCodeBlockBuilder, StatelessCodeBlockBuilder
from .context_parser import MatchPlaceholderSpec, PlaceholderSpec, ValuePlaceholderSpec
from .decode import (
    EncodedSegment,
    FixedEncoding,
    ParsedModeEntry,
    Prefix,
    decompose_encoding,
)
from .expression_builder import (
    BadExpression,
    UnknownNameError,
    build_expression,
    build_reference,
    build_statement_eval,
    convert_definition,
)
from .expression_nodes import (
    DeclarationNode,
    DefinitionNode,
    FlagTestNode,
    IdentifierNode,
    MultiMatchNode,
    ParseNode,
    parse_int,
)
from .expression_parser import (
    parse_context,
    parse_expr,
    parse_expr_list,
    parse_regs,
    parse_statement,
)
from .function_builder import create_func
from .instrset import InstructionSet, PrefixMappingFactory
from .linereader import BadInput, DefLineReader, DelayedError, InputLocation
from .mode import (
    CodeTemplate,
    ComputedPlaceholder,
    Encoding,
    EncodingExpr,
    EncodingItem,
    EncodingMultiMatch,
    MatchPlaceholder,
    MnemItem,
    Mnemonic,
    Mode,
    ModeEntry,
    ValuePlaceholder,
)
from .namespace import (
    ContextNamespace,
    GlobalNamespace,
    LocalNamespace,
    NameExistsError,
    Namespace,
)
from .reference import Reference, bad_reference, int_reference
from .storage import ArgStorage, IOChannel, IOStorage, Variable
from .types import IntType, ReferenceType, Width, parse_type, parse_type_decl
from .utils import bad_type

_name_pat = r"[A-Za-z_][A-Za-z0-9_]*'?"
_name_tok = r"\s*(" + _name_pat + r")\s*"
_type_tok = r"\s*(" + _name_pat + r"&?)\s*"

_re_dot_sep = re.compile(r"\s*\.\s*")


def _parse_regs(
    reader: DefLineReader, args: InputLocation, global_namespace: GlobalNamespace
) -> None:
    if args:
        reader.error("register definition must have no arguments", location=args)

    for line in reader.iter_block():
        try:
            nodes = parse_regs(line)
        except BadInput as ex:
            reader.error("bad register definition line: %s", ex, location=ex.locations)
            continue

        last_type_location = None
        for node in nodes:
            # Parse type declaration.
            decl = node if isinstance(node, DeclarationNode) else node.decl
            decl_type = decl.type
            assert decl_type is not None
            type_location = decl_type.location
            try:
                reg_type = parse_type_decl(decl_type.name)
            except ValueError as ex:
                # Avoid reporting the same error twice.
                if type_location != last_type_location:
                    reader.error("%s", ex, location=type_location)
                continue
            last_type_location = type_location

            match node:
                case DeclarationNode():
                    # Define base register.
                    match reg_type:
                        case ReferenceType():
                            reader.error(
                                "base register cannot have a reference type",
                                location=(decl.name.location, type_location),
                            )
                        case typ:
                            try:
                                global_namespace.add_variable(
                                    decl.name.name, typ, decl.name.location
                                )
                            except NameExistsError as ex:
                                reader.error(
                                    "bad base register definition: %s",
                                    ex,
                                    location=ex.locations,
                                )
                case DefinitionNode(value=value):
                    # Define register alias.
                    name = decl.name.name
                    try:
                        ref = convert_definition(
                            decl.kind, name, reg_type, value, global_namespace
                        )
                    except BadExpression as ex:
                        reader.error(
                            "bad register alias: %s", ex, location=ex.locations
                        )
                        ref = bad_reference(reg_type)
                    try:
                        global_namespace.define(name, ref, decl.name.location)
                    except NameExistsError as ex:
                        reader.error(
                            "failed to define register alias: %s",
                            ex,
                            location=ex.locations,
                        )
                case node:
                    bad_type(node)


_re_comma_sep = re.compile(r"\s*,\s*")
_re_arg_decl = re.compile(_type_tok + r"\s" + _name_tok + r"$")


def _parse_typed_args(
    reader: DefLineReader, args: InputLocation, description: str
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
            reader.error(
                '%s %d not of the form "<type> <name>"',
                description,
                i,
                location=arg_loc,
            )
            continue

        type_loc, name_loc = arg_match.groups
        try:
            arg_type = parse_type_decl(type_loc.text)
        except ValueError as ex:
            reader.error(
                'bad %s %d ("%s"): %s',
                description,
                i,
                name_loc.text,
                ex,
                location=type_loc,
            )
        else:
            yield arg_type, type_loc, name_loc


def _parse_prefix(
    reader: DefLineReader,
    args: InputLocation,
    namespace: GlobalNamespace,
    factory: PrefixMappingFactory,
) -> None:
    header_location = reader.location

    # Parse header line.
    decode_flags = []
    try:
        with reader.check_errors():
            flag_type = IntType.u(1)
            for arg_type, arg_type_loc, arg_name_loc in _parse_typed_args(
                reader, args, "decode flag"
            ):
                match arg_type:
                    case ReferenceType():
                        reader.error(
                            "decode flag cannot be declared as a reference type",
                            location=arg_type_loc,
                        )
                        continue
                    case IntType() as typ:
                        if typ is not flag_type:
                            # Maybe in the future we'll support other types.
                            reader.error(
                                'decode flag of type "%s", expected "%s"',
                                arg_type,
                                flag_type,
                                location=arg_type_loc,
                            )
                    case typ:
                        bad_type(typ)
                arg_name = arg_name_loc.text
                try:
                    namespace.add_variable(arg_name, arg_type, arg_name_loc)
                except ValueError as ex:
                    reader.error(str(ex))
                except NameExistsError as ex:
                    reader.error(
                        "error defining decode flag: %s", ex, location=ex.locations
                    )
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
            reader.error(
                "wrong number of dot-separated fields in prefix line: "
                "expected 3, got %d",
                len(fields),
            )
            continue

        # Parse encoding.
        try:
            with reader.check_errors():
                if len(enc_loc) == 0:
                    reader.error("prefix encoding cannot be empty", location=enc_loc)
                else:
                    try:
                        enc_nodes = parse_expr_list(enc_loc)
                    except BadInput as ex:
                        reader.error(
                            "bad prefix encoding: %s", ex, location=ex.locations
                        )
                    else:
                        enc_items = []
                        for enc_node in enc_nodes:
                            try:
                                enc_items.append(
                                    _parse_encoding_expr(enc_node, namespace, {})
                                )
                            except BadInput as ex:
                                reader.error(
                                    "bad prefix encoding: %s", ex, location=ex.locations
                                )
        except DelayedError:
            encoding = None
        else:
            encoding = Encoding(enc_items, enc_loc)

        # Parse mnemonic.
        if len(mnem_loc) != 0:
            reader.warning("prefix mnemonics are not supported yet", location=mnem_loc)

        # Parse semantics.
        semantics: CodeBlock | None
        try:
            with reader.check_errors():
                if len(sem_loc) == 0:
                    reader.error(
                        'prefix semantics cannot be empty; use "nop" instead',
                        location=sem_loc,
                    )
                else:
                    sem_builder = SemanticsCodeBlockBuilder()
                    sem_namespace = LocalNamespace(namespace, sem_builder)
                    try:
                        _parse_instr_semantics(reader, sem_loc, sem_namespace)
                    except BadInput as ex:
                        reader.error(
                            "bad prefix semantics: %s", ex, location=ex.locations
                        )
                    else:
                        try:
                            semantics = sem_namespace.create_code_block(
                                ret_ref=None, log=reader
                            )
                        except ValueError:
                            # Error was logged inside createCodeBlock().
                            pass
        except DelayedError:
            semantics = None

        if encoding is not None and semantics is not None:
            prefixes.append(Prefix(encoding, semantics))

    try:
        factory.add_prefixes(decode_flags, prefixes)
    except ValueError as ex:
        reader.error(
            "validation of prefix block failed: %s", ex, location=header_location
        )
    except BadInput as ex:
        reader.error("validation of prefix block failed: %s", ex, location=ex.locations)


_re_io_line = re.compile(_name_tok + r"\s" + _name_tok + r"\[" + _name_tok + r"\]$")


def _parse_io(
    reader: DefLineReader, args: InputLocation, namespace: GlobalNamespace
) -> None:
    if args:
        reader.error("I/O definition must have no arguments", location=args)

    for line in reader.iter_block():
        match = line.match(_re_io_line)
        if match is None:
            reader.error("invalid I/O definition line")
        else:
            elem_type_loc, name_loc, addr_type_loc = match.groups

            try:
                elem_type: IntType | None = parse_type(elem_type_loc.text)
            except ValueError as ex:
                reader.error("bad I/O element type: %s", ex, location=elem_type_loc)
                elem_type = None

            try:
                addr_type: IntType | None = parse_type(addr_type_loc.text)
            except ValueError as ex:
                reader.error("bad I/O address type: %s", ex, location=addr_type_loc)
                addr_type = None

            if elem_type is None or addr_type is None:
                continue

            name = name_loc.text
            channel = IOChannel(name, elem_type, addr_type)
            try:
                namespace.define(name, channel, name_loc)
            except NameExistsError as ex:
                reader.error(
                    "error defining I/O channel: %s", ex, location=ex.locations
                )


_re_func_header = re.compile(r"(?:" + _type_tok + r"\s)?" + _name_tok + r"\((.*)\)$")


def _parse_func(
    reader: DefLineReader,
    header_args: InputLocation,
    namespace: GlobalNamespace,
    want_semantics: bool,
) -> None:
    # Parse header line.
    match = header_args.match(_re_func_header)
    if match is None:
        reader.error("invalid function header line", location=header_args)
        reader.skip_block()
        return

    # Parse return type.
    ret_type: IntType | ReferenceType | None
    ret_type_loc: InputLocation | None
    if match.has_group(1):
        ret_type_loc = match.group(1)
        try:
            ret_type = parse_type_decl(ret_type_loc.text)
        except ValueError as ex:
            reader.error("bad return type: %s", ex, location=ret_type_loc)
            reader.skip_block()
            return
    else:
        ret_type_loc = None
        ret_type = None

    # Parse arguments.
    args = {}
    try:
        with reader.check_errors():
            name_locations: dict[str, InputLocation] = {}
            for i, (arg_type, arg_type_loc_, arg_name_loc) in enumerate(
                _parse_typed_args(reader, match.group(3), "function argument"), 1
            ):
                arg_name = arg_name_loc.text
                if arg_name == "ret":
                    reader.error(
                        '"ret" is reserved for the return value; '
                        "it cannot be used as an argument name",
                        location=arg_name_loc,
                    )
                elif arg_name in name_locations:
                    reader.error(
                        "function argument %d has the same name as "
                        "an earlier argument: %s",
                        i,
                        arg_name,
                        location=(arg_name_loc, name_locations[arg_name]),
                    )
                else:
                    args[arg_name] = arg_type
                    name_locations[arg_name] = arg_name_loc
    except DelayedError:
        reader.skip_block()
        return

    if want_semantics:
        func_name_loc = match.group(2)
        func_name = func_name_loc.text

        # Parse body lines.
        func = create_func(
            reader,
            func_name_loc,
            ret_type,
            ret_type_loc,
            args,
            name_locations,
            namespace,
        )

        # Store function in namespace.
        try:
            namespace.define(func_name, func, func_name_loc)
        except NameExistsError as ex:
            reader.error("error declaring function: %s", ex, location=ex.locations)
    else:
        reader.skip_block()


def _parse_mode_context(
    ctx_loc: InputLocation,
    prefixes: PrefixMappingFactory,
    modes: Mapping[str, Mode],
    reader: DefLineReader,
) -> tuple[Mapping[str, PlaceholderSpec], set[str]]:
    placeholder_specs = {}
    flags_required = set()
    for node in parse_context(ctx_loc):
        match node:
            case (DeclarationNode() as decl) | DefinitionNode(decl=decl):
                name = decl.name.name
                decl_type = decl.type
                assert decl_type is not None
                type_name = decl_type.name

                # Figure out whether the name is a mode or type.
                mode = modes.get(type_name)
                placeholder: PlaceholderSpec
                if mode is not None:
                    placeholder = MatchPlaceholderSpec(decl, mode)
                    if isinstance(node, DefinitionNode):
                        reader.error(
                            "filter values for mode placeholders are not supported yet",
                            location=InputLocation.merge_span(
                                node.location, node.value.tree_location
                            ),
                        )
                else:
                    try:
                        # TODO: While the documentation says we do support defining
                        #       references in the context, parse_type() rejects
                        #       "<type>&"; we'd have to use parse_type_decl() instead.
                        typ = parse_type(type_name)
                    except ValueError:
                        reader.error(
                            'there is no type or mode named "%s"',
                            type_name,
                            location=decl_type.location,
                        )
                        continue
                    value = node.value if isinstance(node, DefinitionNode) else None
                    placeholder = ValuePlaceholderSpec(decl, typ, value)
                if name in placeholder_specs:
                    reader.error(
                        'multiple placeholders named "%s"',
                        name,
                        location=decl.name.location,
                    )
                else:
                    placeholder_specs[name] = placeholder
            case FlagTestNode(name=name):
                if prefixes.has_flag(name):
                    flags_required.add(name)
                else:
                    reader.error(
                        'there is no decode flag named "%s"',
                        name,
                        location=node.location,
                    )
            case node:
                bad_type(node)

    return placeholder_specs, flags_required


def _build_placeholders(
    placeholder_specs: Mapping[str, PlaceholderSpec],
    global_namespace: GlobalNamespace,
    reader: DefLineReader,
) -> Iterator[MatchPlaceholder | ValuePlaceholder]:
    """Create placeholders from a spec."""
    sem_namespace = ContextNamespace(global_namespace)

    for name, spec in placeholder_specs.items():
        decl = spec.decl
        sem_type = spec.type
        value = spec.value

        code = None
        if sem_type is not None and value is not None:
            placeholder_namespace = LocalNamespace(
                sem_namespace, SemanticsCodeBlockBuilder()
            )
            try:
                ref = convert_definition(
                    decl.kind, decl.name.name, sem_type, value, placeholder_namespace
                )
            except BadExpression as ex:
                reader.error("%s", ex, location=ex.locations)
            else:
                code = placeholder_namespace.create_code_block(ref)

        if sem_type is not None:
            match sem_type:
                case ReferenceType(type=argType) | (IntType() as argType):
                    try:
                        sem_namespace.add_argument(name, argType, decl.name.location)
                    except NameExistsError as ex:
                        reader.error("%s", ex, location=ex.locations)
                        continue
                case typ:
                    bad_type(typ)

        match spec:
            case ValuePlaceholderSpec():
                if sem_type is not None:
                    # TODO: We don't actually support references types yet.
                    #       See TODO in _parseModeContext().
                    assert isinstance(sem_type, IntType), sem_type
                    if code is None:
                        yield ValuePlaceholder(name, sem_type)
                    else:
                        yield ComputedPlaceholder(name, sem_type, code)
            case MatchPlaceholderSpec(mode=mode):
                yield MatchPlaceholder(name, mode)
            case spec:
                bad_type(spec)


def _parse_encoding_expr(
    enc_node: ParseNode,
    enc_namespace: Namespace,
    placeholder_specs: Mapping[str, PlaceholderSpec],
) -> EncodingExpr:
    """
    Parse encoding node that is not a MultiMatchNode.
    Returns the parse result as an EncodingExpr.
    Raises BadInput if the node is invalid.
    """
    namespace = LocalNamespace(enc_namespace, SemanticsCodeBlockBuilder())
    try:
        enc_ref = build_reference(enc_node, namespace)
    except BadInput as ex:
        if isinstance(ex, UnknownNameError):
            spec = placeholder_specs.get(ex.name)
            if spec is not None:
                if spec.encoding_width is None:
                    # Only MatchPlaceholderSpec.encodingWidth can return None.
                    assert isinstance(spec, MatchPlaceholderSpec), spec
                    raise BadInput(
                        f'cannot use placeholder "{ex.name}" '
                        f'in encoding field, since mode "{spec.mode.name}" '
                        f"has an empty encoding sequence",
                        *ex.locations,
                        spec.decl.tree_location,
                    ) from None
                if spec.value is not None:
                    raise BadInput(
                        f'cannot use placeholder "{ex.name}" '
                        f"in encoding field, since its value is "
                        f"computed in the context",
                        *ex.locations,
                        spec.value.tree_location,
                    ) from None
        raise BadInput(
            f"error in encoding: {ex}", enc_node.tree_location, *ex.locations
        ) from ex

    code = namespace.builder.create_code_block((enc_ref.bits,))
    if len(code.nodes) != 0:
        raise BadInput(
            "encoding expression accesses state or performs I/O", enc_node.tree_location
        )
    (enc_bits,) = code.returned
    for storage in enc_bits.iter_storages():
        match storage:
            case Variable():
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
    identifiers: AbstractSet[str],
    placeholder_specs: Mapping[str, PlaceholderSpec],
) -> EncodingMultiMatch:
    """
    Parse an encoding node of type MultiMatchNode.
    Returns the parse result as an EncodingMultiMatch.
    Raises BadInput if the node is invalid.
    """
    name = enc_node.name
    try:
        placeholder = placeholder_specs[name]
    except KeyError:
        raise BadInput(
            f'placeholder "{name}" does not exist in context', enc_node.tree_location
        ) from None
    if not isinstance(placeholder, MatchPlaceholderSpec):
        raise BadInput(
            f'placeholder "{name}" does not represent a mode match',
            enc_node.tree_location,
            placeholder.decl.tree_location,
        )

    mode = placeholder.mode
    start = 1 if name in identifiers else 0
    return EncodingMultiMatch(name, mode, start, enc_node.tree_location)


def _parse_mode_encoding(
    enc_nodes: Iterable[ParseNode],
    placeholder_specs: Mapping[str, PlaceholderSpec],
    global_namespace: GlobalNamespace,
    logger: DefLineReader,
) -> Iterator[EncodingItem]:
    # Define placeholders in encoding namespace.
    enc_namespace = ContextNamespace(global_namespace)
    for name, spec in placeholder_specs.items():
        if spec.value is None:
            enc_width = spec.encoding_width
            if enc_width is not None:
                enc_type = IntType.u(enc_width)
                location = spec.decl.name.location
                try:
                    enc_namespace.add_argument(name, enc_type, location)
                except NameExistsError as ex:
                    logger.error("bad placeholder: %s", ex, location=ex.locations)

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
            if isinstance(enc_node, MultiMatchNode):
                # Match multiple encoding fields as-is.
                yield _parse_multi_match(enc_node, identifiers, placeholder_specs)
            else:
                # Expression possibly containing single encoding field matches.
                yield _parse_encoding_expr(enc_node, enc_namespace, placeholder_specs)
        except BadInput as ex:
            logger.error("%s", ex, location=ex.locations)


def _check_empty_multi_matches(
    enc_items: Iterable[EncodingItem],
    placeholder_specs: Mapping[str, PlaceholderSpec],
    logger: DefLineReader,
) -> None:
    """
    Warn about multi-matches that always match zero elements.
    Technically there is nothing wrong with those, but it is probably not what
    the user intended.
    """
    for enc_item in enc_items:
        match enc_item:
            case EncodingMultiMatch(mode=mode):
                if mode.encoding_width is None:
                    logger.warning(
                        'mode "%s" does not contain encoding elements',
                        mode.name,
                        location=(
                            enc_item.location,
                            placeholder_specs[enc_item.name].decl.tree_location,
                        ),
                    )
                elif enc_item.start >= 1 and mode.aux_encoding_width is None:
                    logger.warning(
                        'mode "%s" does not match auxiliary encoding units',
                        mode.name,
                        location=(
                            enc_item.location,
                            placeholder_specs[enc_item.name].decl.tree_location,
                        ),
                    )


def _check_missing_placeholders(
    enc_items: Iterable[EncodingItem],
    placeholder_specs: Mapping[str, PlaceholderSpec],
    location: InputLocation,
    logger: DefLineReader,
) -> None:
    """
    Check that our encoding field contains sufficient placeholders to be able
    to make matches in all included mode tables.
    """
    # Take inventory of placeholders in the encoding.
    identifiers = set()
    multi_matches = set()
    for enc_item in enc_items:
        match enc_item:
            case EncodingExpr(bits=bits):
                for storage in bits.iter_storages():
                    if isinstance(storage, ArgStorage):
                        identifiers.add(storage.name)
            case EncodingMultiMatch(name=name):
                multi_matches.add(name)
            case item:
                bad_type(item)

    # Check whether all placeholders from the context occur in the encoding.
    for name, spec in placeholder_specs.items():
        match spec:
            case ValuePlaceholderSpec(value=value, decl=decl):
                if value is not None:
                    # The value is computed, so we don't need to encode it.
                    continue
                if name not in identifiers:
                    logger.error(
                        'value placeholder "%s" does not occur in encoding',
                        name,
                        location=(location, decl.tree_location),
                    )
            case MatchPlaceholderSpec(mode=mode, decl=decl):
                if mode.encoding_width is None:
                    # Mode has empty encoding, no match needed.
                    continue
                if name in multi_matches:
                    # Mode is matched using "X@" syntax.
                    continue
                if name not in identifiers:
                    logger.error(
                        'no placeholder "%s" for mode "%s" in encoding',
                        name,
                        mode.name,
                        location=(location, decl.tree_location),
                    )
                if mode.aux_encoding_width is not None:
                    logger.error(
                        'mode "%s" matches auxiliary encoding units, but there '
                        'is no "%s@" placeholder for them',
                        mode.name,
                        name,
                        location=(location, decl.tree_location),
                    )
            case spec:
                bad_type(spec)


def _check_aux_encoding_width(
    enc_items: Iterable[EncodingItem], logger: DefLineReader
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
            logger.error(
                "encoding item matches width %s, while first auxiliary "
                "encoding match has width %s",
                width,
                first_aux_width,
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
                mode=Mode(aux_encoding_width=aux_width),
                encoded_length=enc_len,
                location=loc,
            ) if first:
                if enc_len != 1 and aux_width is not None:
                    check_aux(aux_width, loc)
            case EncodingMultiMatch(
                mode=Mode(encoding_width=enc_width, aux_encoding_width=aux_width),
                start=start,
                location=loc,
            ):
                if start == 0:
                    check_aux(enc_width, loc)
                if aux_width is not None:
                    check_aux(aux_width, loc)
            case item:
                bad_type(item)
        first = False


def _check_duplicate_multi_matches(
    enc_items: Iterable[EncodingItem], logger: DefLineReader
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
                    logger.error(
                        'duplicate multi-match placeholder "%s@"',
                        name,
                        location=(location, claimed_multi_matches[name]),
                    )
                else:
                    claimed_multi_matches[name] = location


def _combine_placeholder_encodings(
    decode_map: Mapping[str, Sequence[tuple[int, EncodedSegment]]],
    placeholder_specs: Mapping[str, PlaceholderSpec],
    reader: DefLineReader,
) -> Iterator[tuple[str, Sequence[EncodedSegment]]]:
    """
    Yield pairs of placeholder name and the locations where the placeholder
    resides in the encoded items.
    Each such location is a triple of index in the encoded items, bit offset
    within that item and width in bits.
    """
    for name, slices in decode_map.items():
        placeholder_spec = placeholder_specs[name]
        imm_width = placeholder_spec.encoding_width
        # TODO: Can this actually never happen?
        assert imm_width is not None, placeholder_spec
        decoding = []
        problems = []
        prev: Width = 0
        for imm_idx, enc_segment in sorted(slices):
            width = enc_segment.segment.width
            if prev < imm_idx:
                problems.append(f"gap at [{prev:d}:{imm_idx:d}]")
            elif prev > imm_idx:
                problems.append(
                    f"overlap at [{imm_idx:d}:{min(imm_idx + width, prev)}]"
                )
            prev = max(imm_idx + width, prev)
            decoding.append(enc_segment)
        if prev < imm_width:
            problems.append(f"gap at [{prev:d}:{imm_width:d}]")
        elif prev > imm_width:
            assert False, (name, slices)
        if problems:
            reader.error(
                'cannot decode value for "%s": %s',
                name,
                ", ".join(problems),
                location=placeholder_spec.decl.tree_location,
            )
        else:
            yield name, tuple(decoding)


def _check_decoding_order(
    encoding: Encoding,
    sequential_map: Mapping[str, Sequence[EncodedSegment]],
    placeholder_specs: Mapping[str, PlaceholderSpec],
    reader: DefLineReader,
) -> None:
    """
    Verifies that there is an order in which placeholders can be decoded.
    Such an order might not exist because of circular dependencies.
    """
    # Find indices of multi-matches.
    multi_match_indices = {
        enc_elem.name: enc_idx
        for enc_idx, enc_elem in enumerate(encoding)
        if isinstance(enc_elem, EncodingMultiMatch)
    }

    for name, decoding in sequential_map.items():
        # Are we dealing with a multi-match of unknown length?
        placeholder_spec = placeholder_specs[name]
        if not isinstance(placeholder_spec, MatchPlaceholderSpec):
            continue
        multi_idx = multi_match_indices.get(name)
        if multi_idx is None:
            continue
        matcher = encoding[multi_idx]
        if matcher.encoded_length is not None:
            continue

        # Are any parts of the placeholder are located after the multi-match?
        bad_idx = [
            enc_segment.enc_idx
            for enc_segment in decoding
            if enc_segment.enc_idx > multi_idx
        ]
        if bad_idx:
            mode = placeholder_spec.mode
            reader.error(
                'cannot match "%s": mode "%s" has a variable encoding length '
                'and (parts of) the placeholder "%s" are placed after the '
                'multi-match placeholder "%s@"',
                name,
                mode.name,
                name,
                name,
                location=[placeholder_spec.decl.tree_location, matcher.location]
                + [encoding[idx].location for idx in bad_idx],
            )


def _parse_mode_decoding(
    encoding: Encoding,
    placeholder_specs: Mapping[str, PlaceholderSpec],
    reader: DefLineReader,
) -> tuple[Sequence[FixedEncoding], Mapping[str, Sequence[EncodedSegment]]] | None:
    """
    Construct a mapping that, given an encoded instruction, produces the
    values for context placeholders.
    """
    try:
        # Decompose the encoding expressions.
        fixed_matcher, decode_map = decompose_encoding(encoding)
    except BadInput as ex:
        reader.error("%s", ex, location=ex.locations)
        return None
    try:
        with reader.check_errors():
            # Create a mapping to extract immediate values from encoded items.
            sequential_map = dict(
                _combine_placeholder_encodings(decode_map, placeholder_specs, reader)
            )
        with reader.check_errors():
            # Check whether unknown-length multi-matches are blocking decoding.
            _check_decoding_order(encoding, sequential_map, placeholder_specs, reader)
    except DelayedError:
        return None
    else:
        return fixed_matcher, sequential_map


def _parse_mode_semantics(
    # While 'reader' is not used here, it is part of the signature that
    # _parse_mode_entries() uses to call this function.
    reader: DefLineReader,  # pylint: disable=unused-argument
    sem_loc: InputLocation,
    sem_namespace: LocalNamespace,
    mode_type: None | IntType | ReferenceType,
) -> Reference | None:
    semantics = parse_expr(sem_loc)
    if isinstance(mode_type, ReferenceType):
        ref = build_reference(semantics, sem_namespace)
        if ref.type != mode_type.type:
            raise BadInput(
                f"semantics type {ref.type} does not match mode type {mode_type.type}",
                sem_loc,
            )
        sem_namespace.define("ret", ref, sem_loc)
        return ref
    else:
        expr = build_expression(semantics, sem_namespace)
        # Note that modeType can be None because of earlier errors.
        if mode_type is None:
            return None
        ref = sem_namespace.add_variable("ret", mode_type, sem_loc)
        ref.emit_store(sem_namespace.builder, expr, sem_loc)
        return ref


def _parse_instr_semantics(
    reader: DefLineReader,
    sem_loc: InputLocation,
    namespace: LocalNamespace,
    mode_type: None | IntType | ReferenceType = None,
) -> None:
    assert mode_type is None, mode_type
    node = parse_statement(sem_loc)
    build_statement_eval(reader, "semantics field", namespace, node)


_re_mnemonic = re.compile(r"\w+'?|[$%]\w+|[^\w\s]")


def _parse_mnemonic(
    mnem_loc: InputLocation,
    placeholders: Iterable[MatchPlaceholder | ValuePlaceholder],
    reader: DefLineReader,
) -> Iterator[MnemItem]:
    placeholder_map = {p.name: p for p in placeholders}
    seen_placeholders: dict[str, InputLocation] = {}
    for mnem_elem in mnem_loc.find_locations(_re_mnemonic):
        text = mnem_elem.text
        placeholder = placeholder_map.get(text)
        if placeholder is None:
            if "0" <= text[0] <= "9" or text[0] in "$%":
                try:
                    value, width = parse_int(text)
                except ValueError as ex:
                    reader.error("%s", ex, location=mnem_elem)
                else:
                    yield int_reference(value, IntType.u(width))
            else:
                yield text
        elif text in seen_placeholders:
            # In theory we could support repeated placeholders, but the only
            # meaning that would make sense is that they would all match the
            # same mode entry or expression and I don't know of any situation
            # in which that would be a useful feature.
            reader.error(
                'placeholder "%s" occurs multiple times in mnemonic',
                text,
                location=(mnem_elem, seen_placeholders[text]),
            )
        else:
            yield placeholder
            seen_placeholders[text] = mnem_elem


def _parse_mode_entries(
    reader: DefLineReader,
    global_namespace: GlobalNamespace,
    prefixes: PrefixMappingFactory,
    modes: Mapping[str, Mode],
    mode_type: None | IntType | ReferenceType,
    mnem_base: tuple[str, ...],
    parse_sem: Callable[
        [DefLineReader, InputLocation, LocalNamespace, None | IntType | ReferenceType],
        Reference | None,
    ],
    want_semantics: bool,
) -> Iterator[ParsedModeEntry]:
    for line in reader.iter_block():
        # Split mode line into 4 fields.
        fields = list(line.split(_re_dot_sep))
        if len(fields) < 2:
            reader.error('field separator "." missing in mode line')
            continue
        if len(fields) > 4:
            reader.error("too many fields (%d) in mode line", len(fields))
            continue
        fields += [line.end_location] * (4 - len(fields))
        enc_loc, mnem_loc, sem_loc, ctx_loc = fields

        try:
            with reader.check_errors():
                # Parse context.
                if len(ctx_loc) != 0:
                    try:
                        with reader.check_errors():
                            placeholder_specs, flags_required = _parse_mode_context(
                                ctx_loc, prefixes, modes, reader
                            )
                            placeholders = tuple(
                                _build_placeholders(
                                    placeholder_specs, global_namespace, reader
                                )
                            )
                    except DelayedError:
                        # To avoid error spam, skip this line.
                        continue
                else:
                    placeholder_specs, flags_required = {}, set()
                    placeholders = ()

                # Parse encoding.
                enc_nodes: Iterable[ParseNode] | None
                if len(enc_loc) != 0:
                    try:
                        # Parse encoding field.
                        enc_nodes = parse_expr_list(enc_loc)
                    except BadInput as ex:
                        reader.error("error in encoding: %s", ex, location=ex.locations)
                        enc_nodes = None
                else:
                    enc_nodes = ()
                if enc_nodes is None:
                    encoding = None
                else:
                    try:
                        with reader.check_errors():
                            enc_items = tuple(
                                _parse_mode_encoding(
                                    enc_nodes,
                                    placeholder_specs,
                                    global_namespace,
                                    reader,
                                )
                            )
                        with reader.check_errors():
                            _check_aux_encoding_width(enc_items, reader)
                            _check_empty_multi_matches(
                                enc_items, placeholder_specs, reader
                            )
                            _check_duplicate_multi_matches(enc_items, reader)
                            _check_missing_placeholders(
                                enc_items, placeholder_specs, enc_loc, reader
                            )
                    except DelayedError:
                        encoding = None
                    else:
                        encoding = Encoding(enc_items, enc_loc)
                if encoding is None:
                    decoding = None
                else:
                    decoding = _parse_mode_decoding(encoding, placeholder_specs, reader)

                # Parse mnemonic.
                mnem_items = mnem_base + tuple(
                    _parse_mnemonic(mnem_loc, placeholders, reader)
                )
                if len(mnem_items) == 0:
                    reader.error("missing mnemonic", location=mnem_loc)
                else:
                    mnemonic = Mnemonic(mnem_items)

                # Parse semantics.
                if want_semantics:
                    sem_builder = SemanticsCodeBlockBuilder()
                    sem_namespace = LocalNamespace(global_namespace, sem_builder)
                    try:
                        # Define placeholders in semantics builder.
                        for name, spec in placeholder_specs.items():
                            match spec.type:
                                case None:
                                    # TODO: Is this guaranteed impossible?
                                    assert False
                                case ReferenceType(type=argType) | argType:
                                    location = spec.decl.name.location
                                    sem_namespace.add_argument(name, argType, location)

                        if len(sem_loc) == 0:
                            # Parse mnemonic field as semantics.
                            sem_loc = mnem_loc

                        sem_ref = parse_sem(reader, sem_loc, sem_namespace, mode_type)
                    except BadInput as ex:
                        reader.error(
                            "error in semantics: %s", ex, location=ex.locations
                        )
                        # This is the last field.
                        continue
                    try:
                        semantics: CodeBlock | None
                        semantics = sem_namespace.create_code_block(
                            ret_ref=sem_ref, log=reader
                        )
                    except ValueError:
                        # Error was already logged inside createCodeBlock().
                        pass
                else:
                    semantics = None
        except DelayedError:
            pass
        else:
            # TODO: An older version of this code created the ModeEntry
            #       with a None encoding or decoding; was that better or
            #       incorrect?
            if encoding is not None and decoding is not None:
                if semantics is None:
                    template = None
                else:
                    template = CodeTemplate(semantics, placeholders)
                entry = ModeEntry(
                    encoding,
                    mnemonic,
                    template,
                    placeholders,
                    frozenset(flags_required),
                )
                yield ParsedModeEntry(entry, *decoding)


def _format_encoding_width(width: Width | None) -> str:
    return "empty" if width is None else f"{width} bits wide"


def _determine_encoding_width(
    entries: list[ParsedModeEntry],
    aux: bool,
    mode_name: str | None,
    logger: DefLineReader,
) -> int | None:
    """
    Returns the common encoding width for the given list of mode entries.
    Entries with a deviating encoding width will be logged as errors on the
    given logger and removed from the entries list.
    If the 'aux' argument is False, the first matched unit width of each entry
    is checked, otherwise the width of auxiliary encoding units is checked.
    If the entries represent instructions, pass None for the mode name.
    """

    width_attr = "aux_encoding_width" if aux else "encoding_width"

    width_freqs: DefaultDict[int | None, int] = defaultdict(int)
    for entry in entries:
        width_freqs[getattr(entry.entry.encoding, width_attr)] += 1
    if aux:
        width_freqs.pop(None, None)

    if len(width_freqs) == 0:
        # Empty mode, only errors or aux check with no aux items.
        enc_width = None
    elif len(width_freqs) == 1:
        # Single type.
        (enc_width,) = width_freqs.keys()
    else:
        # Multiple widths; use one with the maximum frequency.
        enc_width, _ = max(width_freqs.items(), key=lambda item: item[1])
        valid_widths: Iterable[Width | None]
        if aux:
            valid_widths = (enc_width, None)
        else:
            valid_widths = (enc_width,)
        bad_entry_indices = []
        for idx, entry in enumerate(entries):
            enc_def = entry.entry.encoding
            if cast(Width, getattr(enc_def, width_attr)) not in valid_widths:
                logger.error(
                    "%sencoding match is %s, while %s is dominant %s",
                    ("auxiliary " if aux else ""),
                    _format_encoding_width(getattr(enc_def, width_attr)),
                    _format_encoding_width(enc_width),
                    (
                        "for instructions"
                        if mode_name is None
                        else f'in mode "{mode_name}"'
                    ),
                    location=(
                        enc_def.aux_encoding_location
                        if aux
                        else enc_def.encoding_location
                    ),
                )
                bad_entry_indices.append(idx)
        for idx in reversed(bad_entry_indices):
            del entries[idx]

    return enc_width


_re_mode_args = re.compile(_type_tok + r"\s" + _name_tok + r"$")


def _parse_mode(
    reader: DefLineReader,
    args: InputLocation,
    global_namespace: GlobalNamespace,
    prefixes: PrefixMappingFactory,
    modes: dict[str, Mode],
    mode_entries: dict[str | None, list[ParsedModeEntry]],
    want_semantics: bool,
) -> None:
    # Parse header line.
    match = args.match(_re_mode_args)
    if match is None:
        reader.error(
            'invalid mode arguments, expected "mode <type> <name>"', location=args
        )
        reader.skip_block()
        return
    mode_type_loc, mode_name_loc = match.groups
    sem_type: None | IntType | ReferenceType
    try:
        sem_type = parse_type_decl(mode_type_loc.text)
    except ValueError as ex:
        reader.error("bad mode type: %s", ex, location=mode_type_loc)
        sem_type = None

    # Check whether it's safe to add mode to namespace.
    mode_name = mode_name_loc.text
    add_mode = False
    if mode_name in modes:
        reader.error(
            'mode "%s" redefined; first definition was on line %d',
            mode_name,
            modes[mode_name].location.lineno,
            location=mode_name_loc,
        )
    else:
        try:
            parse_type(mode_name)
        except ValueError:
            add_mode = True
        else:
            reader.error(
                'mode name "%s" conflicts with type', mode_name, location=mode_name_loc
            )

    # Parse entries.
    parsed_entries = list(
        _parse_mode_entries(
            reader,
            global_namespace,
            prefixes,
            modes,
            sem_type,
            (),
            _parse_mode_semantics,
            want_semantics,
        )
    )

    # Create and remember mode object.
    enc_width = _determine_encoding_width(parsed_entries, False, mode_name, reader)
    aux_enc_width = _determine_encoding_width(parsed_entries, True, mode_name, reader)
    entries = tuple(parsed_entry.entry for parsed_entry in parsed_entries)
    mode = Mode(mode_name, enc_width, aux_enc_width, sem_type, mode_name_loc, entries)
    if add_mode:
        modes[mode_name] = mode
        mode_entries[mode_name] = parsed_entries


def _parse_instr(
    reader: DefLineReader,
    args: InputLocation,
    global_namespace: GlobalNamespace,
    prefixes: PrefixMappingFactory,
    modes: Mapping[str, Mode],
    want_semantics: bool,
) -> Iterator[ParsedModeEntry]:
    mnem_base = []
    for mnem_item in _parse_mnemonic(args, {}, reader):
        if isinstance(mnem_item, str):
            mnem_base.append(mnem_item)
        else:
            # TODO: The main reason to disallow this is that it would complicate
            #       the code to support it. Is that a good enough reason?
            reader.error("the mnemonic base cannot contain numbers", location=args)

    for instr in _parse_mode_entries(
        reader,
        global_namespace,
        prefixes,
        modes,
        None,
        tuple(mnem_base),
        _parse_instr_semantics,
        want_semantics,
    ):
        enc_def = instr.entry.encoding
        enc_width = enc_def.encoding_width
        if enc_width is None:
            reader.error(
                "instruction encoding must not be empty",
                location=enc_def.encoding_location,
            )
            # Do not yield the instruction, to avoid this problem from being
            # reporting again as a width inconsistency.
            continue
        aux_encoding_width = enc_def.aux_encoding_width
        if aux_encoding_width not in (enc_width, None):
            reader.error(
                "auxiliary instruction encoding units are %s bits wide, "
                "while first unit is %s bits wide",
                aux_encoding_width,
                enc_width,
                location=enc_def.aux_encoding_location,
            )
        yield instr


_re_header = re.compile(_name_tok + r"(?:\s+(.*\S)\s*)?$")


def parse_instr_set(
    path: Traversable, logger: Logger | None = None, want_semantics: bool = True
) -> InstructionSet | None:
    if logger is None:
        logger = getLogger("parse-instr")
        logger.setLevel(WARNING)

    global_builder = StatelessCodeBlockBuilder()
    global_namespace = GlobalNamespace(global_builder)
    prefixes = PrefixMappingFactory(global_namespace)
    modes: dict[str, Mode] = {}
    mode_entries: dict[str | None, list[ParsedModeEntry]] = {}
    instructions = mode_entries.setdefault(None, [])

    with DefLineReader.open(path, logger) as reader:
        for header in reader:
            if not header:
                continue
            match = header.match(_re_header)
            if match is None:
                reader.error("malformed line outside block")
                continue
            keyword = match.group(1)
            args = match.group(2) if match.has_group(2) else header.end_location
            def_type = keyword.text
            if def_type == "reg":
                _parse_regs(reader, args, global_namespace)
            elif def_type == "io":
                _parse_io(reader, args, global_namespace)
            elif def_type == "prefix":
                _parse_prefix(reader, args, global_namespace, prefixes)
            elif def_type == "func":
                _parse_func(reader, args, global_namespace, want_semantics)
            elif def_type == "mode":
                _parse_mode(
                    reader,
                    args,
                    global_namespace,
                    prefixes,
                    modes,
                    mode_entries,
                    want_semantics,
                )
            elif def_type == "instr":
                instructions += _parse_instr(
                    reader, args, global_namespace, prefixes, modes, want_semantics
                )
            else:
                reader.error('unknown definition type "%s"', def_type, location=keyword)
                reader.skip_block()

        # Check that the program counter was defined.
        try:
            pc = global_namespace["pc"]
        except KeyError:
            reader.error(
                "no program counter defined: "
                'a register or alias named "pc" is required'
            )
        else:
            assert isinstance(pc, Reference), pc

        enc_width = _determine_encoding_width(instructions, False, None, reader)
        any_aux = any(len(instr.entry.encoding) >= 2 for instr in instructions)
        aux_enc_width = enc_width if any_aux else None

        prefix_mapping = prefixes.create_mapping()

        instr_set = None
        if reader.problem_counter.num_errors == 0:
            try:
                if enc_width is None:
                    # Since the last instruction with an identical encoding overrides
                    # earlier ones, only degenerate instruction sets can have an empty
                    # encoding: either the instruction set is empty or it has a single
                    # instruction with no encoding.
                    raise ValueError("no encodings")
                instr_set = InstructionSet(
                    enc_width,
                    aux_enc_width,
                    global_namespace,
                    prefix_mapping,
                    mode_entries,
                )
            except ValueError as ex:
                reader.error("final validation of instruction set failed: %s", ex)

        reader.summarize()

    logger.debug(
        "regs: %s",
        ", ".join(
            f"{name} = {value!r}" for name, value in sorted(global_namespace.items())
        ),
    )

    return instr_set
