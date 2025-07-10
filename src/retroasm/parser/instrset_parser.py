from __future__ import annotations

import re
from collections import defaultdict
from collections.abc import Callable, Iterable, Iterator, Mapping, Sequence, Set
from importlib.resources.abc import Traversable
from logging import WARNING, Logger, getLogger
from operator import itemgetter
from typing import cast

from ..codeblock import FunctionBody
from ..codeblock_builder import (
    SemanticsCodeBlockBuilder,
    StatelessCodeBlockBuilder,
    returned_bits,
)
from ..decode import EncodedSegment, FixedEncoding, ParsedModeEntry, Prefix, decompose_encoding
from ..input import BadInput, DelayedError, ErrorCollector, InputLocation
from ..instrset import InstructionSet, PrefixMappingFactory
from ..mode import (
    Encoding,
    EncodingExpr,
    EncodingItem,
    EncodingMultiMatch,
    MatchPlaceholder,
    Mnemonic,
    Mode,
    ModeEntry,
    ValuePlaceholder,
)
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
    BadExpression,
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
                    except BadExpression as ex:
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
                                enc_items.append(_parse_encoding_expr(enc_node, namespace, {}))
                            except BadInput as ex:
                                collector.error(
                                    f"bad prefix encoding: {ex}", location=ex.locations
                                )
        except DelayedError:
            encoding = None
        else:
            encoding = Encoding(enc_items, {}, enc_loc)

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
    enc_node: ParseNode,
    enc_namespace: ReadOnlyNamespace,
    placeholders: Mapping[str, MatchPlaceholder | ValuePlaceholder],
) -> EncodingExpr:
    """
    Parse encoding node that is not a MultiMatchNode.
    Returns the parse result as an EncodingExpr.
    Raises BadInput if the node is invalid.
    """

    def explain_not_in_namespace(name: str, locations: tuple[InputLocation, ...]) -> None:
        placeholder = placeholders.get(name)
        if placeholder is None:
            # No placeholder with that name exists.
            return

        if placeholder.encoding_width is None:
            if isinstance(placeholder, MatchPlaceholder):
                raise BadInput(
                    f'cannot use placeholder "{name}" '
                    f'in encoding field, since mode "{placeholder.mode.name}" '
                    f"has an empty encoding sequence",
                    *locations,
                    placeholder.location,
                ) from None
            else:
                raise BadInput(
                    f'cannot use placeholder "{name}" in encoding field, '
                    "since its value is computed in the context",
                    *locations,
                    placeholder.location,
                ) from None

    builder = SemanticsCodeBlockBuilder()
    namespace = LocalNamespace(enc_namespace)
    try:
        enc_ref = build_reference(enc_node, namespace, builder)
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
    placeholders: Mapping[str, MatchPlaceholder | ValuePlaceholder],
) -> EncodingMultiMatch:
    """
    Parse an encoding node of type MultiMatchNode.
    Returns the parse result as an EncodingMultiMatch.
    Raises BadInput if the node is invalid.
    """
    name = enc_node.name
    try:
        placeholder = placeholders[name]
    except KeyError:
        raise BadInput(
            f'placeholder "{name}" does not exist in context', enc_node.tree_location
        ) from None
    if not isinstance(placeholder, MatchPlaceholder):
        raise BadInput(
            f'placeholder "{name}" does not represent a mode match',
            enc_node.tree_location,
            placeholder.location,
        )

    mode = placeholder.mode
    start = 1 if name in identifiers else 0
    return EncodingMultiMatch(name, mode, start, enc_node.tree_location)


def _parse_mode_encoding(
    enc_nodes: Iterable[ParseNode],
    placeholders: Mapping[str, MatchPlaceholder | ValuePlaceholder],
    global_namespace: GlobalNamespace,
    collector: ErrorCollector,
) -> Iterator[EncodingItem]:
    # Define placeholders in encoding namespace.
    enc_namespace = ContextNamespace(global_namespace)
    for name, placeholder in placeholders.items():
        if (enc_width := placeholder.encoding_width) is not None:
            imm_type = IntType.u(enc_width)
            imm_expr = ImmediateValue(name, imm_type)
            imm_ref = FixedValueReference(imm_expr, imm_type)
            try:
                enc_namespace.define(name, imm_ref, placeholder.location)
            except NameExistsError as ex:
                collector.error(f"bad placeholder: {ex}", location=ex.locations)

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
                    yield _parse_multi_match(enc_node, identifiers, placeholders)
                case _:
                    # Expression possibly containing single encoding field matches.
                    yield _parse_encoding_expr(enc_node, enc_namespace, placeholders)
        except BadInput as ex:
            collector.error(f"{ex}", location=ex.locations)


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


def _check_empty_multi_matches(
    enc_items: Iterable[EncodingItem],
    placeholders: Mapping[str, MatchPlaceholder | ValuePlaceholder],
    collector: ErrorCollector,
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
                    collector.warning(
                        f'mode "{mode.name}" does not contain encoding elements',
                        location=(enc_item.location, placeholders[enc_item.name].location),
                    )
                elif enc_item.start >= 1 and mode.aux_encoding_width is None:
                    collector.warning(
                        f'mode "{mode.name}" does not match auxiliary encoding units',
                        location=(enc_item.location, placeholders[enc_item.name].location),
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
                mode=Mode(aux_encoding_width=aux_width), encoded_length=enc_len, location=loc
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


def _combine_placeholder_encodings(
    decode_map: Mapping[str, Sequence[tuple[int, EncodedSegment]]],
    placeholders: Mapping[str, MatchPlaceholder | ValuePlaceholder],
    collector: ErrorCollector,
    location: InputLocation | None,
) -> Iterator[tuple[str, Sequence[EncodedSegment]]]:
    """
    Yield pairs of placeholder name and the locations where the placeholder
    resides in the encoded items.
    Each such location is a triple of index in the encoded items, bit offset
    within that item and width in bits.
    """

    imm_widths = {name: p.encoding_width for name, p in placeholders.items()}

    for name, slices in decode_map.items():
        imm_width = imm_widths[name]
        # Note: Encoding width is only None for empty encoding sequences,
        #       in which case the decode map will be empty as well.
        assert imm_width is not None, name
        decoding = []
        problems = []
        prev: Width = 0
        for imm_idx, enc_segment in sorted(slices):
            width = enc_segment.segment.width
            if prev < imm_idx:
                problems.append(f"gap at [{prev:d}:{imm_idx:d}]")
            elif prev > imm_idx:
                problems.append(f"overlap at [{imm_idx:d}:{min(imm_idx + width, prev)}]")
            prev = max(imm_idx + width, prev)
            decoding.append(enc_segment)
        if prev < imm_width:
            problems.append(f"gap at [{prev:d}:{imm_width:d}]")
        elif prev > imm_width:
            assert False, (name, slices)
        if problems:
            collector.error(
                f'cannot decode value for "{name}": {", ".join(problems)}', location=location
            )
        else:
            yield name, tuple(decoding)


def _check_decoding_order(
    encoding: Encoding,
    sequential_map: Mapping[str, Sequence[EncodedSegment]],
    placeholders: Mapping[str, MatchPlaceholder | ValuePlaceholder],
    collector: ErrorCollector,
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
        multi_idx = multi_match_indices.get(name)
        if multi_idx is None:
            continue
        matcher = encoding[multi_idx]
        if matcher.encoded_length is not None:
            continue

        # Are any parts of the placeholder are located after the multi-match?
        bad_idx = [
            enc_segment.enc_idx for enc_segment in decoding if enc_segment.enc_idx > multi_idx
        ]
        if bad_idx:
            placeholder = placeholders[name]
            assert isinstance(placeholder, MatchPlaceholder)
            mode = placeholder.mode
            collector.error(
                f'cannot match "{name}": mode "{mode.name}" has a variable encoding '
                f'length and (parts of) the placeholder "{name}" are placed after '
                f'the multi-match placeholder "{name}@"',
                location=[placeholder.location, matcher.location]
                + [encoding[idx].location for idx in bad_idx],
            )


def _parse_mode_decoding(
    encoding: Encoding,
    placeholders: Mapping[str, MatchPlaceholder | ValuePlaceholder],
    collector: ErrorCollector,
) -> tuple[Sequence[FixedEncoding], Mapping[str, Sequence[EncodedSegment]]] | None:
    """
    Construct a mapping that, given an encoded instruction, produces the
    values for context placeholders.
    """

    try:
        # Decompose the encoding expressions.
        fixed_matcher, decode_map = decompose_encoding(encoding)
    except BadInput as ex:
        collector.error(f"{ex}", location=ex.locations)
        return None

    multi_matches = {
        enc_item.name for enc_item in encoding if isinstance(enc_item, EncodingMultiMatch)
    }

    try:
        with collector.check():
            # Check placeholders that should be encoded but aren't.
            for name, placeholder in placeholders.items():
                if placeholder.encoding_width is None:
                    # Placeholder should not be encoded.
                    continue
                if name in multi_matches:
                    # Mode is matched using "X@" syntax.
                    continue
                if name not in decode_map:
                    # Note: We could instead treat missing placeholders as a special
                    #       case of insufficient bit coverage, but having a dedicated
                    #       error message is more clear for end users.
                    collector.error(
                        f'placeholder "{name}" does not occur in encoding',
                        location=(placeholder.location, encoding.location),
                    )
                elif isinstance(placeholder, MatchPlaceholder):
                    mode = placeholder.mode
                    if mode.aux_encoding_width is not None:
                        collector.error(
                            f'mode "{mode.name}" matches auxiliary encoding units, '
                            f'but there is no "{name}@" placeholder for them',
                            location=(placeholder.location, encoding.location),
                        )

            # Create a mapping to extract immediate values from encoded items.
            sequential_map = dict(
                _combine_placeholder_encodings(
                    decode_map, placeholders, collector, encoding.encoding_location
                )
            )
        with collector.check():
            # Check whether unknown-length multi-matches are blocking decoding.
            _check_decoding_order(encoding, sequential_map, placeholders, collector)
    except DelayedError:
        return None
    else:
        return fixed_matcher, sequential_map


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


def _parse_mode_entries(
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
) -> Iterator[ParsedModeEntry]:
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
                    placeholders = {
                        p.name: p
                        for p in parse_placeholders(ctx_loc, modes, global_namespace, collector)
                    }
            except DelayedError:
                # To avoid error spam, skip this line.
                continue
        else:
            placeholders = {}

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
                                _parse_mode_encoding(
                                    enc_nodes, placeholders, global_namespace, collector
                                )
                            )
                        with collector.check():
                            _check_aux_encoding_width(enc_items, collector)
                            _check_empty_multi_matches(enc_items, placeholders, collector)
                            _check_duplicate_multi_matches(enc_items, collector)
                        # Parse required decode flags.
                        with collector.check():
                            flags_required = set(
                                _parse_flags_required(enc_nodes, prefixes, collector)
                            )
                    except DelayedError:
                        encoding = None
                    else:
                        encoding = Encoding(enc_items, flags_required, enc_loc)
                if encoding is None:
                    decoding = None
                else:
                    decoding = _parse_mode_decoding(encoding, placeholders, collector)

                # Parse mnemonic.
                # Additional placeholders may be inserted by the mnemonic parser.
                mnem_tokens = mnem_base_tokens + MnemonicTokenizer.scan(mnem_loc)
                mnem_items = tuple(parse_mnemonic(mnem_tokens, placeholders, collector))
                if len(mnem_items) == 0:
                    collector.error("missing mnemonic", location=mnem_loc)
                else:
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
                        for name, placeholder in placeholders.items():
                            match placeholder:
                                case MatchPlaceholder(
                                    mode=Mode(semantics_type=sem_type), location=location
                                ):
                                    arg_type = (
                                        sem_type.type
                                        if isinstance(sem_type, ReferenceType)
                                        else sem_type
                                    )
                                    sem_namespace.add_argument(name, arg_type, location)
                                case ValuePlaceholder(
                                    type=arg_type, expr=expr, location=location
                                ):
                                    ref = sem_namespace.add_variable(name, arg_type, location)
                                    ref.emit_store(sem_builder, expr, None)

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
        except DelayedError:
            pass
        else:
            # TODO: An older version of this code created the ModeEntry
            #       with a None encoding or decoding; was that better or
            #       incorrect?
            if encoding is not None and decoding is not None:
                entry = ModeEntry(
                    encoding,
                    # If mnemonic was not defined, DelayedError will have been raised.
                    mnemonic,  # pylint: disable=possibly-used-before-assignment
                    semantics,
                    (p for p in placeholders.values() if isinstance(p, MatchPlaceholder)),
                    {
                        p.name: p.ref
                        for p in placeholders.values()
                        if isinstance(p, ValuePlaceholder)
                    },
                )
                yield ParsedModeEntry(entry, *decoding)


def _format_encoding_width(width: Width | None) -> str:
    return "empty" if width is None else f"{width} bits wide"


def _determine_encoding_width(
    entries: list[ParsedModeEntry], aux: bool, mode_name: str | None, collector: ErrorCollector
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

    width_freqs = defaultdict[int | None, int](int)
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
        enc_width, _ = max(width_freqs.items(), key=itemgetter(1))
        valid_widths: Iterable[Width | None]
        if aux:
            valid_widths = (enc_width, None)
        else:
            valid_widths = (enc_width,)
        bad_entry_indices = []
        for idx, entry in enumerate(entries):
            enc_def = entry.entry.encoding
            if cast(Width, getattr(enc_def, width_attr)) not in valid_widths:
                match_type = f"{'auxiliary ' if aux else ''}encoding match"
                actual_width = _format_encoding_width(getattr(enc_def, width_attr))
                expected_width = _format_encoding_width(enc_width)
                context = "for instructions" if mode_name is None else f'in mode "{mode_name}"'
                collector.error(
                    f"{match_type} is {actual_width}, while {expected_width} is "
                    f"dominant {context}",
                    location=(
                        enc_def.aux_encoding_location if aux else enc_def.encoding_location
                    ),
                )
                bad_entry_indices.append(idx)
        for idx in reversed(bad_entry_indices):
            del entries[idx]

    return enc_width


_re_mode_args = re.compile(_type_tok + r"\s" + _name_tok + r"$")


def _parse_mode(
    reader: DefLineReader,
    collector: ErrorCollector,
    args: InputLocation,
    global_namespace: GlobalNamespace,
    prefixes: PrefixMappingFactory,
    modes: dict[str, Mode],
    mode_entries: dict[str | None, list[ParsedModeEntry]],
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
    sem_type: None | IntType | ReferenceType
    try:
        sem_type = parse_type_decl(mode_type_loc.text)
    except ValueError as ex:
        collector.error(f"bad mode type: {ex}", location=mode_type_loc)
        sem_type = None
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

    # Parse entries.
    parsed_entries = list(
        _parse_mode_entries(
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
    enc_width = _determine_encoding_width(parsed_entries, False, mode_name, collector)
    aux_enc_width = _determine_encoding_width(parsed_entries, True, mode_name, collector)
    if add_mode:
        assert sem_type is not None
        entries = tuple(parsed_entry.entry for parsed_entry in parsed_entries)
        mode = Mode(mode_name, enc_width, aux_enc_width, sem_type, mode_name_loc, entries)
        modes[mode_name] = mode
        mode_entries[mode_name] = parsed_entries


def _parse_instr(
    reader: DefLineReader,
    collector: ErrorCollector,
    args: InputLocation,
    global_namespace: GlobalNamespace,
    prefixes: PrefixMappingFactory,
    modes: Mapping[str, Mode],
    want_semantics: bool,
) -> Iterator[ParsedModeEntry]:
    for instr in _parse_mode_entries(
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
        enc_def = instr.entry.encoding
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
        input_logger = ErrorCollector(logger)

        parser = cls(want_semantics=want_semantics)

        with DefLineReader.open(path) as reader:
            parser.parse(reader, input_logger)
            instr_set = parser.finalize(input_logger, reader.location)
            input_logger.summarize(str(path))

        return instr_set

    def __init__(self, *, want_semantics: bool = True):
        self.want_semantics = want_semantics
        self.global_builder = StatelessCodeBlockBuilder()
        self.global_namespace = global_namespace = GlobalNamespace()
        self.prefixes = PrefixMappingFactory(global_namespace)
        self.modes: dict[str, Mode] = {}
        self.mode_entries: dict[str | None, list[ParsedModeEntry]] = {None: []}

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
        mode_entries = self.mode_entries
        instructions = mode_entries[None]
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
                    mode_entries,
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

        # Check that the program counter was defined.
        pc = self.global_namespace.program_counter
        if pc is None:
            collector.error(
                'no program counter defined: a register or alias named "pc" is required',
                location=location,
            )

        instructions = self.mode_entries[None]
        enc_width = _determine_encoding_width(instructions, False, None, collector)
        any_aux = any(len(instr.entry.encoding) >= 2 for instr in instructions)
        aux_enc_width = enc_width if any_aux else None

        prefix_mapping = self.prefixes.create_mapping()

        if self.attempt_creation:
            if enc_width is None:
                # Since the last instruction with an identical encoding overrides
                # earlier ones, only degenerate instruction sets can have an empty
                # encoding: either the instruction set is empty or it has a single
                # instruction with no encoding.
                collector.error("no instruction encodings defined", location=location)
            elif pc is not None:
                try:
                    return InstructionSet(
                        enc_width, aux_enc_width, pc, prefix_mapping, self.mode_entries
                    )
                except ValueError as ex:
                    collector.error(
                        f"final validation of instruction set failed: {ex}", location=location
                    )

        return None
