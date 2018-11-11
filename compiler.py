from functools import reduce
import json
import yaml
import re

STATE_START_VALUE = 65535


def create_states_from_tokens():
    def merge_flags(flags, flag):
        if flag is None:
            return []
        if len(flags) == 0:
            return []
        return flags + [flag]

    def set_intermediate_state_flags(state, flag):
        global states
        [previous, lastPart] = split_pattern_remove_last_character(state.pattern)
        if len(previous) == 0:
            return
        previous_state = state_for_pattern(previous)
        previous_state.flags = merge_flags(previous_state.flags, flag)
        set_intermediate_state_flags(previous_state, flag)

    def create_intermediate_states(state, placeholders, flag):
        global states
        [previous, lastPart] = split_pattern_remove_last_character(state.pattern)
        new_state_created = False
        if len(previous) == 0:
            previous_state = STATE_START
        else:
            previous_state = state_for_pattern(previous)
            if previous_state is None:
                previous_state = State(previous, generate_name_for_pattern(previous))
                if flag is not None:
                    previous_state.flags = [flag]
                new_state_created = True
                states.append(previous_state)
        if lastPart in previous_state.next:
            raise Exception('Prefix %s has duplicate continuation to %s' % (previous, lastPart))
        previous_state.next[lastPart] = state
        m = re.search("^%(\d+)%$", lastPart)
        if m is not None:
            state.variable = placeholders[int(m.group(1))]
        if new_state_created:
            create_intermediate_states(previous_state, placeholders, flag)

    def create_final_state(name, spec):
        global next_final_state_value
        global states
        global flags
        global next_flag_value
        pattern = spec['pattern']
        if state_for_pattern(pattern) is not None:
            raise Exception('Pattern %s is not unique' % pattern)
        final_state = State(pattern, name, next_final_state_value)
        if 'leave' in spec:
            final_state.leave = spec['leave']
        next_final_state_value = next_final_state_value + 1
        states.append(final_state)
        if 'placeholders' not in spec:
            spec['placeholders'] = []
        if 'flag' not in spec:
            spec['flag'] = None
        final_state.flag = spec['flag']
        final_state.placeholders = spec['placeholders']
        if final_state.flag is not None and final_state.flag not in flags:
            flags[final_state.flag] = next_flag_value
            next_flag_value = next_flag_value * 2
        create_intermediate_states(final_state, spec['placeholders'], spec['flag'])

    def generate_enum_states():
        global states
        global enums
        removed_states = []
        for state in states:
            if state.variable is not None and state.variable['parse'] == "enum":
                original_next = state.next
                next_index = 0
                for key, spec in state.variable['enum'].items():
                    (parent_pattern, _) = split_pattern_remove_last_character(state.pattern)
                    pattern = parent_pattern + spec['pattern']
                    new_state = State(pattern, generate_name_for_pattern(pattern))
                    new_state.set_variable = (state.variable['variable'], "%sENUM_%s" % (token_prefix, key))
                    new_state.next = original_next
                    states.append(new_state)
                    create_intermediate_states(new_state, [], None)

                    enums.append((key, next_index))
                    next_index = next_index + 1

                removed_states.append(state)
                for previous_state in states:
                    remove_keys = []
                    for key, my_state in previous_state.next.items():
                        if state == my_state:
                            remove_keys.append(key)
                    for key in remove_keys:
                        del previous_state.next[key]
        for state in removed_states:
            states.remove(state)

    def optimalize_flags():
        global states
        for state in states:
            if state.pattern is None or len(state.flags) == 0:
                continue
            (previous_pattern, _) = split_pattern_remove_last_character(state.pattern)
            previous_state = state_for_pattern(previous_pattern)
            if previous_state.flags == state.flags:
                state.flags = []

    def number_intermediate_states():
        global next_final_state_value
        global states
        next_intermediate_state_value = next_final_state_value
        for state in states:
            if state.value == -1:
                state.value = next_intermediate_state_value
                next_intermediate_state_value = next_intermediate_state_value + 1

    def link_leave_states():
        for state in states:
            if state.leave is not None:
                state.leave = state_for_pattern(state.leave)

    for name, spec in tokens.items():
        create_final_state(name, spec)
    generate_enum_states()
    optimalize_flags()
    number_intermediate_states()
    link_leave_states()


def split_pattern_remove_last_character(pattern):
    if len(pattern) == 0:
        return None
    m = re.search("^(.*)(%(?:\d*)%|[^%])$", pattern, re.DOTALL)
    if m is None:
        raise Exception("Pattern '%s' should not be shortened" % pattern)
    return [m.group(1), m.group(2)]


def state_for_pattern(pattern):
    for state in states:
        if state.pattern == pattern:
            return state
    return None


def generate_name_for_pattern(pattern):
    name = ""
    for part in iterate_pattern_forward(pattern):
        name = name + generate_name_part_for_part(part)
    return "STATE_" + name


def generate_name_part_for_part(part):
    return {
        '\r': '_cr_',
        '\n': '_lf_',
        '.': '_dot_',
        ',': '_comma_',
        ':': '_colon_',
        ';': '_semi_',
        ' ': '_space_',
        '+': '_plus_',
        '-': '_minus_',
        '_': '_under_',
        '=': '_eq_',
        '<': '_lt_',
        '>': '_gt_',
        '(': '_lbracket_',
        ')': '_rbracket_',
        '[': '_lsbracket_',
        ']': '_rsbracket_',
        '"': '_quote_',
        '/': '_slash_',
        '\'': '_apos_',
        '%0%': '_var0_',
        '%1%': '_var1_',
        '%2%': '_var2_',
        '%3%': '_var3_',
        '%4%': '_var4_',
        '%5%': '_var5_',
        '%6%': '_var6_',
    }.get(part, part)


def iterate_pattern_forward(pattern):
    while len(pattern) > 0:
        m = re.search("^(%(?:\d*)%|[^%])(.*)$", pattern, re.DOTALL)
        yield m.group(1)
        pattern = m.group(2)


def cchar(c):
    if c == '\n':
        return "'\\n'"
    if c == '\r':
        return "'\\r'"
    if c == '\t':
        return "'\\t'"
    return "'%s'" % c


class State:
    def __init__(self, pattern, name, value = -1):
        if pattern is not None:
            (_, self.part) = split_pattern_remove_last_character(pattern)
        else:
            self.part = None
        self.pattern = pattern
        self.name = name
        self.value = value
        self.leave = STATE_START_VALUE
        self.next = dict()
        self.variable = None
        self.leave = None
        self.set_variable = None
        self.flags = []
        self.placeholders = []

    def __repr__(self):
        return "%s:%d %s" % (self.name, self.value, json.dumps(self.pattern))


def create_expects():
    global spec
    if 'expects' not in spec:
        return
    for name, expect_states in spec['expects'].items():
        expects[name] = expect_states


STATE_START = State(None, "STATE_START", STATE_START_VALUE)

states = [
    STATE_START
]

enums = []
flags = { "NEGATIVE_INTEGER": 1 }
expects = dict()

with open("tests/modem_parser.spec.yml", "r") as ymlstream:
    try:
        spec = yaml.load(ymlstream)
    except yaml.YAMLError as exc:
        print(exc)
        exit(1)

next_final_state_value = 0
next_flag_value = 2

parser_prefix = spec['parser']['parserPrefix']
token_prefix = spec['parser']['tokenPrefix']
parser_name = spec['parser']['name']
debug = 'debug' in spec['parser'] and spec['parser']['debug'] == True

state_name = spec['state']['name']
state_variables = spec['state']['variables']

tokens = spec['tokens']

def generate_source_file():
    global states
    global state_name
    global state_variables
    global parser_prefix
    global parser_name
    global token_prefix
    global next_final_state_value

    def only_fixed(it):
        for n, v in it:
            if len(n) == 1:
                yield (n, v)

    def only_variable(it):
        for n, v in it:
            m = re.search("^%(\d+)%$", n)
            if m is not None:
                yield (int(m.group(1)), v)

    def group_leave_states():
        leave = dict()
        for state in states:
            if state.leave is not None:
                if state.leave.name not in leave:
                    leave[state.leave.name] = []
                leave[state.leave.name].append(state)
        return leave

    def state_is_mergeable(state):
        next_keys = list(state.next.keys())
        return len(next_keys) == 1 \
               and state.next[next_keys[0]].value == state.value - 1 \
               and state.next[next_keys[0]].set_variable is None \
               and state.variable is None \
               and state.next[next_keys[0]].variable is None \
               and len(state.flags) == 0

    def collect_mergeable_states(states):
        by_next = dict()
        for state in filter(state_is_mergeable, states):
            next_key = list(state.next.keys())[0]
            if next_key not in by_next:
                by_next[next_key] = []
            by_next[next_key].append(state)
        return by_next

    def check_flags(s, i, state, exec):
        if len(state.flags) > 0:
            s = s + "%*sif ((parserState->flags & (%s)) == 0) {\n" % (i, "", " | ".join(map(lambda x: "%sFLAG_%s" % (token_prefix, x), state.flags)))
            s = s + "%*s  %s;\n" % (i, "", exec)
            s = s + "%*s}\n" % (i, "")
        return s

    def define_parse_decimal(s, type):
        s = s + "static bool parse_decimal_%s(%s *state, unsigned char c, %s *target) {\n" % (type.replace(' ', '_'), state_name, type)
        s = s + "  if (isdigit(c)) {\n"
        s = s + "    *target = 10 * *target + (c - '0');\n"
        s = s + "    state->len++;\n"
        s = s + "    return true;\n"
        s = s + "  }\n"
        s = s + "  return false;\n"
        s = s + "}\n"
        s = s + "\n"
        return s

    def define_parse_integer(s, type):
        s = s + "static bool parse_integer_%s(%s *state, unsigned char c, %s *target) {\n" % (type.replace(' ', '_'), state_name, type)
        s = s + "  if (!state->len && (c == '+' || c == '-')) {\n"
        s = s + "     if (c == '-') { state->flags |= %sFLAG_NEGATIVE_INTEGER; }\n" % token_prefix
        s = s + "  } else if (isdigit(c)) {\n"
        s = s + "    *target = 10 * *target + (c - '0');\n"
        s = s + "    if ((state->flags & %sFLAG_NEGATIVE_INTEGER) != 0 && c != '0') {\n" % token_prefix
        s = s + "      *target = -*target;\n"
        s = s + "       state->flags &= ~%sFLAG_NEGATIVE_INTEGER;\n" % token_prefix
        s = s + "    }\n"
        s = s + "  } else {\n"
        s = s + "    return false;\n"
        s = s + "  }\n"
        s = s + "  state->len++;\n"
        s = s + "  return true;\n"
        s = s + "}\n"
        s = s + "\n"
        return s

    def define_parse_hex(s, type):
        s = s + "static uint16_t parse_hex_%s(%s *state, unsigned char c, %s *target) {\n" % (type.replace(' ', '_'), state_name, type)
        s = s + "  if (isxdigit(c)) {\n"
        s = s + "    *target = 16 * *target + (c >= 'A' ? toupper(c) - 'A' + 10 : c - '0');\n"
        s = s + "    state->len++;\n"
        s = s + "    return true;\n"
        s = s + "  }\n"
        s = s + "  return false;\n"
        s = s + "}\n"
        s = s + "\n"
        return s

    def define_parse_string(s, type):
        s = s + "static bool parse_string_%s(%s *state, unsigned char c, uint8_t len, %s *target) {\n" % (type.replace(' ', '_'), state_name, type)
        s = s + "  if (state->len >= len) {\n"
        s = s + "    return false;\n"
        s = s + "  }\n"
        s = s + "  target[state->len++] = c;\n"
        s = s + "  target[state->len] = '\\0';\n"
        s = s + "  return true;\n"
        s = s + "}\n"
        s = s + "\n"
        return s

    def program_initial_parse_variable(s, variable, state_next):
        s = s + "          %s\n" % change_state(state, next_state)
        s = s + "          goto init_%s;\n" % variable['variable']
        return s

    def program_parse_variable(s, variable):
        if variable['parse'] != "string":
            s = s + "          if (parse_%s_%s(parserState, c, &(parserState->%s))) {\n" % (
                variable['parse'],
                state_variables[variable['variable']]["type"].replace(' ', '_'),
                variable['variable']
            )
            s = s + "            goto character_finished;\n"
            s = s + "          }\n"
        else:
            s = s + "          if (parse_%s_%s(parserState, c, %d, parserState->%s)) {\n" % (
                variable['parse'],
                state_variables[variable['variable']]["type"].replace(' ', '_'),
                state_variables[variable['variable']]["len"],
                variable['variable']
            )
            s = s + "            goto character_finished;\n"
            s = s + "          }\n"
        return s

    def change_state(current_state, next_state):
        s = "lo_state = %s%s & 0xff;" % (token_prefix, next_state.name)
        if current_state is None or current_state.value & 0xff00 != next_state.value & 0xff00:
            s = s + " hi_state = %s%s >> 8;" % (token_prefix, next_state.name)
        return s

    txt = ""
    txt = txt + "#include \"%s.h\"\n" % parser_name
    txt = txt + "#include <ctype.h>\n"
    txt = txt + "\n"
    txt = define_parse_decimal(txt, "uint8_t")
    txt = define_parse_decimal(txt, "uint16_t")
    txt = define_parse_integer(txt, "int8_t")
    txt = define_parse_integer(txt, "int16_t")
    txt = define_parse_hex(txt, "uint8_t")
    txt = define_parse_hex(txt, "uint16_t")
    txt = define_parse_string(txt, "unsigned char")
    txt = define_parse_string(txt, "char")
    txt = txt + "bool %sprocess_character(%s *parserState, unsigned char c) {\n" % ( parser_prefix, state_name )
    txt = txt + "  /* work with state low and high byte individually, it produces much smaller output */\n"
    txt = txt + "  uint8_t hi_state = parserState->state >> 8;\n"
    txt = txt + "  uint8_t lo_state = parserState->state & 0xff;\n"
    txt = txt + "  uint8_t only_match;\n"
    txt = txt + "\n"
    txt = txt + "  if (hi_state == 0 && lo_state <= %s_LAST_) {\n" % token_prefix
    txt = txt + "    return false;\n"
    txt = txt + "  }\n"
    txt = txt + "\n"
    txt = txt + "  for (;;) {\n"

    hi_state = 0
    while True:
        lo_states = list(filter(lambda x: (x.value >> 8) == hi_state, states))
        if len(lo_states) == 0:
            if hi_state < 255:
                hi_state = 255
                continue
            else:
                break

        txt = txt + "    if (hi_state == %d) {\n" % hi_state
        txt = txt + "      switch (lo_state) {\n"

        for state in filter(lambda x: not state_is_mergeable(x), lo_states):
            if state.value < next_final_state_value:
                continue
            txt = txt + "        case %s%s & 0xff:\n" % (token_prefix, state.name)

            fixed_nexts = list(only_fixed(state.next.items()))
            if len(fixed_nexts) > 0:
                txt = txt + "          switch (c) {\n"
                for c, next_state in fixed_nexts:
                    txt = txt + "            case %s:\n" % cchar(c)
                    txt = check_flags(txt, 14, next_state, "break")
                    if next_state.set_variable is not None:
                        txt = txt + "              parserState->%s = %s;\n" % next_state.set_variable
                    if state.value == next_state.value + 1:
                        txt = txt + "              goto decrement_state_and_character_finished;\n"
                    else:
                        txt = txt + "              %s\n" % change_state(state, next_state)
                        txt = txt + "              goto character_finished;\n"
                txt = txt + "          }\n"

            if state.variable is not None:
                txt = program_parse_variable(txt, state.variable)

            variable_nexts = list(only_variable(state.next.items()))
            if len(variable_nexts) > 1:
                raise Exception("We do not support multiple different variables from one state")

            if len(variable_nexts) == 1:
                (variable_index, next_state) = variable_nexts[0]
                txt = check_flags(txt, 10, next_state, "goto unrecognized_character")
                txt = program_initial_parse_variable(txt, next_state.variable, next_state.name)
            else:
                txt = txt + "          goto unrecognized_character;\n"
            txt = txt + "\n"

        for key, grouped_states in collect_mergeable_states(lo_states).items():
            for state in grouped_states:
                txt = txt + "        case %s%s & 0xff:\n" % (token_prefix, state.name)
            txt = txt + "          only_match = %s;\n" % cchar(key)
            txt = txt + "          goto if_match_decrement_state_and_character_finished;\n"
            txt = txt + "\n"

        txt = txt + "      }\n"
        txt = txt + "      goto unrecognized_character;\n"
        txt = txt + "    }\n"
        hi_state = hi_state + 1

    txt = txt + "\n"
    txt = txt + "    /* unreacheable */\n"
    txt = txt + "\n"
    for name, spec in state_variables.items():
        txt = txt + "init_%s:\n" % name
        if spec['type'] == "int8_t" or spec['type'] == "int16_t":
            txt = txt + "    parserState->flags &= ~%sFLAG_NEGATIVE_INTEGER;\n" % token_prefix
        if spec['type'] == "uint8_t" or spec['type'] == "int8_t" or spec['type'] == "uint16_t" or spec['type'] == "int16_t":
            txt = txt + "    parserState->%s = 0;\n" % name
        if spec['type'] == "unsigned char" or spec['type'] == "char":
            txt = txt + "    parserState->%s[0] = '\\0';\n" % name
        txt = txt + "    goto init_common;\n"
        txt = txt + "\n"
    txt = txt + "init_common:\n"
    txt = txt + "    parserState->len = 0;\n"
    txt = txt + "    continue;\n"
    txt = txt + "\n"
    txt = txt + "if_match_decrement_state_and_character_finished:\n"
    txt = txt + "    if (c == only_match) { goto decrement_state_and_character_finished; }\n"
    txt = txt + "\n"
    txt = txt + "unrecognized_character:\n"
    txt = txt + "    /* unrecognized initial character, ignore */\n"
    txt = txt + "    if (hi_state == 0xff && lo_state == 0xff) goto character_finished;  // if state equals STATE_START"
    txt = txt + "\n"
    txt = txt + "    /* unexpected character encountered, this fallback is not 100% correct, but should do */\n"
    txt = txt + "    hi_state = lo_state = 0xff;\n"
    txt = txt + "  }\n"
    txt = txt + "\n"
    txt = txt + "  /* unreacheable */\n"
    txt = txt + "\n"
    txt = txt + "decrement_state_and_character_finished:\n"
    txt = txt + "  lo_state--;\n"
    txt = txt + "  if (lo_state == 0xff) { hi_state --; }"
    txt = txt + "\n"
    txt = txt + "character_finished:\n"
    txt = txt + "  parserState->state = hi_state << 8 | lo_state;\n"
    txt = txt + "  return true;\n"
    txt = txt + "}\n"
    txt = txt + "\n"

    txt = txt + "void %sfinal_token_processed(%s *parserState) {\n" % (parser_prefix, state_name)
    txt = txt + "  switch (parserState->state) {\n"

    for leave, states in group_leave_states().items():
        for state in states:
            txt = txt + "    case %s%s:\n" % (token_prefix, state.name)
        txt = txt + "      parserState->state = %s%s;\n" % (token_prefix, leave)
        txt = txt + "      break;\n"
    txt = txt + "    default:\n"
    txt = txt + "      parserState->state = %sSTATE_START;\n" % token_prefix
    txt = txt + "      break;\n"
    txt = txt + "  }\n"
    txt = txt + "}\n"
    txt = txt + "\n"
    return txt


def generate_header_file():
    global states
    global state_name
    global state_variables
    global parser_prefix
    global token_prefix

    txt = ""
    txt = txt + "#include <stdint.h>\n"
    txt = txt + "#include <stdbool.h>\n"
    if debug:
      txt = txt + "#include <stdio.h>\n"
    txt = txt + "\n"
    l = 1 + reduce(lambda x, y: max(x, y), map(lambda x: len(x.name), states))
    for state in filter(lambda state: len(state.next) == 0 and state != STATE_START, states):
        txt = txt + "#define %s%*s %5d\n" % (token_prefix, -l, state.name, state.value)
    txt = txt + "\n"
    for enum in enums:
        txt = txt + "#define %sENUM_%*s %5d\n" % (token_prefix, -l+5, enum[0], enum[1])
    txt = txt + "\n"
    for flag, value in flags.items():
        txt = txt + "#define %sFLAG_%*s %5d\n" % (token_prefix, -l+5, flag, value)
    txt = txt + "\n"
    txt = txt + "#define %s%*s %5d\n" % (token_prefix, -l, "STATE_START", STATE_START_VALUE)
    txt = txt + "#define %s%*s %5d\n" % (token_prefix, -l, "_LAST_", next_final_state_value - 1)
    txt = txt + "\n"
    txt = txt + "typedef struct %s {\n" % state_name
    txt = txt + "  uint16_t\tstate;\n"
    txt = txt + "  uint8_t\tflags;\n"
    txt = txt + "  uint8_t\tlen;\n"
    for name, spec in state_variables.items():
        if 'len' in spec:
            txt = txt + "  %s\t%s[%d];\n" % (spec['type'], name, spec['len'] + 1)
        else:
            txt = txt + "  %s\t%s;\n" % (spec['type'], name)
    txt = txt + "} %s;\n" % state_name
    txt = txt + "\n"
    txt = txt + "#define %s_INITIALIZER { %sSTATE_START, 0 }\n" % (state_name, token_prefix)
    txt = txt + "\n"
    txt = txt + "extern bool %sprocess_character(%s *parserState, unsigned char c);\n" % (parser_prefix, state_name)
    txt = txt + "extern void %sfinal_token_processed(%s *parserState);\n" % (parser_prefix, state_name)
    txt = txt + "\n"
    txt = txt + "static inline bool %sis_final_token(uint16_t state) {\n" % parser_prefix
    txt = txt + "  return state <= %s_LAST_;\n" % token_prefix
    txt = txt + "}\n"
    txt = txt + "\n"
    for name, expected_states in expects.items():
        txt = txt + "static inline bool %sexpect_%s(%s *parserState) {\n" % (parser_prefix, name, state_name)
        txt = txt + "  uint16_t state = parserState->state;\n"
        txt = txt + "  if (%s) {\n" % " || ".join(map(lambda x: "state == %s%s" % (token_prefix, x), expected_states))
        txt = txt + "    return true;\n"
        txt = txt + "  }\n"
        txt = txt + "  if (%sis_final_token(state)) {\n" % parser_prefix
        txt = txt + "    %sfinal_token_processed(parserState);\n" % parser_prefix
        txt = txt + "  }\n"
        txt = txt + "  return false;\n"
        txt = txt + "}\n"
        txt = txt + "\n"

    if debug:
        txt = txt + "static int %sdebug_final_state(%s *parserState, unsigned char *buffer, size_t bufferLen) {\n" % (parser_prefix, state_name)
        txt = txt + "  switch (parserState->state) {\n"
        for state in filter(lambda s: len(s.next.keys()) == 0, states):
            txt = txt + "    case %s%s:\n" % (token_prefix, state.name)
            format_string = state.name
            format_args = []
            for var in state.placeholders:
                name = var.get('name', var['variable'])
                format_args.append("parserState->%s" % var['variable'])
                format_string = format_string + " %s=" % name
                if var['parse'] == "string":
                    format_string = format_string + "'%s'"
                elif var['parse'] == "decimal" or var['parse'] == "enum":
                    format_string = format_string + "%d"
                elif var['parse'] == "integer":
                    format_string = format_string + "%i"
                else:
                    format_string = format_string + "%04x"
            format_args = ", ".join(format_args)
            if len(format_args) > 0:
                format_args = ", " + format_args
            txt = txt + "      return snprintf(buffer, bufferLen, \"%s\"%s);\n" % (format_string, format_args)
        txt = txt + "  }\n"
        txt = txt + "  return -1;\n"
        txt = txt + "}\n"
        txt = txt + "\n"

    txt = txt + "\n"
    txt = txt + "/* INTERNALS */\n"
    for state in filter(lambda state: len(state.next) > 0 and state != STATE_START, states):
        txt = txt + "\n"
        txt = txt + "// PATTERN: %s\n" % json.dumps(state.pattern)
        if state.variable is not None:
            txt = txt + "// VARIABLE: %s %s\n" % (state.variable['parse'], state.variable['variable'])
        if state.set_variable is not None:
            txt = txt + "// SET VARIABLE: %s = %s\n" % (state.set_variable[0], state.set_variable[1])
        txt = txt + "// TRANSITIONS:\n"
        for key, next_state in state.next.items():
          txt = txt + "//   %s -> %s%s\n" % (json.dumps(key), token_prefix, next_state.name)
        txt = txt + "#define %s%*s %5d\n" % (token_prefix, -l, state.name, state.value)
    txt = txt + "\n"
    return txt



create_states_from_tokens()
create_expects()

# for state in states:
#     print(state)
#     for c, nextState in state.next.items():
#         print("  %s -> %s" % (json.dumps(c), nextState.name))

with open("tests/%s.h" % parser_name, "w") as f:
    f.write(generate_header_file())

with open("tests/%s.c" % parser_name, "w") as f:
    f.write(generate_source_file())
