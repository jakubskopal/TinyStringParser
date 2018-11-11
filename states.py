from functools import reduce
import json
import yaml
import re

CONDITION_DECIMAL_NUMBER = "2DEC"
CONDITION_INTEGER_NUMBER = "1INT"
CONDITION_HEXADECIMAL_NUMBER = "3HEX"
CONDITION_ANY_LETTER = "4ANY"
CONDITION_ENUM = "*ENUM*"

# binary encoding
#
# State:
#
#  byte 0: shortcut only-transition state : match 7bit character and advance one state up if successful
#
#     0CCCCCCC       1 character
#
#  byte 0: state header
#0
#     1LLLLLLL       0 length of state record
#
# Transition:
#
#  byte 0: header
#
#     +              bit 7    flags present
#     |+---          bit 6    keep character
#     ||+---         bit 5    variable operation present
#     |||+---        bit 4    --+ target state reference size: 0 = stay on place    2 = uint_8 relative
#     ||||+---       bit 3    --/                              1 = advance one step 3 = uint16_t absolute
#     |||||+---      bit 2    --+ 0 = character follows
#     ||||||+---     bit 1      | 1 = match integer   3 = match hex   5-7 = reserved
#     |||||||+---    bit 0    --+ 2 = match decimal   4 = match any
#     00000000
#
#  if flags present: following byte is a bitmask to flags
#
#  if character follows: following byte is a character to match
#
#  if variable operation present: following byte is:
#
#     +              bit 7    --+  operation: 0 = reset variable  2 = set constant
#     |+---          bit 6    --+             1 = append to variable
#     ||+---         bit 5    --+
#     |||+---        bit 4      |
#     ||||+---       bit 3      | variable id
#     |||||+---      bit 2      |
#     ||||||+---     bit 1      |
#     |||||||+---    bit 0    --+
#     00000000
#
#  if variable operation present with set constant operation, then byte constant follows
#
#  target state reference follows. either uint16_t or uint8_t
#

class BaseCodeFileWriter:
    def __init__(self, filename):
        self.filename = filename
        self.compiler = None

    def write(self, compiler):
        self.compiler = compiler
        with open(self.filename, "w") as fh:
            self._write_implementation(fh)
            self._write_final_token_processed_function(fh)

    def _write_final_token_processed_function(self, fh):
        fh.write("\n")
        fh.writelines([
            "void %sfinal_token_processed(%s *parserState) {\n" % (self.compiler.interface_prefix, self.compiler.state_variable_type),
            "  switch (parserState->state) {\n"
        ])

        leave_states = dict()
        for state in filter(lambda x: x.reset_state is not None, self.compiler.states):
            if state.reset_state.name not in leave_states:
                leave_states[state.reset_state.name] = []
            leave_states[state.reset_state.name].append(state)

        for leave_state, leaving_states in leave_states.items():
            for state in leaving_states:
                fh.write("    case %s%s:\n" % (self.compiler.const_prefix, state.name))
            fh.write("      parserState->state = %s%s;\n" % (self.compiler.const_prefix, leave_state))
            fh.write("      break;\n")

        fh.writelines([
            "    default:\n",
            "      parserState->state = %sSTATE__START;\n" % self.compiler.const_prefix,
            "      break;\n",
            "  }\n",
            "}\n"
        ])


class EncodedStateMachineCodeFileWriter(BaseCodeFileWriter):
    def __init__(self, filename):
        super().__init__(filename)

    def _write_implementation(self, fh):
        variable_selectors = ""
        for name, variable in self.compiler.variables.items():
            if variable.len is not None:
                selector = "parserState->%s" % variable.name;
            else:
                selector = "&(parserState->%s)" % variable.name;
            variable_selectors = variable_selectors + "                  case %d: variable = %s; goto %s_ops;\n" % (
                variable.index,
                selector,
                variable.type.replace(" ", "_"))

        state_machine = ""
        i = 0
        until_next_state = 0
        is_first_in_state = True
        for c in self.compiler.encode():
            if i > 0:
                state_machine = state_machine + ", "
            if until_next_state == 0:
                state_machine = state_machine + "\n                                          /* %4d */ " % i
                if c & 0x80 != 0:
                    until_next_state = c & 0x7f
                else:
                    until_next_state = 0
                is_first_in_state = True
            else:
                until_next_state = until_next_state - 1
                is_first_in_state = False
            i = i + 1
            state_machine = state_machine + ("'%s'" % bytes([c]).decode('ascii') if is_first_in_state and c >= 32 and c <= 127 else "0x%02x" % c)

        fh.write("""
#include "%s.h"
#include <ctype.h>

static const uint8_t STATE__MACHINE[] = { %s };

#define STATE_LEN_IS_TRIVIAL(x)         !((x)&0x80)
#define STATE_LEN(x)                    ((x)&0x7f)

#define TRANSITION_HEADER_HAS_FLAGS(x)  ((x)&0x80)
#define TRANSITION_HEADER_KEEP(x)       ((x)&0x40)
#define TRANSITION_HEADER_HAS_VAR(x)    ((x)&0x20)
#define TRANSITION_HEADER_TARGET_ARG(x) ((x)&0x10)
#define TRANSITION_HEADER_TARGET(x)     (((x)>>3)&0x03)
#define TRANSITION_HEADER_MATCH_OP(x)   ((x)&0x07)

#define TRANSITION_MATCH_SPECIFIC_CHAR  0
#define TRANSITION_MATCH_INTEGER        1
#define TRANSITION_MATCH_DECIMAL        2
#define TRANSITION_MATCH_HEX            3
#define TRANSITION_MATCH_ANY            4

#define TRANSITION_TARGET_STAY          0
#define TRANSITION_TARGET_STEP_ONE      1
#define TRANSITION_TARGET_REL8          2
#define TRANSITION_TARGET_ABS16         3

#define OPERATION_HAS_ARGUMENT(x)       ((x)&0x80)
#define OPERATION_VARIABLE(x)           ((x)&0x3f)
#define OPERATION(x)                    ((x)>>6)

#define OPERATION_RESET                 0
#define OPERATION_STORE                 1
#define OPERATION_SET                   2

#define FLAG_NEGATIVE                   1

bool %sprocess_character(%s *parserState, unsigned char c) {
    const unsigned char *ip = STATE__MACHINE + parserState->state;
    uint8_t parser_flags = parserState->flags;
    for(;;) {
go_again:
    
        printf("STATE %%d ", ip - STATE__MACHINE);
        uint8_t state_len = *(ip++);

        if (STATE_LEN_IS_TRIVIAL(state_len)) {
            // immediate match and advance
            if (c == state_len) {
                printf("TRANSITION QUICK MATCH '%%d' MATCHED STEPPING\\n", state_len);
                goto character_finished;
            } else {
                printf("TRANSITION QUICK MATCH '%%d' ", state_len);
                goto unmatched_character;
            }
        }

        state_len = STATE_LEN(state_len);
        uint8_t original_state_len = state_len;
        printf("LEN %%d\\n", state_len); 
        while (state_len > 0) {
            uint8_t transition_header;
            uint8_t flags;
            uint8_t variable_operation;
            uint8_t variable_constant;
            uint8_t character;

            printf("TRANSITION (%%d and %%d remaining) ", ip - STATE__MACHINE, state_len);

            transition_header = *(ip++);
            state_len --;

            if (TRANSITION_HEADER_HAS_FLAGS(transition_header)) {
                flags = *(ip++);
                state_len --;
                printf("FLAGS %%d ", flags);
            }

            if (TRANSITION_HEADER_MATCH_OP(transition_header) == TRANSITION_MATCH_SPECIFIC_CHAR) {
                character = *(ip++);
                state_len --;
                printf("MATCH CHARACTER '%%d' ", character);
            } else {
                printf("MATCH OPERATION %%d ", TRANSITION_HEADER_MATCH_OP(transition_header));
            }

            if (TRANSITION_HEADER_HAS_VAR(transition_header)) {
                variable_operation = *(ip++);
                state_len --;

                printf("OP %%d on %%d ", OPERATION(variable_operation), OPERATION_VARIABLE(variable_operation));

                if (OPERATION_HAS_ARGUMENT(variable_operation)) {
                    variable_constant = *(ip++);
                    state_len --;

                    printf("VALUE %%d ", variable_constant);
                }
            }
            
            if (TRANSITION_HEADER_HAS_FLAGS(transition_header)
                && (!(parser_flags & flags))) {
                printf("FLAGS DONT MATCH\\n");
                goto try_next_transition;
            }

            switch (TRANSITION_HEADER_MATCH_OP(transition_header)) {
                case TRANSITION_MATCH_SPECIFIC_CHAR:
                    if (c != character) {
                        printf("CHARACTER DOESNT MATCH\\n");
                        goto try_next_transition;
                    }
                    break;
                case TRANSITION_MATCH_INTEGER:
                    if (c != '+' && c != '-' && !isdigit(c)) {
                        printf("INTEGER DOESNT MATCH\\n");
                        goto try_next_transition;
                    }
                    break;
                case TRANSITION_MATCH_DECIMAL:
                    if (!isdigit(c)) {
                        printf("DECIMAL DOESNT MATCH\\n");
                        goto try_next_transition;
                    }
                    break;
                case TRANSITION_MATCH_HEX:
                    if (!isxdigit(c)) {
                        printf("HEX DOESNT MATCH\\n");
                        goto try_next_transition;
                    }
                    break;
            }
            
            printf("MATCHED ");

            if (TRANSITION_HEADER_HAS_VAR(transition_header)) {
                void *variable;
                switch (OPERATION_VARIABLE(variable_operation)) {
%s                }
                goto end_of_operation;

uint8_t_ops:
int8_t_ops:
                switch (OPERATION(variable_operation)) {
                    case OPERATION_RESET:
                        parser_flags &= ~FLAG_NEGATIVE;
                        *((uint8_t *)variable) = 0;
                        goto reset_common;
                    case OPERATION_STORE:
                        switch (TRANSITION_HEADER_MATCH_OP(transition_header)) {
                            case TRANSITION_MATCH_INTEGER:
                                if (parserState->len == 0) {
                                    if (c == '-') {
                                        parser_flags |= FLAG_NEGATIVE;
                                        goto end_of_operation;
                                    }
                                    if (c == '+') {
                                        goto end_of_operation;
                                    }
                                }
                                // fall thru
                            case TRANSITION_MATCH_DECIMAL:
                                *((uint8_t *)variable) = *((uint8_t *)variable) * 10 + (c - '0');
                                if (c != '0' && parser_flags & FLAG_NEGATIVE) {
                                    parser_flags &= ~FLAG_NEGATIVE;
                                    *((int8_t *)variable) = -*((int8_t *)variable);
                                }
                                goto store_common;
                            case TRANSITION_MATCH_HEX:
                                *((uint8_t *)variable) = *((uint8_t *)variable) << 4 + (c < 'A' ? c - '0' : 10 + c < 'a' ? c - 'A': c - 'a');
                                goto store_common;
                        }
                    case OPERATION_SET:
                        *((uint8_t *)variable) = variable_constant;
                        goto end_of_operation;
                }
                goto end_of_operation;

uint16_t_ops:
int16_t_ops:
                switch (OPERATION(variable_operation)) {
                    case OPERATION_RESET:
                        parser_flags &= ~FLAG_NEGATIVE;
                        *((uint16_t *)variable) = 0;
                        goto reset_common;
                    case OPERATION_STORE:
                        switch (TRANSITION_HEADER_MATCH_OP(transition_header)) {
                            case TRANSITION_MATCH_INTEGER:
                                if (c == '-') {
                                    parser_flags |= FLAG_NEGATIVE;
                                    goto end_of_operation;
                                }
                                if (c == '+') {
                                    goto end_of_operation;
                                }
                                // fall thru
                            case TRANSITION_MATCH_DECIMAL:
                                *((uint16_t *)variable) = *((uint8_t *)variable) * 10 + (c - '0');
                                if (c != '0' && parser_flags & FLAG_NEGATIVE) {
                                    parser_flags &= ~FLAG_NEGATIVE;
                                    *((int16_t *)variable) = -*((int16_t *)variable);
                                }
                                goto store_common;
                            case TRANSITION_MATCH_HEX:
                                *((uint16_t *)variable) = *((uint8_t *)variable) << 4 + (c < 'A' ? c - '0' : 10 + c < 'a' ? c - 'A': c - 'a');
                                goto store_common;
                        }
                }
                goto end_of_operation;

char_ops:
unsigned_char_ops:
                switch (OPERATION(variable_operation)) {
                    case OPERATION_RESET:
                        *((uint8_t *)variable) = '\\0';
                        goto reset_common;
                    case OPERATION_STORE:
                        *((uint8_t *)variable + parserState->len++) = c;
                        *((uint8_t *)variable + parserState->len) = '\\0';
                        goto end_of_operation;
                }
                goto end_of_operation;

store_common:
                parserState->len ++;
                goto end_of_operation;

reset_common:
                parserState->len = 0;

            }

end_of_operation:
            switch (TRANSITION_HEADER_TARGET(transition_header)) {
                case TRANSITION_TARGET_STAY:
                    printf("STAYING\\n");
                    ip -= original_state_len - state_len + 1;
                    break;
                case TRANSITION_TARGET_STEP_ONE:
                    printf("STEPPING\\n");
                    ip += state_len;
                    break;
                case TRANSITION_TARGET_REL8: // advance by uint8_t offset
                    ip = ip - (original_state_len - state_len + 1) + *ip;
                    state_len --;
                    printf("MOVING TO %%d\\n", ip - STATE__MACHINE);
                    break;
                case TRANSITION_TARGET_ABS16:
                    ip = STATE__MACHINE + *(uint16_t *)ip;
                    state_len -= 2;
                    printf("MOVING TO %%d\\n", ip - STATE__MACHINE);
                    break;
            }

            if (TRANSITION_HEADER_KEEP(transition_header)) {
                // try again for the same character
                goto go_again;
            }

            goto character_finished;

try_next_transition:
            if (TRANSITION_HEADER_TARGET_ARG(transition_header)) {
                ip ++;
                state_len --;
                if (TRANSITION_HEADER_TARGET(transition_header) == TRANSITION_TARGET_ABS16) {
                    ip ++;
                    state_len --;
                }
            }
        }
        
        bool felt_out_of_start_state;
unmatched_character:
        printf("CHARACTER NOT MATCHED\\n");
        felt_out_of_start_state = (ip == STATE__MACHINE + %s%s); 
        ip = STATE__MACHINE;

        if (felt_out_of_start_state) {
            goto character_finished;
        }
    }

character_finished:
    parserState->state = ip - STATE__MACHINE;
    parserState->flags = parser_flags;
    return true;
}
        """ % ( self.compiler.parser_name,
                state_machine,
                self.compiler.interface_prefix,
                self.compiler.state_variable_type,
                variable_selectors,
                self.compiler.const_prefix,
                self.compiler.states[1].name))


class BaseHeaderFileWriter:
    def __init__(self, state_id_getter, filename):
        self.state_id_getter = state_id_getter
        self.filename = filename
        self.compiler = None
        self.longest_name_length = None

    def write(self, compiler):
        self.compiler = compiler
        self._find_longest_name_length()

        with open(self.filename, "w") as fh:
            fh.writelines([
                "#include <stdint.h>\n",
                "#include <stdbool.h>\n"
            ])

            self._write_public_states(fh)
            self._write_flags(fh)
            self._write_typedef(fh)
            self._write_public_interface(fh)
            self._write_is_final_token_function(fh)
            self._write_expect_functions(fh)
            if self.compiler.debug:
                self._write_debug_final_state_function(fh)
            self._write_private_states(fh)

    def _find_longest_name_length(self):
        self.longest_name_length = reduce(lambda x, y: max(x, y), map(lambda x: len(x.name), self.compiler.states))

    def _write_public_states(self, fh):
        fh.write("\n")
        fh.write("#define %s%*s %d\n" % (self.compiler.const_prefix, -self.longest_name_length, "STATE__FIRST_FINAL", self.state_id_getter(self.compiler.final_final_state)))
        for state in filter(lambda x: x.is_final or x == self.compiler.start_state, self.compiler.states):
            fh.write("#define %s%*s %d\n" % (self.compiler.const_prefix, -self.longest_name_length, state.name, self.state_id_getter(state)))

    def _write_flags(self, fh):
        fh.write("\n")
        for flag in self.compiler.all_flags.values():
            fh.write("#define %s%*s %d\n" % (self.compiler.const_prefix, -self.longest_name_length, flag.identifier(), 1 << flag.index))

    def _write_typedef(self, fh):
        fh.write("\n")
        fh.write("typedef struct %s {\n" % self.compiler.state_variable_type)
        fh.write("  %-20s %s;\n" % ("uint16_t", "state"))
        fh.write("  %-20s %s;\n" % ("uint8_t", "flags"))
        fh.write("  %-20s %s;\n" % ("uint8_t", "len"))
        for key, spec in self.compiler.variables.items():
            if spec.len is not None:
                fh.write("  %-20s %s[%d];\n" % (spec.type, key, spec.len))
            else:
                fh.write("  %-20s %s;\n" % (spec.type, key))
        fh.write("} %s;\n" % self.compiler.state_variable_type)
        fh.write("\n");
        fh.write("#define %s_INITIALIZER { %sSTATE__START, 0 };\n" % (self.compiler.state_variable_type.upper(), self.compiler.const_prefix))

    def _write_public_interface(self, fh):
        fh.write("\n");
        fh.writelines([
            "extern bool %sprocess_character(%s *parserState, unsigned char c);\n" % (self.compiler.interface_prefix, self.compiler.state_variable_type),
            "extern void %sfinal_token_processed(%s *parserState);\n" % (self.compiler.interface_prefix, self.compiler.state_variable_type),
        ])

    def _write_is_final_token_function(self, fh):
        fh.write("\n");
        fh.writelines([
            "static inline bool %sis_final_token(uint16_t state) {\n" % self.compiler.interface_prefix,
            "  return state >= %sSTATE__FIRST_FINAL;\n" % self.compiler.const_prefix,
            "}\n"
        ])

    def _write_expect_functions(self, fh):
        for key, expect in self.compiler.expects.items():
            fh.write("\n");
            fh.writelines([
                "static inline bool %sexpect_%s(%s *parserState) {\n" % (self.compiler.interface_prefix, expect.name, self.compiler.state_variable_type),
                "  uint16_t state = parserState->state;\n",
                "  if (%s) {\n" % " || ".join(map(lambda x: "state == %s%s" % (self.compiler.const_prefix, x), expect.states)),
                "    return true;\n",
                "  }\n",
                "  if (%sis_final_token(state)) {\n" % self.compiler.interface_prefix,
                "    %sfinal_token_processed(parserState);\n" % self.compiler.interface_prefix,
                "  }\n",
                "  return false;\n",
                "}\n"
            ])

    def _write_debug_final_state_function(self, fh):
        fh.write("\n");
        fh.write("#include <stdio.h>\n")
        fh.write("\n");
        fh.writelines([
            "static int %sdebug_final_state(%s *parserState, unsigned char *buffer, size_t bufferLen) {\n" % ( self.compiler.interface_prefix, self.compiler.state_variable_type),
            "  switch (parserState->state) {\n"
        ])
        for state in filter(lambda s: s.is_final, self.compiler.states):
            fh.write("    case %s%s:\n" % (self.compiler.const_prefix, state.name))
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
            fh.write("      return snprintf(buffer, bufferLen, \"%s\"%s);\n" % (format_string, format_args))
        fh.writelines([
            "  }\n",
            "  return -1;\n",
            "}\n"
        ])

    def _write_private_states(self, fh):
        fh.write("\n")
        for state in filter(lambda x: not x.is_final and x != self.compiler.start_state, self.compiler.states):
            if state.order % 10 == 0:
                fh.write("\n")
            fh.write("#define %s%*s %d\n" % (self.compiler.const_prefix, -self.longest_name_length, state.name, self.state_id_getter(state)))


class EncodedStateMachineHeaderFileWriter(BaseHeaderFileWriter):
    def __init__(self, filename):
        super().__init__(lambda x: x.offset, filename)


class Expect:
    def __init__(self, name, states):
        self.name = name
        self.states = states


class Variable:
    def __init__(self, name, type, len, index):
        self.name = name
        self.type = type
        self.len = len
        self.index = index


class Flag:
    def __init__(self, name, index):
        self.name = name
        self.index = index

    def __eq__(self, other):
        return other.name == self.name

    def __hash__(self):
        return self.name.__hash__()

    def identifier(self):
        return "FLAG_%s" % self.name


class Transition:
    def __init__(self, condition, source_state, target_state):
        self.condition = condition
        self.source_state = source_state
        self.target_state = target_state
        self.keep_character = False
        self.reset_variable = None
        self.store_variable = None
        self.store_constant = None
        self.parse_enum = None
        self.flags = set()
        self.target_state_reference_is_long = False
        source_state.outgoing_transitions.append(self)
        target_state.incoming_transitions.append(self)

    def delete(self):
        self.source_state.outgoing_transitions.remove(self)
        self.target_state.incoming_transitions.remove(self)

    def target_state_is_immediatelly_following(self):
        return self.source_state.order == self.target_state.order - 1

    def target_state_is_cycle(self):
        return self.source_state == self.target_state

    def is_trivial_transition(self):
        return len(self.condition) == 1 \
                and self.condition <= "~" \
                and self.reset_variable is None \
                and self.store_variable is None \
                and (not self.keep_character) \
                and self.target_state_is_immediatelly_following()

    def encoded_size(self):
        size = 1
        if len(self.flags) > 0:
            size = size + 1
        if len(self.condition) == 1:
            size = size + 1
        if self.store_variable is not None or self.reset_variable is not None:
            size = size + 1
        if self.store_constant is not None:
            size = size + 1
        if self.target_state_is_immediatelly_following() or self.target_state_is_cycle():
            size = size + 0
        elif self.target_state_reference_is_long:
            size = size + 2
        else:
            size = size + 1
        return size


class State:
    def __init__(self, is_final, pattern, name):
        self.is_final = is_final
        self.is_visible = is_final
        self.pattern = pattern
        self.name = name

        self.order = None
        self.reduce_state = None
        self.reset_pattern = None
        self.reset_state = None
        self.flag = None
        self.offset = None
        self.outgoing_transitions = []
        self.incoming_transitions = []
        self.placeholders = []

    def outgoing_transition(self, condition):
        for _ in filter(lambda x: x.condition == condition, self.outgoing_transitions):
            return _
        return None

    def is_trivial_state(self):
        return len(self.outgoing_transitions) == 1 \
               and self.outgoing_transitions[0].is_trivial_transition()

    def encoded_size(self):
        if self.is_final or self.is_trivial_state():
            return 1

        size = 1
        for transition in self.outgoing_transitions:
            size = size + transition.encoded_size()
        return size


class Compiler:
    def __init__(self, spec):
        self.start_state = State(False, "", "STATE__START")
        self.start_state.is_visible = True
        self.spec = spec
        self.states = [ self.start_state ]
        self.all_flags = { "*": Flag("*", None) }
        self.next_flag_index = 1
        self.variables = dict()
        self.final_final_state = None
        self.expects = dict()

        parser_spec = self.spec.get('parser', dict())
        state_spec = self.spec.get('state', dict())
        self.const_prefix = parser_spec.get('tokenPrefix', '')
        self.interface_prefix = parser_spec.get('parserPrefix', '')
        self.state_variable_type = state_spec.get('name', 'State')
        self.parser_name = parser_spec.get('name', "parser")
        self.debug = parser_spec.get('debug', False)

    def compile(self):
        self._parse_variables()
        self._create_final_states()
        self._create_intermediate_states()
        self._create_enum_states()
        self._patch_variable_states()
        self._assign_state_orders()
        self._assign_transition_flags()
        self._assign_reset_states()
        self._connect_unrecognized_characters()
        self._sort_outgoing_transitions()
        self._assign_offsets()
        del self.all_flags["*"]
        self._parse_expects()

    def encoded_size(self):
        size = 0
        for state in filter(lambda x: not x.is_final, self.states):
            size = size + state.encoded_size()
        return size

    def encode(self):
        b = b""
        for state in filter(lambda x: not x.is_final, self.states):
            b = b + self.encode_state(state)
        return b
    
    def encode_state(self, state):
        if state.is_final:
            raise Exception("Final states are not encodeable")
        if state.is_trivial_state():
            return state.outgoing_transitions[0].condition.encode('ascii')
        b = bytes([0b10000000 | (state.encoded_size() - 1)])
        for transition in state.outgoing_transitions:
            b = b + self.encode_transition(transition)
        assert len(b) == state.encoded_size()
        return b

    def encode_transition(self, transition):
        b = b""
        header = 0
        if len(transition.flags) > 0:
            bit_mask = reduce(lambda x, y: x | y, map(lambda x: 1 << self.all_flags[x].index, transition.flags))
            b = b + bytes([bit_mask])
            header = header | 0b10000000
        if transition.keep_character:
            header = header | 0b01000000
        if len(transition.condition) == 1:
            b = b + transition.condition.encode('ascii')
        elif transition.condition == CONDITION_INTEGER_NUMBER:
            header = header | 1
        elif transition.condition == CONDITION_DECIMAL_NUMBER:
            header = header | 2
        elif transition.condition == CONDITION_HEXADECIMAL_NUMBER:
            header = header | 3
        elif transition.condition == CONDITION_ANY_LETTER:
            header = header | 4
        else:
            raise Exception("Do not know how to serialize condition %s" % transition.condition)
        if transition.reset_variable is not None:
            b = b + bytes([self.variables[transition.reset_variable].index])
            header = header | 0b00100000
        elif transition.store_constant is not None:
            b = b + bytes([0b10000000 + self.variables[transition.store_variable].index])
            b = b + bytes([0])  # todo constant value
            header = header | 0b00100000
        elif transition.store_variable is not None:
            b = b + bytes([0b01000000 + self.variables[transition.store_variable].index])
            header = header | 0b00100000
        if transition.target_state_is_immediatelly_following():
            header = header | 0b00001000
        elif transition.target_state_is_cycle():
            header = header | 0b00000000
        elif transition.target_state_reference_is_long:
            lo_byte = transition.target_state.offset & 0xff
            hi_byte = transition.target_state.offset >> 8
            b = b + bytes([lo_byte, hi_byte])
            header = header | 0b00011000
        else:
            diff = transition.target_state.offset - transition.source_state.offset
            b = b + bytes([diff])
            header = header | 0b00010000
        b = bytes([header]) + b
        assert len(b) == transition.encoded_size()
        return b

    def to_dot(self):
        s = ""
        s = s + "digraph STATE_MACHINE {\n"
        for state in self.states:
            s = s + "\n"
            label = (state.name if state.is_visible else "") + ((" [" + state.flag + "]") if state.flag is not None else "")
            if state.order is not None:
                label = label + " #%d" % state.order
            if state.offset is not None:
                label = label + "\no=%d" % state.offset
                if not state.is_final:
                    label = label + " s=%d" % state.encoded_size()
            if state.reset_state is not None:
                label = label + "\nreset to #%d" % state.reset_state.order
            elif state.reset_pattern is not None:
                label = label + "\nreset to %s" % json.dumps(state.reset_pattern)
            s = s + "  %s [label=%s %s]\n" % (
                state.name,
                json.dumps(label),
                "color=\"green\"" if state.is_trivial_state() else "")

            for transition in state.outgoing_transitions:
                label = json.dumps(transition.condition)
                if len(transition.flags) > 0:
                    label = label + "\nflags %s" % ", ".join(transition.flags)
                if transition.store_constant is not None:
                    label = label + "\n%s <- %s" % (transition.store_variable, transition.store_constant)
                elif transition.store_variable is not None:
                    label = label + "\n... -> %s" % transition.store_variable
                if transition.reset_variable is not None:
                    label = label + "\nreset %s" % transition.reset_variable
                if transition.keep_character:
                    label = label + "\nkeep character"
                s = s + "  %s -> %s [label=%s %s]\n" % (
                    state.name,
                    transition.target_state.name,
                    json.dumps(label),
                    "color=\"red\"" if transition.target_state_reference_is_long else \
                        "color=\"green\"" if transition.target_state_is_immediatelly_following() or transition.target_state_is_cycle() else "")
        s = s + "}\n"
        return s

    def state_by_pattern(self, pattern):
        for _ in filter(lambda x: x.pattern == pattern, self.states):
            return _
        return None

    def _parse_variables(self):
        index = 0
        for name, spec in self.spec['state']['variables'].items():
            variable = Variable(name, spec["type"], spec.get("len"), index)
            self.variables[name] = variable
            index = index + 1

    def _create_final_states(self):
        for state_name, state_spec in self.spec['tokens'].items():
            self._create_final_state(state_name, state_spec)

    def _create_intermediate_states(self):
        for state in list(filter(lambda x: x.is_final, self.states)):
            self._create_incoming_states(state)

    def _create_final_state(self, state_name, state_spec):
        pattern = state_spec["pattern"]
        flag = state_spec.get('flag', "*")
        placeholders = state_spec.get('placeholders', [])

        if self.state_by_pattern(pattern) is not None:
            raise Exception("Pattern '%s' is not unique trying to add token %s. Already claimed by state %s", pattern, state_name, self.state_by_pattern(pattern).name)
        state = State(True, pattern, state_name)
        state.reset_pattern = state_spec.get("leave")
        state.placeholders = placeholders
        if flag is not None:
            state.flag = flag
            if flag not in self.all_flags:
                self.all_flags[flag] = Flag(flag, self.next_flag_index)
                self.next_flag_index = self.next_flag_index + 1

        self.states.append(state)

    def _create_incoming_states(self, target_state):
        outgoing_pattern = target_state.pattern
        if len(outgoing_pattern) == 0:
            return
        (incoming_pattern, transition_item) = split_pattern_remove_last_item(outgoing_pattern)
        source_state = self.state_by_pattern(incoming_pattern)

        if source_state is None:
            source_state = State(False, incoming_pattern, generate_name_for_pattern(incoming_pattern))
            source_state.placeholders = target_state.placeholders
            self.states.append(source_state)
            self._create_incoming_states(source_state)

        self._create_transition(source_state, transition_item, target_state)

    def _patch_variable_states(self):
        for state in self.states:
            for transition in state.incoming_transitions[:]:
                if transition.store_variable is not None and transition.store_constant is None:
                    store_variable = transition.store_variable
                    condition = transition.condition
                    transition.reset_variable = store_variable
                    transition.store_variable = None
                    transition.keep_character = True
                    extra_transition = Transition(condition, state, state)
                    extra_transition.store_variable = store_variable

    def _create_enum_states(self):
        for state in self.states[:]:
            for transition in state.outgoing_transitions[:]:
                if transition.condition == CONDITION_ENUM:
                    transition.delete()
                    starting_state = transition.source_state
                    final_state = transition.target_state
                    for enum_id, enum_spec in transition.parse_enum.items():
                        pattern = starting_state.pattern + enum_spec['pattern']
                        enum_final_state = State(False, pattern, generate_name_for_pattern(pattern))
                        self._create_incoming_states(enum_final_state)
                        assert len(enum_final_state.incoming_transitions) == 1
                        incoming_transition = enum_final_state.incoming_transitions[0]
                        incoming_transition.delete()
                        transition_out_of_enum = Transition(CONDITION_ANY_LETTER, incoming_transition.source_state, final_state)
                        transition_out_of_enum.condition = incoming_transition.condition
                        transition_out_of_enum.store_constant = enum_id
                        transition_out_of_enum.store_variable = transition.store_variable

    def _sort_outgoing_transitions(self):
        for state in self.states:
            state.outgoing_transitions = list(sorted(state.outgoing_transitions, key=lambda transition: "%04d%s" % (len(transition.condition), transition.condition)))

    def _assign_transition_flags(self):
        def combine_transition_flags_recursively(state, incoming_transition):
            if state.flag is not None:
                flags = {state.flag}
            else:
                flags = set()
                for outgoing_transition in filter(lambda x: not x.target_state_is_cycle(), state.outgoing_transitions):
                    flags = flags | combine_transition_flags_recursively(outgoing_transition.target_state, outgoing_transition)
            if incoming_transition is not None:
                incoming_transition.flags = flags
            return flags

        def erase_transition_flags():
            reverse_states = self.states[:]
            reverse_states.reverse()
            for state in reverse_states:
                state.flag = None
                if state == self.start_state:
                    incoming_flags = set(self.all_flags.keys())
                else:
                    incoming_flags = reduce(lambda x, y: x & y, map(lambda x: x.flags, filter(lambda x: not x.target_state_is_cycle(), state.incoming_transitions)))
                for outgoing_transition in state.outgoing_transitions:
                    if "*" in outgoing_transition.flags or outgoing_transition.flags == incoming_flags:
                        outgoing_transition.flags = set()

        combine_transition_flags_recursively(self.start_state, None)
        erase_transition_flags()

    def _assign_state_orders(self):
        def assign_orders_recursively(state):
            nonlocal index
            if state.order is not None or state.is_final:
                return
            state.order = index
            index = index + 1
            for transition in state.outgoing_transitions:
                # make sure all source nodes of incoming transitions have orders before
                # we recurse to the subtree
                # that way we always travel the orders in ascending order and do not need
                # negative displacement
                if len(list(filter(lambda x: x.source_state.order is None and not x.target_state_is_cycle(), transition.target_state.incoming_transitions))) == 0:
                    assign_orders_recursively(transition.target_state)

        def assign_orders_to_final_states():
            nonlocal index
            for state in filter(lambda x: x.is_final, self.states):
                if self.final_final_state is None:
                    self.final_final_state = state
                state.order = index
                index = index + 1

        index = 0
        assign_orders_recursively(self.start_state)
        assign_orders_to_final_states()
        self.states = list(sorted(self.states, key=lambda x: x.order))

    def _assign_reset_states(self):
        for state in filter(lambda x: x.reset_pattern is not None, self.states):
            state.reset_state = self.state_by_pattern(state.reset_pattern)
            if state.reset_state is None:
                raise Exception("Leave pattern %s of state %s not part of tree" % (json.dumps(state.reset_pattern), state.name))

    def _connect_unrecognized_characters(self):
        def find_known_incoming_suffix(state, found_so_far):
            if len(state.incoming_transitions) == 1 and len(state.incoming_transitions[0].condition) == 1:
                found_so_far = state.incoming_transitions[0].condition + found_so_far
            return found_so_far

        for state in filter(lambda x: not x.is_final, self.states):
            known_incoming_suffix = find_known_incoming_suffix(state, "")
            while True:
                unrecognized_character_state = self.state_by_pattern(known_incoming_suffix)
                if unrecognized_character_state is not None:
                    break
                known_incoming_suffix = known_incoming_suffix[1:]
            if unrecognized_character_state != self.start_state \
                    and unrecognized_character_state != state\
                    and not (len(unrecognized_character_state.outgoing_transitions) == 1
                        and len(state.outgoing_transitions) == 1
                        and unrecognized_character_state.outgoing_transitions[0].condition == state.outgoing_transitions[0].condition):
                transition = Transition(CONDITION_ANY_LETTER, state, unrecognized_character_state)
                transition.keep_character = True

    def _assign_offsets(self):
        def recompute_offsets():
            offset = 0
            for state in self.states:
                for transition in state.outgoing_transitions:
                    if transition.target_state.order < transition.source_state.order:
                        transition.target_state_reference_is_long = True
                state.offset = offset
                offset = offset + state.encoded_size()

        def find_too_long_jump():
            for state in self.states:
                for transition in state.outgoing_transitions:
                    if not transition.target_state_reference_is_long \
                            and transition.target_state.offset > transition.source_state.offset + 256:
                        transition.target_state_reference_is_long = True
                        return True
            return False

        while True:
            recompute_offsets()
            if not find_too_long_jump():
                break

    def _parse_expects(self):
        for key, spec in self.spec.get('expects', dict()).items():
            self.expects[key] = Expect(key, spec)

    def _create_transition(self, source_state, transition_item, target_state):
        store_variable = None
        parse_enum = None
        m = re.search("^%(\\d+)%$", transition_item)
        if m is not None:
            placeholder_index = int(m.group(1))
            placeholder = target_state.placeholders[placeholder_index]
            parse = placeholder.get('parse', 'decimal')
            store_variable = placeholder['variable']
            if parse == "decimal":
                condition = CONDITION_DECIMAL_NUMBER
            elif parse == "integer":
                condition = CONDITION_INTEGER_NUMBER
            elif parse == "hexadecimal":
                condition = CONDITION_HEXADECIMAL_NUMBER
            elif parse == "string":
                condition = CONDITION_ANY_LETTER
            elif parse == "enum":
                condition = CONDITION_ENUM
                parse_enum = placeholder['enum']
            else:
                raise Exception("Unsupported parser %s" % parse)
        else:
            condition = transition_item
        transition = Transition(condition, source_state, target_state)
        transition.store_variable = store_variable
        transition.parse_enum = parse_enum
        return transition


def split_pattern_remove_last_item(pattern):
    if len(pattern) == 0:
        return None
    m = re.search("^(.*)(%(?:\d*)%|[^%])$", pattern, re.DOTALL)
    if m is None:
        raise Exception("Pattern '%s' could not be shortened" % pattern)
    return m.group(1), m.group(2)


def generate_name_for_pattern(pattern):
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

    name = ""
    for part in iterate_pattern_forward(pattern):
        name = name + generate_name_part_for_part(part)
    name = name.replace("__", "_")
    return "STATE_" + name


with open("tests/modem_parser.spec.yml", "r") as ymlstream:
    try:
        all_spec = yaml.load(ymlstream)
        compiler = Compiler(all_spec)
        compiler.compile()
        b = EncodedStateMachineHeaderFileWriter("tests/%s.h" % compiler.parser_name)
        b.write(compiler)
        b = EncodedStateMachineCodeFileWriter("tests/%s.c" % compiler.parser_name)
        b.write(compiler)
        print(compiler.to_dot())
        # print(compiler.encoded_size())
        # print(compiler.encode())
    except yaml.YAMLError as exc:
        print(exc)
        exit(1)
