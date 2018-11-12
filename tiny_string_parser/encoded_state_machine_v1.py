from tiny_string_parser.base_writers import BaseHeaderFileWriter, BaseCodeFileWriter
from tiny_string_parser.compiler import CONDITION_ANY_LETTER, CONDITION_HEXADECIMAL_NUMBER, \
    CONDITION_DECIMAL_NUMBER, CONDITION_INTEGER_NUMBER
from functools import reduce

# binary encoding
#
# State:
#
#  byte 0: shortcut only-transition state : match 7bit character and advance one state up if successful
#
#     0CCCCCCC       1 character
#
#  byte 0: state header
#
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

class EncodedStateMachineBuilder:
    def __init__(self):
        self.offsets = dict()
        self.compiler = None

    def set_offset(self, state, offset):
        self.offsets[state.name] = offset

    def get_offset(self, state):
        return self.offsets.get(state.name)

    def build(self, compiler):
        self.compiler = compiler

        def recompute_offsets():
            offset = 0
            for state in compiler.states:
                for transition in state.outgoing_transitions:
                    if transition.target_state.order < transition.source_state.order:
                        transition.target_state_reference_is_long = True
                self.set_offset(state, offset)
                offset = offset + self.encoded_state_size(state)

        def find_too_long_jump():
            for state in compiler.states:
                for transition in state.outgoing_transitions:
                    if not transition.target_state_reference_is_long \
                            and self.get_offset(transition.target_state) > self.get_offset(transition.source_state) + 256:
                        transition.target_state_reference_is_long = True
                        return True
            return False

        while True:
            recompute_offsets()
            if not find_too_long_jump():
                break

    def encode(self):
        b = b""
        for state in filter(lambda x: not x.is_final, self.compiler.states):
            b = b + self.encode_state(state)
        return b

    def encoded_transition_size(self, transition):
        size = 1
        if len(transition.flags) > 0:
            size = size + 1
        if len(transition.condition) == 1:
            size = size + 1
        if transition.store_variable is not None or transition.reset_variable is not None:
            size = size + 1
        if transition.store_constant is not None:
            size = size + 1
        if transition.target_state_is_immediatelly_following() or transition.target_state_is_cycle():
            size = size + 0
        elif transition.target_state_reference_is_long:
            size = size + 2
        else:
            size = size + 1
        return size

    def encoded_state_size(self, state):
        if state.is_final or state.is_trivial_state():
            return 1

        size = 1
        for transition in state.outgoing_transitions:
            size = size + self.encoded_transition_size(transition)
        return size

    def encode_state(self, state):
        if state.is_final:
            raise Exception("Final states are not encodeable")
        if state.is_trivial_state():
            return state.outgoing_transitions[0].condition.encode('ascii')
        b = bytes([0b10000000 | (self.encoded_state_size(state) - 1)])
        for transition in state.outgoing_transitions:
            b = b + self.encode_transition(transition)
        assert len(b) == self.encoded_state_size(state)
        return b

    def encode_transition(self, transition):
        b = b""
        header = 0
        if len(transition.flags) > 0:
            bit_mask = reduce(lambda x, y: x | y, map(lambda x: 1 << self.compiler.all_flags[x].index, transition.flags))
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
            b = b + bytes([self.compiler.variables[transition.reset_variable].index])
            header = header | 0b00100000
        elif transition.store_constant is not None:
            b = b + bytes([0b10000000 + self.compiler.variables[transition.store_variable].index])
            b = b + bytes([0])  # todo constant value
            header = header | 0b00100000
        elif transition.store_variable is not None:
            b = b + bytes([0b01000000 + self.compiler.variables[transition.store_variable].index])
            header = header | 0b00100000
        if transition.target_state_is_immediatelly_following():
            header = header | 0b00001000
        elif transition.target_state_is_cycle():
            header = header | 0b00000000
        elif transition.target_state_reference_is_long:
            lo_byte = self.get_offset(transition.target_state) & 0xff
            hi_byte = self.get_offset(transition.target_state) >> 8
            b = b + bytes([lo_byte, hi_byte])
            header = header | 0b00011000
        else:
            diff = self.get_offset(transition.target_state) - self.get_offset(transition.source_state)
            b = b + bytes([diff])
            header = header | 0b00010000
        b = bytes([header]) + b
        assert len(b) == self.encoded_transition_size(transition)
        return b

    def enhance_state_label(self, state):
        if self.get_offset(state) is not None:
            return "\noffset %d" % self.get_offset(state)


class EncodedStateMachineCodeFileWriter(BaseCodeFileWriter):
    def __init__(self, compiler, builder):
        super().__init__(compiler)
        self.builder = builder

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
        for c in self.builder.encode():
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
            state_machine = state_machine + (
                "'%s'" % bytes([c]).decode('ascii') if is_first_in_state and c >= 32 and c <= 127 else "0x%02x" % c)

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
    uint8_t state_len;
    
    for(;;) {
go_again:
        state_len = *(ip++);

        if (STATE_LEN_IS_TRIVIAL(state_len)) {
            // immediate match and advance
            if (c == state_len) {
                goto character_finished;
            } else {
                goto unmatched_character;
            }
        }

        state_len = STATE_LEN(state_len);
        uint8_t original_state_len = state_len;
        while (state_len > 0) {
            uint8_t transition_header;
            uint8_t flags;
            uint8_t variable_operation;
            uint8_t variable_constant;
            uint8_t character;

            transition_header = *(ip++);
            state_len --;

            if (TRANSITION_HEADER_HAS_FLAGS(transition_header)) {
                flags = *(ip++);
                state_len --;
            }

            if (TRANSITION_HEADER_MATCH_OP(transition_header) == TRANSITION_MATCH_SPECIFIC_CHAR) {
                character = *(ip++);
                state_len --;
            }

            if (TRANSITION_HEADER_HAS_VAR(transition_header)) {
                variable_operation = *(ip++);
                state_len --;

                if (OPERATION_HAS_ARGUMENT(variable_operation)) {
                    variable_constant = *(ip++);
                    state_len --;
                }
            }

            if (TRANSITION_HEADER_HAS_FLAGS(transition_header)
                && (!(parser_flags & flags))) {
                goto try_next_transition;
            }

            switch (TRANSITION_HEADER_MATCH_OP(transition_header)) {
                case TRANSITION_MATCH_SPECIFIC_CHAR:
                    if (c != character) {
                        goto try_next_transition;
                    }
                    break;
                case TRANSITION_MATCH_INTEGER:
                    if (c != '+' && c != '-' && !(c >= '0' && c <= '9')) {
                        goto try_next_transition;
                    }
                    break;
                case TRANSITION_MATCH_DECIMAL:
                    if (!(c >= '0' && c <= '9')) {
                        goto try_next_transition;
                    }
                    break;
                case TRANSITION_MATCH_HEX:
                    if (!(c >= '0' && c <= '9') && !(c >= 'a' && c <= 'f') && !(c >= 'A' && c <= 'F')) {
                        goto try_next_transition;
                    }
                    break;
            }

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
                    ip -= original_state_len - state_len + 1;
                    break;
                case TRANSITION_TARGET_STEP_ONE:
                    ip += state_len;
                    break;
                case TRANSITION_TARGET_REL8: // advance by uint8_t offset
                    ip = ip - ((original_state_len - state_len + 1) - *ip);
                    state_len --;
                    break;
                case TRANSITION_TARGET_ABS16:
                    ip = STATE__MACHINE + *(uint16_t *)ip;
                    state_len -= 2;
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
        """ % (self.compiler.parser_name,
               state_machine,
               self.compiler.interface_prefix,
               self.compiler.state_variable_type,
               variable_selectors,
               self.compiler.const_prefix,
               self.compiler.states[1].name))


class EncodedStateMachineHeaderFileWriter(BaseHeaderFileWriter):
    def __init__(self, compiler, builder):
        super().__init__(lambda x: builder.get_offset(x), compiler)
        self.builder = builder


