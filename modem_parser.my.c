#include "modem_parser.h"
#include <ctype.h>

static const PROGMEM uint8_t STATE__MACHINE[] = [13, 10, 148, 16, 43, 129, 144, 12, 65, 86, 16, 67, 71, 16, 69, 61, 16, 79, 54, 230, 2, 0, 133, 4, 46, 2, 64, 131, 102, 1, 133, 4, 46, 2, 65, 131, 102, 2, 133, 4, 46, 2, 66, 131, 102, 3, 133, 4, 13, 2, 67, 133, 24, 10, 115, 1, 75, 13, 133, 24, 10, 116, 1, 82, 82, 79, 82, 13, 133, 24, 10, 117, 1, 79, 78, 78, 69, 67, 84, 32, 79, 75, 13, 133, 24, 10, 118, 1, 136, 132, 4, 80, 144, 8, 84, 32, 58, 131, 102, 0, 133, 4, 44, 2, 64, 34, 131, 100, 7, 133, 4, 34, 4, 71, 13, 133, 24, 10, 119, 1, 32, 69, 82, 82, 79, 82, 13, 132, 16, 10, 249, 67, 144, 4, 67, 16, 71, 129, 144, 16, 80, 146, 16, 82, 105, 16, 83, 79, 76, 75, 58, 32, 34, 131, 102, 0, 133, 4, 47, 2, 64, 131, 102, 1, 133, 4, 47, 2, 65, 131, 102, 2, 133, 4, 44, 2, 66, 131, 102, 3, 133, 4, 58, 2, 67, 131, 102, 4, 133, 4, 58, 2, 68, 131, 102, 5, 133, 101, 6, 2, 69, 133, 4, 34, 1, 70, 13, 132, 16, 10, 170, 77, 73, 78, 83, 58, 32, 131, 102, 0, 133, 4, 44, 2, 64, 131, 102, 1, 133, 4, 13, 2, 65, 132, 16, 10, 145, 69, 71, 58, 32, 131, 102, 0, 133, 4, 44, 2, 64, 131, 102, 1, 133, 4, 13, 2, 65, 132, 16, 10, 122, 65, 84, 84, 58, 32, 131, 102, 0, 133, 4, 13, 2, 64, 132, 16, 10, 106, 73, 78, 58, 32, 137, 16, 80, 59, 4, 82, 16, 83, 19, 69, 65, 68, 89, 134, 16, 13, 128, 0, 72, 73, 77, 32, 80, 134, 4, 73, 16, 85, 21, 78, 136, 16, 13, 128, 0, 55, 4, 50, 134, 16, 13, 128, 0, 47, 75, 136, 16, 13, 128, 0, 40, 4, 50, 134, 16, 13, 128, 0, 32, 72, 95, 83, 73, 77, 32, 80, 134, 4, 73, 16, 85, 13, 78, 134, 16, 13, 128, 0, 12, 75, 133, 4, 13, 128, 0, 132, 16, 10, 14];

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

bool modem_parser_process_character(ModemParserState *parserState, unsigned char c) {
    const unsigned char *ip = STATE__MACHINE + parserState->state;
    uint8_t parser_flags = parserState->flags;
    for(;;) {
        uint8_t state_len = *(ip++);

        if (STATE_LEN_IS_TRIVIAL(state_len)) {
            // immediate match and advance
            if (c == state_len) {
                goto character_finished;
            } else {
                state = MODEM_STATE__START;
                continue;
            }
        }

        state_len = STATE_LEN(state_len);
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
            } else {

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
                    if (c != character) goto try_next_transition;
                    break;
                case TRANSITION_MATCH_INTEGER:
                    if (c != '+' && c != '-' && !isdigit(c)) goto try_next_transition;
                    break;
                case TRANSITION_MATCH_DECIMAL:
                    if (!isdigit(c)) goto try_next_transition;
                    break;
                case TRANSITION_MATCH_HEX:
                    if (!isxdigit(c)) goto try_next_transition;
                    break;
            }

            if (TRANSITION_HEADER_HAS_VAR(transition_header)) {
                void *variable;
                switch (OPERATION_VARIABLE(variable_operation)) {
                    case 0: variable = &(parserState->uint8_arg0); goto uint8_ops;
                    case 1: variable = &(parserState->uint8_arg1); goto uint8_ops;
                    case 2: variable = &(parserState->uint16_arg0); goto uint16_ops;
                    ...
                }

uint8_ops:
                switch (OPERATION(variable_operation)) {
                    case OPERATION_RESET:
                        parser_flags &= ~1;
                        ((uint8_t *)variable) = 0;
                        goto reset_common;
                    case OPERATION_STORE:
                        switch (TRANSITION_HEADER_MATCH_OP(transition_header)) {
                            case TRANSITION_MATCH_INTEGER:
                                if (parserState->len == 0) {
                                    if (c == '-') {
                                        parser_flags |= 1;
                                        goto end_of_operation;
                                    }
                                    if (c == '+') {
                                        goto end_of_operation;
                                    }
                                }
                                // fall thru
                            case TRANSITION_MATCH_DECIMAL:
                                *((uint8_t *)variable) = *((uint8_t *)variable) * 10 + (c - '0');
                                if (c != '0' && parser_flags & 0x01) {
                                    parser_flags &= ~1;
                                    ((int8_t *)variable) = -((int8_t *)variable);
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

uint16_ops:
                switch (OPERATION(variable_operation)) {
                    case OPERATION_RESET:
                        parser_flags &= ~1;
                        ((uint16_t *)variable) = 0;
                        goto reset_common;
                    case OPERATION_STORE:
                        switch (TRANSITION_HEADER_MATCH_OP(transition_header)) {
                            case TRANSITION_MATCH_INTEGER:
                                if (c == '-') {
                                    parser_flags |= 1;
                                    goto end_of_operation;
                                }
                                if (c == '+') {
                                    goto end_of_operation;
                                }
                                // fall thru
                            case TRANSITION_MATCH_DECIMAL:
                                *((uint16_t *)variable) = *((uint8_t *)variable) * 10 + (c - '0');
                                if (c != '0' && parser_flags & 0x01) {
                                    parser_flags &= ~1;
                                    ((int16_t *)variable) = -((int16_t *)variable);
                                }
                                goto store_common;
                            case TRANSITION_MATCH_HEX:
                                *((uint16_t *)variable) = *((uint8_t *)variable) << 4 + (c < 'A' ? c - '0' : 10 + c < 'a' ? c - 'A': c - 'a');
                                goto store_common;
                        }
                }
                goto end_of_operation;

string_ops:
                switch (OPERATION(variable_operation)) {
                    case OPERATION_RESET:
                        ((uint8_t *)variable) = '\0'';
                        goto reset_common;
                    case OPERATION_STORE:
                        *((uint8_t *)variable + parserState->len++) = c;
                        *((uint8_t *)variable + parserState->len) = '\0'';
                        goto store_common;
                }
                goto end_of_operation;

store_common:
                parserState->len ++;
                goto end_of_operation;

reset_common:
                parserState->len = 0;

end_of_operation:
            }

            switch (TRANSITION_HEADER_TARGET(transition_header)) {
                case TRANSITION_TARGET_STAY:
                    return;
                case TRANSITION_TARGET_STEP_ONE:
                    ip += state_len;
                    break;
                case TRANSITION_TARGET_REL8: // advance by uint8_t offset
                    ip = STATE__MACHINE + parserState->state + *ip;
                    break;
                case TRANSITION_TARGET_ABS16:
                    ip = STATE__MACHINE + (uint16_t *)ip;
                    break;
            }

            if (TRANSITION_HEADER_KEEP(transition_header)) {
                // try again for the same character
                break;
            }

            goto character_finished;

try_next_transition:
            if (TRANSITION_HEADER_TARGET_ARG(transition_header)) {
                ip ++;
                if (TRANSITION_HEADER_TARGET(transition_header) == TRANSITION_TARGET_ABS16) {
                    ip ++;
                }
            }
        }
    }

character_finished:
    parserState->state = ip - STATE__MACHINE;
    parserState->flags = parser_flags;
}

void modem_parser_final_token_processed(ModemParserState *parserState) {
  switch (parserState->state) {
    case MODEM_AP_RESPONSE:
    case MODEM_AT_ERROR:
    case MODEM_CCLK_RESPONSE:
    case MODEM_CSMINS_RESPONSE:
    case MODEM_CREG_RESPONSE:
    case MODEM_CGATT_RESPONSE:
    case MODEM_CPIN_RESPONSE:
      parserState->state = MODEM_STATE__cr_lf_;
      break;
    default:
      parserState->state = MODEM_STATE__START;
      break;
  }
}
