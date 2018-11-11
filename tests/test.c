#include <stdio.h>
#include "modem_parser.h"

ModemParserState parserState = MODEMPARSERSTATE_INITIALIZER;

unsigned char *testString = "\r\n+CCLK: \"20/05/02,12:34:54-04\"\r\n\r\nAP:1,\"ahoj\"\r\nAP:1,\"\"\r\n+CSMINS: 20,5\r\n+CPIN: SIM PUK\r\n192.168.0.1\r\n\r\nOK\r\n";

int main() {
    unsigned char *c = testString;
    unsigned char buffer[1024];

    parserState.flags = 0xfe;

    while (*c) {
        modem_parser_process_character(&parserState, *c);
        printf("%02x %c: state:%6d is_final: %d\n", *c, *c >= 32 && *c < 128 ? *c : '.', parserState.state, modem_parser_is_final_token(parserState.state));

        if (modem_parser_is_final_token(parserState.state)) {
            int i = modem_parser_debug_final_state(&parserState, buffer, sizeof(buffer));
            if (i >= 0) {
                printf("TOKEN: %s\n", buffer);
            }
            modem_parser_final_token_processed(&parserState);
        }

        c++;
    }
}
