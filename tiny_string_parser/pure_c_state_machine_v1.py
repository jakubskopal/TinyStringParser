# from tiny_string_parser.base_writers import BaseHeaderFileWriter, BaseCodeFileWriter
# from tiny_string_parser.compiler import CONDITION_ANY_LETTER, CONDITION_HEXADECIMAL_NUMBER, \
#     CONDITION_DECIMAL_NUMBER, CONDITION_INTEGER_NUMBER
# from functools import reduce
#
# class PureCStateMachineBuilder:
#     def enhance_state_label(self, state):
#         return ""
#
# class PureCStateMachineHeaderFileWriter(BaseHeaderFileWriter):
#     def __init__(self, compiler):
#         super().__init__(lambda x: x.order, compiler)
#
#
# class EncodedStateMachineCodeFileWriter(BaseCodeFileWriter):
#     def __init__(self, compiler):
#         super().__init__(compiler)
#
#     def _define_parse_decimal(self, fh, type):
#         fh.writelines([
#             "\n",
#             "static void parse_decimal_%s(%s *state, unsigned char c, %s *target) {\n" % (type.replace(' ', '_'), self.compiler.state_variable_type, type),
#             "  *target = 10 * *target + (c - '0');\n",
#             "}\n"
#         ])
#
#     def _define_parse_integer(self, fh, type):
#         fh.writelines([
#             "\n",
#             "static void parse_integer_%s(%s *state, unsigned char c, %s *target) {\n" % (type.replace(' ', '_'), self.compiler.state_variable_type, type),
#             "  if (c == '-') { state->flags |= %sFLAG_NEGATIVE_INTEGER; }\n" % self.compiler.const_prefix,
#             "  else if (c != '+') {\n",
#             "    *target = 10 * *target + (c - '0');\n",
#             "    if (c != '0' && state->flags & %sFLAG_NEGATIVE_INTEGER) {\n" % self.compiler.const_prefix,
#             "      *target = -*target;\n"
#             "       state->flags &= ~%sFLAG_NEGATIVE_INTEGER;\n" % self.compiler.const_prefix,
#             "    }\n"
#             "  }\n",
#             "}\n"
#         ])
#
#     def _define_parse_hex(self, fh, type):
#         fh.writelines([
#             "\n",
#             "static void parse_hex_%s(%s *state, unsigned char c, %s *target) {\n" % (type.replace(' ', '_'), self.compiler.state_variable_type, type),
#             "  *target = 16 * *target + (c < 'A' ? c - '0' : c < 'a' ? c - 'A' + 10: c - 'a' + 10);\n",
#             "}\n"
#         ])
#
#     def _define_parse_string(self, fh, type):
#         fh.writelines([
#             "\n",
#             "static void parse_string_%s(%s *state, unsigned char c, uint8_t len, %s *target) {\n" % (type.replace(' ', '_'), self.compiler.state_variable_type, type),
#             "  if (state->len < len) {\n",
#             "    target[state->len] = c;\n",
#             "    target[state->len+1] = '\\0';\n",
#             "  }\n",
#             "}\n"
#         ])
#
#     def _check_flags(self, fh, i, state, exec):
#         if len(state.flags) > 0:
#             fh.writelines([
#                 "%*sif ((parserState->flags & (%s)) == 0) {\n" % (i, "", " | ".join(map(lambda x: "%sFLAG_%s" % (self.compiler.const_prefix, x), state.flags))),
#                 "%*s  %s;\n" % (i, "", exec),
#                 "%*s}\n" % (i, "")
#             ])
#
#     def _write_operation_implementation(self, fh, i, transition):
#         if transition.store_constant is not None:
#             fh.write("%*sparserState->%s = %s" % i, "", transition.store_variable, transition.store_constant)
#         elif transition.store_variable is not None:
#             variable = self.compiler.variables[transition.store_variable]
#             if variable.len is not None:
#                 fh.write("%*sparse_%s_%s(parserState, c, %d, ")
#
#
#     def _write_implementation(self, fh):
#         fh.writelines([
#             "#include \"%s.h\"\n" % self.compiler.parser_name
#             "#include <ctype.h>\n"
#         ])
#         self._define_parse_decimal(fh, "uint8_t")
#         self._define_parse_decimal(fh, "uint16_t")
#         self._define_parse_integer(fh, "int8_t")
#         self._define_parse_integer(fh, "int16_t")
#         self._define_parse_hex(fh, "uint8_t")
#         self._define_parse_hex(fh, "uint16_t")
#         self._define_parse_string(fh, "unsigned char")
#         self._define_parse_string(fh, "char")
#         fh.writelines([
#             "\n"
#             "bool %sprocess_character(%s *parserState, unsigned char c) {\n" % (
#             self.compiler.interface_prefix, self.compiler.state_variable_type)
#             "  /* work with state low and high byte individually, it produces much smaller output */\n"
#             "  uint8_t hi_state = parserState->state >> 8;\n"
#             "  uint8_t lo_state = parserState->state & 0xff;\n"
#             "  uint8_t only_match;\n"
#             "\n"
#             "  if (hi_state == 0 && lo_state <= %s_LAST_) {\n" % self.compiler.const_prefix
#             "    return false;\n"
#             "  }\n"
#             "\n"
#             "  for (;;) {\n"
#         ])
#
#         hi_state = 0
#         while True:
#             lo_states = list(filter(lambda x: (x.order >> 8) == hi_state, self.compiler.states))
#             if len(lo_states) == 0:
#                 if hi_state < 255:
#                     hi_state = 255
#                     continue
#                 else:
#                     break
#
#             fh.write("    if (hi_state == %d) {\n" % hi_state)
#             fh.write("      switch (lo_state) {\n")
#
#             for state in filter(lambda s: not s.is_trivial_state() and not s.is_final, lo_states):
#                 fh.write("        case %s%s & 0xff:\n" % (self.compiler.const_prefix, state.name))
#
#                 constant_transitions = list(filter(lambda x: len(x.condition) == 1, state.outgoing_transitions))
#                 if len(constant_transitions) > 0:
#                     fh.write("          switch (c) {\n")
#                     for transition in constant_transitions:
#                         fh.write("            case %s:\n" % cchar(c))
#                         self._check_flags(fh, 14, next_state, "break")
#                         if transition.store_constant is not None:
#                             fh.write("              parserState->%s = %s;\n" % (transition.store_variable, transition.store_constant))
#                         if state.order == transition.target_state.order - 1:
#                             fh.write("              goto increment_state_and_character_finished;\n")
#                         else:
#                             txt = txt + "              %s\n" % change_state(state, next_state)
#                             txt = txt + "              goto character_finished;\n"
#                     txt = txt + "          }\n"
#
#                 fixed_nexts = list(only_fixed(state.next.items()))
#                 if len(fixed_nexts) > 0:
#                     txt = txt + "          switch (c) {\n"
#                     for c, next_state in fixed_nexts:
#                         txt = txt + "            case %s:\n" % cchar(c)
#                         txt = check_flags(txt, 14, next_state, "break")
#                         if next_state.set_variable is not None:
#                             txt = txt + "              parserState->%s = %s;\n" % next_state.set_variable
#                         if state.value == next_state.value + 1:
#                             txt = txt + "              goto decrement_state_and_character_finished;\n"
#                         else:
#                             txt = txt + "              %s\n" % change_state(state, next_state)
#                             txt = txt + "              goto character_finished;\n"
#                     txt = txt + "          }\n"
#
#                 if state.variable is not None:
#                     txt = program_parse_variable(txt, state.variable)
#
#                 variable_nexts = list(only_variable(state.next.items()))
#                 if len(variable_nexts) > 1:
#                     raise Exception("We do not support multiple different variables from one state")
#
#                 if len(variable_nexts) == 1:
#                     (variable_index, next_state) = variable_nexts[0]
#                     txt = check_flags(txt, 10, next_state, "goto unrecognized_character")
#                     txt = program_initial_parse_variable(txt, next_state.variable, next_state.name)
#                 else:
#                     txt = txt + "          goto unrecognized_character;\n"
#                 txt = txt + "\n"
#
#             for key, grouped_states in collect_mergeable_states(lo_states).items():
#                 for state in grouped_states:
#                     txt = txt + "        case %s%s & 0xff:\n" % (token_prefix, state.name)
#                 txt = txt + "          only_match = %s;\n" % cchar(key)
#                 txt = txt + "          goto if_match_decrement_state_and_character_finished;\n"
#                 txt = txt + "\n"
#
#             txt = txt + "      }\n"
#             txt = txt + "      goto unrecognized_character;\n"
#             txt = txt + "    }\n"
#             hi_state = hi_state + 1
#
# def cchar(c):
#     if c == '\n':
#         return "'\\n'"
#     if c == '\r':
#         return "'\\r'"
#     if c == '\t':
#         return "'\\t'"
#     return "'%s'" % c
#
