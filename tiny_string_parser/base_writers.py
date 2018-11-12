from functools import reduce


class BaseCodeFileWriter:
    def __init__(self, compiler):
        self.compiler = compiler

    def write(self, filename):
        with open(filename, "w") as fh:
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


class BaseHeaderFileWriter:
    def __init__(self, state_id_getter, compiler):
        self.state_id_getter = state_id_getter
        self.compiler = compiler
        self.longest_name_length = None

    def write(self, filename):
        self._find_longest_name_length()

        with open(filename, "w") as fh:
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
