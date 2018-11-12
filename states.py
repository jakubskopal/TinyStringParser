import yaml
from tiny_string_parser.compiler import Compiler
from tiny_string_parser.encoded_state_machine_v1 import EncodedStateMachineBuilder, EncodedStateMachineCodeFileWriter, \
    EncodedStateMachineHeaderFileWriter

with open("tests/modem_parser.spec.yml", "r") as ymlstream:
    try:
        all_spec = yaml.load(ymlstream)
        compiler = Compiler(all_spec)
        compiler.compile()
        builder = EncodedStateMachineBuilder()
        builder.build(compiler)
        b = EncodedStateMachineHeaderFileWriter(compiler, builder)
        b.write("tests/%s.h" % compiler.parser_name)
        b = EncodedStateMachineCodeFileWriter(compiler, builder)
        b.write("tests/%s.c" % compiler.parser_name)
        print(compiler.to_dot(builder))
        # print(compiler.encoded_size())
        # print(compiler.encode())
    except yaml.YAMLError as exc:
        print(exc)
        exit(1)
