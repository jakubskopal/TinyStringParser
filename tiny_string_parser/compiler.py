from functools import reduce
import json
import re

CONDITION_DECIMAL_NUMBER = "1DEC"
CONDITION_INTEGER_NUMBER = "2INT"
CONDITION_HEXADECIMAL_NUMBER = "3HEX"
CONDITION_ANY_LETTER = "4ANY"
CONDITION_ENUM = "*ENUM*"

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
        del self.all_flags["*"]
        self._parse_expects()

    def to_dot(self, builder):
        s = ""
        s = s + "digraph STATE_MACHINE {\n"
        for state in self.states:
            s = s + "\n"
            label = (state.name if state.is_visible else "") + ((" [" + state.flag + "]") if state.flag is not None else "")
            if state.order is not None:
                label = label + " #%d" % state.order
            label = label + builder.enhance_state_label(state)
            if state.reset_state is not None:
                label = label + "\nreset to #%d" % state.reset_state.order
            elif state.reset_pattern is not None:
                label = label + "\nreset to %s" % json.dumps(state.reset_pattern)
            s = s + "  %s [label=%s %s]\n" % (
                state.name,
                json.dumps(label),
                "color=\"green\"" if state.is_trivial_state() else "")

            i = 0
            for transition in state.outgoing_transitions:
                i = i + 1
                label = "%d:%s" % (i, json.dumps(transition.condition))
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

