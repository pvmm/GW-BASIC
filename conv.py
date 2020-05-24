#!/usr/bin/python
# Copyright (c) 2020 Leandro Pereira <leandro@hardinfo.org>
# Licensed under GPLv2.

from collections import deque
import sys
import traceback

class Lexer:
    def __init__(self, fp):
        self.state = self._lexer_asm
        self.start = 0
        self.pos = 0
        self.contents = fp.read()
        self.end = self.pos + len(self.contents)
        self.queue = deque()
        self.line = 1
        self.col = 1

    def lex(self):
        def queue():
            last = {'type': None}
            while self.queue:
                token = self.queue.popleft()
                if token['type'] == 'newline' and last['type'] == 'newline':
                    continue
                if token['type'] == 'token' and token['value'] == '':
                    continue
                yield token
                last = token

        while self.state is not None:
            yield from queue()
            self.state = self.state()

        yield from queue()

    def _next(self):
        if self.pos >= self.end:
            return None
        c = self.contents[self.pos]
        self.pos += 1

        if c == '\n':
            self.line += 1
            self.col = 1
        else:
            self.col += 1

        return c

    def _current_token(self):
        return self.contents[self.start : self.pos]

    def _emit(self, typ, extra=None, value=None):
        tok = {
            'type': typ,
            'value': self._current_token() if value is None else value
        }
        if extra is not None:
            tok['extra'] = extra
        if typ == 'token':
            tok['value'] = tok['value'].upper()
        self.queue.append(tok)

    def _emit_token(self, typ, extra=None, value=None):
        self._emit(typ, extra=extra, value=value)
        self.start = self.pos

    def _error(self, typ):
        self._emit_token('error', extra={
            'type': typ,
            'position': (self.line, self.col)
        })
        return None

    def _ignore(self):
        self.start = self.pos
    
    def _backup(self):
        self.pos -= 1

    def _remaining(self):
        return self.end - self.pos

    def _str_equal(self, s):
        if self._remaining() < len(s):
            return False
        return self.contents[self.start : self.start + len(s)] == s

    def _lexer_single_line_comment(self):
        self._ignore() # Ignore ;

        while True:
            c = self._next()
            if c is None:
                return self._error('EOF')

            if c in '\r\n':
                self._backup()
                self._emit_token('comment')
                return self._lexer_asm

    def _lexer_directive(self):
        while True:
            c = self._next()
            if c is None:
                return self._error('EOF')
        
            if c.isspace():
                self._backup()
                self._emit_token('directive')
                return self._lexer_asm
            
            if not c.isalpha():
                return self._error('Expecting alphabetic character')
            
    def _lexer_number(self):
        while True:
            c = self._next()
            if c is None:
                return self._error('EOF')

            if not c.isdigit():
                self._backup()
                self._emit_token('number')
                return self._lexer_asm

    def _lexer_multiline_comment(self):
        self._ignore() # Ignore COMMENT

        comment_char = None

        while True:
            c = self._next()
            if c is None:
                return self._error('EOF')

            if c.isspace():
                continue

            comment_char = c
            self._ignore()
            break

        while True:
            c = self._next()
            if c is None:
                return self._error('EOF')
            
            if c == comment_char:
                self._backup()
                self._emit_token('comment')

                self.pos += 1
                self._ignore()

                return self._lexer_asm

    def _lexer_token_until_end_of_line(self, typ, lex_comma=False):
        self._ignore()

        while True:
            c = self._next()
            if c is None or c in ';\r\n':
                self._backup()
                self._emit_token(typ, value=self._current_token().strip())
                return self._lexer_token
            if c == ',':
                self._backup()
                self._emit_token(typ, value=self._current_token().strip())
                self.pos += 1
                self._ignore()

    def _lexer_title(self):
        return self._lexer_token_until_end_of_line('title')

    def _lexer_subtitle(self):
        return self._lexer_token_until_end_of_line('subtitle')

    def _lexer_extern(self):
        return self._lexer_token_until_end_of_line('extern', lex_comma=True)

    def _lexer_db(self):
        return self._lexer_token_until_end_of_line('db', lex_comma=True)

    def _lexer_dw(self):
        return self._lexer_token_until_end_of_line('dw', lex_comma=True)

    def _lexer_assign(self):
        self._backup()
        tok = self._current_token()
        self.pos += 1
        self._ignore()

        while True:
            c = self._next()
            if c is None or c in ';\r\n':
                self._backup()
                self._emit_token('assign', value={
                    'token': tok,
                    'value': self._current_token().strip()
                })
                return None if c is None else self._lexer_asm

    def _lexer_token(self):
        while True:
            c = self._next()
            if c is None:
                return self._error('EOF')

            if c == '=':
                return self._lexer_assign

            if not c.isalnum() and not c in ':.[]?$_&':
                self._backup()

                curtoken = self._current_token().upper()
                if curtoken == 'COMMENT':
                    return self._lexer_multiline_comment
                elif curtoken == 'TITLE':
                    return self._lexer_title
                elif curtoken == 'SUBTTL':
                    return self._lexer_subtitle
                elif curtoken == 'EXTRN':
                    return self._lexer_extern
                elif curtoken == 'DB':
                    return self._lexer_db
                elif curtoken == 'DW':
                    return self._lexer_dw

                self._emit_token('token')
                return self._lexer_asm

    def _lexer_string(self, end_char, token_type='string'):
        while True:
            c = self._next()
            if c is None:
                return self._error('EOF')

            if c == end_char:
                self._emit_token(token_type)
                return self._lexer_asm

    def _lexer_string_double(self):
        return self._lexer_string('"')

    def _lexer_string_single(self):
        return self._lexer_string('\'')

    def _lexer_angle(self):
        return self._lexer_string('>', token_type='token')

    def _lexer_asm(self):
        while True:
            c = self._next()
            if c is None:
                return None

            if c == '\n':
                self._emit_token('newline')
                continue
            
            if c.isspace():
                self._ignore()
                continue

            if c == ';':
                return self._lexer_single_line_comment

            if c == '.':
                return self._lexer_directive

            if c == ',':
                self._emit_token('comma')
                continue

            if c.isdigit():
                return self._lexer_number

            if c.isalpha() or c == '?':
                return self._lexer_token

            if c == '\'':
                return self._lexer_string_single

            if c == '"':
                return self._lexer_string_double

            if c == '<':
                return self._lexer_angle

class Parser:
    def __init__(self, lexer):
        self.tokens = lexer.lex()
        self.token_queue = deque()
        self.queue = deque()
        self.state = self._parse_asm

        self.in_cseg = False
        self.in_dseg = False
        self.forc_counter = 0
        self.macro_args = deque()
        self.radix = 10

    def parse(self):
        def queue():
            while self.queue:
                yield self.queue.popleft()
    
        while self.state is not None:
            yield from queue()
            self.state = self.state()

        yield from queue()

    def _emit(self, token):
        self.queue.append(token)

    def _error(self, msg):
        self._emit({'type': 'error', 'value': msg})
        return None

    def _next(self):
        if self.token_queue:
            return self.token_queue.popleft()
        return next(self.tokens, None)

    def _peek(self):
        token = self._next()
        self.token_queue.appendleft(token)
        return token

    def _must_next(self, msg):
        token = self._next()
        if token is None:
            raise SyntaxError(msg)
        return token

    def _must_next_type(self, typ, msg):
        token = self._must_next(msg)
        if token['type'] != typ:
            raise SyntaxError("Expecting type %s, got %s: %s" % (typ, token['type'], msg))
        return token

    def _parse_directive_radix(self):
        token = self._must_next_type('number', "Expecting radix number")
        self._emit({'type': 'directive_radix', 'value': token['value']})
        self.radix = int(token['value'])
        return self._parse_asm

    def _parse_directive_xlist(self):
        self._emit({'type': 'xlist'})
        return self._parse_asm

    def _parse_directive_sall(self):
        self._emit({'type': 'sall'})
        return self._parse_asm

    def _parse_directive_list(self):
        self._emit({'type': 'list'})
        return self._parse_asm

    def _parse_directive(self, token):
        typ = token['value'][1:]
        return getattr(self, '_parse_directive_%s' % typ.lower())

    def _parse_seg(self, is_cseg=True):
        token = self._must_next_type('token', "Expecting token")
        if token['value'] == 'SEGMENT':
            if is_cseg:
                if self.in_cseg:
                    return self._error("Already in code segment")
            elif self.in_dseg:
                return self._error("Already in data segment")

            token = self._must_next("Expecting segment information")
            if token == {'type': 'token', 'value': 'PUBLIC'}:
                public = True
                token = self._must_next_type('string', "Expecting segment name")
            elif token['type'] == 'string':
                public = False
            else:
                return self._error("Expecting string or PUBLIC")
            self._emit({'type': 'cseg', 'public': public, 'name': token['value'][1:-1]})

            if is_cseg:
                self.in_cseg = True
            else:
                self.in_dseg = True
            return self._parse_asm
        elif token['value'] == 'ENDS':
            if is_cseg:
                self.in_cseg = False
            else:
                self.in_dseg = False
            return self._parse_asm
        else:
            return self._error("Expecting SEGMENT or ENDS")

    def _parse_cseg(self):
        return self._parse_seg(is_cseg=True)

    def _parse_dseg(self):
        return self._parse_seg(is_cseg=False)

    def _parse_assume(self):
        token = self._must_next_type('token', "Expecting segment to assume")
        if not ':' in token['value']:
            return self._error("Segment seems invalid")
        self._emit({'type': 'assume', 'seg': token['value']})
        return self._parse_asm
    
    def _parse_include(self):
        token = self._must_next_type('token', "Expecting file name to include")
        self._emit({'type': 'include', 'filename': token['value']})
        return self._parse_asm

    def _parse_org(self):
        token = self._must_next_type('number', "Expecting ORG argument")
        self._emit({'type': 'org', 'value': token['value']})
        return self._parse_asm

    def _parse_public(self):
        while True:
            token = self._next()
            if token is None:
                return None

            if token['type'] == 'token':
                for identifier in token['value'].split(','):
                    self._emit({'type': 'public', 'identifier': identifier.strip()})
            elif token['type'] == 'comma':
                continue
            elif token['type'] == 'newline':
                break
            elif token['type'] == 'comment':
                self._emit({'type': 'comment', 'value': token['value']})
                break
        return self._parse_asm

    def _parse_extern(self, token):
        for identifier in token['value'].split(','):
            identifier = identifier.split(":")
            if len(identifier) != 2:
                self._error("Don't know what to do")
            self._emit({'type': 'extern', 'identifier': identifier[0], 'attr': identifier[1]})
        return self._parse_asm

    def _parse_assign(self, token):
        token = token['value']
        self._emit({'type': 'assign', 'symbol': token['token'], 'value': token['value']})
        return self._parse_asm

    def _parse_label(self, token):
        self._emit({'type': 'label', 'identifier': token['value'][:-1]})
        return self._parse_asm

    def _is_x86_instruction(self, token):
        return token['value'] in {
            'MOV', 'ADD', 'INC', 'CMP', 'JNZ', 'JMP', 'JZ', 'RET', 'PUSH', 'POP',
            'CALL', 'OR', 'XCHG', 'AND', 'XOR', 'DEC', 'JAE', 'JB', 'SUB', 'PUSHF',
            'POPF', 'JNAE', 'STC', 'JNB', 'LAHF', 'SAHF', 'CMC', 'SBB', 'STOSB',
            'REP', 'JNS', 'JS', 'ROR', 'ROL', 'ADC', 'CLD', 'SHL', 'SHR', 'XLAT',
            'LOOP', 'TEST', 'CBW', 'NEG', 'JLE', 'JO', 'JGE', 'JL', 'MOVS', 'JPO',
            'JNE', 'RCL', 'RCR', 'CLC', 'MOVSW', 'LODS', 'STOSW', 'NOT', 'STD',
            'CMPSW', 'JPE', 'IMUL', 'IDIV', 'MUL', 'SAL', 'JE', 'LODSW', 'LODSB',
            'MOVSB', 'JA', 'DIV', 'JCXZ', 'CLI', 'STI', 'LEA',
        }

    def _is_macro(self, token):
        return token['value'] in {
            'INS86', 'ACRLF', 'DO_EXT', 'HLFHL', 'HLFDE', 'NEGDE', 'POPR', 'UN_DEF',
            'MOVRI', 'T', 'Q', 'QF', 'DERMAK', '?Z0', 'ADD_EXT', 'DEF_MAC', 'M', 'R',
            'R2', '?&S', 'EXPAND_MAC', 'CALLOS', 'DUMY', 'ADR', 'DC', 'NEGHL',

            # FIXME: add_ext macro in BINTRP.H references this, but it's parsed incorrectly
            '?I'
        }

    def _parse_x86_instruction_operand(self, operand):
        if len(operand) == 1:
            try:
                return int(operand[0], self.radix)
            except ValueError:
                return operand[0]

        if operand[0] in ('OFFSET', 'BYTE', 'SHORT', 'LOW', 'PTR'):
            return (operand[0], self._parse_x86_instruction_operand(operand[1:]))

        if operand[0].isdigit():
            if operand[1] == 'H':
                return int(operand[0], 16)
            if operand[1] == 'D':
                return int(operand[0], 10)
            if operand[1] == 'O':
                return int(operand[0], 8)

        def is_expr(op):
            if op[0] == '"' and op[-1] == '"':
                return is_expr(op[1:-1])
            for c in op:
                if not c.isalnum() and not c in '+-$':
                    return False
            return True
        if all(is_expr(op) for op in operand):
            return ''.join(operand)

        return self._error("Unknown kind of operand: %s" % operand)

    def _parse_x86_instruction(self, token):
        instruction = token['value']
        operands = []
        comment = None
        cur_operand = []
        while True:
            token = self._must_next("Expecting operands for %s" % instruction)
            if token['type'] in ('token', 'number', 'string'):
                cur_operand.append(token['value'])
            elif token['type'] == 'comment':
                comment = token['value']
                break
            elif token['type'] == 'newline':
                break
            elif token['type'] == 'comma':
                operands.append(self._parse_x86_instruction_operand(cur_operand))
                cur_operand = []
                continue
            else:
                return self._error("Unexpected token: %s" % token)
        if cur_operand:
            operands.append(self._parse_x86_instruction_operand(cur_operand))
        self._emit({'type': 'instruction', 'op': instruction, 'operands': tuple(operands), 'comment': comment})
        return self._parse_asm

    def _parse_byte_from_db(self, db):
        try:
            return int(db, self.radix)
        except ValueError:
            pass

        if db[-1] == 'D':
            return int(db[:-1], 10)
        if db[-1] == 'H':
            return int(db[:-1], 16)
        if db[-1] == 'O':
            return int(db[:-1], 8)

    def _parse_macro_ins86(self, arguments, comment):
        operands = []
        for ops in arguments:
            for op in ops:
                if op['type'] == 'number':
                    if op['value'].isdigit(): # Arguments to this macro are always in octal
                        operands.append(int(op['value'], 8))
                    elif op['value'] == '+2':
                        operands[-1] += op['value']
                    else:
                        raise SyntaxError("Unknown radix for INS86 numeric argument")
                elif op['type'] in ('token', 'string'):
                    operands.append(op['value'])
                else:
                    raise SyntaxError("Invalid argument to INS86 macro")

        operands = tuple(operands)

        if operands == (6,):
            self._emit({'type': 'instruction', 'op': 'push', 'operands': ('ES',), 'comment': comment})
            return self._parse_asm

        if operands == (7,):
            self._emit({'type': 'instruction', 'op': 'pop', 'operands': ('ES',), 'comment': comment})
            return self._parse_asm

        if operands in ((46,), (38,)): # CS:/ES: prefixes, useless for Z80
            if comment:
                self._emit({'type': 'comment', 'value': comment})
            return self._parse_asm

        if operands == (139, 242, 46, 172):
            self._emit({'type': 'instruction', 'op': 'mov', 'operands': ('SI', 'DX'), 'comment': comment})
            self._emit({'type': 'instruction', 'op': 'lodsb', 'operands': ('AL', '[SI]'), 'comment': None})
            return self._parse_asm

        if operands == (50, 228):
            self._emit({'type': 'instruction', 'op': 'xor', 'operands': ('AH', 'AH'), 'comment': comment})
            return self._parse_asm

        if operands == (57, 23):
            self._emit({'type': 'instruction', 'op': 'cmp', 'operands': ('[BX]', 'DX'), 'comment': comment})
            return self._parse_asm

        if operands == (209, 235):
            self._emit({'type': 'instruction', 'op': 'shr', 'operands': ('BX', 1), 'comment': comment})
            return self._parse_asm

        if operands == (139, 240):
            self._emit({'type': 'instruction', 'op': 'mov', 'operands': ('SI', 'AX'), 'comment': comment})
            return self._parse_asm

        if operands[:2] == (142, 6) and len(operands) == 3:
            self._emit({'type': 'instruction', 'op': 'mov', 'operands': ('ES', operands[-1]), 'comment': comment})
            return self._parse_asm

        if operands[:2] == (255, 180) and len(operands) == 3:
            self._emit({'type': 'instruction', 'op': 'push', 'operands': (operands[-1],), 'comment': comment})
            return self._parse_asm

        if operands[:2] == (137, 22) and len(operands) == 3:
            self._emit({'type': 'instruction', 'op': 'mov', 'operands': (operands[-1], 'DX'), 'comment': comment})
            return self._parse_asm

        if operands[:2] == (137, 38) and len(operands) == 3:
            self._emit({'type': 'instruction', 'op': 'mov', 'operands': (operands[-1], 'SP'), 'comment': comment})
            return self._parse_asm

        if operands[:2] == (246, 6) and len(operands) == 3:
            if comment:
                self._emit({'type': 'comment', 'value': comment})
                comment = None
            while True:
                peek = self._peek()
                if peek['type'] == 'newline':
                    self._next()
                    continue
                if peek['type'] == 'comment':
                    self._next()
                    comment = peek['value']
                    continue
                if peek['type'] == 'db':
                    self._next()
                    op = peek['value']
                    self._emit({'type': 'instruction', 'op': 'test', 'operands': (operands[-1], op), 'comment': comment})
                    return self._parse_asm
                break

        if operands in ((185,), (186,)):
            if comment:
                self._emit({'type': 'comment', 'value': comment})
                comment = None
            dbs = []
            while len(dbs) != 2:
                peek = self._peek()
                if peek['type'] == 'newline':
                    self._next()
                    continue
                if peek['type'] == 'comment':
                    self._next()
                    comment = peek['value']
                    continue
                if peek['type'] == 'db':
                    self._next()
                    dbs.append(peek['value'])
                    continue
                raise SyntaxError("Unexpected token found while parsing INS86 macro: %s" % peek)
            op1 = 'CX' if operands[0] == 185 else 'DX'
            op2 = dbs[0], dbs[1]
            self._emit({'type': 'instruction', 'op': 'movi', 'operands': (op1, op2), 'comment': comment})
            return self._parse_asm

        if operands == (209, 234):
            self._emit({'type': 'instruction', 'op': 'shr', 'operands': ('DX', 1), 'comment': comment})
            return self._parse_asm

        if operands == (209, 233):
            self._emit({'type': 'instruction', 'op': 'shr', 'operands': ('BX', 1), 'comment': comment})
            return self._parse_asm

        if operands == (247, 218):
            self._emit({'type': 'instruction', 'op': 'neg', 'operands': ('DX',), 'comment': comment})
            return self._parse_asm

        if operands == (247, 219):
            self._emit({'type': 'instruction', 'op': 'neg', 'operands': ('BX',), 'comment': comment})
            return self._parse_asm

        if operands == (247, 226):
            self._emit({'type': 'instruction', 'op': 'mul', 'operands': ('DX',), 'comment': comment})
            return self._parse_asm

        if operands == (138, 242):
            self._emit({'type': 'instruction', 'op': 'mov', 'operands': ('DH', 'DL'), 'comment': comment})
            return self._parse_asm

        if operands == (5,):
            if comment:
                self._emit({'type': 'comment', 'value': comment})
                comment = None
            dbs = []
            while len(dbs) != 2:
                peek = self._peek()
                if peek['type'] == 'newline':
                    self._next()
                    continue
                if peek['type'] == 'comment':
                    self._next()
                    comment = peek['value']
                    continue
                if peek['type'] == 'db':
                    self._next()
                    dbs.append(peek['value'])
                    continue
                raise SyntaxError("Unexpected token found while parsing INS86 macro: %s" % peek)
            op = self._parse_byte_from_db(dbs[1]) << 8 | self._parse_byte_from_db(dbs[0])
            self._emit({'type': 'instruction', 'op': 'add', 'operands': ('AX', op), 'comment': comment})
            return self._parse_asm

        if operands[0] == 115 and len(operands) == 2:
            self._emit({'type': 'instruction', 'op': 'jae', 'operands': (operands[1],), 'comment': comment})
            return self._parse_asm

        if operands == (254, 198):
            self._emit({'type': 'instruction', 'op': 'inc', 'operands': ('DH',), 'comment': comment})
            return self._parse_asm

        if operands == (138, 212):
            self._emit({'type': 'instruction', 'op': 'mov', 'operands': ('DL', 'AH'), 'comment': comment})
            return self._parse_asm

        if operands[:2] == (57, 30) and len(operands) == 3:
            self._emit({'type': 'instruction', 'op': 'cmp', 'operands': (operands[2], 'BX'), 'comment': comment})
            return self._parse_asm

        if operands[:2] == (255, 54) and len(operands) == 3:
            self._emit({'type': 'instruction', 'op': 'push', 'operands': (operands[2],), 'comment': comment})
            return self._parse_asm

        debug = []
        for op in operands:
            if isinstance(op, int): debug.append('%02x' % op)
            else: debug.append(' %s ' % op)
        raise SyntaxError("Unknown arguments to INS86 macro: %s    %s" % (''.join(debug), operands))

    def _parse_macro_popr(self, comment):
        self._emit({'type': 'instruction', 'op': 'pop', 'operands': ['CX'], 'comment': comment})
        self._emit({'type': 'instruction', 'op': 'pop', 'operands': ['DX'], 'comment': None})
        return self._parse_asm

    def _parse_macro_call(self, token):
        macro = token['value']
        peek = self._peek()
        if peek['type'] == 'token' and peek['value'] == 'MACRO':
                return self._parse_macro(token)

        operands = []
        comment = None
        cur_operand = []
        while True:
            token = self._must_next("Expecting operands for %s" % macro)
            if token['type'] in ('token', 'number', 'string'):
                cur_operand.append(token)
            elif token['type'] == 'comment':
                comment = token['value']
                break
            elif token['type'] == 'newline':
                break
            elif token['type'] == 'comma':
                operands.append(cur_operand)
                cur_operand = []
                continue
            else:
                return self._error("Unexpected token: %s" % token)
        if cur_operand:
            operands.append(cur_operand)

        if macro == 'INS86':
            return self._parse_macro_ins86(operands, comment)
        if macro == 'POPR':
            return self._parse_macro_popr(comment)

        self._emit({'type': 'macro_call', 'identifier': macro, 'args': operands, 'comment': comment})
        return self._parse_asm

    def _parse_data(self, typ, token):
        bytes = []

        for tok in token['value'].split(','):
            if tok.startswith('OFFSET'):
                bytes.append(('offset', tok[len("OFFSET"):].strip()))
            else:
                bytes.append(tok)
        self._emit({'type': typ, 'bytes': bytes})
        return self._parse_asm

    def _parse_db(self, token):
        return self._parse_data('db', token)

    def _parse_dw(self, token):
        return self._parse_data('dw', token)

    def _parse_label_def(self, token):
        self._next() # Eat LABEL
        identifier = token['value']
        attrs = []
        comment = None
        while True:
            token = self._next()
            if token['type'] == 'token':
                attrs.append(token['value'])
            elif token['type'] == 'comment':
                comment = token['value']
            elif token['type'] == 'newline':
                break
            else:
                return self._error("Unexpected token type %s" % token['type'])
        self._emit({'type': 'label_def', 'identifier': identifier, 'attrs': attrs, 'comment': comment})
        return self._parse_asm

    def _parse_equ(self, token):
        self._next() # Eat EQU
        identifier = token['value']
        attrs = []
        comment = None
        while True:
            token = self._next()
            if token['type'] == 'token':
                attrs.append(token['value'])
            elif token['type'] == 'comment':
                comment = token['value']
                break
            elif token['type'] == 'newline':
                break
            else:
                return self._error("Unexpected token type %s" % token['type'])
        self._emit({'type': 'equ', 'identifier': identifier, 'attrs': attrs, 'comment': comment})
        return self._parse_asm

    def _parse_macro(self, token):
        self._next() # Eat MACRO
        identifier = token['value']
        args = []
        comment = None
        while True:
            token = self._next()
            if token is None:
                break
            if token['type'] == 'newline':
                break
            if token['type'] == 'comment':
                comment = token['value']
                break
            if token['type'] == 'comma':
                continue
            if token['type'] == 'token':
                args.append(token['value'])
            else:
                return self._error("Unexpected token type: %s" % token['type'])
        self.macro_args.append(args)
        self._emit({'type': 'macro', 'identifier': identifier, 'args': args, 'comment': comment})
        return self._parse_asm

    def _parse_if(self, typ):
        cond = []
        comment = None
        while True:
            token = self._next()
            if token['type'] in ('number', 'token'):
                cond.append(token['value'])
            elif token['type'] == 'comment':
                comment = token['value']
                break
            elif token['type'] == 'newline':
                break
            else:
                return self._error("Unexpected token type %s" % token['type'])
        if not cond:
            return self._error("If without condition")
        self._emit({'type': typ.lower(), 'cond': cond, 'comment': comment})
        return self._parse_asm

    def _parse_ifdif(self):
        ops = []
        comment = None
        while True:
            token = self._next()
            if token['type'] in ('number', 'token', 'string'):
                ops.append(token['value'])
            elif token['type'] == 'comment':
                comment = token['value']
                break
            elif token['type'] == 'comma':
                continue
            elif token['type'] == 'newline':
                break
            else:
                return self._error("Unexpected token type %s" % token['type'])
        if len(ops) != 2:
            return self._error("IFDIF requires 2 arguments")
        self._emit({'type': 'ifdif', 'ops': ops, 'comment': comment})
        return self._parse_asm

    def _parse_echo(self):
        msg = []
        while True:
            token = self._next()
            if token['type'] == 'newline':
                break
            msg.append(token)
        self._emit({'type': 'echo', 'msg': msg})
        return self._parse_asm

    def _parse_endm(self, escaping=False):
        if escaping:
            self._emit({'type': 'end_macro_escaped'})
        else:
            if self.forc_counter:
                self.forc_counter -= 1
                self._emit({'type': 'end_forc'})
            elif self.macro_args:
                self.macro_args.popleft()
                self._emit({'type': 'end_macro'})
            else:
                return self._error("Not in macro or forc")
        return self._parse_asm

    def _parse_forc(self):
        self.forc_counter += 1
        comment = None
        args = []
        while True:
            token = self._next()
            if token['type'] == 'newline':
                break
            if token['type'] == 'comma':
                continue
            if token['type'] == 'comment':
                comment = token['value']
                break
            if token['type'] != 'token':
                return self._error("Expecting token, got %s" % token['type'])
            args.append(token)
        self._emit({'type': 'forc', 'args': args, 'comment': comment})
        return self._parse_asm

    def _parse_purge(self):
        token = self._must_next_type('token', "Macro identifier")
        self._emit({'type': 'purge', 'identifier': token['value']})
        return self._parse_asm

    def _parse_strip_macro(self):
        macro_token_count = 0
        while True:
            token = self._next()
            if token is None:
                return self._error("Expecting ENDM")
            if token['type'] == 'token' and token['value'] == 'MACRO':
                macro_token_count += 1
                if macro_token_count > 1:
                    return self._error("Nested macros not supported for stripping")
            if token['type'] == 'token' and token['value'] == 'ENDM':
                return self._parse_asm

    def _is_useless_macro(self, name):
        useless_macros = {
            'LDIR', 'LDDR', 'DJNZ', 'FSIGN', 'PUSHM', 'SYNCHK', 'OUTCHR', 'CHRGET',
            'COMPAR', 'PUSHR', 'INST', 'GETYPE', 'INS86', 'POPR',
        }
        if name in useless_macros:
            peek = self._peek()
            if peek['type'] == 'token' and peek['value'] == 'MACRO':
                return True
        return False

    def _parse_asm(self):
        while True:
            token = self._next()
            if token is None:
                return None

            typ = token['type']

            if typ == 'newline':
                continue

            if typ == 'comment':
                self._emit(token)
                return self._parse_asm

            if typ == 'directive':
                return self._parse_directive(token)

            if typ == 'token':
                typ = token['value']
                if typ == 'CSEG':
                    return self._parse_cseg
                if typ == 'DSEG':
                    return self._parse_dseg
                if typ == 'ASSUME':
                    return self._parse_assume
                if typ == 'INCLUDE':
                    return self._parse_include
                if typ == 'PUBLIC':
                    return self._parse_public
                if typ == 'ORG':
                    return self._parse_org
                if typ == 'PAGE':
                    continue
                if typ in ('%OUT', 'ECHO'):
                    return self._parse_echo
                if typ == 'END':
                    break
                if typ == 'ENDM':
                    return self._parse_endm(escaping=False)
                if typ == '&ENDM':
                    return self._parse_endm(escaping=True)
                if typ == 'ENDIF':
                    self._emit({'type': 'end_if'})
                    return self._parse_asm
                if typ in ('IF', 'IFE', 'IFDEF', 'IFNDEF'):
                    return self._parse_if(typ)
                if typ == 'IFDIF':
                    return self._parse_ifdif
                if typ == 'IF1': # Hack!
                    self.token_queue.appendleft({'type': 'number', 'value': '1'})
                    return self._parse_if('IF')
                if typ == 'ELSE':
                    self._emit({'type': 'else'})
                    return self._parse_asm
                if typ == 'PURGE':
                    return self._parse_purge
                if typ in ('FORC', 'IRPC'):
                    return self._parse_forc
                if token['value'].endswith(':'):
                    return self._parse_label(token)
                if self._is_x86_instruction(token):
                    return self._parse_x86_instruction(token)
                if self._is_useless_macro(typ):
                    self._emit({'type': 'comment', 'value': 'Stripped useless macro: %s' % typ})
                    return self._parse_strip_macro
                if self._is_macro(token):
                    return self._parse_macro_call(token)
                else:
                    peek = self._peek()
                    if peek['type'] == 'token' and peek['value'] == 'LABEL':
                        return self._parse_label_def(token)
                    if peek['type'] == 'token' and peek['value'] == 'EQU':
                        return self._parse_equ(token)

            if typ in ('title', 'subtitle'):
                self._emit(token)
                return self._parse_asm

            if typ == 'assign':
                return self._parse_assign(token)

            if typ == 'extern':
                return self._parse_extern(token)

            if typ == 'db':
                return self._parse_db(token)

            if typ == 'dw':
                return self._parse_dw(token)

            for args in self.macro_args:
                for arg in args:
                    if arg == token['value']:
                        self._emit({'type': 'macro_arg', 'identifier': arg})
                        return self._parse_asm

            return self._error("Don't know how to parse token %s" % token)

class PasmoWriter:
    regmap = {
        'BX': 'HL', 'BH': 'H', 'BL': 'L',
        'DX': 'DE', 'DH': 'D', 'DL': 'E',
        'CX': 'BC', 'CH': 'B', 'CL': 'C',
        'SI': 'IY', 'DI': 'IX',
        'SP': 'SP',
        'AL': 'A',
        # FIXME: will need to spill/fill AX, AH, and AL using the Z80 prime
        # registers because the code uses AX as a 16-bit register extensively
    }

    def __init__(self, parser):
        self.parser = parser
        self.ignore_count = 0

    def lines(self):
        try:
            for token in self.parser.parse():
                if self.ignore_count:
                    self.ignore_count -= 1
                    continue
                if self._is_xthl(token):
                    self.ignore_count = 2 # Ignore XCHG SI, BX / PUSH SI
                    token = {'type': 'instruction', 'op': 'xthl', 'operands': [], 'comment': token['comment']}
                line = getattr(self, '_gen_' + token['type'])(token)
                if line is not None:
                    yield line
        except AttributeError:
            raise SyntaxError("Don't know how to generate token of type %s for Pasmo: %s" % (token['type'], token))

    def _is_xthl(self, token):
        return token['type'] == 'instruction' and (token['op'], token['operands']) == ('POP', ('SI',))

    def _gen_comment(self, token):
        return '\n'.join('; %s' % line for line in token['value'].split('\n'))

    def _gen_directive_radix(self, token):
        return '\t.radix %s' % token['value']

    def _gen_assign(self, token):
        return '\t%s=%s' % (token['symbol'], token['value'])

    def _gen_cseg(self, token):
        return '; CSEG %s (%s)' % (token['name'], 'public' if token['public'] else 'local')

    def _gen_assume(self, token):
        return '; Assuming segment %s' % token['seg']

    def _gen_xlist(self, token):
        return None

    def _gen_list(self, token):
        return None

    def _gen_extern(self, token):
        return None

    def _gen_public(self, token):
        return '\tPUBLIC %s' % token['identifier']

    def _gen_sall(self, token):
        return None

    def _gen_include(self, token):
        return '\tinclude %s' % token['filename']

    def _gen_title(self, token):
        header = ';' * (len(token['value']) + 6)
        return '\n%s\n;; %s ;;\n%s\n' % (header, token['value'], header)

    def _gen_subtitle(self, token):
        return '\n;;; %s ;;;\n' % token['value']

    def _gen_label(self, token):
        return '%s:' % token['identifier']

    def _flatten(self, ops):
        def flatten(to_flatten):
            out = []
            if isinstance(to_flatten, int):
                out.append(str(to_flatten))
            else:
                for op in to_flatten:
                    if isinstance(op, tuple):
                        out.extend(flatten(op))
                    elif isinstance(op, str):
                        out.append(op)
                    elif isinstance(op, int):
                        out.append(str(op))
                    else:
                        raise SyntaxError("Unknown type to flatten! %s" % op)
            return out
        return flatten(ops)

    def _is_16bit_reg(self, reg):
        return reg[1] == 'X' or reg == 'SI' or reg == 'DI' or reg == 'ES'

    def _gen_instruction_mov(self, token):
        # FIXME: not all operations are possible here; for instance, it's not
        # possible to perform LD DE, (HL), which is generated by this function
        # in some cases.  This would need to be a sereies of instructions to
        # load D and E separately as there's no LD DE, r16 instruction.
        operands = []
        for op in token['operands']:
            if op in self.regmap:
                operands.append(self.regmap[op])
            elif isinstance(op, tuple):
                if op[0] == 'BYTE' and op[1][0] == 'PTR':
                    op = op[1][1]
                    if op[0] == '[' and op[1:-1] in self.regmap:
                        operands.append('(%s)' % self.regmap[op[1:-1]])
                    elif op not in self.regmap:
                        operands.append('(%s)' % op)
                else:
                    operands.append(' '.join(self._flatten(op)))
            elif isinstance(op, str) and op[0] == '[':
                if op[1:-1] in self.regmap:
                    operands.append('(%s)' % self.regmap[op[1:-1]])
                else:
                    raise SyntaxError("Don't know how to generate this MOV op: %s" % op)
            else:
                operands.append(op)

        return 'LD %s' % ', '.join(str(op) for op in operands)

    def _gen_instruction_add(self, token, z80='ADD'):
        assert len(token['operands']) == 2
        op1, op2 = token['operands']
        if op1 == 'AL' and self._is_ptr_read_through_bx(op2):
            return '%s A, (HL)' % z80
        if op1 in self.regmap:
            if op2 in self.regmap:
                return '%s %s, %s' % (z80, self.regmap[op1], self.regmap[op2])
            return '%s %s, %s' % (z80, self.regmap[op1], ' '.join(self._flatten(op2)))
        if op1 == 'AX' and isinstance(op2, int):
            # FIXME: figure out what to do with usage of AX...
            return '; ADD AX, %d' % op2
        raise SyntaxError("Don't know how to generate %s: %s, %s" % (z80, op1, op2))

    def _gen_instruction_adc(self, token):
        return self._gen_instruction_add(token, 'ADC')

    def _gen_instruction_inc(self, token):
        return 'INC %s' % self.regmap[token['operands'][0]]

    def _gen_instruction_cmp(self, token):
        assert len(token['operands']) == 2
        op1, op2 = token['operands']
        if op1 == 'AL' and op2 in self.regmap:
            return 'CP %s' % self.regmap[op2]
        if op1 == 'AL':
            return 'CP %s' % ' '.join(self._flatten(op2))
        if op2 in self.regmap:
            reg = self.regmap[op2]
            if op1 == 'BX':
                return ('OR A\n\t' + \
                        'SBC HL, %s\n\t' + \
                        'ADD HL, %s\n\t') % (reg, reg)
            if op1 == '[BX]':
                return ('PUSH HL\n\t' + \
                        'LD HL, (HL)\n\t' + \
                        'OR A\n\t' + \
                        'SBC HL, %s\n\t' + \
                        'ADD HL, %s\n\t' + \
                        'POP HL\n\t') % (reg, reg)
        raise SyntaxError("Don't know how to generate CMP: %s" % token)

    def _gen_instruction_jnz(self, token):
        assert len(token['operands']) == 1
        op = token['operands'][0]
        if isinstance(op, str):
            return 'JP NZ, %s' % op[0]
        if len(op) == 2 and op[0] == 'SHORT':
            return 'JR NZ, %s' % op[1]
        raise SyntaxError("Unsupported JNZ to %s" % op)

    def _gen_instruction_jmp(self, token):
        assert len(token['operands']) == 1
        op = token['operands'][0]
        if isinstance(op, str):
            return 'JP %s' % op[0]
        if len(op) == 2 and op[0] == 'SHORT':
            return 'JR %s' % op[1]
        raise SyntaxError("Unsupported JMP to %s" % op)

    def _gen_instruction_jz(self, token):
        assert len(token['operands']) == 1
        op = token['operands'][0]
        if isinstance(op, str):
            return 'JP Z, %s' % op[0]
        if len(op) == 2 and op[0] == 'SHORT':
            return 'JR Z, %s' % op[1]
        raise SyntaxError("Unsupported JZ to %s" % op)

    def _gen_instruction_ret(self, token):
        assert len(token['operands']) == 0
        # FIXME: maybe look at the code to generate other RET variants?
        return 'RET'

    def _gen_instruction_push(self, token):
        assert len(token['operands']) == 1
        op = token['operands'][0]
        if op == 'ES':
            return None
        if self._is_16bit_reg(op):
            if op != 'AX':
                return 'PUSH %s' % self.regmap[op]
            return 'LD C\', A\n\tPUSH BC\''
        # FIXME: check if op is a dword label first!
        return 'PUSH HL\n\tLD HL, (%s)\n\tEX (SP), HL' % op

    def _gen_instruction_pop(self, token):
        assert len(token['operands']) == 1
        op = token['operands'][0]
        if op == 'ES':
            return None
        if self._is_16bit_reg(op):
            if op != 'AX':
                return 'POP %s' % self.regmap[op]
            return 'POP BC\'\n\tLD A, C\''
        raise SyntaxError("Only 16-bit registers can be popped (trying %s)" % op)

    def _gen_instruction_pushf(self, token):
        assert len(token['operands']) == 0
        return 'PUSH AF' # FIXME: Not really interested in saving A here... maybe store in a prime?

    def _gen_instruction_popf(self, token):
        assert len(token['operands']) == 0
        return 'POP AF' # FIXME: Not really interested in restoring A here... maybe store in a prime?

    def _gen_instruction_or(self, token):
        assert len(token['operands']) == 2
        op1, op2 = token['operands']
        if op1 == 'AL':
            if not self._is_16bit_reg(op2) and op2 in self.regmap:
                return 'OR %s' % self.regmap[op2]
            if self._is_ptr_read_through_bx(op2):
                return 'OR (HL)'
            if len(op2) == 2 and op2[0] == 'LOW':
                return 'OR %s' % op2[1]
        if op1 == op2:
            return 'OR A' # NOP, but clear C/N/P/V flags
        raise SyntaxError("Don't know how to generate an OR with these yet: %s, %s" % (op1, op2))

    def _gen_instruction_dec(self, token):
        assert len(token['operands']) == 1
        op = token['operands'][0]
        if self._is_ptr_read_through_bx(op):
            return 'DEC (HL)'
        if op in self.regmap:
            return 'DEC %s' % self.regmap[op]
        raise SyntaxError("Don't know how to generate DEC with arg %s" % op)

    def _gen_instruction_call(self, token):
        assert len(token['operands']) == 1
        op = token['operands'][0]
        if op.isalnum():
            return 'CALL %s' % op
        raise SyntaxError("Don't know how to generate CALL with arg %s" % op)

    def _gen_instruction_jae(self, token):
        assert len(token['operands']) == 1
        op = token['operands'][0]
        if isinstance(op, int):
            if op >= -128 and op < 127:
                # FIXME: is this correct? The offset here is relative to the 8086
                # instructions, not Z80...
                return 'JR NC, %d' % op
        if len(op) == 1:
            return 'JP NC, %s' % op[0]
        if len(op) == 2 and op[0] == 'SHORT':
            return 'JR NC, %s' % op[1]
        raise SyntaxError("I can't make this jump!")

    def _gen_instruction_js(self, token):
        assert len(token['operands']) == 1
        op = token['operands'][0]
        if len(op) == 1:
            return 'JP M, %s' % op[0]
        return 'JP M, %s' % self._flatten(op)[-1]

    def _gen_instruction_jns(self, token):
        assert len(token['operands']) == 1
        op = token['operands'][0]
        if len(op) == 1:
            return 'JP P, %s' % op[0]
        return 'JP P, %s' % self._flatten(op)[-1]

    def _gen_instruction_jb(self, token):
        assert len(token['operands']) == 1
        op = token['operands'][0]
        if len(op) == 1:
            return 'JP C, %s' % op[0]
        if len(op) == 2 and op[0] == 'SHORT':
            return 'JR C, %s' % op[1]
        raise SyntaxError("I can't make this jump!")

    def _gen_instruction_jnae(self, token):
        return self._gen_instruction_jb(token)

    def _gen_instruction_jnb(self, token):
        return self._gen_instruction_jae(token)

    def _gen_instruction_stc(self, token):
        assert len(token['operands']) == 0
        return 'SCF'

    def _gen_instruction_lahf(self, token):
        assert len(token['operands']) == 0
        return 'LD A, F'

    def _gen_instruction_sahf(self, token):
        assert len(token['operands']) == 0
        return 'LD F, A'

    def _gen_instruction_cmc(self, token):
        assert len(token['operands']) == 0
        return 'CCF'

    def _gen_instruction_xchg(self, token):
        assert len(token['operands']) == 2
        op1, op2 = token['operands']
        if {op1, op2} == {'DX', 'BX'}:
            return 'EX DE, HL'
        if {op1, op2} == {'AL', 'AH'}:
            return 'LD C\', A\n\tLD A, B\'\n\tLD B\', C\''
        raise SyntaxError("Only DX and BX can be exchanged (trying %s, %s)" % (op1, op2))

    def _is_ptr_read_through_bx(self, op):
        return op[0] == 'BYTE' and op[1][0] == 'PTR' and op[1][1] == '[BX]'

    def _gen_instruction_and(self, token):
        assert len(token['operands']) == 2
        op1, op2 = token['operands']
        if op1 == 'AL':
            if op2 in self.regmap:
                return 'AND %s' % self.regmap[op2]
            if self._is_ptr_read_through_bx(op2):
                return 'AND (HL)'
            if len(op2) == 2 and op2[0] == 'LOW':
                return 'AND %s' % ' '.join(self._flatten(op2[1]))
        raise SyntaxError("Don't know how to generate AND with ops %s, %s" % (op1, op2))

    def _gen_instruction_sub(self, token, z80='SUB'):
        assert len(token['operands']) == 2
        op1, op2 = token['operands']
        if op1 == 'AL':
            if op2 in self.regmap:
                return '%s %s' % (z80, self.regmap[op2])
            if self._is_ptr_read_through_bx(op2):
                return '%s (HL)' % z80
            return '%s %s' % (z80, ' '.join(self._flatten(op2)))
        if op1 == 'BX' and op2 == 'DX':
            return 'SBC HL, DE'
        raise SyntaxError("Don't know how to generate %s with ops %s, %s" % (z80, op1, op2))

    def _gen_instruction_sbb(self, token):
        return self._gen_instruction_sub(token, 'SBC')

    def _gen_instruction_xor(self, token):
        assert len(token['operands']) == 2
        op1, op2 = token['operands']
        if op1 == 'AL':
            if op2 in self.regmap:
                return 'XOR %s' % self.regmap[op2]
            if isinstance(op2, tuple) and op2[0] == 'LOW' and isinstance(op2[1], int):
                op2 = op2[1]
            if isinstance(op2, int) and 0 <= op2 <= 255:
                return 'XOR %d' % op2
        if (op1, op2) == ('AH', 'AH'):
            return 'PUSH AF\n\tXOR A\n\tLD B\', 0\n\tPOP AF'
        if (op1, op2) == ('SI', 'SI'):
            return 'PUSH A\n\tXOR A\n\tXOR IYH\n\tXOR IYL\n\tPOP A'
        if (op1, op2) == ('DI', 'DI'):
            return 'PUSH A\n\tXOR A\n\tXOR IXH\n\tXOR IXL\n\tPOP A'
        raise SyntaxError("Don't know how to generate XOR with ops %s, %s" % (op1, op2))

    def _gen_instruction_stosb(self, token):
        assert len(token['operands']) == 0
        return 'LD (IX), A'

    def _gen_instruction_xthl(self, token):
        assert len(token['operands']) == 0
        return 'EX (SP), HL'

    def _gen_instruction_ror(self, token):
        assert len(token['operands']) == 2
        op1, op2 = token['operands']
        if op2 == 1:
            if op1 == 'AL':
                return 'RRA'
            if not self._is_16bit_reg(op1) and op1 in self.regmap:
                return 'RR %s' % self.regmap[op]
            if op1 == '[BX]':
                return 'RR (HL)'
        raise SyntaxError("Don't know how to generate ROR with op %s, %s" % (op1, op2))

    def _gen_instruction_rcr(self, token):
        assert len(token['operands']) == 2
        op1, op2 = token['operands']
        if op2 == 1:
            if op1 == 'AL':
                return 'RRCA'
            if op1 in self.regmap:
                if self._is_16bit_reg(op1):
                    return 'PUSH HL\n\tADC HL, %s\n\tLD %s, HL\n\tPOP HL' % (self.regmap[op1], self.regmap[op1])
                else:
                    return 'RRC %s' % self.regmap[op]
            if op1 == '[BX]':
                return 'RRC (HL)'

        raise SyntaxError("Don't know how to generate RCR with op %s, %s" % (op1, op2))

    def _gen_instruction_rcl(self, token):
        assert len(token['operands']) == 2
        op1, op2 = token['operands']
        if op2 == 1:
            if op1 == 'AL':
                return 'RLCA'
            if op1 in self.regmap:
                if self._is_16bit_reg(op1):
                    return 'PUSH HL\n\tSBC HL, %s\n\tLD %s, HL\n\tPOP HL' % (self.regmap[op1], self.regmap[op1])
                else:
                    return 'RLC %s' % self.regmap[op]
            if op1 == '[BX]':
                return 'RLC (HL)'

        raise SyntaxError("Don't know how to generate RCL with op %s, %s" % (op1, op2))

    def _gen_instruction_rol(self, token):
        assert len(token['operands']) == 2
        op1, op2 = token['operands']
        if op2 == 1:
            if op1 == 'AL':
                return 'RLA'
            if not self._is_16bit_reg(op1) and op1 in self.regmap:
                return 'RL %s' % self.regmap[op]
            if op1 == '[BX]':
                return 'RL (HL)'
        raise SyntaxError("Don't know how to generate ROL with op %s, %s" % (op1, op2))

    def _gen_instruction_lodsb(self, token):
        return '; LODSB %s' % token

    def _gen_instruction_test(self, token):
        return '; TEST %s' % token

    def _gen_instruction_movi(self, token):
        return '; MOVI %s' % token

    def _gen_instruction_neg(self, token):
        assert len(token['operands']) == 1
        op = token['operands'][0]
        if op == 'AL':
            return 'NEG'
        if op == 'BX':
            return ('PUSH A\n\t' +
                    'XOR A\n\t' +
                    'SUB L\n\t' +
                    'LD L, A\n\t' +
                    'SBC A, A\n\t' +
                    'SUB H\n\t' +
                    'LD H, A\n\t' +
                    'POP A')
        if op == 'DX':
            return ('PUSH A\n\t' +
                    'XOR A\n\t' +
                    'SUB E\n\t' +
                    'LD E, A\n\t' +
                    'SBC A, A\n\t' +
                    'SUB D\n\t' +
                    'LD D, A\n\t' +
                    'POP A')
        raise SyntaxError("Don't know how to generate NEG with op %s" % op)

    def _gen_instruction_shr(self, token):
        assert len(token['operands']) == 2
        op1, op2 = token['operands']
        if op2 == 1 and op1 == 'BX':
            return 'SRL H\n\tRR L'
        if op2 == 1 and op1 == 'DX':
            return 'SRL D\n\tRR E'
        raise SyntaxError("Don't know how to generate SHR %s, %s" % (op1, op2))

    def _gen_instruction_not(self, token):
        assert len(token['operands']) == 1
        op = token['operands'][0]
        if op == 'AL':
            return 'CPL'
        raise SyntaxError("Don't know how to generate NOT %s" % op)

    def _gen_instruction_mul(self, token):
        assert len(token['operands']) == 1
        op = token['operands'][0]
        if op == 'DX':
            return '; MUL DX'
        raise SyntaxError("Don't know how to generate MUL %s" % op)

    def _gen_instruction(self, token):
        op = token['op']
        instr = getattr(self, '_gen_instruction_' + op.lower())(token)
        return '\t%s\t\t; %s' % (instr, token['comment']) if token['comment'] else '\t' + instr

    def _gen_macro_call(self, token):
        args = []
        for arg_tuple in token['args']:
            for arg in arg_tuple:
                if arg['type'] in ('token', 'number'):
                    args.append(arg['value'])
                else:
                    raise SyntaxError("Unexpected type for macro argument: %s" % arg['type'])
        line = '\t%s %s' % (token['identifier'], ', '.join(args))
        return '%s ; %s' % (line, token['comment']) if token['comment'] else line

    def _gen_org(self, token):
        return '\torg %s' % token['value']

    def _gen_db(self, token):
        return '\tdb %s' % ', '.join(str(b) for b in token['bytes'])

    def _gen_dw(self, token):
        return '\tdw %s' % ', '.join(str(b) for b in token['bytes'])

    def _gen_label_def(self, token):
        return '%s: ; %s' % (token['identifier'], ' '.join(token['attrs']))

    def _gen_equ(self, token):
        equ = '\t%s equ %s' % (token['identifier'], ' '.join(token['attrs']))
        return '%s ; %s' % (equ, token['comment']) if token['comment'] else equ

    def _gen_if(self, token):
        return 'IF %s' % ' '.join(token['cond'])

    def _gen_ifdef(self, token):
        return 'IFDEF %s' % ' '.join(token['cond'])

    def _gen_end_if(self, token):
        return 'ENDIF'

    def _gen_macro(self, token):
        macro = '%s MACRO %s' % (token['identifier'], ', '.join(token['args']))
        return '%s ; %s' % (macro, token['comment']) if token['comment'] else macro

    def _gen_end_macro(self, token):
        return 'ENDM'

    def _gen_end_forc(self, token):
        return 'ENDM'

    def _gen_echo(self, token):
        return '\t.WARNING %s' % ' '.join(token['value'] for token in token['msg'])

    def _gen_forc(self, token):
        assert len(token['args']) == 2
        op1, op2 = token['args']
        return '\tIRP %s, %s' % (op1['value'], op2['value'])

    def _gen_ifdif(self, token):
        assert len(token['ops']) == 2
        op1, op2 = token['ops']
        return '\tIF %s != %s' % (op1, op2)

    def _gen_ife(self, token):
        assert len(token['cond']) == 2
        op1, op2 = token['cond']
        return '\tIF %s = %s' % (op1, op2)

    def _gen_else(self, token):
        return '\tELSE'

if __name__ == '__main__':
    lexer = Lexer(sys.stdin)
    parser = Parser(lexer)
    writer = PasmoWriter(parser)

    try:
        for line in writer.lines():
            print(line)
    except SyntaxError as e:
        print("Exception: %s" % e)
        traceback.print_exc(file=sys.stdout)

