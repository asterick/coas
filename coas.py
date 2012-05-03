import re, os, struct

_guidCounter = 0
def guid():
    global _guidCounter
    _guidCounter = _guidCounter + 1
    return _guidCounter

class AssemblerException(Exception):
    def __init__(self, pos, err):
        self.pos = pos
        self.error = err
    def __str__(self):
        return "%s: %s" % (repr(self.pos), self.error)

class ExpressionToken:
    def __init__(self, pos, category, token):
        self.pos = pos
        self.category = category
        self.token = token

    def __str__(self):
        return "%s token '%s'" % (self.category, self.token)

class AssemblerAnnotation:
    def __init__(self, pos):
        self.pos = pos

class AssemblerMacro:
    def __init__(self, name, args, terms):
        for a in args:
            if isinstance(a, AssemblerRegister):
                raise AssemblerException(a.pos, "Cannot use register name as argument")
        self.name = name
        self.args = args
        self.terms = terms
        self.instance = 0

    def id(self):
        self.instance += 1
        return "%s%i" % (self.name, self.instance)

    def process(self, t):
        params = t.parameters
        words = {}

        if len(params) > 0:
            for idx, param in enumerate(params[0].list):
                words[self.args[idx].word] = param
            param_count = len(params[0].list)
        else:
            param_count = 0

        if param_count != len(self.args):
            raise AssemblerException(t.pos, "Macro argument count mismatch")

        uuid = self.id()
        for t in self.terms:
            e = t.clone(remap=uuid)
            if isinstance(e, AssemblerMeta) or isinstance(e, AssemblerWord):
                e.parameters = [i.fold(words=words) for i in e.parameters]
            yield e

class AssemblerMeta:
    def __init__(self, pos, name):
        self.pos = pos
        self.name = name
        self.parameters = []

    def clone(self, **kwargs):
        m = AssemblerMeta(self.pos, self.name)
        m.parameters = [s.clone(**kwargs) for s in self.parameters]
        return m

    def setGroup(self, group):
        for p in self.parameters:
            p.setGroup(group)

    def __str__(self):
        return "pseudo-operator '%s' %s" % (self.name, ' '.join([str(s) for s in self.parameters]))

class AssemblerDataBlock:
    def __init__(self, pos, data):
        self.pos = pos
        self.data = data
    def add(self, data):
        self.data += data
    def __str__(self):
        return "binary <%i words>" % len(self.data)

class AssemblerLabel:
    def __init__(self, pos, name):
        self.pos = pos
        self.name = name

    def setGroup(self, group):
        if self.name[0] == '_':
            self.name += ":%i" % group

    def clone(self, **kwargs):
        if self.name[0] == '_' and 'remap' in kwargs:
            return AssemblerLabel(self.pos, "%s:%s" % (self.name, kwargs['remap']))

        return self

    def __str__(self):
        return "label '%s'" % (self.name)

class AssemblerOperator:
    def __init__(self, pos, operation):
        self.pos = pos
        self.operation = operation

    def __str__(self):
        return "operator '%s'" % self.operation.encode('unicode_escape')

class AssemblerEOL:
    def __init__(self):
        pass
    def __str__(self):
        return "end of line"

class AssemblerEOF:
    def __init__(self):
        pass
    def __str__(self):
        return "end of file"

# --- Expression Types
class AssemblerExpression:
    def fold(self, **kwargs):
        return self
    def setGroup(self, group):
        pass
    def clone(self, **kwargs):
        return self

class AssemblerString(AssemblerExpression):
    def __init__(self, pos, string):
        self.pos = pos
        self.string = string

    def __str__(self):
        return '"%s"' % self.string.encode("unicode_escape")

class AssemblerNumber(AssemblerExpression):
    def __init__(self, pos, number):
        self.pos = pos
        self.number = number

    def hasTerms(self):
        return False

    def relocatible(self):
        return False

    def __str__(self):
        return "%s" % self.number

class AssemblerRegister(AssemblerExpression):
    def __init__(self, pos, reg):
        self.pos = pos
        self.register = reg.lower()

    def hasTerms(self):
        return False

    def relocatible(self):
        return False

    def __str__(self):
        return 'reg %s' % self.register

class AssemblerWord(AssemblerExpression):
    def __init__(self, pos, word):
        self.pos = pos
        self.word = word.lower()
        self.parameters = []

    def hasTerms(self):
        return True

    def relocatible(self):
        return True

    def setGroup(self, group):
        for p in self.parameters:
            p.setGroup(group)
        if self.word[0] == '_':
            self.word += ":%i" % (group)

    def clone(self, **kwargs):
        if self.word[0] == '_' and 'remap' in kwargs:
            word = "%s:%s" % (self.word, kwargs['remap'])
        else:
            word = self.word

        w = AssemblerWord(self.pos, word)
        w.parameters = [s.clone(**kwargs) for s in self.parameters]
        return w

    def fold(self, **kwargs):
        word = self.word

        if 'validate' in kwargs:
            kwargs['validate'](word)

        if ('words' in kwargs) and (word in kwargs['words']):
            return kwargs['words'][word].clone()

        return self

    def __str__(self):
        if self.parameters:
            return "term '%s' %s" % (self.word, ' '.join([str(s) for s in self.parameters]))
        else:
            return "term '%s'" % (self.word)

class AssemblerUnary(AssemblerExpression):
    def __init__(self, pos, op, term):
        self.pos = pos
        self.operation = op
        self.term = term

        if isinstance(term, AssemblerString) or isinstance(self.term, AssemblerRegister):
            raise AssemblerException(term.pos, "Cannot operate on %s" % term)

    def hasTerms(self):
        return self.term.hasTerms()

    def relocatible(self):
        return not self.term.hasTerms()

    def setGroup(self, group):
        self.term.setGroup(group)

    def clone(self, **kwargs):
        return AssemblerUnary(self.pos, self.operation, self.term.clone(**kwargs))

    def fold(self, **kwargs):
        self.term = self.term.fold(**kwargs)

        if not isinstance(self.term, AssemblerNumber):
            return self

        if self.operation == '-':
            return AssemblerNumber(self.pos, -self.term.number)
        elif self.operation == '~':
            return AssemblerNumber(self.pos, ~self.term.number)

    def __str__(self):
        return "%s%s" % (self.operation, self.term)

class AssemblerBinary(AssemblerExpression):
    OPERATION = {
    '+': lambda a, b: a + b,
    '*': lambda a, b: a * b,
    '/': lambda a, b: a / b,
    '%': lambda a, b: a % b,
    '^': lambda a, b: a ^ b,
    '|': lambda a, b: a | b,
    '&': lambda a, b: a & b,
    '<<': lambda a, b: a << b,
    '>>': lambda a, b: a >> b,
    '&&': lambda a, b: a and b,
    '||': lambda a, b: a or b,
    '!=': lambda a, b: (a != b) and 1 or 0,
    '==': lambda a, b: (a == b) and 1 or 0,
    '>=': lambda a, b: (a >= b) and 1 or 0,
    '<=': lambda a, b: (a <= b) and 1 or 0,
    '>': lambda a, b: (a > b) and 1 or 0,
    '<': lambda a, b: (a < b) and 1 or 0,
    }

    def __init__(self, pos, op, term_a, term_b):
        self.pos = pos
        self.operation = op

        if isinstance(term_b, AssemblerRegister) or isinstance(term_a, AssemblerRegister):
            if self.operation != '+':
                raise AssemblerException(self.pos, "Cannot perform operation %s on a register" % op)
                
            if isinstance(term_b, AssemblerRegister):
                if isinstance(term_a, AssemblerRegister):
                    raise AssemblerException(self.pos, "Cannot perform an operation against two registers")
                
                term_b, term_a = term_a, term_b

        if isinstance(term_a, AssemblerString):
            raise AssemblerException(term_a.pos, "Cannot operate on %s" % term_a)
        if isinstance(term_b, AssemblerString):
            raise AssemblerException(term_b.pos, "Cannot operate on %s" % term_b)

        self.term_a = term_a
        self.term_b = term_b

    def hasTerms(self):
        return self.term_a.hasTerms() or self.term_b.hasTerms()

    def relocatible(self):
        if self.operation != '+':
            return False
    
        if (self.term_a.hasTerms(), self.term_b.hasTerms()) in [(True, True), (False, False)]:
            return False
    
        return True

    def setGroup(self, group):
        self.term_a.setGroup(group)
        self.term_b.setGroup(group)

    def clone(self, **kwargs):
        return AssemblerBinary(self.pos, self.operation, self.term_a.clone(**kwargs), self.term_b.clone(**kwargs))

    def fold(self, **kwargs):
        self.term_a = self.term_a.fold(**kwargs)
        self.term_b = self.term_b.fold(**kwargs)

        if not isinstance(self.term_a, AssemblerNumber) or not isinstance(self.term_b, AssemblerNumber):
            # Do some stuff with identifies
            if self.operation in ["*", "+"]:
                if isinstance(self.term_a, AssemblerNumber):
                    self.term_a, self.term_b = self.term_b, self.term_a

                if isinstance(self.term_b, AssemblerNumber):
                    if self.operation == '*':
                        if self.term_b.number == 0:
                            return AssemblerNumber(self.pos, 0)
                        if self.term_b.number == 1:
                            return self.term_a
                    elif self.operation == '+':
                        if self.term_b.number == 0:
                            return self.term_a

            return self

        a, b = self.term_a.number, self.term_b.number
        pos = self.term_a.pos

        return AssemblerNumber(pos, self.OPERATION[self.operation](a, b))

    def __str__(self):
        return "(%s %s %s)" % (self.term_a, self.operation, self.term_b)

class AssemblerIndirect(AssemblerExpression):
    def __init__(self, pos, term):
        self.pos = pos
        self.term = term

    def clone(self, **kwargs):
        return AssemblerIndirect(self.pos, self.term.clone(**kwargs))

    def fold(self, **kwargs):
        self.term = self.term.fold(**kwargs)
        return self

    def __str__(self):
        return "[%s]" % (self.term)

class AssemblerParameterList:
    def __init__(self, *kargs):
        self.list = list(kargs)
    def setGroup(self, group):
        for p in self.list:
            p.setGroup(group)
    def clone(self, **kwargs):
        return AssemblerParameterList(*[s.clone(**kwargs) for s in self.list])
    def add(self, e):
        self.list += [e]
    def fold(self, **kwargs):
        self.list = [e.fold(**kwargs) for e in self.list]
        return self
    def __str__(self):
        return ', '.join([str(k) for k in self.list])

def chunks(l, n):
    for i in xrange(0, len(l), n):
        yield l[i:i+n]

class Assembler:
    FLAGS = re.X|re.U|re.I
    RADIX = {'hex_a': 16, 'oct': 8, 'dec': 10, 'bin_a': 2, 'bin_b': 2}
    UNARY = ["-","~"]
    OPEN_CLOSE = { '(': ')', '[': ']' }
    INDIRECT = '['
    ORDER_OF_OPERATIONS = [
        ["%"],
        ["/","*"],
        ["&","|","^"],
        ["<<",">>"],
        [">=","<=",">","<","==","!="],
        ["||"],
        ["&&"],
        ["-"],
        ["+"]
    ]
    REGISTER = re.compile(r"^(push|pop|pc|sp|ex|a|b|c|x|y|z|i|j)$",FLAGS)
    TOKENS = re.compile(r"""
        (?P<comment>;.*)|
        (?P<number>(
            ((0x|\$)(?P<hex_a>[0-9a-fA-F]+))|
            (0b(?P<bin_a>[01]+))|
            ((?P<bin_b>[01]+)b)|
            (0(?P<oct>[0-9]+))|
            (?P<dec>[0-9]+)
        ))|
        (?P<open>(\(|\[))|
        (?P<close>(\)|\]))|
        (?P<comma>,)|
        (?P<operator>(<<|>>|>=|<=|>|<|==|!=|\+|!|~|-|%|/|\*|&|\||\^|&&|\|\|))|
        ('(?P<char>(\\.|[^'])*)')|
        ("(?P<string>(\\.|[^"])*)")|
        (?P<meta>(\.|\#)[\w]+)|
        ((?P<label>[\w]+):)|
        (:(?P<label_legacy>[\w]+))|
        (?P<word>[\w]+)|
        (?P<trap>[^\s])
    """,FLAGS)

    def tokenize(self, filename):
        source = file(filename,"r").read().decode('utf8')
        for line_num, line in enumerate(source.splitlines()):
            for m in self.TOKENS.finditer(line):
                pos, token = (filename, line_num, m.start(0), line), m.groupdict()

                # Exceptional tokens
                if token['comment']:
                    continue
                elif token['trap']:
                    raise AssemblerException(pos, "Cannot parse character '%s'" % token['trap'].encode('unicode_escape'))

                if token['number']:
                    for k, v in token.items():
                        if v and k in self.RADIX:
                            yield AssemblerNumber(pos, int(v, self.RADIX[k]))
                elif token['open']:
                    yield ExpressionToken(pos,'open',token['open'])
                elif token['close']:
                    yield ExpressionToken(pos,'close',token['close'])
                elif token['comma']:
                    yield ExpressionToken(pos,'comma',token['comma'])
                elif token['word']:
                    if self.REGISTER.match(token['word']):
                        yield AssemblerRegister(pos,token['word'])
                    else:
                        yield AssemblerWord(pos,token['word'])
                elif token['label']:
                    yield AssemblerLabel(pos,token['label'])
                elif token['label_legacy']:
                    yield AssemblerLabel(pos,token['label_legacy'])
                elif token['meta']:
                    yield AssemblerMeta(pos,token['meta'])
                elif token['operator']:
                    yield AssemblerOperator(pos,token['operator'])
                elif token['char']:
                    value = token['char'].decode('unicode_escape')
                    if len(value) > 1:
                        raise AssemblerException(pos, "Character literal greater than one character")
                    yield AssemblerNumber(pos, ord(value))
                elif token['string']:
                    yield AssemblerString(pos, token['string'].decode('unicode_escape'))
            yield AssemblerEOL()
        yield AssemblerEOF()

    def term(self, tokens):
        top = tokens[0]
        if isinstance(top, AssemblerExpression):
            return tokens[0], tokens[1:]
        elif isinstance(top, AssemblerOperator):
            if not top.operation in self.UNARY:
                raise AssemblerException(top.pos, "%s used as a unary operator" % top)

            tokens, term = self.term(tokens[1:])

            return AssemblerUnary(top.pos, top.operation, term), tokens
        elif isinstance(top, ExpressionToken) and top.category == 'open':
            exp, tokens = self.expression(tokens[1:])
            close, tokens = tokens[0], tokens[1:]
            if not (isinstance(close, ExpressionToken) and 
                    close.category == 'close' and
                    close.token == self.OPEN_CLOSE[top.token]):
                    raise AssemblerException("Unclosed %s" % top)

            if top.token == self.INDIRECT:
                exp = AssemblerIndirect(top.pos, exp)
            return exp, tokens
        else:
            raise AssemblerException(top.pos, "Unexpected %s" % top)

    def expression(self, tokens):
        term, tokens = self.term(tokens)

        flattened = [term]
        while isinstance(tokens[0], AssemblerOperator):
            op = tokens[0]
            term, tokens = self.term(tokens[1:])
            flattened += [op, term]

        for group in self.ORDER_OF_OPERATIONS:
            if group == ['+']:
                for x in range(0, len(flattened)-2, 2):
                    for y in range(x+2, len(flattened), 2):
                        a, b = flattened[x], flattened[y]

                        if isinstance(a, AssemblerRegister):
                            a, b = b, a

                        flattened[x], flattened[y] = a, b

            idx = 1
            while idx < len(flattened):
                if group == ['-']:
                    # Convert subtraction to addition
                    if flattened[idx].operation == '-':
                        term = flattened[idx+1]
                        flattened[idx].operation = '+'
                        flattened[idx+1:idx+2] = [AssemblerUnary(term.pos, '-', term)]

                    idx += 2
                elif flattened[idx].operation in group:
                    term_a, op, term_b = flattened[idx-1:idx+2]
                    flattened[idx-1:idx+2] = [AssemblerBinary(term_a.pos, op.operation, term_a, term_b)]
                else:
                    idx += 2

        return flattened[0], tokens

    def arguments(self, assembly):
        tokens = []
        while True:
            token = assembly.next()
            tokens += [token]
            if isinstance(token, AssemblerEOL):
                break

        arguments = []
        while len(tokens) > 1:
            exp, tokens = self.expression(tokens)
            params = [exp]

            while isinstance(tokens[0], ExpressionToken) and tokens[0].category == 'comma':
                exp, tokens = self.expression(tokens[1:])
                params += [exp]
            
            arguments += [AssemblerParameterList(*params)]

        return arguments

    def parse(self, source):
        assembly = self.tokenize(source)

        while True:
            token = assembly.next()

            if isinstance(token, AssemblerEOL):
                continue
            elif isinstance(token, AssemblerEOF):
                break
            
            if isinstance(token, AssemblerLabel):
                yield token
                token = assembly.next()

            if isinstance(token, AssemblerMeta) or isinstance(token, AssemblerWord):
                operation = token
                operation.parameters = self.arguments(assembly)
                yield operation

            elif not isinstance(token, AssemblerEOL):
                raise AssemblerException(token.pos, "Unexpected %s" % token)

    def binary(self, pos, type, data):
        if type in ['.big', '.little']:
            # Round up
            if len(data) & 1:
                data += [AssemblerNumber(None, 0)]

            if type == '.big':
                little, big = data[1::2], data[::2]
            elif type == '.little':
                little, big = data[::2], data[1::2]

            data = [AssemblerBinary(l.pos, '|', 
                    AssemblerBinary(l.pos, '<<', big[i], AssemblerNumber(l.pos, 8)),
                    AssemblerBinary(l.pos, '&', l, AssemblerNumber(l.pos, 0xFF)))
                    for i, l in enumerate(little)]

        return AssemblerDataBlock(pos, data)

    """Flatten assembly, handles include, incbytes, incbig, inclittle"""
    def flatten(self, source):
        for d in self.parse(source):
            if isinstance(d, AssemblerMeta) and d.name in [".include", ".incbytes", ".incbig", '.inclittle']:
                if len(d.parameters) != 1:
                    raise AssemblerException(d.pos, "Malformed %s expression" % (d.name))

                base = os.path.dirname(source)
                for p in d.parameters[0].list:
                    if isinstance(p, AssemblerString):
                        filename = os.path.join(base, p.string)
                        if d.name == '.include':
                            for k in self.flatten(filename):
                                yield k
                        else:
                            trans = {'.incbig':'.big', '.inclittle':'.little', '.incbytes':'.data'}[d.name]
                            yield self.binary(p.pos, trans, [ord(c) for c in file(filename,"rb").read()])
                    else:
                        raise AssemblerException(p.pos, "%s is not a string" % p)
            else:
                yield d

    """ Search and replace words """
    def equate(self, token, words):
        if isinstance(token, AssemblerMeta) or isinstance(token, AssemblerWord):
            token.parameters = [s.fold(words=words) for s in token.parameters]
            return token
        else:
            return token

    """ Find and replace all the macros with their respective locations"""
    def macro(self, t, macros, equs):
        if isinstance(t, AssemblerWord) and (t.word in macros):
            macro = macros[t.word]
            for tl in macro.process(t):
                for y in self.macro(tl, macros, equs):
                    yield self.equate(y, equs)
        else:
            yield self.equate(t, equs)

    """ Handle the insertion and replacement of macros and equs """
    def definitions(self, assembly):
        macros, equs = {}, {}

        for t in assembly:
            if isinstance(t, AssemblerMeta):
                if t.name == '.equ':
                    if len(t.parameters) != 2 or len(t.parameters[0].list) != 1 or len(t.parameters[1].list) != 1:
                        raise AssemblerException(d.pos, "Malformed %s expression" % (t.name))

                    name = t.parameters[0].list[0]
                    equ = t.parameters[1].list[0]

                    if not isinstance(name, AssemblerWord):
                        raise AssemblerException(d.pos, "Expected a term, found %s" % t)

                    if not isinstance(equ, AssemblerExpression):
                        raise AssemblerException(d.pos, "Expected an expression, found %s" % t)

                    equs[name.word] = equ
                elif t.name == '.macro':
                    if len(t.parameters) > 2 or len(t.parameters[0].list) != 1:
                        raise AssemblerException(t.pos, "Malformed %s expression" % (t.name))

                    name = t.parameters[0].list[0]
                    if len(t.parameters) == 1:
                        args = []
                    else:
                        args = t.parameters[1].list

                    contents = []
                    nesting = 0

                    if not isinstance(name, AssemblerWord):
                        raise AssemblerException(d.pos, "Expected a term, found %s" % t)

                    for word in args:
                        if not isinstance(name, AssemblerWord):
                            raise AssemblerException(d.pos, "Expected a term, found %s" % t)

                    try:
                        while True:
                            token = assembly.next()
                            if isinstance(token, AssemblerMeta):
                                if token.name == '.endmacro':
                                    break
                                elif token.name == '.equ':
                                    raise AssemblerException(token.pos, "EQU not allowed inside of a macro")
                                elif token.name == '.macro':
                                    raise AssemblerException(token.pos, "Cannot nest macros")
                            if isinstance(token, AssemblerLabel) and token.name[0] != '_':
                                raise AssemblerException(token.pos, "Global label discovered inside macro")
                            contents += [t for t in self.macro(token, macros, equs)]

                    except StopIteration:
                        raise AssemblerException(t.pos, "Unexpected End of File")

                    macros[name.word] = AssemblerMacro(name.word, args, contents)
                else:
                    for k in self.macro(t, macros, equs):
                        yield k
            else:
                for k in self.macro(t, macros, equs):
                    yield k

    def flatten_strings(self, expressions):
        for e in expressions:
            if isinstance(e, AssemblerString):
                for ch in e.string:
                    yield AssemblerNumber(e.pos, ord(ch))
            else:
                yield e

    """ Handle .proc, and break down string """
    def process(self, tokens):
        stack = []
        group, next = 0, 1
        
        for t in tokens:
            t.setGroup(group)

            if isinstance(t, AssemblerMeta):
                if t.name in ['.big', '.little', '.data']:
                    if len(t.parameters) != 1:
                        raise AssemblerException(t.pos, "Malformed expression %s" % t)

                    t.parameters[0].list = [f for f in self.flatten_strings(t.parameters[0].list)]
                    yield t
                elif t.name == '.proc':
                    stack += [group]
                    group = next
                    next += 1
                elif t.name == '.endproc':
                    if len(stack) < 1:
                        raise AssemblerException(t.pos, "Unexpected %s" % t)
                    group, stack = stack[-1], stack[:-1]
                else:
                    yield t
            else:
                yield t

        if len(stack):
            raise AssemblerException(t, "Unexpected end of file")

    def pack(self, tokens):
        for t in tokens:
            if isinstance(t, AssemblerMeta) and t.name in ['.big', '.little', '.data']:
                data = t.parameters[0].list
                binary = self.binary(t.pos, t.name, [d for d in data])
                yield binary
            else:
                yield t

    def label(self, tokens, words, discovered = []):
        position, tokens = 0, list(tokens)
        for t in tokens:
            if isinstance(t, AssemblerMeta) and t.name in ['.align']:
                if len(t.parameters) != 1 or len(t.parameters[0].list) != 1:
                    raise AssemblerException("Malformed expression %s" % t)

                num = t.parameters[0].list[0].fold(words=words)
                t.parameters[0].list = [num]

                if position != None and isinstance(num, AssemblerNumber):
                    offset = position % num.number
                    if offset:
                        yield AssemblerDataBlock(t.pos, [0]*(num.number-offset))
                else:
                    position = None
                    yield t
                    continue
            elif isinstance(t, AssemblerMeta) and t.name in ['.org', '.bss']:
                if len(t.parameters) != 1 or len(t.parameters[0].list) != 1:
                    raise AssemblerException("Malformed expression %s" % t)

                num = t.parameters[0].list[0].fold(words=words)
                t.parameters[0].list = [num]

                yield t

                if not isinstance(num, AssemblerNumber):
                    position = None
                    continue
                
                if t.name == '.bss':
                    if position != None:
                        position += num.number
                else:
                    position = num.number
            elif isinstance(t, AssemblerDataBlock):
                if position != None:
                    position += len(t.data)
                yield t
            elif isinstance(t, AssemblerLabel):
                word = t.name
                
                discovered[:] += [word]

                if position == None:
                    yield t
                    continue

                if word in words:
                    raise AssemblerException(t.pos, "Label redefinition %s" % t)

                words[word] = AssemblerNumber(t.pos, position)
            else:
                position = None
                yield t

    """ Attempt to stuff words into their respective areas """
    def refold(self, tokens, words):
        for t in [t for t in tokens]:
            yield self.equate(t, words)

    """ Convert complete instructions into binaries """
    TWO_FORM = {
        'SET': 0x01, 'ADD': 0x02, 'SUB': 0x03, 'MUL': 0x04,
        'MLI': 0x05, 'DIV': 0x06, 'DVI': 0x07, 'MOD': 0x08,
        'MDI': 0x09, 'AND': 0x0a, 'BOR': 0x0b, 'XOR': 0x0c,
        'SHR': 0x0d, 'ASR': 0x0e, 'SHL': 0x0f, 'IFB': 0x10,
        'IFC': 0x11, 'IFE': 0x12, 'IFN': 0x13, 'IFG': 0x14,
        'IFA': 0x15, 'IFL': 0x16, 'IFU': 0x17, 'ADX': 0x1a,
        'SBX': 0x1b, 'STI': 0x1e, 'STD': 0x1f
    }

    ONE_FORM = {
        'JSR': 0x01, 'INT': 0x08, 'IAG': 0x09, 'IAS': 0x0a,
        'RFI': 0x0b, 'IAQ': 0x0c, 'HWN': 0x10, 'HWQ': 0x11,
        'HWI': 0x12
    }
    
    REG_FIELD = {
        'a': 0, 'b': 1, 'c': 2, 'x': 3, 'y': 4, 'z': 5, 'i': 6, 'j': 7,
        'sp': 0x1b, 'pc': 0x1c, 'ex': 0x1d, 
        'peek': 0x19, 'push':0x18, 'pop': 0x18
    }

    IND_REG = {
        'a': 0x8, 'b': 0x9, 'c': 0xa, 'x': 0xb, 'y': 0xc, 'z': 0xd, 'i': 0xe, 'j': 0xf,
        'sp': 0x19
    }

    IND_REG_OFF = {
        'a': 0x10, 'b': 0x11, 'c': 0x12, 'x': 0x13, 'y': 0x14, 'z': 0x15, 'i': 0x16, 'j': 0x17,
        'sp': 0x1a
    }

    def getField(self, exp, **kwargs):
        if isinstance(exp, AssemblerRegister):
            return self.REG_FIELD[exp.register], None
        elif isinstance(exp, AssemblerIndirect):
            if isinstance(exp.term, AssemblerBinary):
                op, a, b = exp.term.operation, exp.term.term_a, exp.term.term_b

                if op != '+':
                    return None

                if not isinstance(a, AssemblerRegister):
                    return None

                if not isinstance(b, AssemblerNumber):
                    if not 'flatten' in kwargs:
                        return None
                    else:
                        return (self.IND_REG_OFF[a.register], b)

                return (self.IND_REG_OFF[a.register], b.number & 0xFFFF)
            elif isinstance(exp.term, AssemblerRegister):
                return self.IND_REG[exp.term.register], None
            elif isinstance(exp.term, AssemblerNumber):
                return (0x1e, exp.term.number & 0xFFFF)
        elif isinstance(exp, AssemblerNumber):
            num = exp.number & 0xFFFF
        
            if num >= -1 and num <= 30:
                return 0x20 + (num+1), None
            else:
                return (0x1f, num)
        elif 'flatten' in kwargs:
            return (0x1f, exp)

        return None

    def instruct(self, tokens, **kwargs):
        for t in tokens:
            if isinstance(t, AssemblerWord):
                name = t.word.upper()
                if name in self.ONE_FORM:
                    if len(t.parameters) != 1 or len(t.parameters[0].list) != 1:
                        raise AssemblerException(t.pos, "Malformed Expression %s" % t)

                    field_o = 0
                    field_b = self.ONE_FORM[name], None
                    field_a = self.getField(t.parameters[0].list[0], **kwargs)
                elif name in self.TWO_FORM:
                    if len(t.parameters) != 1 or len(t.parameters[0].list) != 2:
                        raise AssemblerException(t.pos, "Malformed Expression %s" % t)

                    field_o = self.TWO_FORM[name]
                    field_b = self.getField(t.parameters[0].list[0], **kwargs)
                    field_a = self.getField(t.parameters[0].list[1], **kwargs)
                else:
                    raise AssemblerException(t.pos, "Unrecognized operation %s" % t)

                if field_b == None or field_a == None:
                    yield t
                    continue

                op_a, data_a = field_a
                op_b, data_b = field_b

                if op_b >= 0x1F:
                    raise AssemblerException(t.pos, "Cannot use literal for target")
                
                data = [(op_b << 5) | (op_a<<10) | field_o]

                if data_a:
                    data += [data_a]
                if data_b:
                    data += [data_b]

                block = AssemblerDataBlock(t.pos, data)
                yield block
            else:
                yield t

    def undefined(self, tokens, discovered):
        for t in [t for t in tokens]:
            def validate(w):
                if not w in discovered:
                    raise AssemblerException(t.pos, "Undefined term %s" % str(w))
                    
            if isinstance(t, AssemblerWord):
                for p in t.parameters:
                    for l in p.list:
                        l.fold(validate=validate)
            yield t

    def finished(self, set):
        for k in set:
            if isinstance(k, AssemblerMeta) and k.name in ['.org', '.bss']:
                continue
            if not isinstance(k, AssemblerDataBlock):
                return False
        return True

    def relocate(self, tokens, relocations):
        tokens = list(tokens)
        position = 0
        for t in tokens:
            if isinstance(t, AssemblerDataBlock):                
                for e in t.data:
                    if isinstance(e, AssemblerExpression) and e.relocatible():
                        relocations[:] += [position]
                    position += 1

            yield t

    def data(self, tokens, words):
        for t in tokens:
            if isinstance(t, AssemblerMeta) and t.name in ['.org', '.bss']:
                continue
            for b in t.data:
                if isinstance(b, AssemblerExpression):
                    o = b.fold(words=words)
                    
                    if isinstance(o, AssemblerNumber):
                        yield o.number & 0xFFFF
                    else:
                        raise AssemblerException(b.pos, "Could not evaluate to a number")
                else:
                    yield b
            yield AssemblerAnnotation(t.pos)

    def assemble(self, filename, relocate=False):
        labels, relocations, discovered, flatten = {}, [], [], False

        tokens = self.definitions(self.flatten(filename))
        tokens = self.pack(self.process(tokens))
            
        # Flatten tokens, flag parameters as relocating here
        if relocate:
            tokens = self.instruct(tokens, flatten=True)
            tokens = self.relocate(tokens, relocations)

        tokens = self.label(tokens, labels, discovered)
        tokens = self.undefined(tokens, discovered)

        while True:
            tokens = self.instruct(tokens, flatten=flatten)

            flatten = True
            tokens = list(tokens)
            if self.finished(tokens): 
                break
            tokens = self.label(tokens, labels)
            tokens = self.pack(tokens)
            tokens = self.refold(tokens, labels)

        return [b for b in self.data(tokens, words=labels)], labels, relocations

# ---- OUTPUT FORMATS

def binaryOutput(data):
    return ''.join([struct.pack(">H", o) for o in data if not isinstance(o, AssemblerAnnotation)])

def intelHex(data):
    data = ''.join([struct.pack(">H", o) for o in bin if not isinstance(o, AssemblerAnnotation)])

    chunkSize = 32
    def chunk(c):
        for i in range(0,len(c),chunkSize):
            yield c[i:i+chunkSize]

    def format(address, rec_type, data = []):
        data = [len(data), address>>8, address & 0xFF, rec_type] + data
        data += [((sum(data) ^ 0xFF) + 1) & 0xFF]
        return ":" + (''.join(["%2X" % d for d in data])).replace(' ','0')

    for addr, block in enumerate(chunk([ord(c) for c in data])):
        yield format(addr * chunkSize, 0, block)
    yield format(0,1)

def datOutput(data):
    leadIn = False
    for k in data:
        if not leadIn:
            yield ".dat "
            leadIn = True

        
        if isinstance(k, AssemblerAnnotation):
            yield "; %s\n" % k.pos[3]
            leadIn = False
        else:
            yield "%i " % k

def verilog(data):
    for k in data:
        if isinstance(k, AssemblerAnnotation):
            yield "// %s\n" % k.pos[3]
        else:
            yield ("%4x" % k).replace(" ","0") + " "

def mapping(words):
    for k, v in words.items():
        if isinstance(k, basestring):
            yield "%s\t%s" % (k, v)
