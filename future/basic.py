###############################
#          IMPORTS            #
###############################

from strings_with_arrows import *

###############################
#         CONSTANTS           #
###############################

DIGITS = '0123456789'

###############################
#           ERRORS            #
###############################

class Error:
    def __init__(self, pos_start, pos_end, err_name, details):
        self.pos_start = pos_start
        self.pos_end = pos_end
        self.err_name = err_name
        self.details = details
    
    def toString(self):
        result = f'{self.err_name}: {self.details}'
        result += f'\nFile: {self.pos_start.filename}, line: {self.pos_start.line + 1}'
        result += '\n\n' + string_with_arrows(self.pos_start.filetxt, self.pos_start, self.pos_end)
        return result

class IllegalCharError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end, 'Illegal Character', details)

class InvalidSyntaxError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end, 'Invalid Syntax', details)

###############################
#          POSITION           #
###############################

class Position:
    def __init__(self, index, line, col, filename, filetxt):
        self.index = index
        self.line = line
        self.col = col
        self.filename = filename
        self.filetxt = filetxt
    
    def fwd(self, curr_char = None):
        self.index += 1
        self.col += 1
        
        if curr_char == '\n':
            self.line += 1
            self.col = 0
        
        return self
    
    def copy(self):
        return Position(self.index, self.line, self.col, self.filename, self.filetxt)

###############################
#           TOKENS            #
###############################

# Token Types
TT_INT         = 'INT'
TT_FLOAT       = 'FLOAT'
TT_PLUS        = 'PLUS'
TT_MINUS       = 'MINUS'
TT_MULT        = 'MULT'
TT_DIV         = 'DIV'
TT_LBRACKET    = 'LBRACKET'
TT_RBRACKET    = 'RBRACKET'
TT_EOF         = 'EOF'

# Main class
class Token:
    def __init__(self, type_, value = None, pos_start = None, pos_end = None):
        self.type = type_
        self.value = value

        if pos_start:
            self.pos_start = pos_start.copy()
            self.pos_end = pos_start.copy()
            self.pos_end.fwd()
        
        if pos_end:
            self.pos_end = pos_end
    
    def __repr__(self):
        if self.value: return f'{self.type}:{self.value}'
        return f'{self.type}'

###############################
#           LEXER             #
###############################

class Lexer:
    def __init__(self, filename, text):
        self.filename = filename
        self.text = text
        self.pos = Position(-1, 0, -1, filename, text)         # incrementing in func fwd
        self.curr_char = None
        self.fwd()
    
    def fwd(self):
        self.pos.fwd(self.curr_char)
        self.curr_char = self.text[self.pos.index] if self.pos.index < len(self.text) else None

    def make_tokens(self):
        tokens = []

        while self.curr_char != None:
            if self.curr_char in ' \t': self.fwd()
            elif self.curr_char in DIGITS:
                tokens.append(self.make_number())    # parsing as number
            elif self.curr_char == '+':
                tokens.append(Token(TT_PLUS, pos_start = self.pos))
                self.fwd()
            elif self.curr_char == '-':
                tokens.append(Token(TT_MINUS, pos_start = self.pos))
                self.fwd()
            elif self.curr_char == '*':
                tokens.append(Token(TT_MULT, pos_start = self.pos))
                self.fwd()
            elif self.curr_char == '/':
                tokens.append(Token(TT_DIV, pos_start = self.pos))
                self.fwd()
            elif self.curr_char == '(':
                tokens.append(Token(TT_LBRACKET, pos_start = self.pos))
                self.fwd()
            elif self.curr_char == ')':
                tokens.append(Token(TT_RBRACKET, pos_start = self.pos))
                self.fwd()
            else:
                pos_start = self.pos.copy()
                char = self.curr_char
                self.fwd()
                return [], IllegalCharError(pos_start, self.pos, f'\'{char}\'')
        
        tokens.append(Token(TT_EOF, pos_start = self.pos))
        return tokens, None
    
    def make_number(self):
        num = ''
        count_dots = 0
        pos_start = self.pos.copy()

        while self.curr_char != None and self.curr_char in DIGITS + '.':
            if self.curr_char != '.': num += self.curr_char
            else:
                if count_dots >= 1: break
                count_dots += 1
                num += '.'
            self.fwd()

        if count_dots == 0: return Token(TT_INT, int(num), pos_start, self.pos)
        return Token(TT_FLOAT, float(num), pos_start, self.pos)

###############################
#           NODES             #
###############################

class NumberNode:
    def __init__(self, token):
        self.token = token

        self.pos_start = self.token.pos_start
        self.pos_end = self.token.pos_end
    
    def __repr__(self):
        return f'{self.token}'

class BinOpNode:
    def __init__(self, left_node, op_token, right_node):
        self.left_node = left_node
        self.op_token = op_token
        self.right_node = right_node

        self.pos_start = self.left_node.pos_start
        self.pos_end = self.right_node.pos_end
    
    def __repr__(self):
        return f'({self.left_node}, {self.op_token}, {self.right_node})'

class UnaryOpNode:
    def __init__(self, op_token, node):
        self.op_token = op_token
        self.node = node

        self.pos_start = self.op_token.pos_start
        self.pos_end = self.op_token.pos_end
    
    def __repr__(self):
        return f'({self.op_token}, {self.node})'

###############################
#        PARSE RESULT         #
###############################

class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None
    
    def register(self, res):
        if isinstance(res, ParseResult):
            if res.error: self.error = res.error
            return res.node
        
        return res

    def success(self, node):
        self.node = node
        return self

    def fail(self, error):
        self.error = error
        return self

###############################
#           PARSER            #
###############################

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.token_index = -1
        self.fwd()
    
    def fwd(self):
        self.token_index += 1
        if self.token_index < len(self.tokens):
            self.curr_token = self.tokens[self.token_index]
        return self.curr_token

    ###

    def parse(self):
        res = self.expr()
        if not res.error and self.curr_token.type != TT_EOF:
            return res.fail(InvalidSyntaxError(
                self.curr_token.pos_start, self.curr_token.pos_end, 'Expected a valid operator syntax'
            ))
        return res
        
    ###

    def factor(self):
        res = ParseResult()
        token = self.curr_token

        if token.type in (TT_PLUS, TT_MINUS):
            res.register(self.fwd())
            factor = res.register(self.factor())
            if res.error: return res
            return res.success(UnaryOpNode(token, factor))

        elif token.type in (TT_INT, TT_FLOAT):
            res.register(self.fwd())
            return res.success(NumberNode(token))

        elif token.type == TT_LBRACKET:
            res.register(self.fwd())
            expr = res.register(self.expr())
            if res.error: return res
            if self.curr_token.type == TT_RBRACKET:
                res.register(self.fwd())
                return res.success(expr)
            else:
                return res.fail(InvalidSyntaxError(
                    self.curr_token.pos_start, self.curr_token.pos_end, 'Expected \')\'' 
                ))
        
        return res.fail(InvalidSyntaxError(
            token.pos_start, token.pos_end, 'Expected INT or FLOAT type'
        ))
    
    def term(self):
        return self.bin_op(self.factor, (TT_MULT, TT_DIV))

    def expr(self):
        return self.bin_op(self.term, (TT_PLUS, TT_MINUS))

    ###

    def bin_op(self, func, ops):
        res = ParseResult()
        left = res.register(func())
        if res.error: return res

        while self.curr_token.type in ops:
            op_token = self.curr_token
            res.register(self.fwd())
            right = res.register(func())
            if res.error: return res
            left = BinOpNode(left, op_token, right)
        
        return res.success(left)

###############################
#           VALUES            #
###############################

class Number:
    def __init__(self, value):
        self.value = value
        self.set_pos()
    
    def set_pos(self, pos_start = None, pos_end = None):
        self.pos_start = pos_start
        self.pos_end = pos_end
        return self
    
    def added_to(self, other):
        if isinstance(other, Number):
            return Number(self.value + other.value)
    
    def subbed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value - other.value)
        
    def mult_by(self, other):
        if isinstance(other, Number):
            return Number(self.value * other.value)
    
    def dived_by(self, other):
        if isinstance(other, Number):
            return Number(self.value / other.value)
    
    def __repr__(self):
        return str(self.value)

###############################
#        INTERPRETER          #
###############################

class Interpreter:
    def visit(self, node):
        method_name = f'visit_{type(node).__name__}'
        method = getattr(self, method_name, self.no_visit_method)
        return method(node)
    
    def no_visit_method(self, node):
        raise Exception(f'No visit_{type(node).__name__} method defined')
    
    ###

    def visit_NumberNode(self, node):
        return Number(node.token.value).set_pos(node.pos_start, node.pos_end)
    
    def visit_BinOpNode(self, node):
        left = self.visit(node.left_node)
        right = self.visit(node.right_node)

        result = None

        if node.op_token == TT_PLUS:
            result = left.added_to(right)
        elif node.op_token == TT_MINUS:
            result = left.subbed_by(right)
        elif node.op_token == TT_MULT:
            result = left.mult_by(right)
        elif node.op_token == TT_DIV:
            result = left.dived_by(right)
        
        return Number(result).set_pos(node.pos_start, node.pos_end)

    def visit_UnaryOpNode(self, node):
        number = self.visit(node.node)

        if node.op_token.type == TT_MINUS:
            number = number.multed_by(Number(-1))
        
        return number.set_pos(node.pos_start, node.pos_end)

###############################
#            RUN              #
###############################

def run(filename, text):
    # generate tokens
    lexer = Lexer(filename, text)
    tokens, error = lexer.make_tokens()
    if error: return None, error

    # generate AST
    parser = Parser(tokens)
    ast = parser.parse()
    if ast.error: return None, ast.error

    # Run program
    interpreter = Interpreter()
    result = interpreter.visit(ast.node)

    return result, None