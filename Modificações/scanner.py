import re

class Token:
    def __init__(self, type, value, line):
        self.type = type
        self.value = value
        self.line = line

    def __repr__(self):
        return f'<{self.type}, "{self.value}">'

TOKEN_SPECIFICATION = [
    ('COMMENT',  r'//.*'),               
    ('DEF',      r'\bdef\b'),
    ('VAR',      r'\bvar\b'),
    ('SET',      r'\bset\b'),
    ('IF',       r'\bif\b'),
    ('ELSE',     r'\belse\b'),
    ('WHILE',    r'\bwhile\b'),
    ('RETURN',   r'\breturn\b'),
    ('PRINT',    r'\bprint\b'),
    ('TYPE',     r'\b(int|real|bool|void)\b'),
    ('BOOL',     r'\b(True|False)\b'),
    ('REAL',     r'\d+\.\d+'),
    ('INT',      r'\d+'),
    ('ID',       r'[a-zA-Z_][a-zA-Z0-9_]*'),
    ('OP_REL',   r'==|!=|<=|>=|<|>'),
    ('OP_ARIT',  r'\+|\-|\*|/'),
    ('ASSIGN',   r'='),
    ('COLON',    r':'),
    ('SCOLON',   r';'),
    ('LPAREN',   r'\('),
    ('RPAREN',   r'\)'),
    ('LBRACE',   r'\{'),
    ('RBRACE',   r'\}'),
    ('COMMA',    r','),
    ('STRING',   r'"[^"]*"'),
    ('NEWLINE',  r'\n'),
    ('SKIP',     r'[ \t\r]+'),
    ('MISMATCH', r'.'),
]

def scanner(source_code):
    tokens = []
    line_num = 1
    regex = '|'.join(f'(?P<{name}>{pattern})' for name, pattern in TOKEN_SPECIFICATION)

    for mo in re.finditer(regex, source_code):
        kind = mo.lastgroup
        value = mo.group()

        if kind == 'NEWLINE':
            line_num += 1
        elif kind == 'SKIP' or kind == 'COMMENT':
            continue
        elif kind == 'MISMATCH':
            print(f"ERRO LÉXICO: Caractere '{value}' inválido na linha {line_num}")
        else:
            if kind == 'STRING':
                value = value.strip('"')
            tokens.append(Token(kind, value, line_num))

    return tokens