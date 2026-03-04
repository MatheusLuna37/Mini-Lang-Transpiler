class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0

    def current_token(self):
        return self.tokens[self.pos] if self.pos < len(self.tokens) else None

    def eat(self, token_type):
        token = self.current_token()
        if token and token.type == token_type:
            self.pos += 1
            return token
        else:
            t_type = token.type if token else "EOF"
            t_line = token.line if token else "final"
            raise Exception(f"Erro Sintático na linha {t_line}: Esperado {token_type}, mas veio {t_type}")

    def parse_program(self):
        decls = []
        while self.pos < len(self.tokens):
            decls.append(self.parse_declaration())
        return ProgramNode(decls)

    def parse_declaration(self):
        t = self.current_token()
        if t is None: return None

        if t.type == 'VAR': return self.parse_var_decl()
        if t.type == 'DEF': return self.parse_def()
        if t.type == 'IF': return self.parse_if()
        if t.type == 'WHILE': return self.parse_while()
        if t.type == 'SET': return self.parse_set()
        if t.type == 'PRINT': return self.parse_print()
        if t.type == 'RETURN': return self.parse_return()
        raise Exception(f"Erro na linha {t.line}: Comando '{t.value}' não reconhecido.")

    def parse_var_decl(self):
        self.eat('VAR')
        name = self.eat('ID').value
        self.eat('COLON')
        tp = self.eat('TYPE').value
        val = None
        if self.current_token() and self.current_token().type == 'ASSIGN':
            self.eat('ASSIGN')
            val = self.parse_expression()
        self.eat('SCOLON')
        return VarDeclNode(name, tp, val)

    def parse_def(self):
        self.eat('DEF')
        name = self.eat('ID').value
        self.eat('LPAREN')
        params = []
        if self.current_token().type != 'RPAREN':
            params = self.parse_params()
        self.eat('RPAREN')
        self.eat('COLON')
        ret_type = self.eat('TYPE').value
        block = self.parse_block()
        return FunctionNode(name, params, ret_type, block)

    def parse_params(self):
        params = []
        params.append({'name': self.eat('ID').value, 'type': (self.eat('COLON'), self.eat('TYPE').value)[1]})
        while self.current_token() and self.current_token().type == 'COMMA':
            self.eat('COMMA')
            name = self.eat('ID').value
            self.eat('COLON')
            tp = self.eat('TYPE').value
            params.append({'name': name, 'type': tp})
        return params

    def parse_block(self):
        self.eat('LBRACE')
        statements = []
        while self.current_token() and self.current_token().type != 'RBRACE':
            statements.append(self.parse_declaration())
        self.eat('RBRACE')
        return statements

    def parse_if(self):
        self.eat('IF')
        self.eat('LPAREN')
        cond = self.parse_expression()
        self.eat('RPAREN')
        then_b = self.parse_block()
        else_b = None
        if self.current_token() and self.current_token().type == 'ELSE':
            self.eat('ELSE')
            else_b = self.parse_block()
        return IfNode(cond, then_b, else_b)

    def parse_while(self):
        self.eat('WHILE')
        self.eat('LPAREN')
        cond = self.parse_expression()
        self.eat('RPAREN')
        block = self.parse_block()
        return WhileNode(cond, block)

    def parse_set(self):
        self.eat('SET')
        name = self.eat('ID').value
        self.eat('ASSIGN')
        val = self.parse_expression()
        self.eat('SCOLON')
        return AssignNode(name, val)

    def parse_print(self):
        self.eat('PRINT')
        val = self.parse_expression()
        self.eat('SCOLON')
        return PrintNode(val)

    def parse_return(self):
        self.eat('RETURN')
        val = self.parse_expression()
        self.eat('SCOLON')
        return ReturnNode(val)

    def parse_expression(self):
        return self.parse_relational()

    def parse_relational(self):
        node = self.parse_arithmetic()
        if self.current_token() and self.current_token().type == 'OP_REL':
            op = self.eat('OP_REL').value
            node = BinOpNode(node, op, self.parse_arithmetic())
        return node

    def parse_arithmetic(self):
        node = self.parse_term()
        while self.current_token() and self.current_token().type == 'OP_ARIT' and self.current_token().value in ['+', '-']:
            op = self.eat('OP_ARIT').value
            node = BinOpNode(node, op, self.parse_term())
        return node

    def parse_term(self):
        node = self.parse_factor()
        while self.current_token() and self.current_token().type == 'OP_ARIT' and self.current_token().value in ['*', '/']:
            op = self.eat('OP_ARIT').value
            node = BinOpNode(node, op, self.parse_factor())
        return node

    def parse_factor(self):
        t = self.current_token()

        if t.type in ['INT', 'REAL', 'BOOL', 'STRING']:
            self.eat(t.type)
            return LiteralNode(t.value, t.type)

        if t.type == 'ID':
            name = self.eat('ID').value
            if self.current_token() and self.current_token().type == 'LPAREN':
                self.eat('LPAREN')
                args = []
                if self.current_token().type != 'RPAREN':
                    args.append(self.parse_expression())
                    while self.current_token() and self.current_token().type == 'COMMA':
                        self.eat('COMMA')
                        args.append(self.parse_expression())
                self.eat('RPAREN')
                return CallNode(name, args)

            return LiteralNode(name, 'ID')

        if t.type == 'LPAREN':
            self.eat('LPAREN')
            node = self.parse_expression()
            self.eat('RPAREN')
            return node

        raise Exception(f"Erro de expressão na linha {t.line}: {t.value}")