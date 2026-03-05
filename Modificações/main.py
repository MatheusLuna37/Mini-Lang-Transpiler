with open('programa_teste.ml', 'r') as f:
    conteudo = f.read()

lista_tokens = scanner(conteudo)

print(f"{'LINHA':<7} | {'TIPO':<10} | {'VALOR'}")
print("-" * 30)
for t in lista_tokens:
    print(f"{t.line:<7} | {t.type:<10} | {t.value}")

parser = Parser(lista_tokens)
ast = parser.parse_program()
print("Sucesso! AST gerada.")