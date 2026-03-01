import json
from parser import Parser

parser = Parser("tests/exemplo1.mini")
ast = parser.start()

# Gera o JSON indentado conforme exigido
print(json.dumps(ast.to_dict(), indent=4, ensure_ascii=False))