import sys
from lexer import Lexer, TAG  # Ajuste conforme o nome do seu arquivo e classe

def main():
    # Nome do arquivo de entrada
    filename = "tests/exemplo1.mini"

    try:
        # Lê todo o conteúdo do arquivo
        with open(filename, 'r') as file:
            # Inicializa o Scanner com o código fonte
            lexer = Lexer(file)
        
        print(f"--- Iniciando Análise Léxica de {filename} ---")
        print(f"{'TAG':<15} | {'VALOR (LEXEMA)':<20}")
        print("-" * 40)

        # Loop para pegar tokens até acabar
        while True:
            # Chama o método que retorna o próximo token.
            # Se o seu método tiver outro nome (ex: next_token), mude aqui.
            token = lexer.scan() 

            if token.tag == 0:
                break
            
            # Imprime no terminal
            print(f"{lexer.get_line()} -> ", end="")
            try:
                print(f"<{TAG(token.tag).name}, {token.lexeme}>")
            except:
                print(f"<{chr(token.tag)}, {token.lexeme}>")


    except FileNotFoundError:
        print(f"Erro: Arquivo '{filename}' não encontrado.")
    except Exception as e:
        print(f"Erro durante a execução: {e}")

if __name__ == "__main__":
    main()