%%writefile programa_teste.ml
var x : int = 5;
var resultado : int = 1;

def calcular(n : int ) : int{
  if ( n>0) {
    return n * calcular(n-1);
  }
  return 1;
}

print "calculando fatorial de 5:";
set resultado = calcular(x);
print resultado;