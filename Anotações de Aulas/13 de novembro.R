### Aula 13/11
## Estruturas de Repetição - Etapa 5

while (condicao) {
  # Código a ser repetido enquanto a condição for verdadeira
}

i <- 1 # sempre definimos o critério de parada fora do loop

while (i < 6) { #difiniu o valor de i
  print(i)
  i <- i + 1 # Sempre alteramos o critério 
  # de parada, senão caímos em um loop infinito
}

i <- 1
while (i < 6) {
  print(i)
  i <- i + 1
  if (i == 4) { #até aqui, funciona igual python
    break
  }
}

## se quiser pular um item, isto é, uma iteração:
i <- 0
while (i < 6) {
  i <- i + 1
  if (i == 3) {
    next
  }
  print(i)
}
# para cancelar um loop infinito --> Esc

