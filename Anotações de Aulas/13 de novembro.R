### Aula 13/11
## Estruturas de Repetição - Parte 5
require(dplyr)
require(magrittr)

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

contador <- 0
i = 0

while (i < 10) {
  i = sample(1:100, size = 1)
  contador = contador + 1
  
  print(contador)
  print(i)
  if(contador == 4) {
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

#Exercício: Suponha o lançamento de um dado não viesado, com seis faces. Quantas vezes devo lançar o dado para obter a face 5?
set.seed(1234)
dado <- seq(1:6)
n_lançamento = 0
sorteio = 0

while(sorteio !=5) {
  sorteio = sample(dado, 1)
  n_lançamento = n_lançamento + 1
  df[[n_lançamento]] = data.frame(n_sorteado = sorteio, 
                  n_lançamento = n_lançamento)
  
  cat(paste0("\n\nLançamento: ", n_lançamento, "\nValor Sorteado: ", sorteio))
}
n_lançamento
df %<>% dplyr::bind_rows()
df

n_lancamento = 0
while (sorteio != 7) {
  sorteio =  sample(dado, 1)
  n_lancamento = n_lancamento + 1
  
  cat(paste0("\n\nLançamento: ", n_lancamento, "\nValor Sorteado: ", sorteio))
  
  if(n_lancamento == 100){
    break
  }
}
  
#para guardar o valor sorteado em um vetor
valor_sorteado = numeric()
n_lancamento = 0
while (sorteio != 7) {
  n_lancamento = n_lancamento + 1
  valor_sorteado[n_lancamento] =  sample(dado, 1)
  
  
  cat(paste0("\n\nLançamento: ", n_lancamento, "\nValor Sorteado: ", sorteio))
  
  if(n_lancamento == 100){
    break
  }
}

valor_sorteado

#Estrutura for
# Código a ser executado para cada valor da sequência
for(a in 1:10) {
  x1 <- a^2
  print(x1)
}

### Função
dado = c(1:6)
soma_dois_dados = function(dado1, dado2){
  soma = dado1 + dado2
  return(soma)
  
}
quadrado_soma = function(soma){
  soma2 = soma^2
  return(soma2)
}

resultado = list()
k = 0 
for(i in dado){
  for(j in dado){
    k = k + 1
    soma = soma_dois_dados(dado[i], dado[j])
    somaqd = quadrado_soma(soma)
    
    resultado[[k]] = data.frame(dado1 = dado[i], 
                                dado2 = dado[j], 
                                soma = soma, 
                                soma2 = somaqd)
  }
}

resultado %<>%
  bind_rows() #transformou a lista em tabela
View(resultado)

##Família apply
matriz1 <- matrix(1:6, nrow = 2)
soma_linhas <- apply(matriz1, 1, sum) #Aplica uma função qualquer a uma matriz, array ou data.frame ao longo de margens específicas
soma_colunas <- apply(matriz1, 2, sum)
apply(matriz1, 1, median)
apply(matriz1, 2, mean)

minha_lista <- list(a = c(1, 2, 3), b = c(4, 5, 6))
resultados <- lapply(minha_lista, mean) #Aplica uma função a cada elemento de uma lista e retorna uma lista com os resultados
resultados
lapply(minha_lista, quantile)

minha_lista <- list(a = c(1, 2, 3), b = c(4, 5, 6), c=c(7,6,8)) #A função sapply() é semelhante ao lapply(), mas tenta simplificar o resultado em um vetor ou matriz sempre que possível
resultados <- sapply(minha_lista, mean)
resultados

resultado <- mapply(soma_dois_dados, 
                    dado, 
                    dado) #Aplica uma função a vários argumentos

print(resultado)

#E se eu quisesse somar todas as combinações como no 'for' aninhado?--> expand.grid
dois_dados = expand.grid(dado, dado)
dois_dados

tres_dados = expand.grid(dado, dado, dado)
tres_dados

resultado <- mapply(soma_dois_dados, 
                    dois_dados$Var1, 
                    dois_dados$Var2)

print(resultado)

##Funções e Expressões - Parte 6
#Uma função é um conjunto de instruções que realizam uma tarefa específica quando chamadas.

##Estrutura
#nome_da_funcao <- function(argumentos) {
# Corpo da função
# Instruções para realizar a tarefa
#return(resultado) # Resultado da função
#}

data("iris")

media_sepal_len = round(sum(iris$Sepal.Length)/length(iris$Sepal.Length),2)
media_sepal_len

M_media <- function(vetor_de_dados) {
  media = sum(vetor_de_dados)/length(vetor_de_dados)
  media = round(media, 2)
  return(media)
}

M_media(iris$Sepal.Length)
M_media(iris$Sepal.Width)
M_media(iris$Petal.Length)
M_media(iris$Petal.Width)

M_media_arredond <- function(vetor_de_dados, arredondamento = 5) {
  media = sum(vetor_de_dados)/length(vetor_de_dados)
  media = round(media, arredondamento)
  return(media)
}
M_media(iris$Sepal.Length)
M_media_arredond(iris$Sepal.Length, 6) #o usuário defin ao arredondamento

##calculando desvio padrão amostral
meu_desvio_padrao_amostral <- function(vetor) {
  media <- M_media_arredond(vetor) 
  diferenca <- vetor - media  # Calcula as diferenças em relação à média
  quadrados <- diferenca^2  # Calcula os quadrados das diferenças
  variancia <- sum(quadrados) / (length(vetor) - 1)  # Calcula a variância
  desvio_padrao <- sqrt(variancia)  # Calcula o desvio padrão
  return(desvio_padrao)
}

meu_desvio_padrao_amostral(iris$Sepal.Length)

##Coeficiente de variação
meu_coeficiente_variacao <- function(vetor, arredondamento = 2) {
  media <- M_media_arredond(vetor, arredondamento = arredondamento)  # Calcula a média
  desvio_padrao <- meu_desvio_padrao_amostral(vetor)  # Calcula o desvio padrão
  coeficiente_variacao <- (desvio_padrao / media) * 100  # Calcula o CV em porcentagem
  coeficiente_variacao = round(coeficiente_variacao, arredondamento)
  return(coeficiente_variacao)
}

meu_coeficiente_variacao(iris$Sepal.Length, arredondamento = 2)

##CV, média e Desvio Padrão
meu_CV_Média_DP <- function(vetor, arredondamento = 2) {
  media <- M_media_arredond(vetor, arredondamento = arredondamento)  # Calcula a média
  desvio_padrao <- meu_desvio_padrao_amostral(vetor)  # Calcula o desvio padrão
  coeficiente_variacao <- (desvio_padrao / media) * 100  # Calcula o CV em porcentagem
  coeficiente_variacao = round(coeficiente_variacao, arredondamento)
  return(data.frame(CV = coeficiente_variacao, 
                    média = media, 
                    dp = desvio_padrao))
  
}

meu_CV_Média_DP(iris$Sepal.Length, arredondamento = 2)

##saída como lista
meu_CV_Média_DP_Lista <- function(vetor, arredondamento = 2) {
  media <- M_media_arredond(vetor, arredondamento = arredondamento)  # Calcula a média
  desvio_padrao <- meu_desvio_padrao_amostral(vetor)  # Calcula o desvio padrão
  coeficiente_variacao <- (desvio_padrao / media) * 100  # Calcula o CV em porcentagem
  coeficiente_variacao = round(coeficiente_variacao, arredondamento)
  return(list(CV = coeficiente_variacao, 
              média = media, 
              dp = desvio_padrao))
  
}

meu_CV_Média_DP_Lista(iris$Sepal.Length, arredondamento = 2)

##Mensagens
x <- -5
if (x < 0) {
  message("O valor de x é negativo.") #apenas mensagem
}

x <- -5
if (x < 0) {
  warning("O valor de x é negativo.") #mensagem de aviso
}

x <- -5
if (x < 0) {
  stop("O valor de x é negativo.") #força a parada da função casa haja um erro
}

## Controle de fluxo
##Estruturas condicionais, basicamente if-else

idade <- 25

if (idade >= 18) {
  cat("Você é maior de idade.\n")
} else {
  cat("Você é menor de idade.\n")
}

##aninhando as estruturas condicionais
pontuacao = 90

if (pontuacao >= 90) {
  nota = "A"
} else {
  if (pontuacao >= 80) {
    nota = "B"
  } else {
    if (pontuacao >= 70) {
      nota = "C"
    } else {
      nota = "D"
    }
  }
}

nota

##atribuindo função às EC
classifica_nota <- function(pontuacao) {
  if (pontuacao >= 90) {
    nota <- "A"
  } else {
    if (pontuacao >= 80) {
      nota <- "B"
    } else {
      if (pontuacao >= 70) {
        nota <- "C"
      } else {
        nota <- "D"
      }
    }
  }
  cat(paste("A nota do aluno é:", nota))
  return(nota)
}

pontuacao_aluno <- 85
nota <- classifica_nota(pontuacao_aluno)

##Determinando o quadrante de um ponto no plano cartesiano

quadrante <- function(x, y) {
  if (x > 0) {
    if (y > 0) {
      quadrante = "Quadrante 1"
      
      cat(paste0("O ponto (", x, ", ", y, ") pertence ao ",  quadrante))
      return(quadrante)
    } else {
      quadrante = "Quadrante 4"
      
      cat(paste0("O ponto (", x, ", ", y, ") pertence ao ",  quadrante))
    }
  } else {
    if (y > 0) {
      quadrante = "Quadrante 2"
      
      cat(paste0("O ponto (", x, ", ", y, ") pertence ao ",  quadrante))
    } else {
      quadrante = "Quadrante 3"
      
      cat(paste0("O ponto (", x, ", ", y, ") pertence ao ",  quadrante))
    }
  }
}


quadrante(1, 1)
quadrante(-1, -2)
quadrante(-1, 4)
quadrante(1, -1)

#nao entendi a diferença destes dois aqui
iris %<>% 
  mutate(cat_petal.len = ifelse(Petal.Length > mean(Petal.Length), "Longa", "Curta"))

iris %<>% 
  mutate(cat_petal.len2 = if_else(Petal.Length > mean(Petal.Length), "Longa", "Curta"))

#Switch
##O comando switch é outra construção de controle de fluxo que permite escolher entre várias alternativas com base em um valor especificado. É útil quando você precisa executar um bloco de código diferente para diferentes valores de uma variável
#exemplo - switch(expressao, caso1, caso2, ..., casoN)
dia_da_semana <- "segunda"

mensagem <- switch(dia_da_semana,
                   "segunda" = "Hoje é segunda-feira.",
                   "terca" = "Hoje é terça-feira.",
                   "quarta" = "Hoje é quarta-feira.",
                   "quinta" = "Hoje é quinta-feira.",
                   "sexta" = "Hoje é sexta-feira.",
                   "sabado" = "Hoje é sábado.",
                   "domingo" = "Hoje é domingo.",
                   "Outro" = "Dia não reconhecido."
)

cat(mensagem)

#Similar ao ifelse, temos o case_when quando nosso interesse está em codificar variáveis de um banco de dados.
iris$cat_sepal = 
  case_when((iris$Sepal.Length < mean(iris$Sepal.Length) - sd(iris$Sepal.Length)) ~ "X < media - 1 sd", 
            (iris$Sepal.Length < mean(iris$Sepal.Length) + sd(iris$Sepal.Length)) ~ "X < media + 1 sd", 
            .default = "X > media + 1 sd")
