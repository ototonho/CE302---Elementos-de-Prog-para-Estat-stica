## Prova 2

## Questão 1
require(tidyverse)
calculo_de_medidas <- function(x){
  # Removendo NA's
  x <- na.omit(x)
  
  # Cálculo da média
  media <- mean(x)
  
  #Cálculo da mediana
  mediana <- median(x)
  
  # Cálculo da variância amostral
  variancia <- sum((x - media)^2) / (length(x) - 1)
  
  # Cálculo do desvio médio absoluto (DMA)
  dma <- sum(abs(x - media)) / (length(x) - 1)

  # Cálculo do coeficiente de assimetria de Pearson (AS2)
  as2 <- 3 * (media - mediana) / sqrt(variancia)
  
  return((list(S2 = variancia, DMA = dma, AS2 = as2)))
}

## a) e b) S² e DMA de Temp
calculo_de_medidas(airquality$Temp)

## c) Nivel 7 de Month - Wind
calculo_de_medidas(airquality$Month)
Niveis <- airquality %>%
  filter(Month == 7)
View(Niveis)

calculo_de_medidas(Niveis$Wind)

## d) Para todas as variáveis do banco airquality a variância amostral é maior do que o desvio médio absoluto. (TRUE = 1/FALSE = 0)?
lista_variaveis <- list(calculo_de_medidas(airquality$Ozone),
calculo_de_medidas(airquality$Solar.R),
calculo_de_medidas(airquality$Wind),
calculo_de_medidas(airquality$Temp),
calculo_de_medidas(airquality$Month),
calculo_de_medidas(airquality$Day))
View(lista_variaveis)


## Questão 2

# Verificar se um número é primo
primo <- function(x) {
  if (x <= 1) return(FALSE)
  for (i in 2:floor(sqrt(x))) {
    if (x %% i == 0) return(FALSE)
  }
  return(TRUE)
}

# Verificar se é um quadrado perfeito
quadrado_perfeito <- function(x) {
  if (is.na(x) || !is.numeric(x) || x < 0) {  # Verifica se x é NA, não numérico ou negativo
    return(FALSE)
  }
  
  raiz <- sqrt(x)
  return(raiz == floor(raiz))
}


# Função principal para aplicar as regras
transforma_matriz <- function(mat) {
  apply(mat, c(1, 2), function(x) {
    if (primo(x)) {
      return(x * 6)
    } else if (quadrado_perfeito(x)) {
      resultado <- x - 15
      if (resultado < 0) {
        return(resultado^5)
      }
      return(resultado)
    } else if (x < 0) {
      return(abs(x)^(1/2))
    } else {
      return(x) # Não faz nada para outros valores
    }
  })
}

Matriz_A <- matrix( c(-5, -8, 4, -4, 9, -2, 6, 5, -3, -1, 3, -9, 10, 7, -6, 1) , nrow = 4, byrow = FALSE)

Matriz_B <- matrix( c(9, -4, -18, 11, -14, 7, 2, 3, 17, 14, 0, 16, -6, -19, -8, -1, -20, 20, 5, 6) , nrow = 4, byrow = FALSE)

Matriz_C <- matrix( c(-29, -23, -24, 21, -12, 25, 17, 16, 8, 29, 0, -5, -17, -4, -7, 14) , nrow = 4, byrow = FALSE)

M_A_transformada <- transforma_matriz(Matriz_A)
print(M_A_transformada)

M_B_transformada <- transforma_matriz(Matriz_B)
print(M_B_transformada)

M_C_transformada <- transforma_matriz(Matriz_C)
print(M_C_transformada)


#a)
soma_MA <- M_A_transformada[1,1] + M_A_transformada[2,2] + M_A_transformada[3,3] + M_A_transformada [4,4]
print(soma_MA)

#b) 
print(M_C_transformada)

#c)
sum(M_C_transformada[,2])

#d)
primo <- function(n) {
  if (n <= 1) return(FALSE)
  if (n == 2) return(TRUE)
  if (n %% 2 == 0) return(FALSE)
  
  
  for (i in matrix(3, floor(sqrt(n)), by = 2)) {
    if (n %% i == 0) return(FALSE)
  }
  
  return(TRUE)
}

contar_primos <- function(i){
  conta_primos <- 0
  for (n in i) {
    if (primo(n)) {
      conta_primos <- conta_primos + 1
    }
  }
  return(conta_primos)
}

i <- M_A_transformada
iA <- contar_primos(i)
print(iA)

i <- M_B_transformada
iB <- contar_primos(i)
print(iB)

i <- M_C_transformada
iC <- contar_primos(i)
print(iC)

iA + iB + iC

## Questão 3

tabela1 <- data.frame(
  estacao = c("Primavera", "Verão", "Outono", "Inverno"),
  temperatura = c(0, 20, 10, 0),
  fatorReprodução = c(0.00, 0.03, -0.03, -0.02)
)
View(tabela1)

tabela2 <- data.frame(
  faixa_PH = c("6.0 - 6.5", "6.6 - 7.0", "7.1 - 7.5", "7.6 - 8.0"),
  PercentualPescado = c(0.12, 0.20, 0.05, 0.16)
)
View(tabela2)

# Simula o lago
simular_lago <- function(dias, peixes_iniciais, estacao, ph){
  if (estacao != "Primavera" & estacao != "Verão" & estacao != "Outono" & estacao != "Inverno") return(FALSE)
  if (estacao == "Primavera" & estacao == "Verão" & estacao == "Outono" & estacao == "Inverno") return(TRUE)
  
  if (estacao == "Primavera"){
    fator_repro <- 0.00 
  } else if (estacao == "Verão") {
    fator_repro <- 0.03
  } else if (estacao == "Outono") {
    fator_repro <- -0.03
  } else if (estacao == "Inverno") {
    fator_repro <- -0.02
  }
  
  if (ph >= 6.0 & ph <= 6.5){
    percentual_pescado <- 0.12
  } else if (ph >= 6.6 & ph <= 7.0){
    percentual_pescado <- 0.20
  } else if (ph >= 7.1 & ph <= 7.5){
    percentual_pescado <- 0.05
  } else if (ph >= 7.6 & ph <= 8.0){
    percentual_pescado <- 0.16
  }
  
  taxa_ajustada <- 0.06 + fator_repro
  
  peixes_pescados <- peixes_iniciais * percentual_pescado
  
  if (dia1 <= dias) {
  peixes_prox_dia <- peixes_iniciais * (1 + taxa_ajustada) - peixes_pescados

  n_peixes <- peixes_iniciais + n_peixes_cada_dia
  
  dia1 <- dia1 + 1
  }

  
  else if (n_peixes <= 5 * 10^5) {
    
    
  } else {stop("O lago atingiu a capacidade limite.")}
  
  return(dias)
  
}

dias <- as.numeric(readline(prompt = "Digite a quantidade de dias: "))
peixes_iniciais <- as.numeric(readline(prompt = "Digite o número de peixes atuais: "))
estacao <- as.numeric(readline(prompt = "Digite a estação do ano: "))
ph <- as.numeric(readline(prompt = "Digite o ph da água: "))

resultados <- simular_lago(dias, peixes_iniciais, estacao, ph)

resultados_DF <- as.data.frame(
  resultados$dias = c(dias),
  resultados$peixes = c(n_peixes)
)

print(resultados)
