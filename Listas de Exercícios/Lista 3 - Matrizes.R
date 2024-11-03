# Lista 3 - Matrizes
## Exercícios sobre Matrizes
#1) 
vetorA <- c(2, 96, 49)
vetorB <- c(35, 2, 100)
vetorC <- c(43, 42, 92)

matrizA <- rbind(vetorA,
                 vetorB,
                 vetorC)
matrizA
class(matrizA)

#2)
soma_Colunas <- colSums(matrizA)
soma_Colunas

#3)
médiaLinhas <- rowMeans(matrizA)
médiaLinhas
#4)
t(matrizA)

#5)
k <- 3
multiplicacao <- k * matrizA
multiplicacao

#6)
det(matrizA)

#7)
vetor11 <- c(1,0,0,0)
vetor12 <- c(0,1,0,0)
vetor13 <- c(0,0,1,0)
vetor14 <- c(0,0,0,1)

matriz_Identidade <- rbind(vetor11,
                           vetor12,
                           vetor13,
                           vetor14)
matriz_Identidade

#8)
