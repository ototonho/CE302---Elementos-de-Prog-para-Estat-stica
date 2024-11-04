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
solve(matrizA)

#9)
set.seed(0)
matrizAle <- matrix(sample(-10:10, 9, replace = TRUE), nrow = 3, ncol = 3)
matrizAle
max(matrizAle)

#10)
matrizAle[matrizAle < 0] <- 0
matrizAle

## Operações com Matrizes
#1)
matrizB <- matrix(sample(0:10,9, replace = TRUE), nrow = 3, ncol = 3)
matrizC <- matrix(sample(0:10,9,replace = TRUE), nrow = 3, ncol = 3)
matrizB
matrizC
matrizB + matrizC

#2)
matrizB * matrizC

#3)
t(matrizB) #Matriz transposta
t(matrizB) * matrizB

#4)
matriZD <- matrix(1:9, nrow = 3, ncol = 3)
matriZD
diag(matriZD)
diag(matriZD) <- c(5,6,7)
matriZD

#5)
upper.tri(matriZD)
sum(diag(matriZD))
sum(upper.tri(matriZD))

#6)
mean(matrizA)

#7)
matriZE <- matrix(1:9, nrow = 3, ncol = 3)
matriZE
matrizES <- (matriZE + t(matriZE)) / 2
matrizES

#8)
eigen(matrizA)

## Exercícios sobre Arrays
#1)
array1 <- array(1:27, dim = c(3,3,3))
array1

#2)
array1[1,2,1]

#3)
mean(array1[,,1])
mean(array1[,,2])
mean(array1[,,3])

#4)
array2 <- array(-10:16, dim = c(3,3,3))
array2
array2[array2 < 0] <- 0
array2

#5)
max(array2)

## Manipulação de Listas de Data Frames
#1)
nomes <- c("Odoacro", "Justino", "Focas", "Tibério")
idades <- c(47,53,27,56)
salarios <- c(3500, 1200, 2700, 5000)
L <- list(nomes = nomes,idades = idades ,salarios = salarios)
L

#2)
df <- as.data.frame(L)
df

#3)
df$genero <- c("masculino", "masculino", "masculino", "masculino")
df

#4)
df$idades <- NULL
df

#5)
L[[2]]

#6)
cor_preferida <- c("roxo", "verde", "azul", "amarelo")
df$cor_preferida <- cor_preferida
df

#7)
