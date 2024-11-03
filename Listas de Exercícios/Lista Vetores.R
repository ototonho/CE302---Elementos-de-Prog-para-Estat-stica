## Lista Vetores

### Atribuição de Variáveis
idade <- c(25)
nome <- "Maria"
notas <- c(8, 9, 7, 6)
frutas <- c("maçã", "banana", "laranja")
idades <- c(30, 40, 50)
divisão <- (idades/2) %% 2 == 1
divisão
um_a_cem <- c(1:100)
um_a_cem
dois_a_duzentos <- seq(2,200, by = 2)
dois_a_duzentos
n_pares_repetidos <- rep(seq(2,10, by = 2), times = 5)
n_pares_repetidos
alturas <- runif(min = 1.5, max = 2, 100)
alturas
pesos <- rnorm(100, mean = 70, sd = 10)
pesos
notasG <- rnorm(100, mean = 7, sd = 1)
notasG
N_uniformes <- sample(1:100, 100, replace = TRUE)
N_uniformes
loteria <- sample(1:60, 6)
loteria
loteria5x <- replicate(5, sample(1:60, 6))
loteria5x

### Operações Aritméticas
idades2x <- idade * 2
idades2x
médiaNotas <- mean(notas)
médiaNotas
Quadrado_Idades <- idades^2
Quadrado_Idades
15/4
15%%4

###Operações Lógicas e Condicionais
I_maior_q18 <- idade > 18
I_maior_q18
P_eh8 <- notas[1] == 8
P_eh8
Menor_q35 <- any(idades < 35)
Menor_q35
menores_q1e85 <- alturas[alturas < 1.85]
menores_q1e85
maiores_q80 <- pesos[pesos >  80]
maiores_q80
qts_maiores7 <- sum(notas > 7)
qts_maiores7

### Funções Matemáticas
sqrt(16)
round(3.78)
ceiling(alturas)
floor(alturas)
abs(10 - 7)

### Vetores Nomeados
salarios <- c(2000, 3000, 1500, 4000)
names(salarios) <- c("João", "Maria", "Carlos", "Ana")
names(salarios)
salarios["Maria"]
salarios["Carlos"] <- 1800
salarios
salarios["Paulo"] <- 2500 #adicionou Paulo
salarios
salarios <- salarios[names(salarios) != "Ana"] #removeu Ana
salarios
sort(salarios)
notasN <- c(8, 7, 9)
names(notasN) <- c("Pedro", "Lucas", "Carla")
notasN
notasN["Pedro", "Carla"]
notasS <- notasN[c("Pedro", "Carla")]
notasS
mean(notasN)

#Manipulação de Strings
StringE <- c("Análise de Dados")
substr(StringE, 1, 5)
StringE2 <- c("R é uma linguagem de programação")
toupper(StringE2)
tolower(StringE2)
StringE2N <- sub("linguagem", "software", StringE2)
print(StringE2N)

##Expressões Regulares
Text <- ("Texto123")
dígitos <- gregexpr("[0-9]", Text)
dígitos_encontrados <- regmatches(Text, dígitos)
print(dígitos_encontrados)
