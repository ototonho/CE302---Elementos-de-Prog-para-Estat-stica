 installed.packages()
 install.packages("dslabs")
 library(tidyverse)
 library(dslabs)
 
 # 1.1 - Basics
data(murders)

murders %>%
  ggplot(aes(population, total, label = abb, color = region)) +
  geom_label()

 # 1.2 - Basics - Objetos
a <- 1
b <- 1
c <- -1

a^2 + b - c
# ls() - mostra as varíaveis/objetos salvas na área de trabalho
ls()

(-b + sqrt(b^2 -4*a*c)) / (2*a)
round((-b + sqrt(b^2 -4*a*c)) / (2*a))
floor((-b + sqrt(b^2 -4*a*c)) / (2*a))
ceiling((-b + sqrt(b^2 -4*a*c)) / (2*a))

## - Funções
ls() # função sem parênteses mostra o código no console

# funções aninhadas - usar uma função para encontrar o argumetno de outra
log(sqrt(4))
log(exp(1))

# pode-se usar a aba help para aprender sobre as funções, são manuais
log(8, base = 2)
log(x = 8, base = 2)
log(8, 2)

# criando funções para a fórmula quadrática
Bhask1 <- (-b + sqrt(b^2 -4*a*c)) / (2*a)
Bhask2 <- (-b - sqrt(b^2 -4*a*c)) / (2*a)

a <- 3
b <- 2
c <- -1


## 1.3 - Data Types
class(a)
class(murders)
str(murders)
head(murders)
tail(murders)

murders$population #linhas de population
names(murders) #nomes das variáveis
pop <- murders$population
length(pop)
class(pop)

a
"a"

z <- 3==2
z
class(z)

levels(murders$region)
class(murders$region)

## 1.4 - Vetores
#Componentes de grandes data.frames

#criando vetores com "c"
nomes <- c("Judas", "Mateus", "Tiago")
alturas_nomes <- c(Judas=168,Mateus=175,Tiago=183)
nomes
alturas_nomes
class(alturas_nomes)

seq(1,15) #sequência, o primeiro inicia, o segundo encerra
seq(1,15,2) #mesma coisa, mas de 2 a 2
    
1:10 #mais fácil

##subconjuntos com []

nomes[2]

# Coerção vetorial
x <- c(1,"canada",3)
x
class(x)
as.numeric(x) #introduz as NAs por coerção
x

class(x)

# Sorting / Ordenação
sort(murders$total)

y <- c(2,35,100,23,5,4,86,6)
y
sort(y) #coloca em ordem
order(y) #onde estão os elementos da ordem original

murders$state[1:10]
murders$abb[1:10]

index <- order(murders$total)
murders$abb[index] #ordenou as iniciais de acordo com os totais de assassinatos
max(murders$total) #maior numero de assassinatos
i_max <- which.max(murders$total) 
i_max
murders$state[i_max] #lider nessa ordenação
i_min <- which.min(murders$total)
i_min
murders$state[i_min]

rank(y) #devolve quais as relativas do item original de menor para maior

# 1.6 aritmética vetorial
murders$state[which.max(murders$population)] #por estado, qual o de maior população
max(murders$population) #simplesmente maior população, sem saber de quem é

alturas_F <- c(69,62,66,70,70,73,67,73,67,70)
alturas_C <- alturas_F * 2.54
alturas_F - 69 #diferença de todas as alturas comparadas a 69
alturas_C - 170

murder_rate <- murders$total/murders$population*100000
murder_rate

murders$state[order(murder_rate, decreasing = TRUE)] #ordenados por murder rate de forma decadente

# 1.7 - Indexar vetores
index <- murder_rate < 0.71
index <- murder_rate <= 0.71
index

murders$state[index]
sum(index) #soma quantos 1's há na operação binária true or false

#encontrar murder rate abaixo de 1 no oeste
west <- murders$region == "West"
safe <- murder_rate <= 1

index <- safe & west
index
murders$state[index]

## indexando funções
x <- c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)
which(x) #quais são True em x

index <- which(murders$state == "Massachusetts")
index #retorna 22 que é o n da observação com esse nome
murder_rate[index] #ver a taxa da obs. selecionada

index <- match(c("New York", "Florida", "Texas"), murders$state)
index
murders$state[index]
murder_rate[index]

x <- c("a", "b", "c", "d", "e")
y <- c("a", "d", "f")
y%in%x #quais itens de y estão em x

#não sabe de os seguintes nomes são estados, pode-se checar com %in%
c("Boston", "Dakota", "Washington") %in% murders$state
#retorna true or false para eles

#1.8 - Basica Plots
population_in_millions <- murders$population/10^6
total_gun_murders <- murders$total
plot(population_in_millions, total_gun_murders)

hist(murder_rate)

murders$state[which.max(murder_rate)]

boxplot(murder_rate~murders$region, data = murders)

#quizes
#1) 
a <- 2
b <- -1
c <- -4

Bhask1 <- (-b + sqrt(b^2 -4*a*c)) / (2*a)
Bhask2 <- (-b - sqrt(b^2 -4*a*c)) / (2*a)
Bhask1
Bhask2

#2) 
log(1024,4) #log de 1024 na base 4

#3)
data("movielens")
class(movielens)
class(movielens)
str(movielens)
nrow(movielens)

#4)
nlevels(movielens$genres)

#5)
help(mean)
?mean

# Part 2 o the Quiz
#1)
x <- c(2, 43, 27, 96, 18)
sort(x)
order(x)
rank(x)

#2)
min(x)
which.min(x)
max(x)
which.max(x)

#3)
name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)

time_to_hours <- time/60
time_to_hours
speed <- distance/time_to_hours
speed

#4)
x <- c(4,"seven",9)
x
class(x)
str(x)

#5)
#miles to km

#6)
data(olive)
head(olive)

plot(olive$palmitic,olive$palmitoleic)

#7)
hist(olive$eicosenoic)

#8)
boxplot(olive$palmitic~olive$region)
