#Harvard 3 - Programming Basics
library(tidyverse)
library(dslabs)
# if-else
a <- 2
if(a!=0){
  print(1/a)
} else {
  print("Não há recíproca para 0")
}

a <- 0
if(a!=0){
  print(1/a)
} else {
  print("Não há recíproca para 0")
}

#Forma Geral:
#if(condição booleana){expressões} else {expressões alternativas}

data("murders")
murder_rate <- murders$total/murders$population*10^5
murders <- mutate(murders, rate = total/population*100000)
ind <- which.min(murder_rate)
if(murder_rate[ind] < 0.5){
  print(murders$state[ind])
} else {
  print("No state has murder rate that low")
}

if(murder_rate[ind] < 0.25){
  print(murders$state[ind])
} else {
  print("No state has murder rate that low")
}

a <- 0
ifelse(a > 0, 1/a, NA)

a <- c(0,1,2,-4,5)
result <- ifelse(a > 0, 1/a, NA) #ifelse funciona com vetores
result
as.table(result)

data("na_example")
sum(is.na(na_example))

no_nas <- ifelse(is.na(na_example), 0, na_example)
no_nas


## any and all
z <- c(TRUE, TRUE, FALSE)
any(z)
w <- c(FALSE, FALSE, FALSE)
any(w)
all(z)
v <- c(TRUE, TRUE, TRUE)
all(v)

## Funções básicas
avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}
avg(murders$total)
mean(murders$total)

identical(mean(x), avg(x))

#Forma Geral:
# my_function <- function(x){operations that operate on x which is defined by user of function. value line is returned}

#Funções podem ter mais de uma variável:
#my_function <- function (x, y, z) {operations that operate on x,y,z which is defined by user of function. value final line is returned}

avg <- function(x, arithmetic = TRUE) {
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}

## 3.2 - for Loops
# criando uma função que compute a soma dos números inteiros entre 1 e n
compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}
compute_s_n(3)
compute_s_n(5)
compute_s_n(100)

#general formula:
# for(i in range of values){operations that use i, which is changing across its range}
for (i in 1:5) {
  print(i)
}

m <- 25
#criando um vetor vazio
s_n <- vector(length = m)
for (n in 1:m) {
  s_n[n] <- compute_s_n(n)
}
s_n

n <- 1:m
plot(n, s_n)
lines(n, n*(n+1)/2)

#outras funções
#no lugar dos loops com for, usa-se apply, sapply, tapply, mapply

## Quiz Seção 3
library(dslabs)
library(tidyverse)
data("heights")
#1)
sum(ifelse(heights$sex == "Female", 1, 2))
#2)
mean(ifelse(heights$height > 72.00000, heights$height, 0))
#3)
inches_to_ft <- function(x){
  x/12
}
inches_to_ft(144)

sum(ifelse(inches_to_ft(heights$height) < 5, 1, 0))

#4)
