# Harvard - 2: Data Wrangling

library(dplyr)
library(dslabs)
library(tidyverse)

data("murders")

# Mutate, select and filter
murders <- mutate(murders, rate = total/population*100000) #criou a coluna chamada rate
head(murders)

filter(murders, rate <= 0.71) #filtrando

new_table <- select(murders,state,region,rate) #criou uma tabela nova com 3 colunas apenas
head(new_table)
filter(new_table, rate <= 0.71) #filtrou em cima da tabela nova apenas com as variáveis de interesse

# all three together with the pipe
#we'll use this to avoid having to create a new table
murders %>% select(
  state, region, rate) %>%
  filter(rate <= 0.71) #tchanan

# 2.1.2 - Creating a Data Frame
grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"), 
                     exam_1 = c(95, 80, 90, 85), 
                     exam_2 = c(90, 85, 85, 90))
grades
class(grades$names)                     

# creating a data frame with stringAsFactors = FALSE
grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"), 
                     exam_1 = c(95, 80, 90, 85), 
                     exam_2 = c(90, 85, 85, 90),
                     stringsAsFactors = FALSE) #não permite que as strings sejam tratadas como fatores

grades
class(grades$names)

#2.2 - Summarizing with dplyr
## the summarize function
s <- murders %>%
  filter(region == "West") %>%
  summarize(minimum = min(rate),
            median = median(rate),
            maximum = max(rate))
s
s$median #virou um data table

us_murder_rate <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^5) #faz os somatórios ao invés de só ter uma média
us_murder_rate

#Summarizing with more than one value
x <- c(5,0,2,8,7,2,3)
quantile(x, c(0, 0.5, 1)) #min, mediana e max

murders %>%
  filter(region == "West") %>%
  summarize(range = quantile(rate, c(0, 0.5, 1)))

#criando uma função para dar em tabela e nao em vetor
# returning minimum, median, and maximum as a data frame
my_quantile <- function(x){
  r <-  quantile(x, c(0, 0.5, 1))
  data.frame(minimum = r[1], median = r[2], maximum = r[3]) 
}
murders %>% 
  filter(region == "West") %>%
  summarize(my_quantile(rate))

## Pull to access columns
class(us_murder_rate) #é data frame
# summarize sempre devolve data frame
#para se ter a devolução de valores numéricos ou vetores, usa-se $ ou pull() do dplyr

us_murder_rate %>% pull(rate)
#alternativamente, já poderia ter sido feito anteriormente
us_murder_rate <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^5) %>%
  pull(rate)
us_murder_rate
class(us_murder_rate)

# the dot placeholder
#o ponto pode substituir o pull
us_murder_rate <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^5) %>%
  .$rate
us_murder_rate
class(us_murder_rate)

#agrupar e depois sumarisar
murders %>% group_by(region)

murders %>% 
  group_by(region) %>%
  summarize(median = median(rate)) #o summarize aplicou a ordem para cada grupo separadamente

#sorting data tables
murders %>% arrange(population) %>% head() #arrange é usado para ordenar tabelas inteiras
#acima, foram ordenados por população
murders %>% arrange(rate) %>% head() #ao lado, por rate
murders %>% arrange(desc(rate)) %>% head() #desc() coloca em ordem decrescente

murders %>% arrange(region, rate) %>% head() #ordenou por região, e dentro de região por rate

murders %>% top_n(10, rate) #no lugar de head(), pode-se selecionar quantas linhas mostrar com top_n

murders %>% arrange(desc(rate)) %>% top_n(10) #mesma coisa que acima, mas com top 10

# 2.3 - Data.table
library(data.table) #pacote novo
library(dplyr)
library(tidyverse)
library(dslabs)


data("murders")
murders <- mutate(murders, rate = total/population*100000)
class(murders)
setDT(murders)
class(murders)
is.data.table(murders)
select(murders, state, region) #no dplyr usa-se assim


murders[, c("state", "region")] |> head() #no data.table
murders[, .(state, region)] |> head() #o ponto informa o R que o que há dentro do () são colunas, não objetos

murders <- mutate(murders, rate = total / population * 10^5) # adicionar ou alterar coluna no dplyr

# adicionar ou alterar coluna no data.table
murders[, rate := total / population * 100000] #update by reference
head(murders)
murders[, ":="(rate = total / population * 100000, rank = rank(population))] #as aspas criam múltiplas colunas
head(murders)

x <- data.table(a = 1)
y <- x

a

x[, a := 1]
y
a

x <- data.table(a = 1)
y <- copy(x)

x[, a := 2]
y

#subconjuntos
#criação de subconjuntos com dplyr
filter(murders, rate <= 0.71)
#select e filter no dplyr
murders %>%
  filter(rate <= 0.71) %>%
  select(state, rate)
#criação de subconjuntos com data.frame
murders[rate <= 0.71]
#combinando filter e select
murders[rate <= 0.71, .(state, rate)]

#Summarizzing com data.table
data("heights")

heights <- setDT(heights)

# summarizing no dplyr
s <- heights %>% 
  summarize(average = mean(height), standard_deviation = sd(height))
s
# summarizing no data.table
s <- heights[, .(average = mean(height), standard_deviation = sd(height))]
s

# subsetting e summarizing no dplyr
s <- heights %>% 
  filter(sex == "Female") %>%
  summarize(average = mean(height), standard_deviation = sd(height))
s

# subsetting e summarizing no data.table
s <- heights[sex == "Female", .(average = mean(height), standard_deviation = sd(height))]
s

# previously defined function
median_min_max <- function(x){
  qs <- quantile(x, c(0.5, 0, 1))
  data.frame(median = qs[1], minimum = qs[2], maximum = qs[3])
}

# multiple summaries in data.table
heights[, .(median_min_max(height))]

# grouping then summarizing in data.table
heights[, .(average = mean(height), standard_deviation = sd(height)), by = sex] #by no lugar de group.by

#Ordenações com data.table
murders <- setDT(murders)
murders[, rate := total / population * 100000]
murders
# order by population
murders[order(population)] |> head()
murders
# order by population in descending order
murders[order(population, decreasing = TRUE)] 
murders
# order by region and then murder rate
murders[order(region, rate)]
murders

#Tibbles
murders %>%
  group_by(region)

gapminder
as_tibble(gapminder)

class(as_tibble(murders)[,1]) #importante pq no tidyverse sempre usa-se data frame

murders$State #dá errado, mas nao diz o quê
as_tibble(murders)$State #como tibble, ele diz o que deu errado

# create a tibble
tibble(id = c(1, 2, 3), func = c(mean, median, sd))

##Quiz parte 2
#1)First, determine the average height in this dataset. Then create a logical vector ind with the indices for those individuals who are above average height. 
# How many individuals in the dataset are above average height?
library(dslabs)
data(heights)
options(digits = 3) 

heights <- setDT(heights)

heights[, acima_media := height > mean(height)]
heights
sum(heights$acima_media)

#2)
heights[, acima_media_e_Mulher := height > mean(height) & sex == "Female"]
sum(heights$acima_media_e_Mulher)

#3)
proportion_F <- sum(heights$sex == "Female")/1050
proportion_F

#4)
#a)
min(heights$height)

#b)
match(c(50), heights$height)

#c)
heights[match(c(50), heights$height), by = sex]

#5)
#a) 
max(heights$height)

#b) Which integer values are between the maximum and minimum heights? For example, if the minimum height is 10.2 and the maximum height is 20.8, your answer should be x <- 11:20 to capture the integers in between those values. (If either the maximum or minimum height are integers, include those values too.)
# Write code to create a vector x that includes the integers between the minimum and maximum heights in this dataset (as numbers).
x <- 50:82
x
heights[sum(x%in%height)]
heights$height %in% x

#c)
#How many of the integers in x are NOT heights in the dataset?
sum(!x%in%heights$height)

#6)
heights[, ht_cm := height * 2.54]
head(heights)
#a) 
heights$ht_cm[18] #18º indivíduo
#b)
mean(heights$ht_cm)

#7)
females <- heights[sex == "Female"]
females
class(females)
#a)
nrow(females)
#b)
mean(females$ht_cm)

#8)
data("murders")
which(murders$state == "Massachusetts")

#9)
filter(murders, region == "Northeast")

murders %>%
  filter(region == "Northeast")

#10)
!match(ind, murders$state)
!ind %in% murders$state
