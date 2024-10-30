install.packages("tidyverse")
install.packages("tzdp")
install.packages("timechanges")

library("tidyverse")

dados <- readr::read_csv("/home/est/apsn24/CE302---Elementos-de-Prog-para-Estat-stica/Brazil Total highway crashes 2010 - 2023.csv")
dados <- data.table::fread("/home/est/apsn24/CE302---Elementos-de-Prog-para-Estat-stica/Mental Health Dataset.csv")

class(dados)
head(dados)
dados

glimpse(dados)

Poland <- subset(dados, Country == "Poland")
dados[, c("Country", "Gender")]

### 
# Pipe
x <- seq(1:10)
y <- sqrt(x)
z <- log(y)
x
y
z
tan(cos(log(sqrt(x)))) #não cria tantos elementos na memória
#|> # o pipe do tydiverse

x %>%
  sqrt() %>%
  log() %>%
  cos() %>%
  tan() %>%
  
## Uso do PIPE
require("magrittr")
set.seed(123)

rnorm(10)    %>%
  multiply_by(5) %>%
  add(5)     

rnorm(10) * 5 + 5


require(dplyr)
## Atribuição explicita
meu_data_frame <- data.frame(
  nome = c("Alice", "Bob", "Carol", "Ana", "João", "Carlos", "Patrícia", "Leonardo"),
  idade = c(25, 30, 28, 20, 27, 50, 60, 45),
  salario = c(5000, 6000, 5500, 8000, 2000, 3500, 10000, 3800 ), 
  meio_de_transporte = c('onibus', 'bicicleta', 'onibus', 'carro', 'carro', 'onibus', 'onibus', 'bicicleta'))

meu_data_frame = meu_data_frame %>%
  mutate(idade_25 = idade > 25)

glimpse(meu_data_frame)

## Atribuição implicita
meu_data_frame %<>% 
  mutate(idade_50 = idade > 50)
glimpse(meu_data_frame)


require(dplyr)
require(tidyr)

car_crash <- data.table::fread("//home/est/apsn24/CE302---Elementos-de-Prog-para-Estat-stica/Brazil Total highway crashes 2010 - 2023.csv.gz")
# Dados extraídos de https://www.kaggle.com/datasets/liamarguedas/brazil-total-highway-crashes-2010-2023

glimpse(car_crash)

car_crash %>% 
  select(data, tipo_de_acidente) %>% 
  head()

car_crash %>% 
  select(starts_with("tipo")) %>% 
  head()

car_crash %>% 
  select(ends_with("feridos")) %>% 
  head()

car_crash %>% 
  select(ends_with("feridos")) %>% 
  tail()

car_crash %>% 
  select(contains("mente")) %>% 
  head()

car_crash %>% 
  select(where(is.numeric)) %>% 
  glimpse()

car_crash %>% 
  select(where(is.character)) %>% 
  glimpse()

car_crash %>% 
  select(where(is.logical)) %>% 
  glimpse()

vars_interesse = c("automovel", "bicicleta", "onibus")
car_crash %>% 
  select(all_of(vars_interesse)) %>% 
  glimpse()

car_crash %>% 
  select(any_of(vars_interesse)) %>% 
  glimpse()

car_crash %>%
  select(automovel, bicicleta, onibus)

dados_filtrados <- car_crash %>%
  filter(automovel >= 3)
dados_filtrados

##qqr coisa