### Aula 30 de outubro de 2024

### Listas de exercícios para o git assim temos acessos na prova

## Organização de banco de dados
install.packages("tidyverse")
install.packages("tzdb")
require(tidyverse)
require(magrittr)
library(tidyverse)

## pivot
table1 %>% select(country, year, cases) %>% pivot_wider(names_from = year,
                                                        values_from = cases,
                                                        names_prefix = "ano_")

# sem tirar população não temos todos os correspondentes
#pivot_wider
table1 %>%
  pivot_wider(names_from = year,
              values_from = cases,
              names_prefix = "ano_",
              values_fill = 0,
              values_fn = length) %>% View() #aplicou uma função dentro dos valores

#pivot_longer
table1 %>% pivot_longer(cols = c(cases, population),
                        names_to = "variavel",
                        values_to = "tamanho")

table1 %>% pivot_longer(cols = -c(country, year),
                        names_to = "variavel",
                        values_to = "tamanho")
#table3
table3

separated = table3 %>%
  separate(rate, into = c("cases", "population"))
separated

separated %>% unite(rate, cases, population, sep ="/")

separated %>%
  mutate(cases = as.numeric(cases),
         population = as.numeric(population)) %>%
  mutate(rate = cases/population * 100)

separated %>% unite(rate, cases, population, sep =":")

## lendo os dados
require(data.table)
TB <- fread("/home/est/apsn24/CE302---Elementos-de-Prog-para-Estat-stica/TB.csv.gz")
names(TB)

TB1 <- TB %>% 
  pivot_longer(
    cols = -c(1:4), 
    names_to = "chave", 
    values_to = "casos", 
    values_drop_na = TRUE #removeu os NAs
  )
TB1

TB1 %>% 
  count(chave) %>% View()

TB1 %<>% filter(chave %like% "^new") #para ver funções com essa variável
View(TB1)

