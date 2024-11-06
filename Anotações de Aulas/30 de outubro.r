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

#filtro para variáveis com new
TB1 %<>% filter(chave %like% "^new") #para ver funções com essa variável
View(TB1)

#separando o new sp e afins em diferentes variáveis
TB2 <- TB1 %>% 
  mutate(chave = stringr::str_replace(chave, "newrel", "new_rel")) #stringr função nova
TB2

TB3 <- TB2 %>% 
  separate(chave, c("new", "type", "sexage"), 
           sep = "_") #separando variáveis
TB3
View(TB3)

TB4 <- TB3 %>% 
  select(-new, -iso2, -iso3)
View(TB4)

TB5 <- TB4 %>% 
  separate(sexage, c("sexo", "idade"), sep = 1) 
TB5
View(TB5)

#Strings
require(tidyverse)
require(magrittr)

#símbolos e seus códigos
var_com_aspas <-"Ela disse: 'Eu adoro lasanha.'"
var_com_aspas
str_view(var_com_aspas)

var_com_aspas3 <- "Ela disse: \"Eu adoro lasanha.\""
var_com_aspas3
str_view(var_com_aspas3)

var_com_aspas4 <- 'Ela disse: \'Eu adoro lasanha.\''
var_com_aspas4
str_view(var_com_aspas4)

texto_multilinhas <- "Primeira linha\nSegunda linha\nTerceira linha"
str_view(texto_multilinhas)

texto_tabulado <- "Primeira coluna\tSegunda coluna\tTerceira coluna" 
str_view(texto_tabulado)

texto_unicode_grau <- "A temperatura é de 25\u00B0C."
str_view(texto_unicode_grau)

simbolo_somatorio <- "O símolo do somatório é: \u2211"
str_view(simbolo_somatorio)

emoji <- "OMG! Também posso usar emoji! \U1F631"
str_view(emoji)

##Concatenando Strings
df <- data.frame(nome = c("Ana", "Maria", "João", NA), 
                 sobrenome= c("Santos", "Silva", "Souza", NA))
df %>% 
  mutate(ola = str_c("Boa noite ", nome, " ", sobrenome, "!")) #Escrevendo as frases

df %>% 
  mutate(mensagem = str_glue("Boa noite {nome} {sobrenome}!")) #alternativa

df$nome %>% 
  paste(., collapse = ", ") #unindo strings

df$nome %>% 
  str_flatten(na.rm = TRUE) #unindo strings

df$nome %>% 
  str_flatten(na.rm = TRUE, collapse = ", ", last = " e ") #unindo e acrescentando 'e' 

#Formatação básica
texto_exemplo = c("caixa baixa", "CAIXA ALTA", "Texto de sentença", "Texto Em Título")
str_to_lower(texto_exemplo) #caixa baixa
str_to_sentence(texto_exemplo) #a primeira palavra começa com letra maiúscula
str_to_title(texto_exemplo) #Todas as palavras com letra maiúscula
str_to_upper(texto_exemplo) #caixa alta

#retirando espaços nas extremidades
texto_com_espaços = "  Olá, esse texto tem    diversos        espaços completamente desnecessários. "

str_trim(texto_com_espaços, side = "left")
str_trim(texto_com_espaços, side = "right")
str_trim(texto_com_espaços, side = "both")
str_squish(texto_com_espaços)

#Comprimentos de Strings
df %>% 
  mutate(comprimento_nome = str_length(nome)) #tamanho do nome
df %>% 
  mutate(qtd_a = str_count(nome, "a")) #contagem de letras

df %>% 
  mutate(qtd_vogais = str_count(nome, "[aeiou]")) %>% #especificamente minúsculas
  mutate(qtd_consoantes = str_count(nome, "[^aeiou]")) #mais contagens (^indica o contrário do que foi indicado)

#Subtituição de strings
df %>% 
  mutate(nome = str_replace(nome, "ã", "a"))
df %>% 
  mutate(nome = str_remove(nome, "[aeiouã]"))
df %>% 
  mutate(nome = str_remove(nome, "[aeiouã]"))
df %>% 
  mutate(nome = str_remove_all(nome, "[aeiouã]"))

#RegEx - Expressões Regulares
texto <- "O gato é um animal adorável."
padrao <- "gato"
str_detect(texto, padrao) #Detectou se gato está no texto

texto <- c("O rato correu para o buraco.", 
           "O gato correu para o buraco.",
           "O mato.")
padrao <- "(g|r)ato"
str_detect(texto, padrao) # a | corresponde ao OU lógico

#Âncoras ancoram padrões em posições específicas em strings
texto <- c("Banana", "Ana", "Ananas")
padrao <- "^ana"
str_detect(texto, padrao)

## Ignorar case
str_detect(texto, "(?i)ana") #(?i) = ignore se é maiúsculo ou minúsculo

## Ignorar case, terminar com ana
str_detect(texto, "(?i)ana$")
## Ignorar case, começar com ana
str_detect(texto, "(?i)^ana")
## Ignorar case, exatamente  ana
str_detect(texto, "(?i)^ana$")

#Quando interessado em subexpressões
str_extract(texto, "(na)+")

#Quantificando
texto <- "Os números 123 e 456 são importantes."
padrao <- "\\d{3}"
numeros <- str_extract_all(texto, padrao, simplify = TRUE)
numeros

texto <- "Os números 123, 456 e 78 são importantes."
padrao <- "\\d{2}"
numeros <- str_extract_all(texto, padrao, simplify = TRUE)
numeros


##estudas joint/junções e pivotagens e summarise e coisinhas