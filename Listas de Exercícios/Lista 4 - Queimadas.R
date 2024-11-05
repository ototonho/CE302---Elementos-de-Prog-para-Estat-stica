### Lista 4 - Queimadas
library(dplyr)

Q1_queimadas <- read.csv("C:/Users/anton/Desktop/Área de Trabalho/Estatística/CE302/CE302---Elementos-de-Prog-para-Estat-stica/Listas de Exercícios/Dataset_FireWatch_Brazil_Q1_2024.csv")
Q2_queimadas <- read.csv("C:/Users/anton/Desktop/Área de Trabalho/Estatística/CE302/CE302---Elementos-de-Prog-para-Estat-stica/Listas de Exercícios/Dataset_FireWatch_Brazil_Q2_2024.csv")
Q3_queimadas <- read.csv("C:/Users/anton/Desktop/Área de Trabalho/Estatística/CE302/CE302---Elementos-de-Prog-para-Estat-stica/Listas de Exercícios/Dataset_FireWatch_Brazil_Q3_2024.csv")

head(Q1_queimadas)
str(Q1_queimadas)

Queimadas <- bind_rows(Q1_queimadas, Q2_queimadas, Q3_queimadas)
head(Queimadas)

#1)
Incêndios_estado <- Queimadas %>%
  group_by(estado) %>%
  summarise(contagem = n()) %>%
  arrange(desc(contagem))

Incêndios_estado
View(Incêndios_estado)

#2)
Queimadas <- Queimadas %>%
  mutate(região = case_when(
    estado %in% c("PARANÁ", "SANTA CATARINA", "RIO GRANDE DO SUL") ~ "SUL",
    estado %in% c("SÃO PAULO", "RIO DE JANEIRO", "MINAS GERAIS", "ESPÍRITO SANTO") ~ "SUDESTE",
    estado %in% c("DISTRITO FEDERAL", "GOIÁS", "MATO GROSSO", "MATO GROSSO DO SUL") ~ "CENTRO-OESTE",
    estado %in% c("ACRE", "AMAZONAS", "AMAPÁ", "PARÁ", "RONDÔNIA", "RORAIMA", "TOCANTINS") ~ "NORTE",
    estado %in% c("ALAGOAS", "BAHIA", "CEARÁ", "MARANHÃO", "PARAÍBA", "PERNAMBUCO", "PIAUÍ", "RIO GRANDE DO NORTE", "SERGIPE") ~ "NORDESTE",
    TRUE ~ NA_character_
  ))

head(Queimadas)

Queimadas_por_regiao <- Queimadas %>%
  group_by(região) %>%
  summarise(contagem = n()) %>%
  arrange(desc(contagem))
View(Queimadas_por_regiao)

#3)
região_mais_queimada <- Queimadas %>%
  group_by(região) %>%
  summarise(contagem = n()) %>%
  arrange(desc(contagem)) %>%
  slice(1) %>%
  pull(região)
região_mais_queimada

Região_M_Queimada <- Queimadas %>%
  filter(região == região_mais_queimada)

Cidade_M_Queimada <- Região_M_Queimada %>%
  group_by(municipio) %>%
  summarise(contagem = n()) %>%
  arrange(desc(contagem)) %>%
  slice(1) %>%
  pull(municipio)

Cidade_M_Queimada

Data_M_Queimada <- Região_M_Queimada %>%
  group_by(data) %>%
  summarise(contagem = n()) %>%
  arrange(desc(contagem)) %>%
  slice(1) %>%
  pull(data)

Data_M_Queimada

Queimadas <- Queimadas %>%
  mutate(data = as.Date(data, format = "%Y-%m-%d"))

Queimadas <- Queimadas %>%
  mutate(mes = format(data, "%Y-%m"))

Mes_M_Queimado <- Queimadas %>%
  group_by(mes) %>%
  summarise(contagem = n()) %>%
  arrange(desc(contagem)) %>%
  slice(1) %>%
  pull(mes)
Mes_M_Queimado

#4) 
C_alto_risco <- Queimadas %>%
  filter(mes == "2024-07", avg_risco_fogo > 90) %>%
  distinct(municipio) %>%
  nrow()
C_alto_risco

#5)
media_risco_estado <- Queimadas %>%
  group_by(estado) %>%
  summarise(media_risco_estado = mean(avg_risco_fogo, na.rm = TRUE)) %>%
  arrange(desc(media_risco_estado))
View(media_risco_estado)

