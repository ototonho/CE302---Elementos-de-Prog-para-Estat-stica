## 23 de outubro

install.packages("tidyr")

require(dplyr)
require(tidyr)
require(data.table)

car_crash <- data.table::fread("//home/est/apsn24/CE302---Elementos-de-Prog-para-Estat-stica/Brazil Total highway crashes 2010 - 2023.csv.gz")

glimpse(car_crash)

car_crash %>%
  select(moto, 
         starts_with("tr"),
         ends_with("feridos"))

## Vetor de moto
car_crash %>%
  pull(moto)

##data frame de moto
car_crash %>%
  select(moto)

car_crash %>%
  select(moto, automovel, data) %>%
  filter(moto > 2 & automovel == 2)

car_crash %>%
  select(moto, automovel, data) %>%
  filter(moto > 2 | automovel == 2)

car_crash %>% 
  group_by(tipo_de_ocorrencia) %>%
  summarise(media = mean(automovel, na.rm = T), #com os NAs removidos
            n = n()) %>% View() #visualizando o n de observações

car_crash %>%
  filter(tipo_de_ocorrencia %in% c("sem vítima", "com vítima")) %>%
  summarise(media_carros = mean(automovel, na.rm = T),
            media_motos = mean(moto, , na.rm = T),
            medianana_carros = median(automovel, na.rm = T),
            n = n(),
            quantil_25 = quantile(automovel, probs = 0.25, na.rm = T)
            ) %>%  arrange(desc(n))

## tipo de acidente
car_crash %>% 
  group_by(tipo_de_acidente) %>%
  summarise(media = mean(automovel, na.rm = T), #com os NAs removidos
            n = n()) %>% View() #visualizando o n de observações

car_crash %>% 
  filter(tipo_de_ocorrencia %in% c("sem vítima", "com vítima") &
           tipo_de_acidente %in% c("Colisão Traseira", "Saida de Pista")) %>%
  group_by(tipo_de_ocorrencia, tipo_de_acidente) %>%
  summarise(media_carros = mean(automovel, na.rm = T),
            media_motos = mean(moto, , na.rm = T),
            medianana_carros = median(automovel, na.rm = T),
            n = n(),
            quantil_25 = quantile(automovel, probs = 0.25, na.rm = T)
  ) %>%  arrange(desc(n))

## dados dentro do R / Exercício Star Wars
starwars
SW <- starwars
SW %>%
  select(species) %>%
  glimpse()

tabelsa_SW <- SW %>%
  group_by(species) %>%
  summarise(n = n())

Altura_H_e_M <- SW %>%
  group_by(gender) %>%
  summarise(media_altura = mean(height, na.rm = T))

media_I_H <- SW %>%
  filter(gender %in% c("masculine")) %>%
  group_by(species) %>%
  summarise(media_Idade_p_Especie = mean(birth_year, na.rm = T))

##Transformando a data de string para data
data_string <- "23/10/2024"

class(data_string)


data <- as.Date(data_string,
                format = "%d/%m/%Y")
print(data)
class(data)

#somando DIAS
data + 31
data + 365

#Comparação de datas
data1 <- as.Date("2023-08-21")
data2 <- as.Date("2023-08-15")
data1 > data2  # Verifica se data1 é posterior a data2

#formatação de datas para strings
data <- as.Date("2023-08-21")
data_formatada <- format(data, "%d/%m/%Y")

#cálculo de diferença entre datas
data1 <- as.Date("2023-08-21")
data2 <- as.Date("2023-08-15")
diferenca <- difftime(data1, data2, units = "days")  # Diferença em dias
diferencaM <- difftime(data1, (data2+365), units = "days")  # Diferença em dias


print(diferenca)
diferencaM

#extração de componentes de data (dia, mês, ano)
data <- as.Date("2023-08-21")
ano <- format(data, "%Y")
mes <- format(data, "%m")
dia <- format(data, "%d")
dia
data
ano
mes

### Lubridate
install.packages("lubridate")
require(lubridate)
data_ymd <- ymd("2023-08-21")
data_mdy <- mdy("08-21-2023")
data_dmy <- dmy("21-08-2023")

print(data_ymd)

#Operações com datas
data <- ymd("2023-08-21")
data_nova <- data + days(7)  # Adiciona 7 dias
data_anterior <- data - months(2)  # Subtrai 2 meses

print(data_nova)
print(data_anterior)

day(data)
month(data)
year(data)

## extraindo infos das datas
data <- ymd_hms("2023-08-21 15:30:45") #com horário e fuso
ano <- year(data)
mes <- month(data)
dia <- day(data)
hora <- hour(data)
minuto <- minute(data)
segundo <- second(data)

print(ano)
data
print(segundo)
print(minuto)

#funções de resumo de datas
data1 <- ymd("2023-08-21")
data2 <- ymd("2023-08-15")
diferenca_em_dias <- as.numeric(data2 - data1)
diferenca_em_semanas <- as.numeric(weeks(data2 - data1))

print(diferenca_em_dias)
print(diferenca_em_semanas)

#lidando com fusos
# Data original no fuso horário de Nova Iorque
data_ny <- ymd_hms("2023-08-21 12:00:00", tz = "America/New_York")

# Converter para o fuso horário de Londres
data_london <- with_tz(data_ny, tz = "Europe/London")

print(data_ny)
print(data_london)

#diferença de tempo entre datas em fusos diferentes
# Duas datas em fusos horários diferentes
data_ny <- ymd_hms("2023-08-21 12:00:00", tz = "America/New_York")
data_london <- ymd_hms("2023-08-21 17:00:00", tz = "Europe/London")

# Calcular a diferença de tempo em horas
diferenca_horas <- as.numeric(data_london - data_ny)

print(diferenca_horas)

##fusos diferentes em data frames
dados <- data.frame(
  nome = c("Evento 1", "Evento 2"),
  data = c(
    ymd_hms("2023-08-21 12:00:00", tz = "America/New_York"),
    ymd_hms("2023-08-21 17:00:00", tz = "Europe/London")
  )
)

# Converter todas as datas para um fuso horário comum, por exemplo, UTC
dados$data_utc <- with_tz(dados$data, tz = "UTC")

print(dados)

