### Teste dia da prova
##testando testando
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

##aredondamento 
ceiling() #p cima
floor() #p baixo
round() #mais próx