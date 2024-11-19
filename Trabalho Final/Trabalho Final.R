library(tidyverse)

Cogus <- read.csv("C:/Users/anton/Desktop/Área de Trabalho/Estatística/CE302/CE302---Elementos-de-Prog-para-Estat-stica/Trabalho Final/one_million_mushrooms.csv/one_million_mushrooms.csv", sep = ";")

View(Cogus)
head(Cogus)
Cogus
glimpse(Cogus)

#Stem Height X Stem Width X Diameter
dados_agregados <- Cogus %>%
  group_by(class, cap.color) %>%
  summarize(count = n(), média_diametro = mean(cap.diameter, na.rm = TRUE))
View(dados_agregados)

amostra_Cogus <- Cogus %>% sample_n(15000)

g1 <- ggplot(amostra_Cogus, aes(x = stem.height, y = stem.width, size = cap.diameter)) +
  geom_point(col = "#ef9a9a", alpha = 0.2) +
  scale_size_continuous(range = c(2,10)) +
  theme_minimal()
g1


#Shape x Diameter
g2 <- ggplot(amostra_Cogus, aes(x = cap.diameter, y = cap.shape)) +
  geom_point() +
  theme_minimal()
g2  

#Poison x Color x Diâmetro
g3 <- ggplot(amostra_Cogus, aes(x = class, y = cap.color)) +
  geom_violin()
g3

##Densidade Cor
g4 <- ggplot(amostra_Cogus, aes(x = cap.color)) +
  geom_bar() 
g4

#Diameter X Habitat
