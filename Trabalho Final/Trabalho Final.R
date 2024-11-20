library(tidyverse)

Cogus <- read.csv("C:/Users/anton/Desktop/Área de Trabalho/Estatística/CE302/CE302---Elementos-de-Prog-para-Estat-stica/Trabalho Final/one_million_mushrooms.csv/one_million_mushrooms.csv", sep = ";")

View(Cogus)
head(Cogus)
Cogus
glimpse(Cogus)

#agrupando e criando amostra
amostra_Cogus <- Cogus %>% sample_n(15000)
View(amostra_Cogus)

amostra_veneno <- Cogus %>% sample_n(20000)
View(cor_veneno)
#Veneno x Não-veneno
cor_veneno <- amostra_veneno %>%
  group_by(class, cap.color) %>%
  summarize(count = n(), .groups = "drop") %>%
  mutate(proportions = count/ sum(count))
g1 <- ggplot(cor_veneno, aes(x = cap.color, y = count, fill = class)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Proporção de cogumelos venenosos por cor",
    x = "Cor",
    y = "Proporção",
    fill = "Class"
  ) +
  scale_fill_manual(values = c("p" = "#6a1b9a", "e" = "#ce93d8")) +
  theme_minimal()

g2 <- ggplot(cor_veneno, aes(x = cap.color, y = class, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "#e1bee7", high = "#7b1fa2") +
  labs(
    title = "Mapa de calor de venenosidade por cor",
    x = "Cor",
    y = "Venenoso ou não",
    fill = "Proporção"
  ) +
  theme_minimal()

tabela_de_veneno <- table(Cogus$cap.color, Cogus$class)
chisq.test(tabela_de_veneno)

g3 <- ggplot(amostra_veneno, aes(x = stem.height, y = stem.width, color = class)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Tamanho e altura do caule por classe",
    x = "Altura",
    y = "Largura",
    color = "Classe"
  ) +
scale_color_manual(values = c("p" = "#6a1b9a", "e" = "#ce93d8")) +
  theme_minimal()

g4 <- ggplot(amostra_veneno, aes(x = class, y = cap.diameter, fill = class)) +
  geom_violin() +
  facet_wrap(~ habitat) +
  labs(
    title = "Distribuição do diamêtro do píleo por classe e habitat",
    x = "Classe",
    y = "Diâmetro do Píleo",
    fill = "Classe"
  ) +
  scale_fill_manual(values = c("p" = "#7b1fa2", "e" = "#e1bee7")) +
  theme_minimal()

g1
g2
g3
g4
