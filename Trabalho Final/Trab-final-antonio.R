library(vegan)
library(tidyverse)
library(gridExtra)
library(grid)
require(data.table)
library(networkD3)

df <- read.csv("C:/Users/anton/Desktop/Área de Trabalho/Estatística/CE302/CE302---Elementos-de-Prog-para-Estat-stica/Trabalho Final/one_million_mushrooms.csv/one_million_mushrooms.csv", sep = ";")

names(df) <- gsub("-","_",names(df))
View(Cogus)
head(Cogus)
Cogus
glimpse(Cogus)

##alterando a tabela
df <- Cogus %>%
  mutate(
    class = case_when(
      class == "e" ~ "edible",
      class == "p" ~ "poisonous",
      TRUE ~ "Unknown"
    ),
    cap.shape = case_when(
      cap.shape == "b" ~ "bell",
      cap.shape == "c" ~ "conical",
      cap.shape == "x" ~ "convex",
      cap.shape == "f" ~ "flat",
      cap.shape == "k" ~ "knobbed",
      cap.shape == "s" ~ "sunken",
      TRUE ~ "Unknown"
    ),
    cap.surface = case_when(
      cap.surface == "f" ~ "fibrous",
      cap.surface == "g" ~ "grooves",
      cap.surface == "y" ~ "scaly",
      cap.surface == "s" ~ "smooth",
      TRUE ~ "Unknown"
    ),
    cap.color = case_when(
      cap.color == "n" ~ "brown",
      cap.color == "b" ~ "buff",
      cap.color == "c" ~ "cinnamon",
      cap.color == "g" ~ "gray",
      cap.color == "r" ~ "green",
      cap.color == "p" ~ "pink",
      cap.color == "u" ~ "purple",
      cap.color == "e" ~ "red",
      cap.color == "w" ~ "white",
      cap.color == "y" ~ "yellow",
      TRUE ~ "Unknown"
    ),
    does.bruise.or.bleed = case_when(
      does.bruise.or.bleed == "t" ~ "bruises",
      does.bruise.or.bleed == "f" ~ "no",
      TRUE ~ "Unknown"
    ),
    gill.attachment = case_when(
      `gill.attachment` == "a" ~ "attached",
      `gill.attachment` == "d" ~ "descending",
      `gill.attachment` == "f" ~ "free",
      `gill.attachment` == "n" ~ "notched",
      TRUE ~ "Unknown"
    ),
    gill.spacing = case_when(
      `gill.spacing` == "c" ~ "close",
      `gill.spacing` == "w" ~ "crowded",
      `gill.spacing` == "d" ~ "distant",
      TRUE ~ "Unknown"
    ),
    gill.color = case_when(
      `gill.color` == "k" ~ "black",
      `gill.color` == "n" ~ "brown",
      `gill.color` == "b" ~ "buff",
      `gill.color` == "h" ~ "chocolate",
      `gill.color` == "g" ~ "gray",
      `gill.color` == "r" ~ "green",
      `gill.color` == "o" ~ "orange",
      `gill.color` == "p" ~ "pink",
      `gill.color` == "u" ~ "purple",
      `gill.color` == "e" ~ "red",
      `gill.color` == "w" ~ "white",
      `gill.color` == "y" ~ "yellow",
      TRUE ~ "Unknown"
    ),
    
    habitat = case_when(
      habitat == 'g' ~ 'grass',
      habitat == 'l' ~'leaves',
      habitat == 'm' ~ 'meadow',
      habitat == 'p' ~ 'paths',
      habitat == 'h' ~ 'heath',
      TRUE ~'Unknown'
    ),
    
    season = case_when(
      season == 's' ~ 'spring',
      season == 'u' ~'summer',
      season == 'w' ~'winter',
      season == 'a' ~'autumn',
      TRUE ~'Unknown'
    ),
    stem.root = case_when(
      stem.root == 'b' ~'bulbous',
      stem.root == 's' ~'swollen',
      stem.root == 'c' ~'club', 
      stem.root == 'u' ~'cup',
      stem.root == 'e' ~ 'equal',
      stem.root == 'z' ~'rhizomorphs',
      stem.root == 'r' ~'rooted',
      TRUE ~ 'Unknown'
      
    ),
    
    stem.surface = case_when(
      stem.surface == "n" ~ "brown",
      stem.surface == "b" ~ "buff",
      stem.surface == "c" ~ "cinnamon",
      stem.surface == "g" ~ "gray",
      stem.surface == "r" ~ "green",
      stem.surface == "p" ~ "pink",
      stem.surface == "u" ~ "purple",
      stem.surface == "e" ~ "red",
      stem.surface == "w" ~ "white",
      stem.surface == "y" ~ "yellow",
      TRUE ~ "Unknown"
    ),
    
    
    stem.color = case_when(
      stem.color == "n" ~ "brown",
      stem.color == "b" ~ "buff",
      stem.color == "c" ~ "cinnamon",
      stem.color == "g" ~ "gray",
      stem.color == "r" ~ "green",
      stem.color == "p" ~ "pink",
      stem.color == "u" ~ "purple",
      stem.color == "e" ~ "red",
      stem.color == "w" ~ "white",
      stem.color == "y" ~ "yellow",
      TRUE ~ "Unknown"
      
    ),
    
    veil.type = case_when(
      veil.type == 'p' ~'partial',
      veil.type == 'u' ~'universal',
      TRUE ~'Unknown'
      
    ),
    
    veil.color = case_when(
      veil.color == "n" ~ "brown",
      veil.color == "b" ~ "buff",
      veil.color == "c" ~ "cinnamon",
      veil.color == "g" ~ "gray",
      veil.color == "r" ~ "green",
      veil.color == "p" ~ "pink",
      veil.color == "u" ~ "purple",
      veil.color == "e" ~ "red",
      veil.color == "w" ~ "white",
      veil.color == "y" ~ "yellow",
      TRUE ~ "Unknown"
      
    ),
    has.ring = case_when(
      has.ring == 't' ~'ring',
      has.ring == 'f' ~'none'
    ),
    ring.type = case_when(
      ring.type == 'c' ~ 'cobwebby',
      ring.type == 'e' ~'evanescent',
      ring.type == 'r' ~'flaring',
      ring.type == 'g' ~'grooved',
      TRUE ~ 'Unknown'
    ),
    spore.print.color = case_when(
      `spore.print.color` == "k" ~ "black",
      `spore.print.color` == "n" ~ "brown",
      `spore.print.color` == "b" ~ "buff",
      `spore.print.color` == "h" ~ "chocolate",
      `spore.print.color` == "g" ~ "gray",
      `spore.print.color` == "r" ~ "green",
      `spore.print.color` == "o" ~ "orange",
      `spore.print.color` == "p" ~ "pink",
      `spore.print.color` == "u" ~ "purple",
      `spore.print.color` == "e" ~ "red",
      `spore.print.color` == "w" ~ "white",
      `spore.print.color` == "y" ~ "yellow",
      TRUE ~ 'Unknown'
      
    )
    
    
    
  )
View(head(df))
View(df)

#agrupando e criando amostra
amostra_veneno <- df %>% sample_n(15000)

#Veneno x Não-veneno
cor_veneno <- amostra_veneno %>%
  group_by(class, cap.color) %>%
  summarize(count = n(), .groups = "drop") %>%
  mutate(proportions = count/ sum(count))

View(cor_veneno)

##Distribuição de cores por classe

g1 <- ggplot(cor_veneno, aes(x = class, y = proportions, fill = cap.color)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Distribuição de cores por classe",
    x = "Classe",
    y = "Proporção",
    fill = "Cor"
  ) +
  theme_minimal()

g1

##Frequência das cores por cada uma das classes

g2 <- ggplot(cor_veneno, aes(x = cap.color, y = count, fill = class)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Frequência de cores por classe",
    x = "Classe",
    y = "Frequência",
    fill = "Cor"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

g2

##Proporção das cores por classes com relação ao n total 
##não gostei

g3 <- ggplot(cor_veneno, aes(x = class, y = cap.color, fill = proportions)) +
  geom_tile() +
  scale_fill_gradient(low = "#d1c4e9", high = "#512da8") +
  labs(
    title = "Proporção de cores por classe",
    x = "Classe",
    y = "Cor",
    fill = "Proporção"
  ) +
  theme_minimal()
g3

proporção_cores <- cor_veneno %>%
  group_by(cap.color) %>%
  mutate(diff = max(proportions) - min(proportions),
         dominant_class = ifelse(
           proportions[class == "poisonous"] > proportions[class == "edible"],
           "poisonous",
           "edible"
         )) %>%
  arrange(desc(diff)) %>%
  ungroup()

##Diferença de proporção por cor e classe
##Se olhar para o g3, o gráfico agora coloca numa escala de cores as diferenças entre classes

g4 <- ggplot(proporção_cores, aes(x = class, y = cap.color, fill = diff)) +
  geom_tile(aes(fill = diff)) +
  scale_fill_gradientn(
    colours = c("#a1887f", "#4e342e", "lightcoral", "darkred"),
    values = scales::rescale(c(0, 1)),
    guide = guide_colourbar(title = "Diferença")
  ) +
  facet_wrap(~dominant_class, nrow = 1) +
  labs(
    title = "Diferença de proporção por cor e classe",
    x = "Classe",
    y = "Cor",
    fill = "Diferença"
  ) +
  theme_minimal()

g4