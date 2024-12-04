install.packages("vegan")
install.packages("networkD3")
install.packages("fmsb")
install.packages("Hmisc")
install.packages("reshape2")
library(Hmisc)
library(vegan)
library(tidyverse)
library(gridExtra)
library(grid)
require(data.table)
library(networkD3)
library(fmsb)

df <- read.csv("C:/Users/anton/Desktop/Área de Trabalho/Estatística/CE302/CE302---Elementos-de-Prog-para-Estat-stica/Trabalho Final/one_million_mushrooms.csv/one_million_mushrooms.csv", sep = ";")

names(df) <- gsub("-","_",names(df))
View(Cogus)
head(Cogus)
Cogus
glimpse(Cogus)

##alterando a tabela
df <- df %>%
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

g1 <- ggplot(cor_veneno, aes(x = cap.color, y = count, fill = class)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(
    values = c(
      "edible" = "#ffcc80",
      "poisonous" = "#388e3c"
    )
  ) +
  labs(
    title = "Frequência de cores por classe",
    x = "Classe",
    y = "Frequência",
    fill = "Cor"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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

g3 <- ggplot(cor_veneno, aes(x = cap.color, y = count, fill = class)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_line(aes(group = class), linetype = "dotted") +
  labs(
    title = "Frequência de cores por classe",
    x = "Cores",
    y = "Frequência",
    fill = "Classe"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
g3

cor_veneno <- amostra_veneno %>%
  group_by(class, cap.color) %>%
  summarize(count = n(), .groups = "drop") %>%
  mutate(proportions = count/ sum(count))

edible_data <- cor_veneno %>%
  filter(class == "edible")
poisonous_data <- cor_veneno %>%
  filter(class == "poisonous")

edible_plot <- ggplot(edible_data, aes(x = cap.color, y = count, group = 1)) +
  geom_polygon(fill = "lightgreen", alpha = 0.5, color = "darkgreen") +
  geom_point(size = 2, color = "darkgreen") +
  coord_polar() +
  labs(title = "Radar Chart: Edible Mushrooms", y = "Proportion", x = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))
edible_plot


poisonous_plot <- ggplot(poisonous_data, aes(x = cap.color, y = count, group = 1)) +
  geom_polygon(fill = "lightcoral", alpha = 0.5, color = "darkred") +
  geom_point(size = 2, color = "darkred") +
  coord_polar() +
  labs(title = "Radar Chart: Poisonous Mushrooms", y = "Proportion", x = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))
poisonous_plot

reshape_data <- function(class) {
  class %>%
    select(class, cap.color, proportions) %>%
    pivot_wider(names_from = cap.color, values_from = proportions) %>%
    as.data.frame()
}
radar_edible <- reshape_data(edible_data)
radar_poisonous <- reshape_data(poisonous_data)

add_max_min_rows <- function(radar_data) {
  radar_data <- radar_data[, -1]  # Remove the 'class' column
  radar_data <- rbind(rep(0.21, ncol(radar_data)),  # Max values
                      rep(0, ncol(radar_data)),  # Min values
                      radar_data)
  return(radar_data)
}
radar_edible <- add_max_min_rows(radar_edible)
radar_poisonous <- add_max_min_rows(radar_poisonous)
dev.off()

radarchart(radar_edible)
title(main = "Radar Chart for Edible")

radarchart(radar_poisonous)
title(main = "Radar Chart for Poisonous")


radarchart(radar_edible,
           axistype = 1,               # Type of axis labels (1 = radial)
           pcol = rgb(0.2, 0.5, 0.5, 0.7),  # Color of the plot area
           pfcol = rgb(0.2, 0.5, 0.5, 0.3),  # Fill color for the plot area
           plwd = 4,                   # Line width for the plot
           cglcol = "gray",            # Color of the grid lines
           cglty = 1,                  # Style of the grid lines
           axislabcol = "blue",        # Axis label color
           caxislabels = seq(0, 1, by = 0.2), # Adjust axis labels
           cglwd = 0.8,                # Grid line thickness
           vlcex = 0.8,                # Label font size
           maxmin = TRUE) 


dev.off()
radarchart(radar_edible)
title(main = "Radar Chart for Edible")

radarchart(radar_poisonous)
title(main = "Radar Chart for Poisonous")


##fazer o mesmo com ambos mas retirando brown

