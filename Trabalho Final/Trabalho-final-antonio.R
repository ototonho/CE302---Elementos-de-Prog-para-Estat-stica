##Trabalho Final, Antonio

install.packages("dplyr")

library(dplyr)
library(tidyverse)

library(Hmisc)
library(gridExtra)
library(grid)
library(reshape2)

df <- read.csv("C:/Users/anton/Desktop/Área de Trabalho/Estatística/CE302/CE302---Elementos-de-Prog-para-Estat-stica/Trabalho Final/one_million_mushrooms.csv/one_million_mushrooms.csv", sep = ";")

names(df) <- gsub("-","_",names(df))
View(df)
head(df)
glimpse(df)

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
cor_chapéu <- amostra_veneno %>%
  group_by(class, cap.color) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(proportions = count/ sum(count))

View(cor_chapéu)

##Distribuição de cores do chapéu por classe

g1 <- ggplot(cor_chapéu, aes(x = cap.color, y = count, fill = class)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(
    values = c(
      "edible" = "#ffcc80",
      "poisonous" = "#388e3c"
    )
  ) +
  labs(
    title = "Frequência de cores do chapéu por classe",
    x = "Classe",
    y = "Frequência",
    fill = "Cor"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

g1

##Distribuição de cores do himênio por classe
cor_himenio <- amostra_veneno %>%
  group_by(class, gill.color) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(proportions = count/ sum(count))

View(cor_himenio)

g2 <- ggplot(cor_himenio, aes(x = gill.color, y = count, fill = class)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(
    values = c(
      "edible" = "#ffcc80",
      "poisonous" = "#388e3c"
    )
  ) +
  labs(
    title = "Frequência de cores do himênio por classe",
    x = "Classe",
    y = "Frequência",
    fill = "Cor"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

g2

##Distribuição de cores do véu por classe
cor_véu <- amostra_veneno %>%
  group_by(class, veil.color) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(proportions = count/ sum(count))

View(cor_véu)

g3 <- ggplot(cor_véu, aes(x = veil.color, y = count, fill = class)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(
    values = c(
      "edible" = "#ffcc80",
      "poisonous" = "#388e3c"
    )
  ) +
  labs(
    title = "Frequência de cores do véu por classe",
    x = "Classe",
    y = "Frequência",
    fill = "Cor"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

g3

##Distribuição de cores do estipe por classe
cor_estipe <- amostra_veneno %>%
  group_by(class, stem.color) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(proportions = count/ sum(count))

View(cor_estipe)

g4 <- ggplot(cor_estipe, aes(x = stem.color, y = count, fill = class)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(
    values = c(
      "edible" = "#ffcc80",
      "poisonous" = "#388e3c"
    )
  ) +
  labs(
    title = "Frequência de cores do estipe por classe",
    x = "Classe",
    y = "Frequência",
    fill = "Cor"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

g4

## Gráficos facetados
grid.arrange(
  arrangeGrob(
    g1, g2, g3, g4, ncol = 2,
    top = textGrob("As quatro distribuições de cores", gp = gpar(fontsize = 16, fontface = "bold"))
  ))

##Correlação entre as distribuições
amostra_veneno_num <- amostra_veneno %>%
  mutate(across(c(cap.color, gill.color, veil.color, stem.color), 
                ~ as.numeric(factor(.))))

correlacao_por_classe <- amostra_veneno_num %>%
  select(cap.color, gill.color, veil.color, stem.color, class) %>%  # Selecionar as colunas de interesse
  group_by(class) %>%  # Agrupar por 'class'
  summarise(
    cor = list(cor(across(c(cap.color, gill.color, veil.color, stem.color)))),  # Calcular a correlação para as colunas selecionadas
    .groups = "drop"
  )

print(correlacao_por_classe)

# extraindo a correlação para 'edible'
cor_edible <- correlacao_por_classe$cor[[1]]

# visualizando a correlação
print(cor_edible)

#extraindo a correlação para 'poisonous'
cor_poisonous <- correlacao_por_classe$cor[[2]]

# visualizando a correlação'
print(cor_poisonous)

#Conversão para long-format
cor_edible_melt <- melt(cor_edible)
cor_poisonous_melt <- melt(cor_poisonous)

##adicionando coluna para classe
cor_edible_melt$class <- "edible"
cor_poisonous_melt$class <- "poisonous"
combined_cor <- rbind(cor_edible_melt, cor_poisonous_melt) ##união
g5 <- ggplot(combined_cor, aes(Var1, Var2, fill = value)) + #gráfico
  geom_tile() +
  facet_wrap(~ class) +  # Create a facet for each class
  scale_fill_gradient2(midpoint = 0, low = "#ef9a9a", high = "#b71c1c", mid = "white") +  # Color scale for the correlation
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate axis labels
  labs(title = "Matrizes de correlação: Edible vs Poisonous", x = "Coloração", y = "Coloração") +
  coord_fixed(ratio = 1)

g5

# Excluindo a diagonal
cor_cores_filtrado <- combined_cor %>%
  filter(Var1 != Var2)

# Gráfico sem a diagonal
g5_filtrado <- ggplot(cor_cores_filtrado, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  facet_wrap(~ class) +  # Facet by class
  scale_fill_gradient2(midpoint = 0, low = "#ef9a9a", high = "#b71c1c", mid = "white") +  # Color scale
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate axis labels
  labs(
    title = "Matrizes de correlação (sem diagonal): Edible vs Poisonous",
    x = "Coloração",
    y = "Coloração"
  ) +
  coord_fixed(ratio = 1)

g5_filtrado


##Distância entre distribuições - chapéu
dados_edible <- amostra_veneno_num %>% filter(class == "edible")
dados_poisonous <- amostra_veneno_num %>% filter(class == "poisonous")

# Align levels for cap.color
levels_cap <- union(names(edible_dist_cap), names(poisonous_dist_cap))
edible_dist_cap <- table(factor(dados_edible$cap.color, levels = levels_cap)) / nrow(dados_edible)
poisonous_dist_cap <- table(factor(dados_poisonous$cap.color, levels = levels_cap)) / nrow(dados_poisonous)

# Align levels for gill.color
levels_gill <- union(names(edible_dist_gill), names(poisonous_dist_gill))
edible_dist_gill <- table(factor(dados_edible$gill.color, levels = levels_gill)) / nrow(dados_edible)
poisonous_dist_gill <- table(factor(dados_poisonous$gill.color, levels = levels_gill)) / nrow(dados_poisonous)

# Align levels for veil.color
levels_veil <- union(names(edible_dist_veil), names(poisonous_dist_veil))
edible_dist_veil <- table(factor(dados_edible$veil.color, levels = levels_veil)) / nrow(dados_edible)
poisonous_dist_veil <- table(factor(dados_poisonous$veil.color, levels = levels_veil)) / nrow(dados_poisonous)

# Align levels for stem.color
levels_stem <- union(names(edible_dist_stem), names(poisonous_dist_stem))
edible_dist_stem <- table(factor(dados_edible$stem.color, levels = levels_stem)) / nrow(dados_edible)
poisonous_dist_stem <- table(factor(dados_poisonous$stem.color, levels = levels_stem)) / nrow(dados_poisonous)

# Calculate Euclidean distances after aligning levels
euclidean_distance_cap <- sqrt(sum((edible_dist_cap - poisonous_dist_cap)^2))
euclidean_distance_gill <- sqrt(sum((edible_dist_gill - poisonous_dist_gill)^2))
euclidean_distance_veil <- sqrt(sum((edible_dist_veil - poisonous_dist_veil)^2))
euclidean_distance_stem <- sqrt(sum((edible_dist_stem - poisonous_dist_stem)^2))

# Print the Euclidean distances
cat("Euclidean Distance for Cap Color: ", euclidean_distance_cap, "\n")
cat("Euclidean Distance for Gill Color: ", euclidean_distance_gill, "\n")
cat("Euclidean Distance for Veil Color: ", euclidean_distance_veil, "\n")
cat("Euclidean Distance for Stem Color: ", euclidean_distance_stem, "\n")

distance_data <- data.frame(
  color_variable = c("Cap Color", "Gill Color", "Veil Color", "Stem Color"),
  euclidean_distance = c(
    euclidean_distance_cap, 
    euclidean_distance_gill, 
    euclidean_distance_veil, 
    euclidean_distance_stem
  )
)

# Plot the Euclidean distances
g6 <- ggplot(distance_data, aes(x = color_variable, y = euclidean_distance)) +
  geom_bar(stat = "identity", fill = "#ab47bc", color = "black") +
  theme_minimal() +
  labs(
    title = "Distância entre as dist. Edible e Poisonous",
    x = "Colorações",
    y = "Distância entre as distribuições"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
g6



##Qui-quadrado
quidradado <- amostra_veneno %>%
  select(class, cap.color, gill.color, veil.color, stem.color) %>%
  gather(key = "variable", value = "color_value", -class) %>%
  group_by(variable) %>%
  summarise(
    chi_sq_test = list(chisq.test(table(class, color_value))),
    p_value = chi_sq_test[[1]]$p.value,
    .groups = "drop"
  )

quidradado$chi_sq_test
summary(quidradado$p_value)
quidradado <- quidradado %>%
  filter(!is.na(p_value))

#gráfico da brincadeira
View(quidradado)
quidradado %>%
  count(p_value) %>%
  ggplot(aes(x = p_value, y = n)) +
  geom_bar(stat = "identity", fill = "#ba68c8", color = "black") +
  theme_minimal() +
  labs(
    title = "Frequency of p-values",
    x = "p-value",
    y = "Count"
  )


grid.arrange(
  arrangeGrob(
    g1, g2, g3, g4, g5, g6, ncol = 2,
    top = textGrob("As quatro distribuições de cores", gp = gpar(fontsize = 16, fontface = "bold"))
  ))
