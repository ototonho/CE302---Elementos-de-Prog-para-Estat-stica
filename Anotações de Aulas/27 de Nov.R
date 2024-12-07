##27 de novembro
require(tidyverse)
require(magrittr)

#system = list("Filmes)
files = list.files("filmes")

Files = list()
for (i in 1:length(files)) {
 Files [[i]] = data.table::fread(paste0("filmes/", files[i])) %>%
   mutate(year = as.numeric(year)) %>%
   mutate(tipo = stringr::str_remove(files[i], ".csv"))
}

Files[[1]]$movie_id


Files %<>% bind_rows()

##Gráfico de Dispersão
Files %>% # Carregar o banco
  ggplot() + ## Chamar o ggplot
  aes( x = year, y = `gross(in $)`,
       color = tipo, #size por ano
       size = year) + ## aplicar a estética, isto é, quais são as variáveis e o que elas significam, x e y, neste caso
  geom_point(alpha = 0.2) +
  scale_size_continuous(range = c(0, 2)) + 
  theme_minimal() # Fazer o scatterplot

#Facetar
Files %>% 
  ggplot() +
  aes( x = year, 
       y = `gross(in $)`, 
       color = tipo,
       size = rating) + #tamanho por rating
  geom_point(alpha = 0.2) + 
  scale_size_continuous(range = c(0, 2)) + 
  facet_wrap(vars(tipo), scales = "fixed") + ## Fazemos o gráfico separado por tipo
  theme_minimal()

Files %>% 
  filter(tipo %in% "animation") %>% 
  ggplot() +
  aes( x = year, 
       y = `gross(in $)`, 
       color = tipo, 
       size = rating) + 
  geom_point(alpha = 0.2) + 
  scale_size_continuous(range = c(0, 2)) + 
  scale_y_continuous(breaks = c(0, 1000, 100000)) + #se usar transform = "log10", joga pra cima / labels = scales::label_log(), trans = "log10
  theme_minimal() +
  labs(x = "Ano",
       y = "Faturamento (em US$)",
       size = "Nota",
       color = "Tipo") +
  theme(legend.position = "bottom",
        text = element_text(size = 14))


##Para linhas de tendência
Files %>% 
  filter(tipo %in% "animation") %>% 
  ggplot() +
  aes( x = year, 
       y = `gross(in $)`, 
       color = tipo, 
       size = rating) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "loess") + # O método aqui se refere ao método utilizado para estimação da curva
  scale_size_continuous(range = c(0, 2)) + 
  scale_y_continuous(breaks = c(0, 1000, 100000)) + #se usar transform = "log10", joga pra cima / labels = scales::label_log(), trans = "log10
  theme_minimal() +
  labs(x = "Ano",
       y = "Faturamento (em US$)",
       size = "Nota",
       color = "Tipo") +
  theme(legend.position = "bottom",
        text = element_text(size = 14))

#Para gráficos em linhas
Files %>% 
  group_by(year, tipo) %>% 
  summarise(Valor_Gasto_Medio = mean(`gross(in $)`, na.rm = TRUE)) %>%
  ggplot() +
  aes( x = year, 
       y = Valor_Gasto_Medio, 
       color = tipo) + 
  geom_line(alpha = 0.5) + 
  geom_point() +
  facet_wrap(vars(tipo)) + 
  theme_minimal()

#Para gasto médio
Files %>% 
  group_by(year) %>% 
  summarise(Valor_Gasto_Medio = mean(`gross(in $)`, na.rm = TRUE)) %>%
  ggplot() +
  aes( x = year, 
       y = Valor_Gasto_Medio) + 
  geom_line(alpha = 0.2) + 
  geom_point() + 
  labs(x = "Ano de Lançamento", 
       y = "Investimento ($)", 
       title = "Gráfico do investimento médio em filmes por ano") + 
  scale_y_continuous(labels = scales::label_dollar()) +
  scale_x_continuous(breaks = seq(from = 1900, to = 2020, by = 10)) + 
  theme_minimal() +
  theme(text = element_text(size = 12, 
                            hjust = 0.5, 
                            face = "bold")) # tamanho da fonte


##Gráfico de Barras - geom_col
Files %>% 
  group_by(tipo) %>% 
  summarise(Total_Investido = sum(`gross(in $)`, na.rm = TRUE)) %>%
  mutate(tipo = reorder(tipo, Total_Investido)) %>% 
  ggplot() +
  aes( x = tipo, 
       y = Total_Investido, 
       color = tipo, 
       fill = tipo) +
  geom_col(alpha = 0.2, 
           ltw = 2) + 
  scale_y_continuous(labels = scales::label_dollar()) +
  coord_flip() +
  labs(x = "Tipo de Filme", 
       y = "Total Investido ($)") + 
  theme_minimal() +
  theme(legend.position = "none")

#Para cada categoria ter sua própria escala por ano - e inverteu a coordenada
Files %>% 
  mutate(tipo = stringr::str_to_title(tipo)) %>% 
  mutate(ano_categorico = case_when(year < 1900 ~ "< 1900", 
                                    year < 1925 ~ "< 1925", 
                                    year < 1950 ~ "< 1950", 
                                    year < 1975 ~ "< 1975", 
                                    year < 2000 ~ "< 2000", 
                                    year < 2025 ~ "< 2025")) %>%
  group_by(tipo, ano_categorico) %>% 
  summarise(Total_Investido = sum(`gross(in $)`, na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(tipo) %>% 
  mutate(total = sum(Total_Investido, na.rm = TRUE)) %>%
  ggplot() +
  aes( x = reorder(tipo, total), 
       y = Total_Investido, 
       color = ano_categorico, 
       fill = ano_categorico) +
  geom_col() + 
  scale_y_continuous(labels = scales::label_dollar()) +
  scale_color_manual(values = colorRampPalette(c("Blue", "Pink"))(7)) + 
  scale_fill_manual(values = colorRampPalette(c("Blue", "Pink"))(7)) + 
  
  coord_flip() +
  labs(x = "Tipo de Filme", 
       y = "Total Investido ($)",
       color = "Ano", 
       fill = "Ano") + 
  theme_minimal() +
  theme(legend.position = "bottom", 
        text = element_text(size = 18))

##Histograma
ggplot(Files) +
  aes(x = rating) +
  geom_histogram(bins = 100, fill = "#555555") + #o próprio usuário precisa dizer quantas bins acha necessário
  theme_minimal()

##Gráfico de densidade
ggplot(Files) +
  aes(x = rating, fill = tipo, color = tipo) +
  geom_density(alpha = 0.1) +
  theme_minimal() +
  theme(legend.position = "bottom")

#Separados
ggplot(Files) +
  aes(x = rating, fill = tipo, color = tipo) +
  geom_density(alpha = 0.1) +
  facet_wrap(vars(tipo)) + 
  theme_minimal() +
  theme(legend.position = "none")

#Boxplot
ggplot(Files) +
  aes(x = rating, fill = tipo, color = tipo) +
  geom_boxplot(alpha = 0.1) +
  theme_minimal() +
  theme(legend.position = "none")

#Mais pica
ggplot(Files) +
  aes(x = rating, 
      y = tipo, 
      fill = tipo, 
      color = tipo) +
  geom_boxplot(alpha = 0.1, 
               outlier.shape = NA) +
  geom_jitter(size = 0.1, 
              alpha = 0.1, 
              pch = 20) + 
  theme_minimal() +
  theme(legend.position = "none")

##Violino
ggplot(Files) +
  aes(x = rating, y = tipo, color = tipo) +
  geom_violin(alpha = 0.1) +
  theme_minimal() +
  theme(legend.position = "none")
#Boxplot dentro do violino
ggplot(Files) +
  aes(x = rating, y = 1) +
  geom_violin(alpha = 0.1) +
  geom_boxplot(width = 0.4) + 
  theme_minimal() +
  theme(legend.position = "none")

#Gráfico de Contorno
Files %>%
  filter(tipo %in% c("war", "animation", "horror")) %>%
  mutate(runtime_num = stringr::str_remove(runtime, "min") %>% 
           str_squish() %>% 
           as.numeric()) %>% 
  filter(!is.na(runtime_num)) %>% 
  filter(`gross(in $)` > 0 | !is.na(`gross(in $)`)) %>% 
  ggplot() + 
  aes(x = year, 
      y = runtime_num) + 
  geom_density_2d_filled(contour_var = "density") +
  theme_minimal() + 
  theme(legend.position = "none") +
  facet_wrap(vars(tipo), scales = "free")

#Salvar imagem
pdf("CE302---Elementos-de-Prog-para-Estat-stica/Gráfico1.pdf", width = 1200, height = 800)
##tem o problema de carregar coisas do R que nao funcionam em outros softwares e lê-se como cruzes
dev.off() #Encerra, antes tudo é salvo
##portanto, usa-se a biblioteca Cairo
Cairo::CairoPDF("CE302---Elementos-de-Prog-para-Estat-stica/Gráfico1.pdf", width = 1200, height = 800)
dev.off() 


###esquisse - ele gera o código
##Addins -> esquisse -> ggplot

##Mais de um gráfico ao mesmo tempo
require(patchwork)

p1 = 
  Files %>% 
  filter(tipo %in% c("animation")) %>% 
  mutate(nome = ifelse(`gross(in $)` > quantile(`gross(in $)`, 0.95, na.rm = T), movie_name, NA)) %>% 
  mutate(col = ifelse(!is.na(nome), "Importante", "Não Importante")) %>%
  ggplot() +
  aes(x = year, 
      y = rating, 
      label = nome, 
      color = col)+
  geom_point() +
  geom_text_repel(box.padding = 0.5, 
                  max.overlaps = 1000, 
                  nudge_x = .15,
                  nudge_y = 1,
                  segment.curvature = -0.1,
                  segment.ncp = 3,
                  segment.angle = 20
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

p2 = Files %>% 
  filter(tipo %in% c("animation")) %>% 
  ggplot() +
  aes(group = year, 
      y = rating, 
  )+
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "bottom")

p3 = Files %>% 
  filter(tipo %in% c("animation")) %>% 
  group_by(year) %>% 
  summarise(rate = mean(rating, na.rm = TRUE)) %>% 
  ggplot()+
  aes(x = year, 
      y = rate) + 
  geom_line() +
  geom_point() +
  theme_minimal()

p1 / p2

##Comunicação - PARTE 8

#alterando dados do documento
#---
#  title: "Seu Título Aqui"
#subtitle: "Seu Subtítulo Aqui"
#author: "Seu Nome aqui"
#date: 10/10/2023
#date-format: long # date-format: dddd MMM D, YYYY   
#lang: pt
#---
  
#Temas do código
#title: "Seu Título Aqui"
#subtitle: "Seu Subtítulo Aqui"
#author: "Seu Nome aqui"
#date: 10/10/2023
#date-format: long # date-format: dddd MMM D, YYYY   
#lang: pt
#theme: 
#  light: yeti
#dark: slate
#format: 
#  pdf: 
#  highlight-style: dracula
#html:
#  highlight-style: solarized
#docx: default

##Utilizar o pacote shinyextra e tbm o shinyui
##E também o ShinyUiEditor


