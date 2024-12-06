---
  title: "Diamonds Explorer"
author: "Barkamian Analytics"
output: html_document
runtime: shiny
---


require(shiny)
library(ggplot2)

require(data.table)
require(dplyr)
require(tidyverse)
require(ggplot2)
library(networkD3)
library(gridExtra)
library(grid)
list.files()
getwd()
load("C:/Users/anton/Desktop/Área de Trabalho/Estatística/CE302/CE302---Elementos-de-Prog-para-Estat-stica/Trabalho Final/dados.RData")


getwd()

# Mudando o nome das colunas para garantir que fiquem com _ ao invés de - 
names(df) <- gsub("-","_",names(df))


# Vamos substituir os registros de acordo com o dicionário de dados. 
# Caso não haja uma correspondência no nosso dicionário, substituiremos por unknown

df <- df %>%
  mutate(
    class = case_when(
      class == "e" ~ "Comestível",
      class == "p" ~ "Venenoso",
      TRUE ~ "Desconhecido"
    ),
    cap_shape = case_when(
      cap_shape == "b" ~ "Campanulado",
      cap_shape == "c" ~ "Cônico",
      cap_shape == "x" ~ "Convexo",
      cap_shape == "f" ~ "Plano",
      cap_shape == "o" ~ "Umbonado",
      cap_shape == "s" ~ "Umbilicado",
      cap_shape == "p" ~ "Afundado",
      TRUE ~ "Unknown"
      
    ),
    cap_surface = case_when(
      cap_surface == "f" ~ "fibrous",
      cap_surface == "g" ~ "grooves",
      cap_surface == "y" ~ "scaly",
      cap_surface == "s" ~ "smooth",
      TRUE ~ "Unknown"
    ),
    cap_color = case_when(
      cap_color == "n" ~ "Marrom",
      cap_color == "b" ~ "Bege",
      cap_color == "c" ~ "Canela",
      cap_color == "g" ~ "Cinza",
      cap_color == "r" ~ "Verde",
      cap_color == "p" ~ "Rosa",
      cap_color == "u" ~ "Roxo",
      cap_color == "e" ~ "Vermelho",
      cap_color == "w" ~ "Branco",
      cap_color == "y" ~ "Amarelo",
      TRUE ~ "Desconhecida"
    ),
    does_bruise_or_bleed = case_when(
      does_bruise_or_bleed == "t" ~ "bruises",
      does_bruise_or_bleed == "f" ~ "no",
      TRUE ~ "Unknown"
    ),
    gill_attachment = case_when(
      `gill_attachment` == "a" ~ "attached",
      `gill_attachment` == "d" ~ "descending",
      `gill_attachment` == "f" ~ "free",
      `gill_attachment` == "n" ~ "notched",
      TRUE ~ "Unknown"
    ),
    gill_spacing = case_when(
      `gill_spacing` == "c" ~ "close",
      `gill_spacing` == "w" ~ "crowded",
      `gill_spacing` == "d" ~ "distant",
      TRUE ~ "Unknown"
    ),
    gill_color = case_when(
      `gill_color` == "k" ~ "black",
      `gill_color` == "n" ~ "brown",
      `gill_color` == "b" ~ "buff",
      `gill_color` == "h" ~ "chocolate",
      `gill_color` == "g" ~ "gray",
      `gill_color` == "r" ~ "green",
      `gill_color` == "o" ~ "orange",
      `gill_color` == "p" ~ "pink",
      `gill_color` == "u" ~ "purple",
      `gill_color` == "e" ~ "red",
      `gill_color` == "w" ~ "white",
      `gill_color` == "y" ~ "yellow",
      TRUE ~ "Unknown"
    ),
    
    habitat = case_when(
      habitat == "d" ~ "Florestas",
      habitat == "w" ~ "Lixões",
      habitat ==  "u" ~ "Cidades",
      habitat == "p" ~ "Veredas",
      habitat == "m" ~ "Prados",
      habitat == "l" ~ "Folhas",
      habitat == "g" ~ "Gramas",
      habitat == "h" ~ "Charnecas",
      TRUE ~'Unknown'
    ),
    
    season = case_when(
      season == 's' ~ 'spring',
      season == 'u' ~'summer',
      season == 'w' ~'winter',
      season == 'a' ~'autumn',
      TRUE ~'Unknown'
    ),
    stem_root = case_when(
      stem_root == 'b' ~'bulbous',
      stem_root == 's' ~'swollen',
      stem_root == 'c' ~'club', 
      stem_root == 'u' ~'cup',
      stem_root == 'e' ~ 'equal',
      stem_root == 'z' ~'rhizomorphs',
      stem_root == 'r' ~'rooted',
      TRUE ~ 'Unknown'
      
    ),
    
    stem_surface = case_when(
      stem_surface == "n" ~ "brown",
      stem_surface == "b" ~ "buff",
      stem_surface == "c" ~ "cinnamon",
      stem_surface == "g" ~ "gray",
      stem_surface == "r" ~ "green",
      stem_surface == "p" ~ "pink",
      stem_surface == "u" ~ "purple",
      stem_surface == "e" ~ "red",
      stem_surface == "w" ~ "white",
      stem_surface == "y" ~ "yellow",
      TRUE ~ "Unknown"
    ),
    
    
    stem_color = case_when(
      stem_color == "n" ~ "brown",
      stem_color == "b" ~ "buff",
      stem_color == "c" ~ "cinnamon",
      stem_color == "g" ~ "gray",
      stem_color == "r" ~ "green",
      stem_color == "p" ~ "pink",
      stem_color == "u" ~ "purple",
      stem_color == "e" ~ "red",
      stem_color == "w" ~ "white",
      stem_color == "y" ~ "yellow",
      TRUE ~ "Unknown"
      
    ),
    
    veil_type = case_when(
      veil_type == 'p' ~'partial',
      veil_type == 'u' ~'universal',
      TRUE ~'Unknown'
      
    ),
    
    veil_color = case_when(
      veil_color == "n" ~ "brown",
      veil_color == "b" ~ "buff",
      veil_color == "c" ~ "cinnamon",
      veil_color == "g" ~ "gray",
      veil_color == "r" ~ "green",
      veil_color == "p" ~ "pink",
      veil_color == "u" ~ "purple",
      veil_color == "e" ~ "red",
      veil_color == "w" ~ "white",
      veil_color == "y" ~ "yellow",
      TRUE ~ "Unknown"
      
    ),
    has_ring = case_when(
      has_ring == 't' ~'ring',
      has_ring == 'f' ~'none'
    ),
    ring_type = case_when(
      ring_type == 'c' ~ 'cobwebby',
      ring_type == 'e' ~'evanescent',
      ring_type == 'r' ~'flaring',
      ring_type == 'g' ~'grooved',
      TRUE ~ 'Unknown'
    ),
    spore_print_color = case_when(
      `spore_print_color` == "k" ~ "black",
      `spore_print_color` == "n" ~ "brown",
      `spore_print_color` == "b" ~ "buff",
      `spore_print_color` == "h" ~ "chocolate",
      `spore_print_color` == "g" ~ "gray",
      `spore_print_color` == "r" ~ "green",
      `spore_print_color` == "o" ~ "orange",
      `spore_print_color` == "p" ~ "pink",
      `spore_print_color` == "u" ~ "purple",
      `spore_print_color` == "e" ~ "red",
      `spore_print_color` == "w" ~ "white",
      `spore_print_color` == "y" ~ "yellow",
      TRUE ~ 'Unknown'
      
    )
    
    
    
  )





freq_gross_caule <- ggplot(data = df , aes(x= stem_width,fill = class))+geom_density(alpha = 0.5) + ylab("Frequência")+
  xlab("Grossura do Caule (cm)") + theme_classic() +labs(fill = "Classe",title = "Frequência grossura do caule") 

df$season <- factor(df$season, levels = c("winter", "spring", "summer", "autumn"))

graf1 <- df |>
  filter(!is.na(season), !is.na(class)) |> 
  ggplot(mapping = aes(x = factor(season), fill = class)) + 
  geom_bar(stat = "count", position = "stack", width = 0.7) +  
  labs(title = "Cogumelos comestíveis e venenosos em diferentes estações do ano",
       x = "Estações do Ano",        
       y = "Quantidade", 
       fill = "") + 
  scale_fill_manual(values = c("Comestível" = "pink", "Venenoso" = "lightblue"),
                    labels = c("edible" = "Comestível", "poisonous" = "Venenoso")) + 
  theme_minimal() +               
  scale_x_discrete(labels = c("summer" = "Verão", 
                              "autumn" = "Outono", 
                              "winter" = "Inverno", 
                              "spring" = "Primavera")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  scale_y_continuous(
    labels = scales::label_number(big.mark = " ") 
  )


graf2 <- ggplot(df, aes(x = factor(habitat), fill = factor(class))) + 
  geom_bar(position = "dodge") +  
  labs(title = "Cogumelos comestíveis e venenosos em diferentes Habitats", 
       x = "Habitat", 
       y = "Quantidade", 
       fill = "") + 
  scale_fill_manual(values = c("Comestível" = "lightblue", "Venenoso" = "pink"), 
                    labels = c("Comestível", "Venenoso")) +
  theme_minimal() + 
  scale_x_discrete(labels = c("d" = "Florestas", "w" = "Lixões", "u" = "Cidades", "p" = "Veredas", "m" = "Prados", "l" = "Folhas", "g" = "Gramas", "h" = "Charnecas")) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +  
  scale_y_continuous(labels = scales::label_number(big.mark = " ")) 

graf3 <- df |>
  filter(!is.na(`cap_shape`), !is.na(class)) |> 
  ggplot(mapping = aes(x = factor(`cap_shape`), fill = class)) + 
  geom_bar(stat = "count", position = "stack", width = 0.7) +  
  labs(title = "Distribuição de Cogumelos por Formato do Píleo e Toxicidade",
       x = "Formato do Píleo",        
       y = "Quantidade", 
       fill = "") + 
  scale_fill_manual(values = c("Comestível" = "pink", "Venenoso" = "lightblue"),
                    labels = c("Comestível" = "Comestível", "Venenoso" = "Venenoso")) + 
  theme_minimal() +               
  scale_x_discrete(labels = c("b" = "Campanulado", "c" = "Cônico", "x" = "Convexo", "f" = "Plano", "o" = "Umbonado", "s" = "Umbilicado", "p" = "Afundado")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  scale_y_continuous(
    labels = scales::label_number(big.mark = " ") 
  )

#agrupando e criando amostra
amostra_veneno <- df %>% sample_n(15000)
cor_chapéu <- amostra_veneno %>%
  group_by(class, cap_color) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(proportions = count/ sum(count))


##Distribuição de cores do chapéu por classe

g1 <- ggplot(cor_chapéu, aes(x = cap_color, y = count, fill = class)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(
    values = c(
      "Comestível" = "#ffcc80",
      "Venenoso" = "#388e3c"
    )
  ) +
  labs(
    title = "Frequência de cores do chapéu por classe",
    x = "Classe",
    y = "Frequência",
    fill = "Classe"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cor_himenio <- amostra_veneno %>%
  group_by(class, gill_color) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(proportions = count/ sum(count))

g2 <- ggplot(cor_himenio, aes(x = gill_color, y = count, fill = class)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(
    values = c(
      "Comestível" = "#ffcc80",
      "Venenoso" = "#388e3c"
    )
  ) +
  labs(
    title = "Frequência de cores do himênio por classe",
    x = "Classe",
    y = "Frequência",
    fill = "Classe"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cor_véu <- amostra_veneno %>%
  group_by(class, veil_color) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(proportions = count/ sum(count))



g3 <- ggplot(cor_véu, aes(x = veil_color, y = count, fill = class)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(
    values = c(
      "Comestível" = "#ffcc80",
      "Venenoso" = "#388e3c"
    )
  ) +
  labs(
    title = "Frequência de cores do véu por classe",
    x = "Classe",
    y = "Frequência",
    fill = "Classe"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


cor_estipe <- amostra_veneno %>%
  group_by(class, stem_color) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(proportions = count/ sum(count))



g4 <- ggplot(cor_estipe, aes(x = stem_color, y = count, fill = class)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(
    values = c(
      "Comestível" = "#ffcc80",
      "Venenoso" = "#388e3c"
    )
  ) +
  labs(
    title = "Frequência de cores do estipe por classe",
    x = "Classe",
    y = "Frequência",
    fill = "Classe"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ui <- fluidPage(
  tags$style(HTML("
    /* Alterando o fundo da sidebar para azul claro */
    .well {
      background-color: #81D4FA;  /* Azul Claro */
      color: black;
      padding: 20px;
      border-radius: 10px;
    }

    /* Estilizando os inputs e o slider */
    .form-group select, .form-group sliderInput {
      background-color: #FFFFFF;
      color: #0277BD;  /* Azul escuro */
      border: 2px solid #0277BD;
      border-radius: 5px;
    }
    
    .form-group select:focus, .form-group sliderInput:focus {
      border-color: #FFEB3B;
    }

    /* Títulos dentro da sidebar */
    .sidebarPanel h3 {
      color: #0277BD;  /* Azul escuro */
      font-weight: bold;
    }
    
    /* Personalizando as tabs no mainPanel */
    .tab-content {
      background-color: #F1F8E9;
      padding: 20px;
      border-radius: 10px;
    }
    
    /* Customizando os headers */
    .titlePanel {
      background-color: #81D4FA;  /* Azul Claro */
      color: black;
      padding: 10px;
      border-radius: 10px;
    }
    
    /* Estilizando a barra lateral no topo */
    .sidebarPanel {
      position: fixed;
      top: 0;
      left: 0;
      width: 100%;
      z-index: 1000;
      padding: 10px;
      background-color: #81D4FA;
    }
    
    /* Ajustando o conteúdo do mainPanel para que a barra não se sobreponha */
    .main-panel {
      margin-top: 150px; /* Dando espaço para a barra no topo */
    }
  ")),
  
  titlePanel("Cogumelos"),
  
  # Customização de estilo
  
  fluidRow(
    # Barra no topo
    column(12,
           sidebarPanel(
             h3("Parâmetros do Cogumelo"),
             sliderInput(
               'sampleSize', 
               'Tamanho da Amostra', 
               min = 1, 
               max = 1000,
               value = 500, 
               step = 100, 
               round = 0
             ),
             selectInput('x', 'Cor do Cogumelo', c('Todas as Cores', 'Vermelho', 'Azul', 'Verde')),
             selectInput('y', 'Variáveis Numéricas', choices = c('Variável 1', 'Variável 2')),
             selectInput('color', 'Color', c('None', 'Variável X'))
           )
    )
  ),
  
  # Layout do conteúdo principal
  mainPanel(
    class = "main-panel",
    tabsetPanel(
      tabPanel(
        "Gráficos de Densidade",
        h3("Frequências de características por classe"),
        plotOutput('scatterPlot1'),
        plotOutput('scatterPlot2'),
        plotOutput('scatterPlot3')
      ),
      tabPanel(
        "Density Plots",
        h4("Density Plot by Price"),
        plotOutput('densityPlot1'),
        h4("Density Plot by Carat"),
        plotOutput('densityPlot2')
      ),
      tabPanel(
        "Histograms",
        h4("Histogram of Diamond Price"),
        plotOutput('histogramPlot1'),
        h4("Histogram of Diamond Depth"),
        plotOutput('histogramPlot2')
      ),
      tabPanel(
        "Data Table",
        h4("Sampled Data"),
        tableOutput('data')
      )
    )
  )
)

server <- function(input, output) {
  # Dados reativos com base no tamanho da amostra
  dataset <- reactive({
    df[sample(nrow(df), input$sampleSize), ]
  })
  
  # Primeiro gráfico de 
  output$scatterPlot1 <- renderPlot({
    # Filtrando dados com base na cor do cogumelo
    if (input$x == "Todas as Cores") {
      filtered_data <- dataset()  # Não aplica filtro por cor
    } else {
      filtered_data <- dataset()[dataset()$cap_color == input$x, ]  # Aplica filtro por cor
    }
    
    # Criando o gráfico com a variável 'class' no fill e filtrando pelos dados de cor
    ggplot(data = filtered_data, aes_string(x = input$y, fill = "class")) +
      geom_density(alpha = 0.5) +
      ylab(paste("Frequência ",input$y)) +
      xlab(input$y) +
      theme_classic() +
      labs(fill = "Classe")
  })
  

  
  # Primeiro gráfico de densidade por preço
  output$densityPlot1 <- renderPlot({
    ggplot(dataset(), aes(x = price, fill = cut)) +
      geom_density(alpha = 0.6) +
      theme_minimal() +
      scale_fill_brewer(palette = "Set2") +
      ggtitle("Density Plot of Price by Cut")
  })
  
  # Segundo gráfico de densidade por peso (carat)
  output$densityPlot2 <- renderPlot({
    ggplot(dataset(), aes(x = carat, fill = color)) +
      geom_density(alpha = 0.6) +
      theme_minimal() +
      scale_fill_brewer(palette = "Set3") +
      ggtitle("Density Plot of Carat by Color")
  })
  
  # Primeiro histograma por preço
  output$histogramPlot1 <- renderPlot({
    ggplot(dataset(), aes(x = price)) +
      geom_histogram(binwidth = 500, fill = "darkorange", color = "white") +
      theme_minimal() +
      ggtitle("Histogram of Price")
  })
  
  # Segundo histograma por profundidade (depth)
  output$histogramPlot2 <- renderPlot({
    ggplot(dataset(), aes(x = depth)) +
      geom_histogram(binwidth = 0.5, fill = "darkblue", color = "white") +
      theme_minimal() +
      ggtitle("Histogram of Depth")
  })
  
  # Tabela de dados
  output$data <- renderTable({
    dataset()
  })
}


shinyApp(ui = ui, server = server)

    