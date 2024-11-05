### Lista 5 - Tidyverse
library(tidyverse)

## Lendo a pasta zipada

caminho_para_zip <- "C:/Users/anton/Desktop/Área de Trabalho/Estatística/CE302/CE302---Elementos-de-Prog-para-Estat-stica/Listas de Exercícios/Dados.zip"
Arquivos <- unzip("C:/Users/anton/Desktop/Área de Trabalho/Estatística/CE302/CE302---Elementos-de-Prog-para-Estat-stica/Listas de Exercícios/Dados.zip", list = TRUE)
Nome_do_arquivo <- "Dados/chocolate.csv.gz"
temp_dir <- tempdir()
unzip("C:/Users/anton/Desktop/Área de Trabalho/Estatística/CE302/CE302---Elementos-de-Prog-para-Estat-stica/Listas de Exercícios/Dados.zip", 
      files = Nome_do_arquivo, exdir = temp_dir)
caminho_para_Gzip <- file.path(temp_dir, Nome_do_arquivo)
chocolate <- read_csv(gzfile(caminho_para_Gzip))
print(chocolate)

## Chocolate
#Lista de ingredientes: "#": representa o número de ingredientes no chocolate; B: Grãos, S: Açúcar, S*: Adoçante diferente de açúcar de cana branco ou beterraba, C: Manteiga de Cacau, V: Baunilha, L: Lecitina, Sa: Sal

#a) 
list(chocolate$local_compania)
P_produtores <- chocolate %>%
  group_by(local_compania) %>%
  summarise(contagem = n()) %>%
  arrange(desc(contagem))
View(P_produtores)

#b)
pelomenos_3I <- chocolate %>%
  filter(ingredientes >= 3) %>%
  summarise(contagem = n()) %>%
  arrange(desc(contagem))
View(pelomenos_3I)

#c)
com_5I <- chocolate %>%
  filter(ingredientes >= 5 & ingredientes < 6) %>%
  summarise(contagem = n()) %>%
  arrange(desc(contagem))
View(com_5I)

#d)
Quatro_características <- chocolate %>%
  separate_rows(caracteristicas, sep = ",") %>%
  group_by(ID) %>%
  summarise(num_caracteristicas = n()) %>%
  filter(num_caracteristicas == 4) %>%
  summarise(contagem = n())
View(Quatro_características)

#e)
Sal_na_composição <- chocolate %>%
  filter(str_detect(ingredientes, "Sa")) %>%
  summarise(contagem = n()) %>%
  arrange(desc(contagem))
View(Sal_na_composição)

#f)
Baunilha_na_composição <- chocolate %>%
  filter(str_detect(ingredientes, "V")) %>%
  summarise(contagem = n()) %>%
  arrange(desc(contagem))
View(Baunilha_na_composição)

#g)
L_e_B_naC <- chocolate %>%
  filter(str_detect(ingredientes, "V") & str_detect(ingredientes, "L")) %>%
  summarise(contagem = n()) %>%
  arrange(desc(contagem))
View(L_e_B_naC)

## Artistas do MoMA
