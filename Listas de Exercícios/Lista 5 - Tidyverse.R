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
Nome_do_arquivo <- "Dados/Art_Moma.csv.gz"
temp_dir <- tempdir()
unzip("C:/Users/anton/Desktop/Área de Trabalho/Estatística/CE302/CE302---Elementos-de-Prog-para-Estat-stica/Listas de Exercícios/Dados.zip", 
      files = Nome_do_arquivo, exdir = temp_dir)
caminho_para_Gzip <- file.path(temp_dir, Nome_do_arquivo)
Art_Moma <- read_csv(gzfile(caminho_para_Gzip))
print(Art_Moma)

Nome_do_arquivo <- "Dados/Art.csv.gz"
temp_dir <- tempdir()
unzip("C:/Users/anton/Desktop/Área de Trabalho/Estatística/CE302/CE302---Elementos-de-Prog-para-Estat-stica/Listas de Exercícios/Dados.zip", 
      files = Nome_do_arquivo, exdir = temp_dir)
caminho_para_Gzip <- file.path(temp_dir, Nome_do_arquivo)
Art <- read_csv(gzfile(caminho_para_Gzip))
print(Art)

dados_unidos <- left_join(Art, Art_Moma, by = "artist_unique_id")
View(dados_unidos)

#a) média MoMMA e Whitney
Média_Whitney <- dados_unidos %>%
  group_by(year) %>%
  summarise(total_de_expos = sum(whitney_count_to_year, na.rm = TRUE), .groups = "drop") %>%
  summarise(Média_Whitney = mean(total_de_expos))
print(Média_Whitney)

Média_Moma <- dados_unidos %>%
  group_by(year) %>%
  summarise(total_de_expos = sum(moma_count_to_year, na.rm = TRUE), .groups = "drop") %>%
  summarise(Média_Moma = mean(total_de_expos))
print(Média_Moma)

#b) Média MoMMA e Whitney por anor para artistas não brancos
Média_Whitney_Non_White <- dados_unidos %>%
  filter(artist_race_nwi == "Non-White") %>%
  group_by(year) %>%
  summarise(total_de_expos = sum(whitney_count_to_year, na.rm = TRUE), .groups = "drop") %>%
  summarise(Média_Whitney_Non_White = mean(total_de_expos))

print(Média_Whitney_Non_White)

Média_Moma_Non_White <- dados_unidos %>%
  filter(artist_race_nwi == "Non-White") %>%
  group_by(year) %>%
  summarise(total_de_expos = sum(moma_count_to_year, na.rm = TRUE), .groups = "drop") %>%
  summarise(Média_Moma_Non_White = mean(total_de_expos))

print(Média_Moma_Non_White)

#c)4 artistas com mais exposições no MoMA 
top4_artistas_MoMA <- dados_unidos %>%
  group_by(artist_name) %>%
  summarise(total_de_expos = sum(moma_count_to_year, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_de_expos)) %>%
  slice_head(n = 4)

View(top4_artistas_MoMA)

#d) artistas homens e mulheres
gender_count <- dados_unidos %>%
  distinct(artist_unique_id, artist_gender) %>%
  group_by(artist_gender) %>%
  summarise(gender_count = n(), .groups = "drop")
print(gender_count)

#e) cinco nacionalidades mais frequentes
nat_count <- dados_unidos %>%
  distinct(artist_unique_id, artist_nationality) %>%
  group_by(artist_nationality) %>%
  summarise(total_artists =n(), .groups = "drop") %>%
  arrange(desc(total_artists)) %>%
  slice_head(n = 5)
print(nat_count)

#f) quantos de cada museu aparecem em cada livro
artists_per_book <- dados_unidos %>%
  filter(moma_count_to_year > 0 | whitney_count_to_year > 0) %>%
  mutate(museu = case_when(moma_count_to_year > 0 & whitney_count_to_year > 0 ~ "both",
                           moma_count_to_year > 0 ~ "MoMA",
                           whitney_count_to_year > 0 ~ "Whitney")) %>%
  distinct(artist_unique_id, book, museu) %>%
  group_by(book, museu) %>%
  summarise(total_artists = n(), .groups = "drop")
print(artists_per_book)

#g) média de espaço por artista nas páginas
space_ratio_mean <- dados_unidos %>%
  group_by(artist_unique_id) %>%
  summarise(space_ratio_per_page_total = mean(space_ratio_per_page_total, na.rm = TRUE), .groups = "drop")

print(space_ratio_mean)

library(data.table)
library(dplyr)
## Refugiados
Nome_do_arquivo <- "Dados/refugiados.csv.gz"
temp_dir <- tempdir()
unzip("C:/Users/anton/Desktop/Área de Trabalho/Estatística/CE302/CE302---Elementos-de-Prog-para-Estat-stica/Listas de Exercícios/Dados.zip", 
      files = Nome_do_arquivo, exdir = temp_dir)
caminho_para_Gzip <- file.path(temp_dir, Nome_do_arquivo)
refugiados <- read_csv(gzfile(caminho_para_Gzip))
print(refugiados)

Nome_do_arquivo <- "Dados/refugiados_pais.csv.gz"
temp_dir <- tempdir()
unzip("C:/Users/anton/Desktop/Área de Trabalho/Estatística/CE302/CE302---Elementos-de-Prog-para-Estat-stica/Listas de Exercícios/Dados.zip", 
      files = Nome_do_arquivo, exdir = temp_dir)
caminho_para_Gzip <- file.path(temp_dir, Nome_do_arquivo)
refugiados_pais <- read_csv(gzfile(caminho_para_Gzip))
print(refugiados_pais)

refugiados_novo <- refugiados %>%
  left_join(refugiados_pais, by = c("id_origem" = "id"))

refugiados_juntos <- refugiados_novo %>%
  left_join(refugiados_pais, by = c("id_origem" = "id"), suffix = c("id_origem", "id_destino"))

View(refugiados_juntos)


#1) Média de refugiados por país
refugiados[, .(average = mean(refugiados)), by = "id_origem"]

refugiados %>%
  group_by(id_origem) %>%
  summarize(mean(refugiados))

#2) Média de refugiados saindo do Afeganistão
refugiados %>%
  filter(id_origem == "AFG") %>%
  group_by(id_origem) %>%
  summarize(mean(refugiados))
#e em 1990?
refugiados %>%
  filter(id_origem == "AFG", ano == "1990")%>%
  group_by(id_origem) %>%
  summarize(mean(refugiados))
# E a partir de 2000?
refugiados %>%
  filter(id_origem == "AFG", ano > "1999")%>%
  group_by(id_origem) %>%
  summarize(mean(refugiados))

#3)matriz de migração intercontinental (de -> para) de refugiados do ano 2005
refugiados_2005 <- refugiados %>%
  filter(ano == 2005)

refugiados_2005 <- refugiados_2005 %>%
  left_join(refugiados_pais, by = c("id_origem" = "id")) %>%
  rename(origem = nome) %>%
  left_join(refugiados_pais, by = c("id_destino" = "id")) %>%
  rename(destino = nome)

matriz_migração <- refugiados_2005 %>%
  group_by(origem, destino) %>%
  summarize(total_refugiados = sum(refugiados, na.rm=TRUE)) %>%
  tidyr::pivot_wider(names_from = destino, values_from = total_refugiados, values_fill = 0)

print(matriz_migração)
View(matriz_migração)

#4) Qual país mais recebeu refugiados a partir de 2005?
refugiados %>%
  filter(ano == 2005) %>%
  group_by(id_destino) %>%
  summarize(total_Ref = sum(refugiados, na.rm = TRUE)) %>%
  arrange(desc(total_Ref)) %>%
  slice(1)

refugiados %>%
  filter(ano == 2010) %>%
  group_by(id_destino) %>%
  summarize(total_Ref = sum(refugiados, na.rm = TRUE)) %>%
  arrange(desc(total_Ref)) %>%
  slice(1)

#5) Quantos refugiados os 3 países que mais receberam refugiados em 2010 receberam em 2005?
top3_2010 <- refugiados %>%
  filter(ano == 2010) %>%
  group_by(id_destino) %>%
  summarize(total_Ref = sum(refugiados, na.rm = TRUE)) %>%
  arrange(desc(total_Ref)) %>%
  slice(1:3)

top3_2010_em_2005 <- refugiados %>%
  filter(ano == 2005, id_destino %in% top3_2010) %>%
  group_by(id_destino) %>%
  summarize(total_Ref_2005 = sum(refugiados, na.rm = TRUE)) %>%
  left_join(refugiados_pais, by = c("id_destino" = "id"))
view(top3_2010_em_2005)
