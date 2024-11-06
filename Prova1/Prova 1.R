### Prova 1
library(tidyverse)
#1) 
DF1 <-data.frame(
  Eficiência = c(36.53, 82.66, 97.48, 43.86, 54.63, 32.54, 7.47, 51.09, 58.78, 97.65),
  Testes_Qualidade = c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
)
View(DF1)
#a)
M_Eficiência <- mean(DF1$Eficiência)
sqrt(M_Eficiência)
#b)
Mediana_passaram <- by(DF1$Eficiência, DF1$Testes_Qualidade, median)
print(Mediana_passaram)
#c)]
Maior_Eficiência <- by(DF1$Eficiência, DF1$Testes_Qualidade, max)
print(Maior_Eficiência)
#d)
Passaram <- DF1[DF1$Eficiência >75, ]
Passaram_Test <- nrow(Passaram)
print(Passaram_Test)
Manutenção <- DF1[DF1$Eficiência < 60, ]
Precisam_Manutenção <- nrow(Manutenção)
print(Precisam_Manutenção)
#e)
DesvioP <- by(DF1$Eficiência, DF1$Testes_Qualidade, sd)
print(DesvioP)

#2)
Indivíduo_1 <- c(14.4, 7.3, 11.0, 8.3, 12.5, 8.1, 12.7, 11.9, 11.3, 10.5)
Indivíduo_2 <- c(13.6, 8.9, 5.4, 14.0, 8.6, 12.0, 8.8, 7.5, 12.8, 10.7)
Indivíduo_3 <- c(8.9, 10.4, 11.9, 13.0, 14.0, 13.8, 9.1, 5.0, 6.7, 14.6)
Indivíduo_4 <- c(13.0, 9.7, 13.5, 11.5, 12.0, 6.7, 6.2, 14.0, 6.2, 14.5)
Indivíduo_5 <- c(8.7, 6.2, 13.1, 8.1, 9.1, 12.3, 7.6, 6.7, 6.3, 10.5)

Matrix_X_linhas <- rbind(Indivíduo_1,
                         Indivíduo_2,
                         Indivíduo_3,
                         Indivíduo_4,
                         Indivíduo_5) #Pré-tratamento
colnames(Matrix_X_linhas) <- c("gene1", "gene2", "gene3", "gene4", "gene5", "gene6", "gene7", "gene8", "gene9", "gene10")

Yndivíduo_1 <- c(9.2, 7.8, 13.5, 13.9, 8.1, 5.2, 8.8, 10.0, 8.7, 5.5)
Yndivíduo_2 <- c(9.1, 9.6, 5.5, 8.0, 5.7, 6.3, 6.1, 7.1, 9.1, 7.1)
Yndivíduo_3 <- c(10.6, 10.7, 8.7, 14.4, 8.9, 10.0, 9.3, 10.5, 13.2, 10.8)
Yndivíduo_4 <- c(12.9, 6.7, 5.9, 11.3, 13.5, 6.7, 9.9, 8.9, 9.4, 6.6)
Yndivíduo_5 <- c(10.9, 11.4, 14.4, 5.8, 12.9, 6.6, 5.6, 7.1, 14.3, 14.2)

Matrix_Y_linhas <- rbind(Yndivíduo_1,
                         Yndivíduo_2,
                         Yndivíduo_3,
                         Yndivíduo_4,
                         Yndivíduo_5) #Pós-tratamento
colnames(Matrix_Y_linhas) <- c("gene1", "gene2", "gene3", "gene4", "gene5", "gene6", "gene7", "gene8", "gene9", "gene10")

#a)
Médias_Colunas_X <- colMeans(Matrix_X_linhas)
print(Médias_Colunas_X)
Gen_maior <- max(Médias_Colunas_X)
print(Gen_maior)

#b)
Trans_X <- t(Matrix_X_linhas)
print(Trans_X)
mult_X_T <- tcrossprod(Matrix_X_linhas, Trans_X)

#c)
Médias_Colunas_Y <- colMeans(Matrix_Y_linhas)
print(Médias_Colunas_Y)
Alterações <- Médias_Colunas_X - Médias_Colunas_Y
print(Alterações)
Menor_alt <- min(Alterações)
print(Menor_alt)

#e)
Médias_Colunas_X <- colMeans(Matrix_X_linhas)
print(Médias_Colunas_X)
Maiores_X <- sort(Médias_Colunas_X)
print(Maiores_X)
Sem2 <- Matrix_X_linhas[, -2]
Sem2_9 <- Sem2[, -9]
Sem2_9_7 <- Sem2_9[, -7]
Sem2_9_7_8 <- Sem2_9_7[, -8]
Cinco_M_X <- Sem2_9_7_8[, -6]
Det_cinco <- det(Cinco_M_X)

#3)
DF_FastFood <- read.csv("/home/est/apsn24/CE302---Elementos-de-Prog-para-Estat-stica/Prova1/Datafiniti_Fast_Food_Restaurants.csv")
#a)
Rest_Prov <- DF_FastFood %>%
  group_by(province) %>%
  summarise(contagem = n()) %>%
  arrange(desc(contagem))
View(Rest_Prov)

CA_Rest <- Rest_Prov %>%
  filter(province == "CA") %>%
  summarise(contagem = contagem) %>%
  arrange(desc(contagem))
View(CA_Rest)

OH_Rest <- Rest_Prov %>%
  filter(province == "OH") %>%
  summarise(contagem = contagem) %>%
  arrange(desc(contagem))
View(OH_Rest)

IA_Rest <- Rest_Prov %>%
  filter(province == "IA") %>%
  summarise(contagem = contagem) %>%
  arrange(desc(contagem))
View(IA_Rest)

CA_OH_Rest <- full_join(CA_Rest, OH_Rest, by = "contagem")
View(CA_OH_Rest)

CA_OH_IA_Rest <- full_join(CA_OH_Rest, IA_Rest, by = "contagem")
View(CA_OH_IA_Rest)
T_CA_OH_IA_Rest <- sum(CA_OH_IA_Rest)
View(T_CA_OH_IA_Rest)

CA_OH_IA_Rest <- full_join(CA_OH_Rest, IA_Rest, by = "contagem")
View(CA_OH_IA_Rest)

#b)
View(CA_OH_IA_Rest)
P1201_Rest <- Rest_Prov %>%
  filter(contagem == "1201") %>%
  summarise(contagem = contagem) %>%
  arrange(desc(contagem))
View(P1201_Rest)

#c)
MaisC_Pits <- DF_FastFood %>%
  filter(categories == "Pitas Republic")