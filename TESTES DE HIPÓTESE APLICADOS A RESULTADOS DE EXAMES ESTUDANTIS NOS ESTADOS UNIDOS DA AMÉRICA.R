#Vitor Eduardo Centurione Magalhães

#Carregar pacotes

library(tidyverse)
library(plotly)
library(dplyr)


#Ler base de Dados

df <- read_csv("StudentsPerformance.csv")


#Transformar variáveis categóricas em Fatores e mudando os nomes das colunas

df$gender <- as.factor(df$gender)
df$`race/ethnicity` <- as.factor(df$`race/ethnicity`)
df$`parental level of education` <- as.factor(df$`parental level of education`)
df$lunch <- as.factor(df$lunch)
df$`test preparation course` <- as.factor(df$`test preparation course`)

colnames(df) <- c("genero", "etnia", "nivel_de_educacao_pais", 
                  "almoco", "preparacao", "matematica", "leitura",
                  "escrita")


#Gerar o relatório GERAL antes de avaliar cada caso particular

GERAL <- df %>%
            group_by(genero) %>%
            summarise(MPM = mean(matematica), MPL = mean(leitura), MPE = mean(escrita))

tibble(GERAL)


Geral_2 <- select(df, c(genero, etnia, matematica, leitura, escrita))


Gender <- data.frame(key = c(1,2),
                     genero = c("male", "female"))


Geral_3 <- full_join(Gender, Geral_2, by = "genero")

mean(Geral_3$matematica)
mean(Geral_3$leitura)
mean(Geral_3$escrita)

median(Geral_3$matematica)
median(Geral_3$leitura)
median(Geral_3$escrita)

sd(Geral_3$matematica)
sd(Geral_3$leitura)
sd(Geral_3$escrita)

var(Geral_3$matematica)
var(Geral_3$leitura)
var(Geral_3$escrita)

Masculino <- Geral_3 %>%
                     filter(key == 1)

mean(Masculino$matematica)
mean(Masculino$leitura)
mean(Masculino$escrita)

median(Masculino$matematica)
median(Masculino$leitura)
median(Masculino$escrita)

sd(Masculino$matematica)
sd(Masculino$leitura)
sd(Masculino$escrita)

var(Masculino$matematica)
var(Masculino$leitura)
var(Masculino$escrita)

Feminino <- Geral_3 %>%
                    filter(key == 2)

mean(Feminino$matematica)
mean(Feminino$leitura)
mean(Feminino$escrita)

median(Feminino$matematica)
median(Feminino$leitura)
median(Feminino$escrita)

sd(Feminino$matematica)
sd(Feminino$leitura)
sd(Feminino$escrita)

var(Feminino$matematica)
var(Feminino$leitura)
var(Feminino$escrita)


#Realizando o teste para cada gênero com amostra total masculina

Media_Geral_Masc_Mat <- mean(Masculino$matematica)
Media_Geral_Masc_Lei <- mean(Masculino$leitura)
Media_Geral_Masc_Esc <- mean(Masculino$escrita)


#Gráficos Masculinos por Matéria

grafico_masc_mat <- Masculino %>%
                              ggplot(mapping = aes(x = matematica))+
                              geom_histogram(aes(y = ..density..), bins = 50)+
                              geom_density(fill = "red", alpha = 0.2)

grafico_masc_mat %>%
                 ggplotly()



grafico_masc_lei <- Masculino %>%
                              ggplot(mapping = aes(x = leitura))+
                              geom_histogram(aes(y = ..density..), bins = 50)+
                              geom_density(fill = "red", alpha = 0.2)

grafico_masc_lei %>%
                 ggplotly()


grafico_masc_esc <- Masculino %>%
                              ggplot(mapping = aes(x = escrita))+
                              geom_histogram(aes(y = ..density..), bins = 50)+
                              geom_density(fill = "red", alpha = 0.2)

grafico_masc_esc %>%
                 ggplotly()


#Realizando o teste para cada gênero com amostra total feminina

Media_Geral_Fem_Mat <- mean(Feminino$matematica)
Media_Geral_Fem_Lei <- mean(Feminino$leitura)
Media_Geral_Fem_Esc <- mean(Feminino$escrita)


#Gráficos Femininos por Matéria

grafico_fem_mat <- Feminino %>%
                            ggplot(mapping = aes(x = matematica))+
                            geom_histogram(aes(y = ..density..), bins = 50)+
                            geom_density(fill = "red", alpha = 0.2)

grafico_fem_mat %>%
                ggplotly()



grafico_fem_lei <- Feminino %>%
                             ggplot(mapping = aes(x = leitura))+
                             geom_histogram(aes(y = ..density..), bins = 50)+
                             geom_density(fill = "red", alpha = 0.2)

grafico_fem_lei %>%
                 ggplotly()


grafico_fem_esc <- Feminino %>%
                             ggplot(mapping = aes(x = escrita))+
                             geom_histogram(aes(y = ..density..), bins = 50)+
                             geom_density(fill = "red", alpha = 0.2)

grafico_fem_esc %>%
                 ggplotly()


#Segmentando o Grupo Masculino por etnia

Auxiliar_Masc <- data.frame(chave = c(1,2,3,4,5),
                            etnia = c("group A", "group B",
                                      "group C", "group D", 
                                      "group E")) 

Masc_Segmentado <- full_join(Auxiliar_Masc, Masculino, by = "etnia")



#Grupo A

Masc_A <- Masc_Segmentado %>%
                          filter(chave == 1)

Amostra_Masc_A <- Masc_A %>%
                         sample_n(10)

#Grupo A - Matemática - Masculino

Media_Amostra_A_Masc_Mat <- mean(Amostra_Masc_A$matematica)

grafico_amostra_masc_A_mat <- Amostra_Masc_A %>%
                                         ggplot(mapping = aes(x = matematica))+
                                         geom_histogram(aes(y = ..density..), bins = 5)+
                                         geom_density(fill = "red", alpha = 0.2)

grafico_amostra_masc_A_mat %>%
                           ggplotly()


grafico_outlier_masc_A_mat <- Amostra_Masc_A %>%
                                             ggplot(mapping = aes(x = "", y = matematica))+
                                             geom_boxplot()

grafico_outlier_masc_A_mat %>%
                           ggplotly()


resultado_masc_A_mat <- t.test(x = Amostra_Masc_A$matematica,
                              alternative = "two.sided",
                              mu = 60)

#Comparar P-Valor com Alpha

resultado_masc_A_mat$p.value <= 0.05    #Sempre que a saída for falsa, rejeita-se H0.
resultado_masc_A_mat$p.value <= 0.01    #Sempre que a saída for verdadeira, não rejeita-se H0.


#Grupo A - Leitura - Masculino

Media_Amostra_A_Masc_Lei <- mean(Amostra_Masc_A$leitura)

grafico_amostra_masc_A_lei <- Amostra_Masc_A %>%
                                             ggplot(mapping = aes(x = leitura))+
                                             geom_histogram(aes(y = ..density..), bins = 5)+
                                             geom_density(fill = "red", alpha = 0.2)

grafico_amostra_masc_A_lei %>%
                           ggplotly()


grafico_outlier_masc_A_lei <- Amostra_Masc_A %>%
                                             ggplot(mapping = aes(x = "", y = leitura))+
                                             geom_boxplot()

grafico_outlier_masc_A_lei %>%
                           ggplotly()


resultado_masc_A_lei <- t.test(x = Amostra_Masc_A$leitura,
                              alternative = "two.sided",
                              mu = 60)

#Comparar P-Valor com Alpha

resultado_masc_A_lei$p.value <= 0.05    #Sempre que a saída for falsa, rejeita-se H0.
resultado_masc_A_lei$p.value <= 0.01    #Sempre que a saída for verdadeira, não rejeita-se H0.


#Grupo A - Escrita - Masculino

Media_Amostra_A_Masc_Esc <- mean(Amostra_Masc_A$escrita)

grafico_amostra_masc_A_esc <- Amostra_Masc_A %>%
                                             ggplot(mapping = aes(x = escrita))+
                                             geom_histogram(aes(y = ..density..), bins = 5)+
                                             geom_density(fill = "red", alpha = 0.2)

grafico_amostra_masc_A_esc %>%
                           ggplotly()


grafico_outlier_masc_A_esc <- Amostra_Masc_A %>%
                                             ggplot(mapping = aes(x = "", y = escrita))+
                                             geom_boxplot()

grafico_outlier_masc_A_esc %>%
                           ggplotly()


resultado_masc_A_esc <- t.test(x = Amostra_Masc_A$escrita,
                              alternative = "two.sided",
                              mu = 60)

#Comparar P-Valor com Alpha

resultado_masc_A_esc$p.value <= 0.05    #Sempre que a saída for falsa, rejeita-se H0.
resultado_masc_A_esc$p.value <= 0.01    #Sempre que a saída for verdadeira, não rejeita-se H0.



#Grupo B

Masc_B <- Masc_Segmentado %>%
                          filter(chave == 2)

Amostra_Masc_B <- Masc_B %>%
                         sample_n(10)

#Grupo B - Matemática - Masculino

Media_Amostra_B_Masc_Mat <- mean(Amostra_Masc_B$matematica)

grafico_amostra_masc_B_mat <- Amostra_Masc_B %>%
                                             ggplot(mapping = aes(x = matematica))+
                                             geom_histogram(aes(y = ..density..), bins = 5)+
                                             geom_density(fill = "red", alpha = 0.2)

grafico_amostra_masc_B_mat %>%
                           ggplotly()


grafico_outlier_masc_B_mat <- Amostra_Masc_B %>%
                                             ggplot(mapping = aes(x = "", y = matematica))+
                                             geom_boxplot()

grafico_outlier_masc_B_mat %>%
                           ggplotly()


resultado_masc_B_mat <- t.test(x = Amostra_Masc_B$matematica,
                               alternative = "two.sided",
                               mu = 60)

#Comparar P-Valor com Alpha

resultado_masc_B_mat$p.value <= 0.05    #Sempre que a saída for falsa, rejeita-se H0.
resultado_masc_B_mat$p.value <= 0.01    #Sempre que a saída for verdadeira, não rejeita-se H0.


#Grupo B - Leitura - Masculino

Media_Amostra_B_Masc_Lei <- mean(Amostra_Masc_B$leitura)

grafico_amostra_masc_B_lei <- Amostra_Masc_B %>%
                                             ggplot(mapping = aes(x = leitura))+
                                             geom_histogram(aes(y = ..density..), bins = 5)+
                                             geom_density(fill = "red", alpha = 0.2)

grafico_amostra_masc_B_lei %>%
                           ggplotly()


grafico_outlier_masc_B_lei <- Amostra_Masc_B %>%
                                             ggplot(mapping = aes(x = "", y = leitura))+
                                             geom_boxplot()

grafico_outlier_masc_B_lei %>%
                           ggplotly()


resultado_masc_B_lei <- t.test(x = Amostra_Masc_B$leitura,
                               alternative = "two.sided",
                               mu = 60)

#Comparar P-Valor com Alpha

resultado_masc_B_lei$p.value <= 0.05    #Sempre que a saída for falsa, rejeita-se H0.
resultado_masc_B_lei$p.value <= 0.01    #Sempre que a saída for verdadeira, não rejeita-se H0.


#Grupo B - Escrita - Masculino

Media_Amostra_B_Masc_Esc <- mean(Amostra_Masc_B$escrita)

grafico_amostra_masc_B_esc <- Amostra_Masc_B %>%
                                             ggplot(mapping = aes(x = escrita))+
                                             geom_histogram(aes(y = ..density..), bins = 5)+
                                             geom_density(fill = "red", alpha = 0.2)

grafico_amostra_masc_B_esc %>%
                           ggplotly()


grafico_outlier_masc_B_esc <- Amostra_Masc_B %>%
                                             ggplot(mapping = aes(x = "", y = escrita))+
                                             geom_boxplot()

grafico_outlier_masc_B_esc %>%
                           ggplotly()


resultado_masc_B_esc <- t.test(x = Amostra_Masc_B$escrita,
                               alternative = "two.sided",
                               mu = 60)

#Comparar P-Valor com Alpha

resultado_masc_B_esc$p.value <= 0.05    #Sempre que a saída for falsa, rejeita-se H0.
resultado_masc_B_esc$p.value <= 0.01    #Sempre que a saída for verdadeira, não rejeita-se H0.



#Grupo C

Masc_C <- Masc_Segmentado %>%
                          filter(chave == 3)

Amostra_Masc_C <- Masc_C %>%
                         sample_n(10)

#Grupo C - Matemática - Masculino

Media_Amostra_C_Masc_Mat <- mean(Amostra_Masc_C$matematica)

grafico_amostra_masc_C_mat <- Amostra_Masc_C %>%
                                             ggplot(mapping = aes(x = matematica))+
                                             geom_histogram(aes(y = ..density..), bins = 5)+
                                             geom_density(fill = "red", alpha = 0.2)

grafico_amostra_masc_C_mat %>%
                           ggplotly()


grafico_outlier_masc_C_mat <- Amostra_Masc_C %>%
                                             ggplot(mapping = aes(x = "", y = matematica))+
                                             geom_boxplot()

grafico_outlier_masc_C_mat %>%
                           ggplotly()


resultado_masc_C_mat <- t.test(x = Amostra_Masc_C$matematica,
                               alternative = "two.sided",
                               mu = 60)

#Comparar P-Valor com Alpha

resultado_masc_C_mat$p.value <= 0.05    #Sempre que a saída for falsa, rejeita-se H0.
resultado_masc_C_mat$p.value <= 0.01    #Sempre que a saída for verdadeira, não rejeita-se H0.



#Grupo C - Leitura - Masculino

Media_Amostra_C_Masc_Lei <- mean(Amostra_Masc_C$leitura)

grafico_amostra_masc_C_lei <- Amostra_Masc_C %>%
                                             ggplot(mapping = aes(x = leitura))+
                                             geom_histogram(aes(y = ..density..), bins = 5)+
                                             geom_density(fill = "red", alpha = 0.2)

grafico_amostra_masc_C_lei %>%
                           ggplotly()


grafico_outlier_masc_C_lei <- Amostra_Masc_C %>%
                                             ggplot(mapping = aes(x = "", y = leitura))+
                                             geom_boxplot()

grafico_outlier_masc_C_lei %>%
                           ggplotly()


resultado_masc_C_lei <- t.test(x = Amostra_Masc_C$leitura,
                               alternative = "two.sided",
                               mu = 60)

#Comparar P-Valor com Alpha

resultado_masc_C_lei$p.value <= 0.05    #Sempre que a saída for falsa, rejeita-se H0.
resultado_masc_C_lei$p.value <= 0.01    #Sempre que a saída for verdadeira, não rejeita-se H0.


#Grupo C - Escrita - Masculino

Media_Amostra_C_Masc_Esc <- mean(Amostra_Masc_C$escrita)

grafico_amostra_masc_C_esc <- Amostra_Masc_C %>%
                                             ggplot(mapping = aes(x = escrita))+
                                             geom_histogram(aes(y = ..density..), bins = 5)+
                                             geom_density(fill = "red", alpha = 0.2)

grafico_amostra_masc_C_esc %>%
                           ggplotly()


grafico_outlier_masc_C_esc <- Amostra_Masc_C %>%
                                             ggplot(mapping = aes(x = "", y = escrita))+
                                             geom_boxplot()

grafico_outlier_masc_C_esc %>%
                           ggplotly()


resultado_masc_C_esc <- t.test(x = Amostra_Masc_C$escrita,
                               alternative = "two.sided",
                               mu = 60)

#Comparar P-Valor com Alpha

resultado_masc_C_esc$p.value <= 0.05    #Sempre que a saída for falsa, rejeita-se H0.
resultado_masc_C_esc$p.value <= 0.01    #Sempre que a saída for verdadeira, não rejeita-se H0.



#Realizando o teste para cada gênero com amostra total feminina

Media_Geral_Fem_Mat <- mean(Feminino$matematica)
Media_Geral_Fem_Lei <- mean(Feminino$leitura)
Media_Geral_Fem_Esc <- mean(Feminino$escrita)


#Gráficos Femininos por Matéria

grafico_fem_mat <- Feminino %>%
                            ggplot(mapping = aes(x = matematica))+
                            geom_histogram(aes(y = ..density..), bins = 5)+
                            geom_density(fill = "red", alpha = 0.2)

grafico_fem_mat %>%
                ggplotly()



grafico_fem_lei <- Feminino %>%
                            ggplot(mapping = aes(x = leitura))+
                            geom_histogram(aes(y = ..density..), bins = 5)+
                            geom_density(fill = "red", alpha = 0.2)

grafico_fem_lei %>%
                ggplotly()


grafico_fem_esc <- Feminino %>%
                            ggplot(mapping = aes(x = escrita))+
                            geom_histogram(aes(y = ..density..), bins = 5)+
                            geom_density(fill = "red", alpha = 0.2)

grafico_fem_esc %>%
                ggplotly()
  

#Segmentando o Grupo Feminino por etnia

Auxiliar_Fem <- data.frame(chave = c(1,2,3,4,5),
                            etnia = c("group A", "group B",
                                      "group C", "group D", 
                                      "group E")) 

Fem_Segmentado <- full_join(Auxiliar_Fem, Feminino, by = "etnia")



#Grupo A

Fem_A <- Fem_Segmentado %>%
                        filter(chave == 1)

Amostra_Fem_A <- Fem_A %>%
                       sample_n(10)

#Grupo A - Matemática - Feminino

Media_Amostra_A_Fem_Mat <- mean(Amostra_Fem_A$matematica)

grafico_amostra_fem_A_mat <- Amostra_Fem_A %>%
                                           ggplot(mapping = aes(x = matematica))+
                                           geom_histogram(aes(y = ..density..), bins = 5)+
                                           geom_density(fill = "red", alpha = 0.2)

grafico_amostra_fem_A_mat %>%
                          ggplotly()


grafico_outlier_fem_A_mat <- Amostra_Fem_A %>%
                                            ggplot(mapping = aes(x = "", y = matematica))+
                                            geom_boxplot()

grafico_outlier_fem_A_mat %>%
                          ggplotly()


resultado_fem_A_mat <- t.test(x = Amostra_Fem_A$matematica,
                               alternative = "two.sided",
                               mu = 60)

#Comparar P-Valor com Alpha

resultado_fem_A_mat$p.value <= 0.05    #Sempre que a saída for falsa, rejeita-se H0.
resultado_fem_A_mat$p.value <= 0.01    #Sempre que a saída for verdadeira, não rejeita-se H0.


#Grupo A - Leitura - Feminino

Media_Amostra_A_Fem_Lei <- mean(Amostra_Fem_A$leitura)

grafico_amostra_fem_A_lei <- Amostra_Fem_A %>%
                                             ggplot(mapping = aes(x = leitura))+
                                             geom_histogram(aes(y = ..density..), bins = 5)+
                                             geom_density(fill = "red", alpha = 0.2)

grafico_amostra_fem_A_lei %>%
                          ggplotly()


grafico_outlier_fem_A_lei <- Amostra_Fem_A %>%
                                             ggplot(mapping = aes(x = "", y = leitura))+
                                             geom_boxplot()

grafico_outlier_fem_A_lei %>%
                          ggplotly()


resultado_fem_A_lei <- t.test(x = Amostra_Fem_A$leitura,
                               alternative = "two.sided",
                               mu = 60)

#Comparar P-Valor com Alpha

resultado_fem_A_lei$p.value <= 0.05    #Sempre que a saída for falsa, rejeita-se H0.
resultado_fem_A_lei$p.value <= 0.01    #Sempre que a saída for verdadeira, não rejeita-se H0.


#Grupo A - Escrita - Feminino

Media_Amostra_A_Fem_Esc <- mean(Amostra_Fem_A$escrita)

grafico_amostra_fem_A_esc <- Amostra_Fem_A %>%
                                           ggplot(mapping = aes(x = escrita))+
                                           geom_histogram(aes(y = ..density..), bins = 5)+
                                           geom_density(fill = "red", alpha = 0.2)

grafico_amostra_fem_A_esc %>%
                          ggplotly()


grafico_outlier_fem_A_esc <- Amostra_Fem_A %>%
                                           ggplot(mapping = aes(x = "", y = escrita))+
                                           geom_boxplot()

grafico_outlier_fem_A_esc %>%
                          ggplotly()


resultado_fem_A_esc <- t.test(x = Amostra_Fem_A$escrita,
                               alternative = "two.sided",
                               mu = 60)

#Comparar P-Valor com Alpha

resultado_fem_A_esc$p.value <= 0.05    #Sempre que a saída for falsa, rejeita-se H0.
resultado_fem_A_esc$p.value <= 0.01    #Sempre que a saída for verdadeira, não rejeita-se H0.



#Grupo B

Fem_B <- Fem_Segmentado %>%
                        filter(chave == 2)

Amostra_Fem_B <- Fem_B %>%
                         sample_n(10)

#Grupo B - Matemática - Feminino

Media_Amostra_B_Fem_Mat <- mean(Amostra_Fem_B$matematica)

grafico_amostra_fem_B_mat <- Amostra_Fem_B %>%
                                           ggplot(mapping = aes(x = matematica))+
                                           geom_histogram(aes(y = ..density..), bins = 5)+
                                           geom_density(fill = "red", alpha = 0.2)

grafico_amostra_fem_B_mat %>%
                          ggplotly()


grafico_outlier_fem_B_mat <- Amostra_Fem_B %>%
                                           ggplot(mapping = aes(x = "", y = matematica))+
                                           geom_boxplot()

grafico_outlier_fem_B_mat %>%
                          ggplotly()


resultado_fem_B_mat <- t.test(x = Amostra_Fem_B$matematica,
                               alternative = "two.sided",
                               mu = 60)

#Comparar P-Valor com Alpha

resultado_fem_B_mat$p.value <= 0.05    #Sempre que a saída for falsa, rejeita-se H0.
resultado_fem_B_mat$p.value <= 0.01    #Sempre que a saída for verdadeira, não rejeita-se H0.


#Grupo B - Leitura - Feminino

Media_Amostra_B_Fem_Lei <- mean(Amostra_Fem_B$leitura)

grafico_amostra_fem_B_lei <- Amostra_Fem_B %>%
                                           ggplot(mapping = aes(x = leitura))+
                                           geom_histogram(aes(y = ..density..), bins = 5)+
                                           geom_density(fill = "red", alpha = 0.2)

grafico_amostra_fem_B_lei %>%
                          ggplotly()


grafico_outlier_fem_B_lei <- Amostra_Fem_B %>%
                                           ggplot(mapping = aes(x = "", y = leitura))+
                                           geom_boxplot()

grafico_outlier_fem_B_lei %>%
  ggplotly()


resultado_fem_B_lei <- t.test(x = Amostra_Fem_B$leitura,
                               alternative = "two.sided",
                               mu = 60)

#Comparar P-Valor com Alpha

resultado_fem_B_lei$p.value <= 0.05    #Sempre que a saída for falsa, rejeita-se H0.
resultado_fem_B_lei$p.value <= 0.01    #Sempre que a saída for verdadeira, não rejeita-se H0.


#Grupo B - Escrita - Feminino

Media_Amostra_B_Fem_Esc <- mean(Amostra_Fem_B$escrita)

grafico_amostra_fem_B_esc <- Amostra_Fem_B %>%
                                           ggplot(mapping = aes(x = escrita))+
                                           geom_histogram(aes(y = ..density..), bins = 5)+
                                           geom_density(fill = "red", alpha = 0.2)

grafico_amostra_fem_B_esc %>%
                          ggplotly()


grafico_outlier_fem_B_esc <- Amostra_Fem_B %>%
                                           ggplot(mapping = aes(x = "", y = escrita))+
                                           geom_boxplot()

grafico_outlier_fem_B_esc %>%
                          ggplotly()


resultado_fem_B_esc <- t.test(x = Amostra_Fem_B$escrita,
                               alternative = "two.sided",
                               mu = 60)

#Comparar P-Valor com Alpha

resultado_fem_B_esc$p.value <= 0.05    #Sempre que a saída for falsa, rejeita-se H0.
resultado_fem_B_esc$p.value <= 0.01    #Sempre que a saída for verdadeira, não rejeita-se H0.



#Grupo C

Fem_C <- Fem_Segmentado %>%
                        filter(chave == 3)

Amostra_Fem_C <- Fem_C %>%
                       sample_n(10)

#Grupo C - Matemática - Feminino

Media_Amostra_C_Fem_Mat <- mean(Amostra_Fem_C$matematica)

grafico_amostra_fem_C_mat <- Amostra_Fem_C %>%
                                          ggplot(mapping = aes(x = matematica))+
                                          geom_histogram(aes(y = ..density..), bins = 5)+
                                          geom_density(fill = "red", alpha = 0.2)

grafico_amostra_fem_C_mat %>%
                          ggplotly()


grafico_outlier_fem_C_mat <- Amostra_Fem_C %>%
                                             ggplot(mapping = aes(x = "", y = matematica))+
                                             geom_boxplot()

grafico_outlier_fem_C_mat %>%
                          ggplotly()


resultado_fem_C_mat <- t.test(x = Amostra_Fem_C$matematica,
                               alternative = "two.sided",
                               mu = 60)

#Comparar P-Valor com Alpha

resultado_fem_C_mat$p.value <= 0.05    #Sempre que a saída for falsa, rejeita-se H0.
resultado_fem_C_mat$p.value <= 0.01    #Sempre que a saída for verdadeira, não rejeita-se H0.



#Grupo C - Leitura - Feminino

Media_Amostra_C_Fem_Lei <- mean(Amostra_Fem_C$leitura)

grafico_amostra_fem_C_lei <- Amostra_Fem_C %>%
                                           ggplot(mapping = aes(x = leitura))+
                                           geom_histogram(aes(y = ..density..), bins = 5)+
                                           geom_density(fill = "red", alpha = 0.2)

grafico_amostra_fem_C_lei %>%
                          ggplotly()


grafico_outlier_fem_C_lei <- Amostra_Fem_C %>%
                                           ggplot(mapping = aes(x = "", y = leitura))+
                                           geom_boxplot()

grafico_outlier_fem_C_lei %>%
                          ggplotly()


resultado_fem_C_lei <- t.test(x = Amostra_Fem_C$leitura,
                               alternative = "two.sided",
                               mu = 60)

#Comparar P-Valor com Alpha

resultado_fem_C_lei$p.value <= 0.05    #Sempre que a saída for falsa, rejeita-se H0.
resultado_fem_C_lei$p.value <= 0.01    #Sempre que a saída for verdadeira, não rejeita-se H0.


#Grupo C - Escrita - Feminino

Media_Amostra_C_Fem_Esc <- mean(Amostra_Fem_C$escrita)

grafico_amostra_fem_C_esc <- Amostra_Fem_C %>%
                                           ggplot(mapping = aes(x = escrita))+
                                           geom_histogram(aes(y = ..density..), bins = 5)+
                                           geom_density(fill = "red", alpha = 0.2)

grafico_amostra_fem_C_esc %>%
                          ggplotly()


grafico_outlier_fem_C_esc <- Amostra_fem_C %>%
                                           ggplot(mapping = aes(x = "", y = escrita))+
                                           geom_boxplot()

grafico_outlier_fem_C_esc %>%
                          ggplotly()


resultado_fem_C_esc <- t.test(x = Amostra_Fem_C$escrita,
                               alternative = "two.sided",
                               mu = 60)

#Comparar P-Valor com Alpha

resultado_fem_C_esc$p.value <= 0.05    #Sempre que a saída for falsa, rejeita-se H0.
resultado_fem_C_esc$p.value <= 0.01    #Sempre que a saída for verdadeira, não rejeita-se H0.
