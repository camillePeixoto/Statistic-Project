#Carrega 10 anos de dados da SARESP
#Os dados de serie nao estao regularizados 2011 pra tras(pelo menos o padrao mudou desde essa epoca, se soubermos como era antes vai ser possivel usar esses dados)
carrega_todos_dados_educacao <- function(){
  print("Carregando dados...")
  cat("2022...","\n")
  df2022 <- read.csv("MICRODADOS_SARESP_2022_0.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
  cat("2021...","\n")
  df2021 <- read.csv("MICRODADOS_SARESP_2021.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
  cat("2019...","\n")
  df2019 <- read.csv("MICRODADOS_SARESP_2019.csv",   sep = ";", header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
  cat("2018...","\n")
  df2018 <- read.csv("MICRODADOS_SARESP_2018.csv",   sep = ";", header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
  cat("2017...","\n")
  df2017 <- read.csv("MICRODADOS_SARESP_2017.csv",   sep = ";", header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
  cat("2016...","\n")
  df2016 <- read.csv("MICRODADOS_SARESP_2016.csv",   sep = ";", header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
  cat("2015...","\n")
  df2015 <- read.csv("MICRODADOS_SARESP_2015.csv",   sep = ";", header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
  cat("2014...","\n")
  df2014 <- read.csv("MICRODADOS_SARESP_2014.csv",   sep = ";", header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
  cat("2013...","\n")
  df2013 <- read.csv("MICRODADOS_SARESP_2013.csv",   sep = ";", header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
  #cat("2012...","\n")
  #df2012 <- read.csv("MICRODADOS_SARESP_2012.csv",   sep = ";", header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")

  print("Tratando dados...")
  #Altera alguns nomes de colunas para padronizacao
  colnames(df2021)[45] <- "VALIDADE"
  colnames(df2022)[45] <- "VALIDADE"
  colnames(df2022)[11] <- "SERIE"
  colnames(df2021)[11] <- "SERIE"

  #Remove dos dados da prova de ciencia
  df2022 <- df2022[,-c(26, 29, 32, 35, 38, 41, 44)]
  df2021 <- df2021[,-c(26, 29, 32, 35, 38, 41, 44)]
  
  #Adiciona da coluna "Ano"
  df2022$Ano_Estudo <- 2022
  df2021$Ano_Estudo <- 2021
  df2019$Ano_Estudo <- 2019
  df2018$Ano_Estudo <- 2018
  df2017$Ano_Estudo <- 2017
  df2016$Ano_Estudo <- 2016
  df2015$Ano_Estudo <- 2015
  df2014$Ano_Estudo <- 2014
  df2013$Ano_Estudo <- 2013
  #df2012$Ano_Estudo <- 2012

  df2022 <- subset(df2022, df2022$VALIDADE != 0)
  df2021 <- subset(df2021, df2021$VALIDADE != 0)
  df2019 <- subset(df2019, df2019$VALIDADE != 0)
  df2018 <- subset(df2018, df2018$VALIDADE != 0)
  df2017 <- subset(df2017, df2017$VALIDADE != 0)
  df2016 <- subset(df2016, df2016$VALIDADE != 0)
  df2015 <- subset(df2015, df2015$VALIDADE != 0)
  df2014 <- subset(df2014, df2014$VALIDADE != 0)
  df2013 <- subset(df2013, df2013$VALIDADE != 0)
  #df2012 <- subset(df2012, df2012$VALIDADE != 0)
  listadf <-  list(df2022, df2021, df2019, df2018, df2017, df2016, df2015, df2014, df2013)
  print("Dados Carregados!")
  return(listadf)
}


#Carrega somente 4 anos de dados da SARESP. Implementado para agilizar testes.
carrega_poucos_dados_educacao <- function(){
  print("Carregando dados...")
  cat("2022...","\n")
  df2022 <- read.csv("MICRODADOS_SARESP_2022_0.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
  cat("2021...","\n")
  df2021 <- read.csv("MICRODADOS_SARESP_2021.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
  cat("2019...","\n")
  df2019 <- read.csv("MICRODADOS_SARESP_2019.csv",   sep = ";", header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
  cat("2018...","\n")
  df2018 <- read.csv("MICRODADOS_SARESP_2018.csv",   sep = ";", header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")

  print("Tratando dados...")
  #Altera alguns nomes de colunas para padronizacao
  colnames(df2021)[45] <- "VALIDADE"
  colnames(df2022)[45] <- "VALIDADE"
  colnames(df2022)[11] <- "SERIE"
  colnames(df2021)[11] <- "SERIE"

  #Remove dos dados da prova de ciencia
  df2022 <- df2022[,-c(26, 29, 32, 35, 38, 41, 44)]
  df2021 <- df2021[,-c(26, 29, 32, 35, 38, 41, 44)]

  #Adiciona da coluna "Ano"
  df2022$Ano_Estudo <- 2022
  df2021$Ano_Estudo <- 2021
  df2019$Ano_Estudo <- 2019
  df2018$Ano_Estudo <- 2018

  df2022 <- subset(df2022, df2022$VALIDADE != 0)
  df2021 <- subset(df2021, df2021$VALIDADE != 0)
  df2019 <- subset(df2019, df2019$VALIDADE != 0)
  df2018 <- subset(df2018, df2018$VALIDADE != 0)
  listadf <- list(df2022, df2021, df2019, df2018)
  print("Dados carregados!")
  return(listadf)
}


#Carrega somente um ano de dados. Ideal para testar funcoes que so dependam de uma lista de dados
carrega_2022_educacao <- function(){
  df2022 <- read.csv("MICRODADOS_SARESP_2022_0.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
  colnames(df2022)[45] <- "VALIDADE"
  colnames(df2022)[11] <- "SERIE"
  df2022 <- df2022[,-c(26, 29, 32, 35, 38, 41, 44)]
  df2022$Ano_Estudo <- 2022
  df2022 <- subset(df2022, df2022$VALIDADE != 0)
  return(df2022)
}


carrega_lista_2022_educacao <- function(){
  df2022 <- read.csv("MICRODADOS_SARESP_2022_0.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
  colnames(df2022)[45] <- "VALIDADE"
  colnames(df2022)[11] <- "SERIE"
  df2022 <- df2022[,-c(26, 29, 32, 35, 38, 41, 44)]
  df2022$Ano_Estudo <- 2022
  df2022 <- subset(df2022, df2022$VALIDADE != 0)
  return(list(df2022))
}


carrega_investimentos_educacao <- function(){
  df <- read.csv("transferencias_para_municipios.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)
  df <- df[, -c(1,6,7)]
  colnames(df) <- c("Municipio","Ano","Transferencia","Valor")
  
  df$Ano <- as.numeric(df$Ano)
  df$Valor <- gsub("[R$\\.]", "", df$Valor)
  df$Valor <- gsub(",", ".", df$Valor)
  df$Valor <- as.numeric(df$Valor)
  
  for (i in 1:nrow(df)){
    x <- df$Municipio[i]
    x <- iconv(x, from="UTF-8", to="ASCII//TRANSLIT")
    x <- toupper(x)
    df$Municipio[i] <- x
  }

  ValorTotal <- aggregate(Valor ~ Ano + Municipio, data=df, FUN = sum)
  cat("Dados de investimento carregados!","\n")
  return(ValorTotal)
}
