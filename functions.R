# Funcao Moda
mode <- function(x) {
  unique_vals <- unique(x)
  counts <- tabulate(match(x, unique_vals))
  max_count <- max(counts)
  modes <- unique_vals[counts == max_count]
  
  return(mean(modes))
}


#Remove linhas NULL ou NA, e converte os valores para numeros
formata_lista <- function(lista){
  lista <- subset(lista, !is.na(lista) & lista != "NULL")
  lista <- gsub(",", ".", lista)
  lista <- as.numeric(lista)
  return(lista)
}


#Retorna medidas descritivas de uma lista de dados
medidas_descritivas <- function(lista, limiteamostral=250000){
  lista <- formata_lista(lista)

  if (length(lista) > limiteamostral){
    lista <- sample(lista, limiteamostral, replace=TRUE)
  }
  
  media <- mean(lista)
  moda <- mode(lista)
  variancia <- var(lista)
  desvpad <- sd(lista)
  n <- length(lista)
  minimo <- min(lista)
  q1 <- as.numeric(quantile(lista, 0.25))
  mediana <- median(lista)
  q3 <-as.numeric(quantile(lista, 0.75))
  maximo <- max(lista)

  return(c(media, moda, variancia, desvpad, n, minimo, q1, mediana, q3, maximo))
}


#Gera um dataframe com as medidas descritivas de todos os subsets definidos pela lista "iteradores"
gera_resultados <- function(lista_dados, iteradores,limiteamostral=250000){
  #Cria dataframe de resultados
  Resultados <- data.frame(
      Disciplina =NA,
      Media =NA,
      Moda =NA,
      Variancia =NA,
      Desvpad =NA,
      NumeroAmostras =NA, 
      Minimo =NA, 
      Q1 =NA, 
      Mediana =NA, 
      Q3 =NA, 
      Maximo =NA, 
      NumeroSubBasico =NA, 
      NumeroBasico =NA, 
      NumeroAdequado =NA, 
      NumeroAvancado =NA
      )[numeric(0), ]

  #Cria colunas no df Resultados
  for (iter in seq_along(iteradores)){
    Resultados[[iteradores[iter]]] <- vector(length = 0)
  }

  subsets <- list()
  print("Separando dados...")
  #Separa os dados baseado na lista de iteradores
  for (d in seq_along(lista_dataframes_educacao)){
      data <- lista_dataframes_educacao[[d]]
      subsets <- append(subsets, split(data, do.call(interaction, data[iteradores])))
  }
  subsets <- Filter(function(df) nrow(df) > 0, subsets)

  
  print("Calculando medidas descritivas...")
  for (i in seq_along(subsets)){
      df <- subsets[[i]]
      informacoes <- c()
      for (iter in iteradores){
          informacoes <- c(informacoes, df[[iter]][1])
      }

      print(informacoes)
      
      informacoes_lp <- c("Lingua Portuguesa", medidas_descritivas(df$profic_lp,limiteamostral), 
          as.vector(table(df$nivel_profic_lp)[1:4]), informacoes)
      informacoes_mat <- c("Matematica", medidas_descritivas(df$profic_mat,limiteamostral), 
          as.vector(table(df$nivel_profic_mat)[1:4]), informacoes)
      print(length(informacoes_lp))
      Resultados <- rbind(Resultados, informacoes_lp)
      Resultados <- rbind(Resultados, informacoes_mat)
  }

  print("Tratando dados...")
  #Esse trecho de código existe puramente pq a formatação das colunas se perde no meio do for loop. Não gosto dessa solução mas é o que tem pra hoje
  colnames(Resultados) <- c(c("Disciplina", "Media", "Moda", "Variancia", "Desvpad",
    "NumeroAmostras", "Minimo", "Q1", "Mediana", "Q3", "Maximo", "NumeroSubBasico",
    "NumeroBasico", "NumeroAdequado", "NumeroAvancado"), iteradores)
  Resultados <- Resultados %>%
    mutate(across(all_of(c("Media","Desvpad","NumeroAmostras","NumeroSubBasico")), as.numeric))
  
  #Como os caras do banco de dados não conseguem ser consistentes, esse trecho de código tira o til da palavra manhã
  #Esse código está meio redundante com a variável x, mas eu estou a tanto tempo tentando fazer ele dar certo que tenho medo de mexer em qualquer coisa
  if ("PERIODO" %in% iteradores){
    for (i in 1:nrow(Resultados)){
      x <- Resultados$PERIODO[i]
      x <- iconv(x, from="UTF-8", to="ASCII//TRANSLIT")
      Resultados$PERIODO[i] <- x
    }
    
  }
  
  print("Resultados calculados!")
  return(Resultados)
}


compara_medias <- function(media1, dp1, n1, media2, dp2, n2, alfa = 0.05) {
  # Calcular os vetores de dados das amostras
  x1 <- rnorm(n1, mean = media1, sd = dp1)
  x2 <- rnorm(n2, mean = media2, sd = dp2)
  
  # Executar o teste de médias usando t.test()
  resultado <- t.test(x1, x2, alternative = "two.sided", conf.level = 1 - alfa)
  
  # Verificar o valor-p
  p_valor <- resultado$p.value
  
  # Comparar as médias e retornar o resultado
  if (p_valor < alfa) {
    if (mean(x1) > mean(x2)) {
      return("Maior")
    } else if (mean(x1) < mean(x2)) {
      return("Menor")
    }
  }
  
  return("Igual")
}


comparar_proporcoes <- function(sucessos1, n1, sucessos2, n2, alfa = 0.05) {
  resultado <- prop.test(c(sucessos1, sucessos2), c(n1, n2), alternative = "two.sided", conf.level = 1 - alfa)
  
  # Verificar o valor-p
  p_valor <- resultado$p.value
  
  # Comparar as proporções e retornar o resultado
  if (p_valor < alfa) {
    if (sucessos1 / n1 > sucessos2 / n2) {
      return("Maior")
    } else if (sucessos1 / n1 < sucessos2 / n2) {
      return("Menor")
    }
  }
  
  return("Igual")
}


#Compara médias dos resultados em duas categorias de uma mesma coluna (ex: Regiao SP e Interior)
compara_media_categorias <- function(dfresultados, coluna, categoria1, categoria2, iteradores){
  iteradores <- c(setdiff(iteradores, coluna), "Disciplina")
  
  dfresultados <- subset(dfresultados, dfresultados[coluna] == categoria1 | dfresultados[coluna] == categoria2)
  dfresultados <- dfresultados[,-c(3,4,7,8,9,10,11,12,13,14,15)]
  
  listadf <- split(dfresultados, dfresultados[coluna])
  listadf <- lapply(listadf, function(x) { x[coluna] <- NULL;x})
  
  comparador <- na.omit(left_join(listadf[[1]], listadf[[2]], by=iteradores))
  
  comparador$Maior_media <- "A decidir"
  for (i in 1:nrow(comparador)){
    print(i)
    teste <- compara_medias(comparador[i,"Media.x"],comparador[i,"Desvpad.x"],comparador[i,"NumeroAmostras.x"], 
                                                 comparador[i,"Media.y"],comparador[i,"Desvpad.y"],comparador[i,"NumeroAmostras.y"])
    if (teste == "Maior"){
      comparador$Maior_media[i] <- categoria1
    } else if (teste == "Menor"){
      comparador$Maior_media[i] <- categoria2
    } else{
      comparador$Maior_media[i] <- "Igual"
    }

  }
  return(comparador)
}


#Compara proporções de alunos na categoria "abaixo do básico"
compara_proporcao_categorias <- function(dfresultados, coluna, categoria1, categoria2, iteradores){
  iteradores <- c(setdiff(iteradores, coluna), "Disciplina")
  
  dfresultados <- subset(dfresultados, dfresultados[coluna] == categoria1 | dfresultados[coluna] == categoria2)
  dfresultados <- dfresultados[,-c(2,3,4,7,8,9,10,11,13,14,15)]
  
  listadf <- split(dfresultados, dfresultados[coluna])
  listadf <- lapply(listadf, function(x) { x[coluna] <- NULL;x})
  
  comparador <- na.omit(left_join(listadf[[1]], listadf[[2]], by=iteradores))
  
  comparador$Maior_proporcao <- "A decidir"
  for (i in 1:nrow(comparador)){
    teste <- comparar_proporcoes(comparador[i,"Media.x"],comparador[i,"NumeroAmostras.x"], 
                                                comparador[i,"Media.y"],comparador[i,"NumeroAmostras.y"])
    if (teste == "Maior"){
      comparador$Maior_proporcao[i] <- categoria1
    } else if (teste == "Menor"){
      comparador$Maior_proporcao[i] <- categoria2
    } else{
      comparador$Maior_proporcao[i] <- "Igual"
    }
  }
  return(comparador)
}