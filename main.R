#Carrega funcoes de outros scripts.
library(dplyr)
library(ggplot2)
source("funcoes.R")
source("carrega_dados.R")
lista_dataframes_educacao <- carrega_todos_dados_educacao()


iteradores <- c("Ano_Estudo", "SERIE", "PERIODO")
resultados <- gera_resultados(lista_dataframes_educacao, iteradores)


#TRECHO DE CÓDIGO PARA REGRESSÃO LINEAR
#tratamento de dados
invest <- carrega_investimentos_educacao()

iteradoresreg <- c("Ano_Estudo","MUN")
resultadosreg <- gera_resultados(lista_dataframes_educacao, iteradoresreg)

colnames(resultadosreg)[17] <- "Municipio"
colnames(resultadosreg)[16] <- "Ano"
resultadosreg$Ano <- as.numeric(resultadosreg$Ano)


notas <- aggregate(cbind(Valor, Media, NumeroAmostras) ~ Ano + Municipio, 
                     data = merge(invest, resultadosreg, by = c("Ano", "Municipio")), 
                     FUN = function(x) c(mean(x), length(x)))
colnames(notas)[4] <- "NotaMedia"

notas$Valor <- notas$Valor[,1]
notas$NumeroAmostras <- notas$NumeroAmostras[,1]
notas$NotaMedia <- notas$NotaMedia[,1]

notas$ValorMedio <- 0
notas$ValorMedio <- notas$Valor / notas$NumeroAmostras
notas$ValorMedio <- as.numeric(notas$ValorMedio)


#Plot do modelo
Modelo1 <- lm(NotaMedia~ValorMedio, data=notas)
summary(Modelo1)
ggplot(notas, aes(x=ValorMedio, y=NotaMedia)) + geom_point(size=1) +
  geom_smooth(method=lm, linetype="solid", color="blue", se=FALSE)
