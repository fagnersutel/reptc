#Definimos local do arquivo como diretório de trabalho
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#verificamos o WD
getwd()
#listamos os arquivos
list.files()
#biblioteca para ler DBF
library(foreign)
#carga de dados
acidentes = read.dbf("acidentes.dbf")
#dimensionalidade dos dados
dim(acidentes)
#carga de dados
acidentes = read.csv("acidentes2019.csv", header = T, sep = ";" )
dim(acidentes)
#carga de dados
acidentes = read.csv("acidentes2019.csv", header = T, sep = "," )
dim(acidentes)
head(acidentes)
tail(acidentes)

table(acidentes$TEMPO)

table(acidentes$TIPO_ACID)

table(acidentes$TIPO_ACID, acidentes$TEMPO)

table(acidentes$TIPO_ACID, acidentes$NOITE_DIA)

prop.table(table(acidentes$TIPO_ACID, acidentes$NOITE_DIA))

prop.table(table(acidentes$TIPO_ACID, acidentes$NOITE_DIA)) %>% round(2)

prop.table(table(acidentes$TIPO_ACID, acidentes$NOITE_DIA)) %>% {. * 100} %>% 
  round(2)

table(acidentes$TIPO_ACID, acidentes$NOITE_DIA, acidentes$TEMPO) 
library(dplyr)
tabela = prop.table(table(acidentes$TIPO_ACID, acidentes$NOITE_DIA)) %>% {. * 100} %>% 
  round(2)

df = as.data.frame(tabela)
df

df = as.data.frame.matrix(tabela) 
df

df$class = rownames(df)
plot(df$DIA, df$NOITE, col=df$class)


library(ggplot2)

qplot(
  x = DIA,
  y = NOITE,
  data = df,
  color = df$class # color by factor color (I know, confusing)
)




prop.table(table(acidentes$TIPO_ACID)) %>% round(2)
acid = table(acidentes$TIPO_ACID)
#acid = as.data.frame.matrix(acid) 
dim(acid)
acidentes2 = acid
acidentes2
 
colors <- rainbow(length(acidentes2))
 
acidentes_labels <- round(acidentes2/sum(acidentes2) * 100, 1)

acidentes_labels <- paste(acidentes_labels, "%", sep="")

pie(acidentes2, main="Acidentes", col=colors, labels=acidentes_labels,
    cex=0.8)

legend(1.1, 0.5, names(acid), cex=0.8, 
       fill=colors)

df = as.data.frame.matrix(table(acidentes$TIPO_ACID, acidentes$FX_HORA)) 
df

#Plotagem de abalroamento
plot(names(df), df[1,], type="b", pch=19, col="red", xlab="x", ylab="y")
# adicionamos a linha de atropelamentos
lines(names(df), df[2,], pch=13, col="blue", type="b", lty=4)
# adicionamos a linha de colisão
lines(names(df), df[5,], pch=15, col="green", type="b", lty=4)
# adicionamos a linha de choque
lines(names(df), df[4,], pch=10, col="black", type="b", lty=4)
# adicionamos a linha de queda
lines(names(df), df[8,], pch=3, col="pink", type="b", lty=4)
# adicionamenos a legenda
legend(0.1, 285, legend=c("Abalroamento", "Atropelamento", "Colisão", "Choque", "Queda"),
       col=c("red", "blue", "green", "black", "pink"), lty=1:3, cex=0.8)
