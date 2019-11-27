#Definimos local do arquivo como diretório de trabalho
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#verificamos o WD
getwd()
#carregamos as bibliotecas
library(jsonlite)
library(dplyr)
library(tidyr)
library(sp)
library(mapview)
library(rgeos)
#Verificamos os arquivos existentes
list.files()
arquivos = list.files(pattern = 'wazej')
arquivos
dados = jsonlite::fromJSON(arquivos[1])
#Verificamos os dados de fila
head(dados$jams)
#Verificamos os dados de alertas
head(dados$alerts)
# Capturamos a data pelo nome do arquivo
data = paste(substr(arquivos[1], 14,17), substr(arquivos[1], 12,13), substr(arquivos[1], 10,11), sep = "-")
data
#Definimos o intervalo horário
inicio =  paste(data, "05:00:00", sep = " ")
fim =  paste(data, "09:00:00", sep = " ")
# verificamos o tamanho do conjunto de dados
length(dados)
#visualizamos o dataset
View(dados)
# testamos a conversão de milesegundos para data
as.POSIXct(Sys.Date())+dados$jams$pubMillis/1000
# criamosa função de conversão
ms_to_date = function(ms, t0="1970-01-01", timezone) {
  sec = ms / 1000
  as.POSIXct(sec, origin=t0, tz=timezone)
}
# realizamosa conversão de datas
dados$jams$pubMillis = ms_to_date(dados$jams$pubMillis, timezone="America/Sao_Paulo")
head(dados$jams)
# tornamos a matriz em data frame
congestionamentos = as.data.frame(dados$jams)
head(congestionamentos)
#verificamos o tamanho do conjunto de dados
dim(congestionamentos)
# fazemos um subset
congestionamentos = congestionamentos[congestionamentos$pubMillis > inicio,]
congestionamentos = congestionamentos[congestionamentos$pubMillis < fim,]
#verificamos o tamanho do conjunto de dadps
dim(congestionamentos)
# eliminamos a colina espúria
congestionamentos$startNode = NULL

#Criamos uma matriz a ser povoada com os dados de filas
waze = data.frame(matrix(, nrow = 0, ncol = 18))
dim(waze)

for (variable in arquivos) {
  print(variable)
}

#iniciamos um loop para cada arquivo
for (variable in arquivos) {
  # Capturamos a data pelo nome do arquivo
  data = paste(substr(arquivos, 14,17), substr(arquivos, 12,13), substr(arquivos, 10,11), sep = "-")
  data
  inicio =  paste(data, "05:00:00", sep = " ")
  fim =  paste(data, "09:00:00", sep = " ")
  dados = jsonlite::fromJSON(variable)
  dados
  length(dados)
  as.POSIXct(Sys.Date())+dados$jams$pubMillis/1000
  ms_to_date = function(ms, t0="1970-01-01", timezone) {
    sec = ms / 1000
    as.POSIXct(sec, origin=t0, tz=timezone)
  }
  
  dados$jams$pubMillis = ms_to_date(dados$jams$pubMillis, timezone="America/Sao_Paulo")
  congestionamentos = as.data.frame(dados$jams)
  head(congestionamentos$pubMillis)
  
  dim(congestionamentos)
  congestionamentos = congestionamentos[congestionamentos$pubMillis > inicio,]
  congestionamentos = congestionamentos[congestionamentos$pubMillis < fim,]
  dim(congestionamentos)
  congestionamentos$startNode = NULL
  waze = rbind(waze, congestionamentos)  
}
head(waze)
congestionamentos = subset(waze,city %in% c('Porto Alegre'))


registros_manha = unique(congestionamentos$uuid)
tamanho_manha = length(congestionamentos$city)
tamanho_manha

lista = list()
nomes_linhas = list()

for (i in 1:length(congestionamentos$country)) {
  nomes_linhas[i] = i
  teste = congestionamentos$line[i]
  teste = as.data.frame(teste)
  l1 = cbind(c(teste$x),c(teste$y))
  #print(l1)
  if (congestionamentos$street =="Av. Independência - Pista Dir.") {
    l1$x = (l1$x * 1.000001)
  }
  Sl1 = Line(l1)
  S1 = Lines(list(Sl1), ID=i) # fazer umamulti-Line
  lista[i] = S1
}


Sl = SpatialLines(lista)
proj4string(Sl) <- CRS("+init=epsg:4326")
row.names(congestionamentos) = row.names(Sl)
SlDF <- SpatialLinesDataFrame(Sl, data = congestionamentos %>% select(-c("line", "segments") )  )
paletta = colorRampPalette(c("#ff0000","#cc0099", "orange", "yellow", "green", "blue"), space="Lab" )

mapview(SlDF, zcol= 'speedKMH', color = paletta, layer.name = 'Velocidade Média', lwd=4,
        legend = TRUE, map.types=c("Esri.WorldImagery", "CartoDB.DarkMatter", "OpenStreetMap", "Esri.WorldStreetMap"))


frame = as.data.frame(congestionamentos)

frame = frame[, c("street", "speedKMH", "length", "speed", "delay", "pubMillis", "pubMillis")]

frame$velocidadeMediaEstimada = (congestionamentos$length / ((congestionamentos$length/congestionamentos$speed)-congestionamentos$delay) ) * 3.6

names(frame) = c("Via", "Velocidade_Verificada_Km_h", "Fila_em_metros","Vel. verificada m/s", "Atraso (segundos)", "Data",  "Hora", "Velocidade Media Estimada*")

frame$Hora = as.character(substr(frame$Data, 12,20))

frame = frame[frame$Data> "2019-09-01 00:00:00", ]

frame_atual_manha = frame

frame_atual_manha$pico = "manha"

frame_atual_manha$fase = "atual"

head(frame_atual_manha)

df <- data.frame(periodo=frame_atual_manha$Via,
                 velocidade=frame_atual_manha$Velocidade_Verificada_Km_h,
                 local=frame_atual_manha$`Velocidade Media Estimada*`,
                 fila=frame_atual_manha$Fila_em_metros,
                 atraso=frame_atual_manha$`Atraso (segundos)`)

df
df$velocidade = round(df$velocidade, digits=2)
df$local = round(df$local, digits=2)
df<- df[seq(dim(df)[1],1),]

library(ggplot2)

ggplot(df, aes(x=periodo, y=velocidade, fill = periodo)) +
  geom_bar(aes(fill = periodo), position = position_dodge(), stat = "identity",  na.rm=TRUE, colour = "black") +
  facet_wrap(~periodo) +
  coord_flip() + theme(legend.position = "top") +
  geom_text(data = df, 
            aes(x=periodo, y=velocidade, label = velocidade), 
            hjust = 2.5,
            color = "white",
            fontface = "bold") +
  guides(fill=FALSE) +
  #scale_fill_manual(values = c("#82b74b", "#3e4444", "#405d27"), name = "Períodos") + 
  theme_classic() + 
  facet_grid(local~., scales = "free", space = "free") +
  labs(title = "Velocidades Médias - pico da manhã:",
       subtitle = "Av. Independência e região de influência",
       caption = paste("Fonte: Waze CCP (Atualização ", datafim, ")",sep = ""),
       x = "Período", y = "Velocidade média km/h",
       tag = "Gráfico 1")

df = df[unique(df$periodo), ]
df = df[1:6,]
ggplot(df, aes(x=periodo, y=velocidade, fill = periodo)) +
  geom_bar(aes(fill = periodo), position = position_dodge(), stat = "identity",  na.rm=TRUE, colour = "black") +
  facet_wrap(~periodo) +
  coord_flip() + theme(legend.position = "top") +
  geom_text(data = df, 
            aes(x=periodo, y=velocidade, label = velocidade), 
            hjust = 2.5,
            color = "white",
            fontface = "bold") +
  guides(fill=FALSE) +
  scale_fill_manual(values = c("#6b5b95", "#feb236", "#d64161", "#ff7b25", "#878f99", "#c94c4c"), name = "Local") + 
  theme_classic() + 
  facet_grid(local~., scales = "free", space = "free") +
  labs(title = "Velocidades Médias - pico da manhã:",
       subtitle = "Av. Independência e região de influência",
       caption = paste("Fonte: Waze CCP (Atualização ", datafim, ")",sep = ""),
       x = "Local", y = "Velocidade média km/h",
       tag = "Gráfico 1")


ggplot(df, aes(x=periodo, y=atraso, fill = periodo)) +
  geom_bar(aes(fill = periodo), position = position_dodge(), stat = "identity",  na.rm=TRUE, colour = "black") +
  facet_wrap(~periodo) +
  coord_flip() + theme(legend.position = "top") +
  geom_text(data = df, 
            aes(x=periodo, y=atraso, label = atraso), 
            hjust = 2.5,
            color = "white",
            fontface = "bold") +
  guides(fill=FALSE) +
  scale_fill_manual(values = c("#6b5b95", "#feb236", "#d64161", "#ff7b25", "#878f99", "#c94c4c"), name = "Local") + 
  theme_classic() + 
  facet_grid(local~., scales = "free", space = "free") +
  labs(title = "Velocidades Médias no congestionamento - pico da manhã:",
       subtitle = "Av. Independência e região de influência",
       caption = paste("Fonte: Waze CCP (Atualização ", datafim, ")",sep = ""),
       x = "Local", y = "Atraso s",
       tag = "Gráfico 2")



ggplot(df, aes(x=periodo, y=local, fill = periodo)) +
  geom_bar(aes(fill = periodo), position = position_dodge(), stat = "identity",  na.rm=TRUE, colour = "black") +
  facet_wrap(~periodo) +
  coord_flip() + theme(legend.position = "top") +
  geom_text(data = df, 
            aes(x=periodo, y=local, label = local), 
            hjust = 2.5,
            color = "white",
            fontface = "bold") +
  guides(fill=FALSE) +
  scale_fill_manual(values = c("#6b5b95", "#feb236", "#d64161", "#ff7b25", "#878f99", "#c94c4c"), name = "Local") + 
  theme_classic() + 
  facet_grid(local~., scales = "free", space = "free") +
  labs(title = "Velocidades Médias Padrão - pico da manhã:",
       subtitle = "Av. Independência e região de influência",
       caption = paste("Fonte: Waze CCP (Atualização ", datafim, ")",sep = ""),
       x = "Local", y = "Velocidade media estimada",
       tag = "Gráfico 3")


alertas = dados$alerts
min(dados$alerts$pubMillis)
max(dados$alerts$pubMillis)
dim(dados$alerts)
alertas = as.data.frame(dados$alerts)
head(alertas)

library(leaflethex)
library(tibble)

pal <- colorFactor(
  palette = 'Dark2',
  domain = alertas$type
)

alertas$type = as.factor(alertas$subtype)
alertas$tipo = as.integer(as.factor(alertas$subtype))

teste = alertas$subtype

lookup = c(
  "HAZARD_ON_ROAD_POT_HOLE" = "Buraco na via",
  "JAM_STAND_STILL_TRAFFIC" = "Trafego ainda fluindo",
  "ROAD_CLOSED EVENT" = "Evento Bloqueando a via",
  "JAM_MODERATE_TRAFFIC" = "Congestionamento moderado",
  "JAM_HEAVY_TRAFFIC" = "Congestionamento pesado",
  "HAZARD_ON_ROAD_CONSTRUCTION" = "Obras na via",
  "HAZARD_ON_ROAD_CAR_STOPPED" = "Carro parado na via",
  "HAZARD_WEATHER_FOG" = "Risco climático",
  "HAZARD_ON_SHOULDER_MISSING_SIGN" = "HAZARD ON SHOULDER MISSING SIGN",
  "ACCIDENT_MAJOR" = "Acidente",
  "ACCIDENT_MINOR" = "Acidente leve"
)

alertas$subtype = lookup[teste]
head(alertas)

library(leaflet.extras)
library(tibble)


alertas$type = as.factor(alertas$subtype)
alertas$tipo = as.integer(as.factor(alertas$subtype))
table(alertas$tipo)
table(alertas$subtype)

df = tibble(
  lat = alertas$location$y,
  lng = alertas$location$x,
  color = alertas$tipo
)

pal <- colorFactor(
  palette = 'Accent',
  domain = df$color
)

leaflet(df, width = 910) %>% 
  addTiles() %>% 
  addCircles(color =~pal(color), stroke = TRUE, radius = 50, weight = 1, fillOpacity = 1) 



df1 = tibble(
  lat = alertas$location$y,
  lng = alertas$location$x,
  #size = runif(322, 1, 1),
  color = alertas$subtype,
  subtype = alertas$subtype,
  magvar = alertas$magvar,
  reliability = alertas$reliability,
  confidence = alertas$confidence,
  reportRating = alertas$reportRating
)
head(df1)


pal <- colorFactor(
  palette = 'Paired',
  domain = df1$color
)

leaflet(df1) %>%
  addTiles(group = "Mapa") %>%
  addCircles(group = "Pontos" ,color =~pal(color), stroke = TRUE, radius = 50, weight = 1, fillOpacity = 1, popup =~paste("Tipo: ", subtype, "<br>Direção°", magvar,"<br>Confiabilidade", reliability, "<br>Confança", confidence, "<br>ReportRating", reportRating, sep = " ")) %>%
  addLegend(pal = pal, values = ~color, group = "circles", position = "bottomleft", title = "Ocorrências")
