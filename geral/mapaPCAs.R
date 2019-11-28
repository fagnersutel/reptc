rm(list = ls(all.names = TRUE))
#rodar OneDrive/r-files/mapaacidentes/cluster_acidentes para pagar o DF dados
setwd("/Users/fagne/OneDrive/r-files/mapaacidentes/")
load("dados_latitudes.Rda")
head(dados_latitudes)
names(dados_latitudes)
data = dados_latitudes
dim(data)
dados <- as.data.frame(data)
dados$LATITUDE <- as.numeric(as.character(dados$LATITUDE))
dados$LONGITUDE <- as.numeric(as.character(dados$LONGITUDE))
dim(dados)

dados = dados[dados$TIPO_ACID == "ATROPELAMENTO",]
dim(dados)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
list.files()

library(sf)

Centro = st_read("Limites PCA.kml")
plot(Centro[3])


Leste = st_read("LimitesLeste.kml")
plot(Leste[3])

Norte = st_read("LimitesNorte.kml")
plot(Norte[3])


Sul = st_read("LimitesSul.kml")
plot(Sul[3])
class(Sul)

st_area(Sul) /st_area(Centro)
st_area(Sul) /st_area(Leste)
st_area(Sul) /st_area(Norte)

#https://cran.r-project.org/web/packages/sf/vignettes/sf1.html
units::set_units(Sul, km^2)
st_area(Sul) * 1.0E-6
st_area(Norte) * 1.0E-6
st_area(Leste) * 1.0E-6
st_area(Centro) * 1.0E-6

Sul$km2 = st_area(Sul) * 1.0E-6
Norte$km2 = st_area(Norte) * 1.0E-6
Leste$km2 = st_area(Leste) * 1.0E-6
Centro$km2 = st_area(Centro) * 1.0E-6


library(mapview)
mapview(Centro, col.regions="red", alpha.regions=0.5, map.types = c("OpenStreetMap", "CartoDB.DarkMatter")) + mapview(Sul, col.regions="green", alpha.regions=0.3)+mapview(Leste, col.regions="orange", alpha.regions=0.5) + mapview(Norte, alpha.regions=0.5)

library(sp)
temp = sf::st_coordinates(Leste$geometry)
class(temp)
coords1 <- cbind(c(temp[, 1]), c(temp[,2]))
spy1 <- coords2Polygons(coords1, ID = "A")
plot(spy1)
library(raster)
projection(spy1)=projection(dados)

class(spy1)

dat <- dados 
coordinates(dat)<- ~LONGITUDE+LATITUDE

projection(dat)=projection(spy1)
gIntersects(dat, spy1)

inter <-gIntersection(dat, spy1) 
class(inter)
plot(spy1)
plot(inter, add = TRUE, col="red")

rownames(inter@coords) = c(1:length(inter@coords[,1]))
mapview(inter)
class(inter)

library(leaflet)
library(leaflet.extras)
central = as.data.frame(inter@coords)
tamanho = length(central$x)
leaflet(central) %>%
  addTiles(group="Mapa") %>% 
  addHeatmap(group="Calor", lng=central$x, lat=central$y, max=5, blur = 70) %>%
  addCircles(group="Acidentes", ~x, ~y, weight = 0.1, radius=20, color="red",
             stroke = TRUE, fillOpacity = 0.5)  %>%
  addLegend(group="Legenda", "topright", colors= "red", labels=paste(length(central$x), "atropelamentos", "entre", min(dados$ANO), "e", max(dados$ANO)), title="Áreas de acidentalidade na região Centro") %>% 
  addLayersControl(overlayGroups = c("Calor", "Acidentes", "Legenda"),
                   options = layersControlOptions(collapsed = TRUE))

