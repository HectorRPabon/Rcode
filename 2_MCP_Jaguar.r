########## MCP ##############
install.packages("rgdal")
install.packages("terra")
install.packages("maptools")
library(geosphere)
library(sp)
library(raster)
# Cargar las bibliotecas necesarias
library(move)
library(maptools)
library(adehabitatHR)
library(scales)
library(mapview)
library(move2)
library(sf)
library(rgdal)
library(dplyr)
library(readr)
library(terra)


######MCP###########
# Cargar los datos de movimiento

setwd("C:/DATA/AnimalMov") # set wd to the workshop "data" folder in your computer
jaguarMove1 <- readRDS("Jaguar_move1.rds")

### Convert move2 object to move
jaguarMove <- to_move(jaguarMove1)
class(jaguarMove)

jaguarM1pj <- spTransform(jaguarMove, center = TRUE)
jaguarM1pj$id <- "Jaguares"


?mcp
# Calcular el Polígono Convexo Mínimo (MCP)
jaguarMCP <- mcp(as(jaguarM1pj[,"id"], 'SpatialPointsDataFrame'))
jaguarMCP1 <- spTransform(jaguarMCP, projection(jaguarMove))
#si un modelo encaja puede predecir potenciales posiciones entre los puntos dados

summary(jaguarMCP1)

windows()
# Configurar y mostrar el gráfico
plot(jaguarMove, type="n", bty="n", xlab="Longitud", ylab="Latitud", asp=1)
#lines(Kingv, col="grey")
points(jaguarMove, pch=16, col=alpha("red", 0.5), cex=0.75)
#points(Kingv, pch=1, cex=0.75, col=alpha("grey35", 0.5))
plot(jaguarMCP1, add = TRUE, lwd = 1.5, border = "white")
plot(jaguarMCP1, add = TRUE, lwd = 1.5, lty = 2, border = "black")

# Agregar leyenda y título al gráfico
legend("topright", "95% MCP", lty = 2, bty = "n")
title("MCP Jaguar")

# EXPORTO EL SHAPEFILE
writeOGR(jaguarMCP1, "Jaguar MCP", layer = "jaguarMCP1", driver="ESRI Shapefile")


#--------------------------------------#
#MCP individual #### MALE

unique(mt_track_id(jaguarMove1))
jaguarM1 <- filter_track_data(jaguarMove1, .track_id = "101")

##ORGANIZAR DATOS
jaguarM1 <- dplyr::arrange(jaguarM1, mt_track_id(jaguarM1), mt_time(jaguarM1))

jaguarM1move <- to_move(jaguarM1)
class(jaguarM1move)

jaguarMale1pj <- spTransform(jaguarM1move, center = TRUE)
jaguarMale1pj$id <- "JaguarMale"


jaguarM1MCP <- mcp(as(jaguarMale1pj[,"id"], 'SpatialPointsDataFrame'))
jaguarM1MCP2 <- spTransform(jaguarM1MCP, projection(jaguarM1))

summary(jaguarM1MCP2)

windows()
plot(jaguarM1move, type="n", bty="n", xlab="Longitud", ylab="Latitud", asp=1)
points(jaguarM1move, pch=16, col=alpha("red", 0.5), cex=0.75)
plot(jaguarM1MCP2, add = TRUE, lwd = 1.5, border = "white")
plot(jaguarM1MCP2, add = TRUE, lwd = 1.5, lty = 2, border = "black")

legend("topright", "95% MCP", lty = 2, bty = "n")
title("MCP Jaguar male")

writeOGR(jaguarM1MCP2, "Jaguar male MCP", layer = "jaguarM1MCP2", driver="ESRI Shapefile")

#--------------------------------------#
#MCP individual #### FEMALE

unique(mt_track_id(jaguarMove1))
jaguarF1 <- filter_track_data(jaguar1, .track_id = "102")

jaguarF1move <- to_move(jaguarF1)
class(jaguarF1move)

jaguarFemale1pj <- spTransform(jaguarF1move, center = TRUE)
jaguarFemale1pj$id <- "JaguarFemale"

jaguarF1MCP <- mcp(as(jaguarFemale1pj[,"id"], 'SpatialPointsDataFrame'))
jaguarF1MCP2 <- spTransform(jaguarF1MCP, projection(jaguarF1))

summary(jaguarF1MCP2)

windows()

plot(jaguarF1move, type="n", bty="n", xlab="Longitud", ylab="Latitud", asp=1)
points(jaguarF1move, pch=16, col=alpha("red", 0.5), cex=0.75)
plot(jaguarF1MCP2, add = TRUE, lwd = 1.5, border = "white")
plot(jaguarF1MCP2, add = TRUE, lwd = 1.5, lty = 2, border = "black")

legend("topright", "95% MCP", lty = 2, bty = "n")
title("MCP Jaguar female")

writeOGR(jaguarF1MCP2, "Jaguar female MCP", layer = "jaguarF1MCP2", driver="ESRI Shapefile")


