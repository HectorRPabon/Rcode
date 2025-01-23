########## MCP ##############
install.packages("rgdal")
install.packages("terra")
install.packages("recurse")
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
library(recurse)
library(raster)

######MCP###########
# Cargar los datos de movimiento

setwd("C:/DATA/AnimalMov") # set wd to the workshop "data" folder in your computer
jaguarMove1 <- readRDS("Jaguar_move1.rds")

### Convert move2 object to move
jaguarMove <- to_move(jaguarMove1)
class(jaguarMove)

jaguarM1pj <- spTransform(jaguarMove, center = TRUE)
jaguarM1pj$id <- "Jaguares"


########## Kernel UD ##############
# Importar la biblioteca terra
library(terra)

#KERNEL DE PROBABILIDAD PARA EL MACHO
# Pasar de Move2 a Move
jaguarM1move <- to_move(jaguarM1)
class(jaguarM1move)

#transformar coordenadas WGS 84 utm 18N
jaguarMale1pj <- spTransform(jaguarM1move, crs("EPSG:32618"))
#jaguarMale1pj <- spTransform(jaguarM1move,center = TRUE)
jaguarMale1pj$id <- "JaguarMale"

cobertura <- sf::read_sf("COBERTURA/LandCover_LaAurora.shp")

##Crear un template vacio para definir la extension del analisis
templ_rast <- raster::raster(ext=extent(cobertura), crs=crs(cobertura), resolution=30)

#Crear la capa de probabilidad (UD) y agregrala al kernel
kernel.JM1proj <- kernelUD(jaguarMale1pj[,"id"], grid=as(templ_rast, "SpatialPixels"))

JMRastProj_rast <- rast(cbind(kernel.JM1proj$JaguarMale@coords, kernel.JM1proj$JaguarMale@data), type="xyz")

#Estandariza el
kernel.standardized <- rast(kernel.JM1proj$JaguarMale)/sum(getValues(raster(kernel.JM1proj$JaguarMale)))

plot(kernel.standardized)

# Crear el raster de probabilidad del Macho
writeRaster(kernel.standardized, "JaguarUD_Male1.tif", T)

## OBTENER EL VOLUMEN DEL UD

kernel.JM1vol <- getvolumeUD(kernel.JM1proj)

# Crear un objeto raster con las coordenadas y datos del kernel
JM_KenelVol <- rast(cbind(kernel.JM1vol$JaguarMale@coords, kernel.JM1vol$JaguarMale@data), type="xyz")

windows()

# Obtener los vértices del kernel
KernM <- getverticeshr(kernel.JM1proj)

# Ajustar la extensión del gráfico
bb <- bbox(jaguarMale1pj)*1.5

# Configurar los márgenes del gráfico
par(mar=c(1,1,1,1))

# Crear un gráfico de imagen con el raster 'JaguarMaleProj'
RasterM1<-image(JM_KenelVol, col=gray(0:8 / 8), xlim=bb[1,], ylim=bb[2,], axes=F)

# Superponer los contornos del kernel en el gráfico
plot(KernM, add=T, main=NA)

# Dibujar líneas que representan los límites en gris
lines(jaguarMale1pj, col="grey")

# Dibujar puntos con colores y formas específicas
points(jaguarMale1pj, pch=16, col=alpha("firebrick", 0.5), cex=0.75)
points(jaguarMale1pj, pch=1, cex=0.75, col=alpha("red", 0.5))

# Agregar una leyenda en la esquina superior derecha
legend("topright", "95% Kernel Jaguar Male", lty = 1, bty = "o", cex=1.5, box.col="white")

# CREAR EL RASTER DEL MACHO
writeRaster(JM_KenelVol, "Jaguar_Male_Volum.tif", T)


#KERNEL FEMALE
unique(mt_track_id(jaguarMove1))
jaguarF1 <- filter_track_data(jaguarMove1, .track_id = "102")
jaguarF1move <- to_move(jaguarF1)
class(jaguarF1move)

jaguarFemale1pj <- spTransform(jaguarF1move, crs("EPSG:32618"))
jaguarFemale1pj$id <- "JaguarFemale"

templ_rast <- rast("rasterTemplate.rds")
class(templ_rast)

#templ_rast <- raster::raster(ext=extent(cobertura), crs=crs(cobertura), resolution=30)
kernel.JF1prob <- kernelUD(jaguarFemale1pj[,"id"], grid=as(templ_rast, "SpatialPixels"))

kernelF1.standardized <- rast(kernel.JF1prob$JaguarFemale)/sum(getValues(raster(kernel.JF1prob$JaguarFemale)))
plot (kernelF1.standardized)


JFProb_rast <- rast(cbind(kernel.JF1prob$JaguarFemale@coords, kernel.JF1prob$JaguarFemale@data), type="xyz")
plot(JFProb_rast)


# Crear el raster de UD del Hembra
writeRaster(kernelF1.standardized, "JaguarPropUD_Female2.tif", T)

JF1volmenUD <- getvolumeUD(kernel.JF1prob)
JFRastvol <- rast(cbind(JF1volmenUD$JaguarFemale@coords, JF1volmenUD$JaguarFemale@data), type="xyz")

windows()
KernF <- getverticeshr(kernel.JF1prob)
bbf <- bbox(jaguarFemale1pj)*1.5
par(mar=c(1,1,1,1))
RasterF1<-image(JFRastvol, col=gray(0:8 / 8), xlim=bbf[1,], ylim=bbf[2,], axes=F)
plot(KernF, add=T, main=NA)
lines(jaguarFemale1pj, col="grey")
points(jaguarFemale1pj, pch=16, col=alpha("firebrick", 0.5), cex=0.75)
points(jaguarFemale1pj, pch=1, cex=0.75, col=alpha("red", 0.5))
legend("topright", "95% Kernel Jaguar Female", lty = 1, bty = "o", cex=1.5, box.col="white")
writeRaster(JFRastvol, "Jaguar_Female2Volumen.tif", T)
