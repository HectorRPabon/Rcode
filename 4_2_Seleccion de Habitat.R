library(readr)
library(move2)
library(tidyverse)
library(mapview)
library(units)
library(lubridate)
library(classInt)
library(circular)  #masked on stats
library(patchwork) #need to update
library(ggmap)
devtools::install_github('oswaldosantos/ggsn')
library(ggsn)
library(amt)
library(keyring)
library(sf)
library(terra)

setwd("C:/DATA/AnimalMov")

### PREPARAR CAPAS AMBIENTALES####

########RASTERIZAR LAS COBERTURAS###############
cobertura2 <- vect("COBERTURA/LandCover_LaAurora.shp")
cobertura <- vect("COBERTURA/LandCover_LaAurora.shp")
plot(cobertura2)
plot(cobertura2)

#Crear template
#templ_rast <- rast(extent=ext(cobertura), crs=crs(cobertura), resolution=30)
saveRDS(templ_rast, file="rasterTemplate.rds")

#Rasterizo la cobertura
cobertura_rast <- rasterize(cobertura2, templ_rast, cobertura$gridcode, touches=T)
plot(cobertura_rast)

#Importo variables
drenaje <- vect("COBERTURA/DrenajeSencillo.shp")
plot(drenaje) 
build <- vect("COBERTURA/Builds.shp")
plot(build)

#Filtro los Drenajes y Bosques
drenajes_sel <- drenaje[drenaje$PROYECTO == 'SI',]
plot(drenajes_sel)

bosques <- cobertura[cobertura$gridcode == '10',]

###DISTANCIA EU A CAPA DE BOSQUES, DRENAJES Y EDIFICIOS

distToForest <- terra::distance(templ_rast, bosques)
plot(distToForest)

distToDren <- terra::distance(templ_rast, drenajes_sel)
plot(distToDren)

distToEdificio <- terra::distance(templ_rast, build)
plot(distToEdificio)

mask()

## Combina todos rasters
capasEnv <- c(cobertura_rast, distToDren, distToEdificio, distToForest)
names(capasEnv) <- c("Land cover", "Distance to Rivers","Distance to Building", "Distance to Forest")
plot(capasEnv)
table(is.na(values(capasEnv[["Land cover"]])))
plot(capasEnv[["Land cover"]])

##Importo las UD (proporcion del tiempo que pasa en el AOI)
UDProbF1 <- rast("Jaguar_UDProbF1.tif")
UDProbM1 <- rast("Jaguar_UDProbM1.tif")

# UD = 1 La suma de valores de la capa debe dar 1
plot(UDProbF1)
plot(UDProbM1)
sum(values(UDProbF1))
sum(values(UDProbM1))

#OPCION 1 VECTOR POINTS 
##Convierto la UDprob de F1 en Vect-puntos 
UDprobF1_poin <- as.points(kernelF1.standardized, values=TRUE)
plot(UDprobF1_poin)

##Extraer los datos del ambiente para cada punto
UDprobF1_poin_env <- terra::extract(capasEnv, UDprobF1_poin, bind=T, method="simple") #tengo problema con el valor de cobertura
summary(UDprobF1_poin_env)

## Remove UD prob with NA values in environment (POR LOS VALORES NA de la CAPA)
UDprobF1_poin_env_noNA <- UDprobF1_poin_env[!is.na(UDprobF1_poin_env$Land.cover),]
nrow(UDprobF1_poin_env);nrow(UDprobF1_poin_env_noNA)

sum(UDprobF1_poin_env$ud)
sum(UDprobF1_poin_env_noNA$ud)

# Bring the total UD sum values back to 1 (DEBERIAN SER 1)
UDprobF1_poin_env_noNA$ud <- UDprobF1_poin_env_noNA$ud/sum(UDprobF1_poin_env_noNA$ud)
sum(UDprobF1_poin_env$ud)
sum(UDprobF1_poin_env_noNA$ud)
summary(UDprobF1_poin_env_noNA$ud)

# Look at time spent in each land cover category
timeInLC <- as.data.frame(UDprobF1_poin_env_noNA) %>% 
  group_by(as.character(Land.cover)) %>%
  summarise(timePerLandClass=sum(ud))

# Look at time spent at certain distances
test <- as.data.frame(UDprobF1_poin_env_noNA) %>% 
  mutate(timeNearRiver = ud * Distance.to.Rivers,
         timeNearBuilding = ud * Distance.to.Building,
         timeNearForest = ud * Distance.to.Forest) %>%
  subset(select = c(ud,timeNearRiver,timeNearBuilding,timeNearForest))

#Estadisticas para la Hembra F1
avgDistToRiv <- sum(test$timeNearRiver)  
avgDistToBuild <- sum(test$timeNearBuilding)  
avgDistToForest <- sum(test$timeNearForest)  

summary(test$timeNearRiver)
plot (test$timeNearRiver)

##PARA EL MACHO

##Convierto la UDprob en Vect-puntos
UDprobM1_poin <- as.points(kernel.standardized, values=TRUE)
plot(UDprobM1_poin)

##Extraer los datos del ambiente para cada punto
UDprobM1_poin_env <- terra::extract(capasEnv, UDprobM1_poin, bind=T, method="simple") #tengo problema con el valor de cobertura
summary(UDprobM1_poin_env)

## Quito los valores sin datos del UD prop de las capas ambientales
UDprobM1_poin_env_noNA <- UDprobM1_poin_env[!is.na(UDprobM1_poin_env$Land.cover),]
nrow(UDprobM1_poin_env);nrow(UDprobM1_poin_env_noNA)

#Reviso que el UD sume 1 en el noNA
sum(UDprobF1_poin_env$ud)
sum(UDprobF1_poin_env_noNA$ud)

# Observo el tiempo gastado en cada cobertura
timeInLC <- as.data.frame(UDprobM1_poin_env_noNA) %>% 
  group_by(as.character(Land.cover)) %>%
  summarise(timePerLandClass=sum(ud))

# Observo el tiempo gastado a ciertas distancias
test2 <- as.data.frame(UDprobM1_poin_env_noNA) %>% 
  mutate(timeNearRiver = ud * Distance.to.Rivers,
         timeNearBuilding = ud * Distance.to.Building,
         timeNearForest = ud * Distance.to.Forest) %>%
  subset(select = c(ud,timeNearRiver,timeNearBuilding,timeNearForest))

#Estadisticas del Macho M1
avgDistToRiv2 <- sum(test2$timeNearRiver)  
avgDistToBuild2 <- sum(test2$timeNearBuilding)  
avgDistToForest2 <- sum(test2$timeNearForest)  

summary(test$timeNearRiver)


###GENERO ALGUNAS GRAFICAS
plot (test$timeNearRiver)


plot(valuesM1_UD, UDM1_values$Distance.to.Rivers)
plot(valuesM1_UD, UDM1_values$Distance.to.Building)
plot(valuesM1_UD, UDM1_values$Distance.to.Forest)

plot(valuesF1_UD, UDF1_values$Distance.to.Rivers)
plot(valuesF1_UD, UDF1_values$Distance.to.Building)
plot(valuesF1_UD, UDF1_values$Distance.to.Forest)


#OPCION 2 USANDO DATAFRAME -  Convierto la UDprob en el dataframe
#Obtengo los valores de las capas
values_amb <- terra::values(capasEnv)
valuesF1_UD <- terra::values(UDProbF1)
valuesM1_UD <- terra::values(UDProbM1)


UDF1_values <- data.frame(
  udF1 = valuesF1_UD,
  EnvSk = values_amb
)


UDM1_values <- data.frame(
  udM1 = valuesM1_UD,
  EnvSk = values_amb
)

summary(UDM1_values)


