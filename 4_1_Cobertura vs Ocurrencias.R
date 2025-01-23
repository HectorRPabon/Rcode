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

#### RELACIONAR LAS OCURRENCIAS CON LA CAPA DE COBERTURAS ####
#### RELACIONAR LAS OCURRENCIAS CON LA CAPA DE COBERTURAS ####
#Cargar la tabla
jaguar1 <- read.cvs ("Jaguar_GPSlocation_v1.csv")

# Crear un sf 
Jaguar_points <- st_as_sf(jaguar1, coords = c("location.long", "location.lat"), crs = "WGS84")

#Convertir a sf solo de las coberturas
cobertura_sf <- sf::read_sf("COBERTURA/LandCover_LaAurora.shp")

## Reproyectar la cobertura a geograficas
CobertWGS <- sf::st_transform(cobertura,crs = sf::st_crs(Jaguar_points))

##Vectorizar los puntos y coberturas
Jaguar_points_vect <- vect(Jaguar_points)
cobertura_vect <- vect(CobertWGS)

##Extraer los datos de coberturas para cada punto
puntosCobertura <- terra::extract(cobertura_vect,Jaguar_points_vect)

class (puntosCobertura)

#Unir los datos de los puntos y los datos de cobertura
jaguar1[8:13] <-puntosCobertura

##Guardar la tabla
saveRDS(jaguar1, file="Jaguar_Cobertura.rds")

##Cargar tabla
jaguarCovers <- readRDS("Jaguar_Cobertura.rds")

class(jaguarCovers)

###Sacar las estadisticas


#tabla de frecuencias
freq_tabla <- jaguarCovers %>%
  group_by(sex, gridcode)%>%
  summarise (n=n()) %>%
  st_drop_geometry() %>%
  spread(gridcode, n)
freq_tabla

#tabla de proporciones
prop_tabla <- jaguarCovers %>%
  group_by(sex, gridcode)%>%
  summarise (n=n()) %>%
  mutate(prop=n/sum(n)) %>%
  subset(select=c("sex","gridcode","prop"))%>%
  st_drop_geometry() %>%
  spread(gridcode, prop)
prop_tabla

##Exporto las tablas