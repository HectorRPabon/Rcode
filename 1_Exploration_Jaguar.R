##JAGUAR TELEMETRY DATA ANALYSIS

library(readr)
library(move2)
library(tidyverse)
library(sf)
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


setwd("C:/DATA/AnimalMov")

jaguar1 <- mt_read ("Jaguar_GPSlocation_v1.csv")

# first make sure the date/time is in POSIXct format
head(jaguar1$Date)
jaguar1$timestamp <- as.POSIXct(paste0(as.Date(jaguar1$Date,format="%d/%m/%Y"), " ",jaguar1$hours), format="%Y-%m-%d %H:%M:%S", tz="America/Bogota")
# now create a move2 object from a data.frame
jaguarMove1 <- mt_as_move2(jaguar1,
                           time_column = "timestamp",
                           track_id_column = "individual-local-identifier",
                           coords= c("location.long", "location.lat"),
                           crs="EPSG:4326")

class(jaguarMove1)
jaguarMove1

### Unidades del objeto
str(jaguarMove1)

### Definir variables
movebank_get_vocabulary(jaguarMove1)

##Visualizacion con el map view Mapview #

jaguarSF1 <- jaguarMove1 # by using mapview, after changing the class so that it is recognised as an SF object
class(jaguarSF1) <- class(jaguarMove1) %>% setdiff("move2") # remove class "move2" from object
mapview::mapView(jaguarSF1, zcol="individual-local-identifier", legend=F) #as points

##ORGANIZAR DATOS
jaguarMove1 <- dplyr::arrange(jaguarMove1, mt_track_id(jaguarMove1), mt_time(jaguarMove1))

mapview::mapView(mt_track_lines(jaguarMove1), zcol="individual-local-identifier", legend=F) #as lines

### Numero de ubicaciones
nrow(jaguarMove1)

### Numero of tracks
mt_n_tracks(jaguarMove1)

### Time lag entre localizaciones####
timeLags <- mt_time_lags(jaguarMove1)
head(timeLags)
timeLags <- units::set_units(timeLags, hours)
head(timeLags)

### Timelag, distance, speed, and direction are properties of the segment (2 locations, not one)
# therefore the number of valid values will be nrow(track) - 1 , and the last value will be NA
# the vector will contain as many NAs as the number of tracks, one at the end of each track

tail(timeLags)
table(is.na(timeLags))


### Distribution of time lags
summary(timeLags)
timeLags_h <- units::drop_units(timeLags) #hist is not compatible with time "units", we drop them
hist(timeLags_h, breaks=50, main=NA, xlab="Time lag in hours") 
arrows(24.5,587.5,20.7,189.7, length=0.1)
arrows(49.5,587.5,45.7,189.7, length=0.1)

### Distribution of timelags shorter than 1h
hist(timeLags_h[timeLags_h < 1], breaks="FD", main=NA, xlab="Time lag in hours")


### Number of locations in time ####
jnlocal <- mt_time(jaguarMove1)
# transform timestamps into local time of the study for better interpretation
#tsLocal <- lubridate::with_tz(ts, tzone="America/Bogota")
# N. of location per hour (geometry gets inherited)
jaguarMove1 %>% group_by(hour(jnlocal)) %>% 
  summarize(n())
# N. of locations per month and hour, rename columns and drop geometry
#jaguarMove1 %>% group_by(Month = month(jnLocal), Hour = hour(jnLocal)) %>% 
#  summarize(N.locations = n()) %>% 
#  sf::st_drop_geometry()


### Distance between locations ####
dist <- set_units(mt_distance(jaguarMove1), m)
summary(dist)
hist(dist)


### Speed between locations ####
speeds <- set_units(mt_speed(jaguarMove1), km/h)
summary(speeds)
hist(drop_units(speeds), breaks = "FD") #"Freedman-Diaconis" : Extreme outliers; the "FD" rule would take very large number of 'breaks'

### Have a look at the realistic speeds (e.g. < 20m/s)
speedsRealistic <- speeds[speeds < set_units(20, m/s)] # remember to set the unit before the operation
hist(drop_units(speedsRealistic), xlab = "Speed [m/s]", breaks = "FD") # this is the common shape for speeds

### Speed vs timelags
speedVsTimeLag <- data.frame(timeLag = timeLags, speeds = speeds)
speedVsTimeLag <- speedVsTimeLag[speedVsTimeLag$timeLag < set_units(10, hour) & speedVsTimeLag$speeds < set_units(20, m/s),]
# with longer timelags the speeds seem lower... you will learn a lot more about this in the following days.
plot(speedVsTimeLag$timeLag, speedVsTimeLag$speeds, xlab='Time lag', ylab='Speed', pch=19) # when "units" are recognised, they are automatically added to the plot

### Plot highlighting segments with high and low speeds

# select Jaguar Female 102
unique(mt_track_id(jaguarMove1))
jaguarF1 <- filter_track_data(jaguar1, .track_id = "102")

# store speed
vF1 <- set_units(mt_speed(jaguarF1), m/s)
# find 9 colors for 10 breaks in the speed vector, the myPal vector has the same length as the number of segments
myBreaks <- classIntervals(vF1[!is.na(vF1)], n=10, style="equal")$brks
#myPal <- as.character(cut(v, breaks = myBreaks, labels = grey.colors(10)))
ggplot() +
  #geom_sf(data = mt_segments(bat191), linewidth = 1.5, color = myPal) +
  #two alternative ways to assign colors, either with myPal and the line above, or with the line below and scale_color_gradient2
  geom_sf(data = mt_segments(jaguarF1), aes(color = drop_units(vF1)), linewidth = 1.5) +
  xlab("Longitude") + ylab("Latitude") +
  scale_color_gradient2(low = "black", mid = "#434343", high = "white", 
                        midpoint = drop_units(median(vF1, na.rm=T)),
                        breaks = drop_units(myBreaks),
                        name = "Speed [m/s]") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # get rid of panel grids
        panel.background = element_rect(fill = '#434343')) # Change panel background

# select Jaguar Male 101
unique(mt_track_id(jaguarMove1))
jaguarM1 <- filter_track_data(jaguar1, .track_id = "101")


##ORGANIZAR DATOS
jaguarM1 <- dplyr::arrange(jaguarM1, mt_track_id(jaguarM1), mt_time(jaguarM1))

# store speed
vM1 <- set_units(mt_speed(jaguarM1), m/s)
# find 9 colors for 10 breaks in the speed vector, the myPal vector has the same length as the number of segments
myBreaks <- classIntervals(vM1[!is.na(vM1)], n=10, style="equal")$brks
#myPal <- as.character(cut(v, breaks = myBreaks, labels = grey.colors(10)))
ggplot() +
  #geom_sf(data = mt_segments(bat191), linewidth = 1.5, color = myPal) +
  #two alternative ways to assign colors, either with myPal and the line above, or with the line below and scale_color_gradient2
  geom_sf(data = mt_segments(jaguarM1), aes(color = drop_units(vM1)), linewidth = 1.5) +
  xlab("Longitude") + ylab("Latitude") +
  scale_color_gradient2(low = "black", mid = "#434343", high = "white", 
                        midpoint = drop_units(median(vM1, na.rm=T)),
                        breaks = drop_units(myBreaks),
                        name = "Speed [m/s]") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # get rid of panel grids
        panel.background = element_rect(fill = '#434343')) # Change panel background

### Direction of movement / azimuth / heading of movement ####

# NOTE: the words heading or bearing are mostly referred to the direction of body axis
# When analysing tracking data we are not observing the animal but only its movement, so we can only know the direction of movement (not body orientation).
# BUT: many devices record "heading" which is the orientation of the tag (not the direction of movement). SO if the tag does not shift its position it is an indication of body orientation.
direction <- mt_azimuth(jaguarMove1) # Angles in radians relative to the N
head(direction)
summary(direction)
hist(directionF1,  breaks = 18, xlab="Direction of movement", main = NA)
# DIRECTION OF FEMALE
directionF1 <- mt_azimuth(jaguarF1) # Angles in radians relative to the N
head(directionF1)
summary(directionF1)
hist(directionF1,  breaks = 18, xlab="Direction of movement F1", main = NA)
#MALE direction
directionM1 <- mt_azimuth(jaguarM1) # Angles in radians relative to the N
head(directionM1)
summary(directionM1)
hist(directionM1,  breaks = 18, xlab="Direction of movement M1", main = NA)

saveRDS(jaguarMove1, file="Jaguar_move1.rds")

###############RESPONDER PREGUNTAS ESPECIFICAS ###########################################

##CONOCER A QUE HORAS SON MAS ACTIVOS CREAR BOXPLOT ENTRE SPEED Y HOURS
jaguarMove1 <- readRDS("Jaguar_move1.rds")

jaguarMove1$hourOfday <- hour(jaguarMove1$timestamp)
speed <- mt_speed(jaguarMove1, "m/s")
                  
boxplot(speed~hourOfday, data=jaguarMove1)


#A QUE HORAS ES  MAS ACTIVO EL MACHO
unique(mt_track_id(jaguarMove1))
jaguarM1 <- filter_track_data(jaguarMove1, .track_id = "101")

hourOfdayM1 <- hour(jaguarM1$timestamp)

vM1 <- set_units(mt_speed(jaguarM1), m/s)

boxplot(vM1~hourOfdayM1, data=jaguarM1)

#A QUE HORAS ES  MAS ACTIVA LA HEMBRA
unique(mt_track_id(jaguarMove1))
jaguarF1 <- filter_track_data(jaguarMove1, .track_id = "102")

hourOfdayF1 <- hour(jaguarF1$timestamp)
vF1 <- set_units(mt_speed(jaguarF1), m/s)

boxplot(vF1~hourOfdayF1, data=jaguarF1)

##CUAL ES LA DISTANCIA MAXIMA RECORRIDA EN UN DIA y CUAL ES LA PROMEDIO

dist <- set_units(mt_distance(jaguarMove1), m)
summary(dist)
plot(dist)


# DISTANCIA RECORRIDO POR LA HEMBRA

distF1 <- set_units(mt_distance(jaguarF1), m)
summary(distF1)
plot(distF1)

#A CUANTO ES LO MAXIMO RECORRIDO POR EL MACHO

distM1 <- set_units(mt_distance(jaguarM1), m)
summary(distM1)
plot(distM1)

# DISTANCIA/TIEMPO HEMBRA
# when "units" are recognised, they are automatically added to the plot

jaguarM1$distance <- set_units(mt_distance(jaguarM1), m)
jaguarF1$distance <- set_units(mt_distance(jaguarF1), m)

plot(jaguarM1$timestamp , jaguarM1$distance, xlab='Time lag', ylab='Distance M1', pch=19) 

plot(jaguarF1$timestamp , jaguarF1$distance, xlab='Time lag', ylab='Distance F1', pch=19) 

###GRABAR TABLAS DE MACHO Y HEMBRA
saveRDS(jaguarM1, file="Jaguar_Male1.rds")

saveRDS(jaguarF1, file="Jaguar_Female1.rds")
