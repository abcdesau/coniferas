#--- Data Set Pinophyta de México
#--- 12/05/26
#--- Morales Esu

#DESCRIPCION
#En estas lineas se escribe codigo para cumplir con 3 objetivos: 
#---1. Visualizar el numero de generos y especies por estado
#---2. Visualizar puntos por cada registro
#---3. Agregar el occurrenceID a cada punto para abrir fotografias
#---La intencion es que sea todo interactivo y se pueda abrir desde una URL publica

#Primeo debemos cargar los 4 .csv para crear una copia, modificar la copia y eliminar
#todas las columnas excepto 13 campos y hacerlo para los 4 csv. 

#---Abriendo los .csv

library(here)
library(readr)
library(sf)
library( leaflet )
library( dplyr )
library(ggplot2)

# #Creamos la variable csvF donde vamos a guardar con la funcion list.files lo que
# hay en here() utilizando el patron .csv, con el nombre de la ruta de archivo
# antepuesta usando full.names y buscando dentro del directorio con recursive TRUE

csvF <- list.files( path = here(),
                        pattern = ".csv",
                        full.names = TRUE,
                        recursive = TRUE)

coniferas <- read_delim( file = csvF,
                       delim = "\t")
#                   locale = locale( encoding = "latin1" ) )

shpF <- list.files( path = here("Division_politica"),
                        pattern = ".shp",
                        full.names = TRUE )

estados <- st_read( shpF )

#---Crendo mapa con leaflet

leaflet( data = coniferas ) %>%
  addTiles() %>%
  addCircleMarkers( ~decimalLongitude, ~decimalLatitude,
                    popup = ~`species`,
                    radius = 4, 
                    color = "red")
# Calculando estadisticas por estado

conteo <- coniferas %>%
  group_by( stateProvince ) %>%
  summarise( num_coni = n() ) %>%
  arrange( desc(num_coni) )

# Creando grafico de barras

default_mar <- par( "mar" )

par( mar = c( 5, 12, 4, 2 ) )
barplot( conteo$num_coni,
         names.arg = conteo$stateProvince,
         col = "steelblue",
         main = "Distribución de pinus por Estado",
         xlab = "Número de ",
         ylab = "",
         horiz = TRUE,
         cex.names = .7,
         las = 1)
# Graficando con ggplot

coniferas %>%
  group_by( stateProvince ) %>%
  summarise( num_coni = n() ) %>%
  arrange( desc(num_coni) ) %>%
  ggplot( aes( x = reorder( stateProvince, num_coni ), y = num_coni ) ) +
  geom_col( fill = "steelblue" ) +
  labs( x = "Estado", y = "Número de coniferas",
        title = "Distribución de coniferas por Estado") +
  coord_flip()

# Agregando informacion al mapa

coniferas <- coniferas %>%
  add_count( stateProvince, name = "n_coni" )

leaflet( data = coniferas ) %>%
  addTiles() %>%
  addCircleMarkers( ~decimalLongitude, ~decimalLatitude,
                    radius = 4, 
                    color = "red", 
                    
                    popup = ~paste0(
                      `species`, "<br>",
                      "<b>Estado: </b>", stateProvince, "<br>",
                      "<b>No. coniferas por Estadi: </b>", n_coni
                    ))






