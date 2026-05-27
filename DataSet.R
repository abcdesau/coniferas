#--- Data Set Pinophyta de México
#--- 12/05/26
#--- Esau Morales

#DESCRIPCION
#En estas lineas se escribe codigo para cumplir con 3 objetivos: 
#---1. Visualizar el numero de generos y especies por estado.
#---2. Visualizar puntos por cada registro.
#---3. Agregar el occurrenceID a cada punto para abrir fotografias.
#---La intencion es que sea todo interactivo y se pueda abrir desde una URL publica.

#Primero debemos cargar los 4 .csv. 


#---Librerias

library(here)
library(readr)
library(sf)
library( leaflet )
library( dplyr )
library(ggplot2)
library(htmlwidgets)

#---Abriendo los .csv

# Creamos la variable csvF donde vamos a guardar con la funcion list.files lo que
# hay en here() utilizando el patron .csv, con el nombre de la ruta de archivo
# antepuesta usando full.names y buscando dentro del directorio con recursive TRUE.

csvF <- list.files( path = here(),
                        pattern = ".csv",
                        full.names = TRUE,
                        recursive = TRUE)

coniferas <- read_delim( file = csvF,
                       delim = "\t")

# Luego creamos shpF donde vamos a guardar el archivo .shp de division politica
# de INEGI. 

shpF <- list.files( path = here("Division_politica"),
                        pattern = ".shp",
                        full.names = TRUE )

estados <- st_read( shpF )

#--- Modificando estados con dplyr
# Creamos una nueva variable diccionario_estados donde vivan las asignaciones 
# para cada estado con un nombre oficial y su respectiva reasignacion usando
# concatenar.

diccionario_estados <- c(
  "Coahuila de Zaragoza"            = "Coahuila",
  "Ciudad de México"                = "CDMX",
  "México"                          = "EDOMEX",
  "Michoacán de Ocampo"            = "Michoacán",
  "Veracruz de Ignacio de la Llave" = "Veracruz"
)

# Se crea la variable estados_state que en esencia es la misma que estados, pero
# toma este ultimo como argumento de la funcion de la derecha usando pipe, mutate
# permite crear o modificar columnas. Crea la columna NOMGEO_nuevo donde se usa la 
# funcion recode para la reclasificacion, utiliza como primer argumento NOMGEO y el 
# operador !!! que desempaqueta la lista de elementos. 

estados_state <- estados %>%
  mutate(
    NOMGEO_nuevo = recode(NOMGEO, !!!diccionario_estados)
  )

#--- Modificando stateProvince con pipe
# La modificacion de coniferas es similar al shp, solo que en este caso se utiliza 
# la funcion case_when para evaluar multiples condiciones, por ejemplo se compara que
# en stateProvince exista el elemento de la lista y en caso de que sea TRUE asigna
# un valor con el operador ~, para los casos donde solo hay una opcion se utiliza el 
# operador de comparacion == y en otros casos donde la comprobacion de stateProvince
# sea TRUE y no este en alguno de las condiciones establecidas se conserva el nombre
# original. Luego se filtran los valores para mostrar los que no son NA.

coniferas_state <- coniferas %>%
  mutate(stateProvince = case_when(
    stateProvince %in% c("Michoacan de ocampo", "Michoacán") ~ "Michoacán",
    stateProvince %in% c("Estado de México (ME)", "Mexico", "México") ~ "EDOMEX",
    stateProvince == "Veracruz de ignacio de la llave" ~ "Veracruz",
    stateProvince %in% c("Hidalgo (HG)", "Hidalgo") ~ "Hidalgo",
    stateProvince == "San luis potosi" ~ "San Luis Potosí",
    stateProvince == "Coahuila de zaragoza" ~ "Coahuila",
    stateProvince == "Nuevo leon" ~ "Nuevo León",
    stateProvince %in% c("Distrito federal", "Distrito Federal", "Ciudad de mexico") ~ "CDMX",
    stateProvince == "Baja california" ~ "Baja California",
    stateProvince == "Queretaro de arteaga" ~ "Querétaro",
    stateProvince == "COLIMA, JALISCO" ~ NA_character_, 
    TRUE ~ stateProvince
  )) %>%
  filter(!is.na(stateProvince))

#---Crendo mapa con leaflet

# leaflet( data = coniferas_state ) %>%
#   addTiles() %>%
#   addCircleMarkers( ~decimalLongitude, ~decimalLatitude,
#                     popup = ~paste0("<b> Género: </b>", "<em>", genus,"<em/>", "<br>",
#                                     "<b> Especie: </b>", "<em>", species,"</em>"),
#                     radius = 4, 
#                     color = "red")

# Calculando estadisticas por estado

# conteo <- coniferas_state %>%
#   group_by( stateProvince ) %>%
#   summarise( num_coni = n() )

# Calculo de generos y especies unicas por estado

riqueza_estados <- coniferas_state %>%
  group_by(stateProvince) %>%
  summarise(
    num_registros = n(),
    num_generos = n_distinct(genus),
    num_especies = n_distinct(species)
  ) %>%
  arrange(desc(num_especies))

# Agregando conteo a SHP estados

estados_state <- estados_state %>%
  left_join(riqueza_estados, by = c("NOMGEO_nuevo" = "stateProvince"))%>%
  mutate(
   # num_coni = ifelse(is.na(num_coni), 0, num_coni),
    num_registros = ifelse(is.na(num_registros), 0, num_registros),
    num_generos = ifelse(is.na(num_generos), 0, num_generos),
    num_especies = ifelse(is.na(num_especies), 0, num_especies)
    )

# Creando grafico de barras

default_mar <- par( "mar" )

par( mar = c( 5, 12, 4, 2 ) )
barplot( conteo$num_coni,
         names.arg = conteo$stateProvince,
         col = "steelblue",
         main = "Distribución de pinus por Estado",
         xlab = "Número de registros",
         ylab = "",
         horiz = TRUE,
         cex.names = .7,
         las = 1)

# Graficando con ggplot

coniferas_state %>%
  group_by( stateProvince ) %>%
  summarise( num_coni = n() ) %>%
  arrange( desc(num_coni) ) %>%
  ggplot( aes( x = reorder( stateProvince, num_coni ), y = num_coni ) ) +
  geom_col( fill = "steelblue" ) +
  labs( x = "Estado", y = "Número de coniferas",
        title = "Distribución de coniferas por Estado") +
  coord_flip()

# Agregando informacion al mapa

coniferas_state <- coniferas_state %>%
  add_count( stateProvince, name = "n_coni" )

leaflet( data = coniferas_state ) %>%
  addTiles() %>%
  addCircleMarkers( ~decimalLongitude, ~decimalLatitude,
                    radius = 4,
                    color = "red",

                    popup = ~paste0("<b> Género: </b>", "<em>", genus,"<em/>", "<br>",
                                    "<b> Especie: </b>", "<em>", species,"</em>", "<br>",
                                    "<b>Estado: </b>", stateProvince, "<br>",
                                    "<b>No. coniferas por Estado: </b>", n_coni
                    ))

plot( st_geometry(estados))

# Creando una paleta para los registros

pal_registros <- colorNumeric(
  palette = c("#ffffcc", "#fd8d3c", "#bd0026"),
  domain = estados_state$num_registros
)

pal_generos <- colorNumeric(
  palette = c("#ffffcc", "#fd8d3c", "#bd0026"),
  domain = estados_state$num_generos
)

pal_especies <- colorNumeric(
  palette = c("#ffffcc", "#fd8d3c", "#bd0026"),
  domain = estados_state$num_especies
)
#---Agregando capa de estados, registros y control de capas

MapaConiferas <-leaflet( data = estados_state ) %>%
  addProviderTiles(providers$CartoDB.DarkMatter,
                   group = "Mapa oscuro") %>%
  addTiles(group = "Mapa base") %>%
  addPolygons( fillColor = "white",      
               fillOpacity = 0.1,        
               color = "gray",           
               weight = 1.5,
               opacity = 0.8,
               popup = ~paste0("<b> Estado: </b>", NOMGEO_nuevo,"<br>",
                              "<b> Num. Géneros: </b>", num_generos,"<br>",
                              "<b> Num. Especies: </b>", num_especies,"<br>",
                              "<b> Num. Registros por estado: </b>", num_registros),
               group = "Limites Estatales")%>%
  addPolygons(fillColor = ~pal_registros(num_registros),
              fillOpacity = 0.8,
              color = "white",
              group = "Registros por estado") %>%
  addPolygons(fillColor = ~pal_generos(num_generos),
              fillOpacity = 0.8,
              color = "white",
              group = "Géneros") %>%
  addPolygons(fillColor = ~pal_especies(num_especies),
              fillOpacity = 0.8,
              color = "white",
              group = "Especies") %>%
  addCircleMarkers( data = coniferas_state,
                    ~decimalLongitude, ~decimalLatitude,
                    radius = 4, 
                    color = "red", 
                    popup = ~paste0("<b> Género: </b>", "<em>", genus,"<em/>", "<br>",
                                    "<b> Especie: </b>", "<em>", species,"</em>", "<br>",
                                    "<b> Imágenes: </b> <a href='", occurrenceID, "' target='_blank' style='color: blue;'>Ver imagen</a>"),
                    group = "Registros individuales") %>%
  addLayersControl(
    baseGroups = c("Mapa base", "Mapa oscuro"),
    overlayGroups = c("Limites Estatales",
                      "Registros por estado",
                      "Géneros",
                      "Especies",
                      "Registros individuales"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  hideGroup(c("Limites Estatales",
              "Registros por estado",
              "Géneros",
              "Especies",
              "Registros individuales"))
  
MapaConiferas

# Exportando el mapa a un archivo HTML

saveWidget(MapaConiferas, file = "temp.html", selfcontained = TRUE)

texto_html <- readLines("temporal.html", warn = FALSE)
texto_html <- gsub("<!DOCTYPE html>", "", texto_html)

writeLines(texto_html, "index.html")
file.remove("temp.html")



