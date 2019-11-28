######################################################################
#Mapas de violencia contra la mujer en la CDMX
#Elaborado por: Gatitos Contra la Desigualdad

######################################################################


##############
#Configuración
rm(list = ls())

rm(list = ls())
library(pacman)
# Abrimos las paqueterías con un sólo comando:
p_load(haven, readr, readxl, ggplot2, shiny, tidyverse, knitr,gmodels,foreign,expss,fishualize)

##############
#Paqueterías para mapear
library(sf)
#install.packages("ggspatial")
library("ggspatial")
theme_set(theme_bw())
library("colorspace")
library("magick") # this is call to animate/read pngs
#hcl_palettes(plot = TRUE)
p_load(rstudioapi, lintr, raster, viridis, cowplot, rmarkdown)
sessionInfo()

#remove.packages("ggmap")
require("devtools")
#devtools::install_github("dkahle/ggmap")
library(ggmap)
library(devtools)



##############
#Directorio de trabajo
setwd ("~/Documents/Data Science/Repos/2019B/Gatitos Contra La Desigualdad/violencia_CDMX")
##############

#Función para determinar theme de las gráficas
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Verdana",
                          color = "#939486"),
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_line(color = "#F5F5F3", size = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = "#F5F5F3",
                                     color = NA),
      panel.background = element_rect(fill = "#F5F5F3",
                                      color = NA),
      legend.background = element_rect(fill = "#F5F5F3",
                                       color = NA),
      # borders and margins
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9, hjust = 0,
                                 color = "#939486"),
      plot.title = element_text(size = 15, hjust = 0.5,
                                color = "#4B4C47"),
      plot.subtitle = element_text(size = 10, hjust = 0.5,
                                   color = "#939486",
                                   margin = margin(b = -0.1,
                                                   t = -0.1,
                                                   l = 2,
                                                   unit = "cm"),
                                   debug = F),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...
    )
}



################################################
################################################

#Previos

################################################
################################################


################################################
#1. AGEBs para índice de marginación
################################################

ageb_cdmx <- st_read("~/Documents/Encuestas/Shape/Ageb y mas 2010/shps/df/df_ageb_urb.shp")

#Crear variable para filtrar por municipios de interés
ageb_cdmx$mun <- substr(ageb_cdmx$cvegeo, 3, 5)
#Filtrar a CDMX
# ageb_jal <- ageb_jal %>% 
#   filter(mun=="039")

#Seleccionar variables de interés
ageb_cdmx <- ageb_cdmx %>% 
  dplyr::select(cvegeo, mun)

##############
#Datos complementarios
#Leer los datos complementarios
datos_complemen <- read_dta("/Users/maximoernestojaramillomolina/Documents/Encuestas/Shape/Ageb y mas 2010/shps/df/df_ageb_urb.dta")

##Cuantiles de marginación
#####################

# ¿Cuántas clases quiero?
no_classes <- 4
# Extraer cuantiles
qPC1 <- datos_complemen %>%
  pull(i_marg) %>%
  quantile(na.rm=T, probs = seq(0, 1, length.out = no_classes + 1)) %>%
  as.vector() # to remove names of quantiles, so idx below is numeric
# Así se crean las etiquetas
labels <- c("Muy baja", "Baja", "Media", "Media Alta")

# Se elimina la última etiqueta
# En caso contrario sería hasta NA
# labels <- labels[1:length(labels) - 1]
labels
# Crear la variable
#manz_complemen %<>%
datos_complemen <- datos_complemen %>%
  mutate(q = cut(i_marg,
                 breaks = qPC1,
                 labels = labels,
                 include.lowest = T))

# Unir bases
ageb_complemen<-merge(x=ageb_cdmx,      y=datos_complemen, by="cvegeo")

table(ageb_complemen$q)


################################################
#2. Capa municipios
################################################

##############
#Municipios
mun_cdmx <- st_read("~/Documents/Data Science/Repos/2019B/Mapas/30DaysMapChallenge/Day 8. Trabajo de cuidados CDMX/df_municipio.shp")
#Crear variable para filtrar por municipios de interés
mun_cdmx$mun <- substr(mun_cdmx$CVEGEO, 3, 5)
#Filtrar municipios
# mun_cdmx <- mun_cdmx %>% 
#   filter(mun=="002")



################################################
#3. Capa raster de fondo terrain
################################################

#register_google(key="")

s <- "element:geometry%7Ccolor:0xf5f5f5&style=element:labels%7Cvisibility:off&style=element:labels.icon%7Cvisibility:off&style=element:labels.text.fill%7Ccolor:0x616161&style=element:labels.text.stroke%7Ccolor:0xf5f5f5&style=feature:administrative%7Celement:geometry%7Cvisibility:off&style=feature:administrative.country%7Celement:geometry.stroke%7Ccolor:0x000000%7Cvisibility:on&style=feature:administrative.land_parcel%7Cvisibility:off&style=feature:administrative.land_parcel%7Celement:labels.text.fill%7Ccolor:0xbdbdbd&style=feature:administrative.neighborhood%7Cvisibility:off&style=feature:poi%7Cvisibility:off&style=feature:poi%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:poi%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:poi.park%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:poi.park%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:road%7Cvisibility:off&style=feature:road%7Celement:geometry%7Ccolor:0xffffff&style=feature:road%7Celement:labels.icon%7Cvisibility:off&style=feature:road.arterial%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:road.highway%7Celement:geometry%7Ccolor:0xdadada&style=feature:road.highway%7Celement:labels.text.fill%7Ccolor:0x616161&style=feature:road.local%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:transit%7Cvisibility:off&style=feature:transit.line%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:transit.station%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:water%7Celement:geometry%7Ccolor:0xc9c9c9&style=feature:water%7Celement:labels.text.fill%7Ccolor:0x4BA9C5&size=480x360"

# Get the basemap
cdmx <- get_googlemap(
  c(lon=-99.1276627,lat=19.4284706),
  zoom = 10, crop=T, 
  scale = 4, 
  #color="bw", 
  maptype="terrain", # can change to terrain
  style = s)

ggmap(cdmx)  

gg <- ggmap(cdmx) 







################################################
################################################

#Datos de violencia

################################################
################################################


##############
#abrir base de datos con todos los datos de delitos:
#https://datos.cdmx.gob.mx/explore/dataset/victimas-en-carpetas-de-investigacion-pgj/export/?refine.ano_hecho=2019&refine.tipopersona=FISICA&refine.sexo=Femenino
carp_invest <- read.csv(file = "~/Documents/Encuestas/Violencia/ADIP/victimas-en-carpetas-de-investigacion-pgj.csv", sep=";")
table(carp_invest$Sexo)
carp_invest <- carp_invest%>%
  filter(Sexo=="Femenino" & Año_hecho==2019 )

table(carp_invest$Delito)

table(carp_invest$Mes_hecho)






#Ejemplo: ABUSO SEXUAL  ->    2,840  -> 9.3 x dia, -> 1 denuncia cada 2.5 horas
abuso <- carp_invest %>%
  filter(Delito== "ABUSO SEXUAL")

gg + 
  # Agrego la capa principal
  geom_sf(data = ageb_complemen,
          mapping = aes(
            fill = q
          ),
          color = "black",
          size = 0.1,
          alpha=.4,
          inherit.aes =FALSE
  ) +
  # Viridis color scale
  scale_fill_manual(values=c("#FCFF6C","#D7FFAB","#D89D6A","#6D454C")) + 
  #scale_fill_manual(values=c("#140F2D","#3F88C5","#F49D37","#D72638")) +
  
  # Utilizar un borde más grueso para los municipios
  geom_sf(
    data = mun_cdmx,
    fill = "transparent",
    color = "black",
    size = 0.6,
    alpha=0,
    inherit.aes =FALSE
  ) +
  
  #Etiquetas de municipios
#geom_label(data = mun_jal, aes(X, Y, label = NOMBRE), size = 1.5, fontface = "bold")+ 

  # Capa de delitos
  geom_point(data=abuso, aes(x=lon, y=lat), 
             size=1, 
             alpha=0.5,
             inherit.aes = FALSE,
             color="black")+
  
  # Viridis color scale
  scale_color_manual(values=c("black")) +

  # Agregar títulos
  labs(x = NULL,
       y = NULL,
       title = "Mapa del delito: Abuso sexual (2,840)",
       subtitle = "con víctimas mujeres, 2019.
       ",
       caption = "Fuente: Elaborado por @GatitosVsDesig con datos de ADIP y CPV-2010 de INEGI.",
       fill = "Marginación")+
       #color = "Margniación") +
  # Hacer un pequeño zoom
  coord_sf(
    xlim = c(-99.35, -98.94), 
    ylim = c(19.16,19.63), 
    expand = FALSE) +
  # Finalmente, agregar theme
  theme_map()

#Guardar el mapa
ggsave("puntos/abuso.png", width = 7)




#Ejemplo: ACOSO SEXUAL   ->  669 -> 2 denuncias por día
acoso <- carp_invest %>%
  filter(Delito== "ACOSO SEXUAL")

gg + 
  
  # Agrego la capa principal
  geom_sf(data = ageb_complemen,
          mapping = aes(
            fill = q
          ),
          color = "black",
          size = 0.1,
          alpha=.4,
          inherit.aes =FALSE
  ) +
  # Viridis color scale
  scale_fill_manual(values=c("#FCFF6C","#D7FFAB","#D89D6A","#6D454C")) + 
  #scale_fill_manual(values=c("#140F2D","#3F88C5","#F49D37","#D72638")) +
  
  # Utilizar un borde más grueso para los municipios
  geom_sf(
    data = mun_cdmx,
    fill = "transparent",
    color = "black",
    size = 0.75,
    alpha=0,
    inherit.aes =FALSE
  ) +
  
  #Etiquetas de municipios
  #geom_label(data = mun_jal, aes(X, Y, label = NOMBRE), size = 1.5, fontface = "bold")+ 
  
  # Capa de delitos
  geom_point(data=acoso, aes(x=lon, y=lat), 
             size=1, 
             alpha=0.5,
             inherit.aes = FALSE,
             color="black")+
  
  # Viridis color scale
  scale_color_manual(values=c("black")) +
  
  # Agregar títulos
  labs(x = NULL,
       y = NULL,
       title = "Mapa del delito: Acoso sexual (669)",
       subtitle = "con víctimas mujeres, 2019.
       ",
       caption = "Fuente: Elaborado por @GatitosVsDesig con datos de ADIP y CPV-2010 de INEGI.",
       fill = "Marginación")+
       #color = "Margniación") +
  # Hacer un pequeño zoom
  coord_sf(
    xlim = c(-99.35, -98.94), 
    ylim = c(19.16,19.63), 
    expand = FALSE) +
  # Finalmente, agregar theme
  theme_map()

#Guardar el mapa
ggsave("puntos/acoso.png", width = 7)







#Ejemplo: FEMINICIDIO  -> 50    -> una carp de inv por fem cada 6 días
feminicidio <- carp_invest %>%
  filter(Delito== "FEMINICIDIO")

gg + 
  # Agrego la capa principal
  geom_sf(data = ageb_complemen,
          mapping = aes(
            fill = q
          ),
          color = "black",
          size = 0.1,
          alpha=.4,
          inherit.aes =FALSE
  ) +
  # Viridis color scale
  scale_fill_manual(values=c("#FCFF6C","#D7FFAB","#D89D6A","#6D454C")) + 
  #scale_fill_manual(values=c("#140F2D","#3F88C5","#F49D37","#D72638")) +
  
  # Utilizar un borde más grueso para los municipios
  geom_sf(
    data = mun_cdmx,
    fill = "transparent",
    color = "black",
    size = 0.75,
    alpha=0,
    inherit.aes =FALSE
  ) +
  
  #Etiquetas de municipios
  #geom_label(data = mun_jal, aes(X, Y, label = NOMBRE), size = 1.5, fontface = "bold")+ 
  
  # Capa de delitos
  geom_point(data=feminicidio, aes(x=lon, y=lat), 
             size=2, 
             alpha=0.8,
             inherit.aes = FALSE,
             color="black")+
  
  # Viridis color scale
  scale_color_manual(values=c("black")) +
  
  # Agregar títulos
  labs(x = NULL,
       y = NULL,
       title = "Mapa del delito: Feminicidio (50)",
       subtitle = "con víctimas mujeres, 2019.
       ",
       caption = "Fuente: Elaborado por @GatitosVsDesig con datos de ADIP y CPV-2010 de INEGI.",
       fill = "Marginación")+
       #color = "Margniación") +
  # Hacer un pequeño zoom
  coord_sf(
    xlim = c(-99.35, -98.94), 
    ylim = c(19.16,19.63), 
    expand = FALSE) +
  # Finalmente, agregar theme
  theme_map()

#Guardar el mapa
ggsave("puntos/feminicidio.png", width = 7)




 


#Ejemplo: violencia familiar -> 16,260   -> 53 carp de inv. por día
violencia <- carp_invest %>%
  filter(Delito== "VIOLENCIA FAMILIAR")

gg + 
  # Agrego la capa principal
  geom_sf(data = ageb_complemen,
          mapping = aes(
            fill = q
          ),
          color = "black",
          size = 0.1,
          alpha=.4,
          inherit.aes =FALSE
  ) +
  # Viridis color scale
  scale_fill_manual(values=c("#FCFF6C","#D7FFAB","#D89D6A","#6D454C")) + 
  #scale_fill_manual(values=c("#140F2D","#3F88C5","#F49D37","#D72638")) +
  
  # Utilizar un borde más grueso para los municipios
  geom_sf(
    data = mun_cdmx,
    fill = "transparent",
    color = "black",
    size = 0.75,
    alpha=0,
    inherit.aes =FALSE
  ) +
  
  #Etiquetas de municipios
  #geom_label(data = mun_jal, aes(X, Y, label = NOMBRE), size = 1.5, fontface = "bold")+ 
  
  # Capa de delitos
  geom_point(data=violencia, aes(x=lon, y=lat), 
             size=1, 
             alpha=0.15,
             inherit.aes = FALSE,
             color="black")+
  
  # Viridis color scale
  scale_color_manual(values=c("black")) +
  
  # Agregar títulos
  labs(x = NULL,
       y = NULL,
       title = "Mapa del delito: Violencia Familiar (16,260)",
       subtitle = "con víctimas mujeres, 2019.
       ",
       caption = "Fuente: Elaborado por @GatitosVsDesig con datos de ADIP y CPV-2010 de INEGI.",
       fill = "Marginación")+
  #color = "Margniación") +
  # Hacer un pequeño zoom
  coord_sf(
    xlim = c(-99.35, -98.94), 
    ylim = c(19.16,19.63), 
    expand = FALSE) +
  # Finalmente, agregar theme
  theme_map()

#Guardar el mapa
ggsave("puntos/violencia_fam.png", width = 7)







#Ejemplo: VIOLACION  -> 1,012   ->   3.3 denuncias por día
violacion <- carp_invest %>%
  filter(Delito %in% c("VIOLACION", "VIOLACION EQUIPARADA", "VIOLACION EQUIPARADA POR CONOCIDO", 
                    "VIOLACION TUMULTUARIA","VIOLACION TUMULTUARIA EQUIPARADA", "VIOLACION TUMULTUARIA EQUIPARADA POR CONOCIDO"))

gg + 
  # Agrego la capa principal
  geom_sf(data = ageb_complemen,
          mapping = aes(
            fill = q
          ),
          color = "black",
          size = 0.1,
          alpha=.4,
          inherit.aes =FALSE
  ) +
  # Viridis color scale
  scale_fill_manual(values=c("#FCFF6C","#D7FFAB","#D89D6A","#6D454C")) + 
  #scale_fill_manual(values=c("#140F2D","#3F88C5","#F49D37","#D72638")) +
  
  # Utilizar un borde más grueso para los municipios
  geom_sf(
    data = mun_cdmx,
    fill = "transparent",
    color = "black",
    size = 0.75,
    alpha=0,
    inherit.aes =FALSE
  ) +
  
  #Etiquetas de municipios
  #geom_label(data = mun_jal, aes(X, Y, label = NOMBRE), size = 1.5, fontface = "bold")+ 
  
  # Capa de delitos
  geom_point(data=violacion, aes(x=lon, y=lat), 
             size=1, 
             alpha=0.7,
             inherit.aes = FALSE,
             color="black")+
  
  # Viridis color scale
  scale_color_manual(values=c("black")) +
  
  # Agregar títulos
  labs(x = NULL,
       y = NULL,
       title = "Mapa del delito: Violación (1,012)",
       subtitle = "con víctimas mujeres, 2019.
       ",
       caption = "Fuente: Elaborado por @GatitosVsDesig con datos de ADIP y CPV-2010 de INEGI.",
       fill = "Marginación")+
  #color = "Margniación") +
  # Hacer un pequeño zoom
  coord_sf(
    xlim = c(-99.35, -98.94), 
    ylim = c(19.16,19.63), 
    expand = FALSE) +
  # Finalmente, agregar theme
  theme_map()

#Guardar el mapa
ggsave("puntos/violacion.png", width = 7)






#Ejemplo: TRATA DE PERSONAS  -> 92    -> cada 3.3 días hay una denuncia de trata 
trata <- carp_invest %>%
  filter(Delito== "TRATA DE PERSONAS")

gg + 
  # Agrego la capa principal
  geom_sf(data = ageb_complemen,
          mapping = aes(
            fill = q
          ),
          color = "black",
          size = 0.1,
          alpha=.4,
          inherit.aes =FALSE
  ) +
  # Viridis color scale
  scale_fill_manual(values=c("#FCFF6C","#D7FFAB","#D89D6A","#6D454C")) + 
  #scale_fill_manual(values=c("#140F2D","#3F88C5","#F49D37","#D72638")) +
  
  # Utilizar un borde más grueso para los municipios
  geom_sf(
    data = mun_cdmx,
    fill = "transparent",
    color = "black",
    size = 0.75,
    alpha=0,
    inherit.aes =FALSE
  ) +
  
  #Etiquetas de municipios
  #geom_label(data = mun_jal, aes(X, Y, label = NOMBRE), size = 1.5, fontface = "bold")+ 
  
  # Capa de delitos
  geom_point(data=trata, aes(x=lon, y=lat), 
             size=2, 
             alpha=0.7,
             inherit.aes = FALSE,
             color="black")+
  
  # Viridis color scale
  scale_color_manual(values=c("black")) +
  
  # Agregar títulos
  labs(x = NULL,
       y = NULL,
       title = "Mapa del delito: Trata de Personas (92)",
       subtitle = "con víctimas mujeres, 2019.
       ",
       caption = "Fuente: Elaborado por @GatitosVsDesig con datos de ADIP y CPV-2010 de INEGI.",
       fill = "Marginación")+
  #color = "Margniación") +
  # Hacer un pequeño zoom
  coord_sf(
    xlim = c(-99.35, -98.94), 
    ylim = c(19.16,19.63), 
    expand = FALSE) +
  # Finalmente, agregar theme
  theme_map()

#Guardar el mapa
ggsave("puntos/trata.png", width = 7)






#Ejemplo: PORNOGRAFIA INFANTIL  -> 79 
pornografia_inf <- carp_invest %>%
  filter(Delito== "PORNOGRAFIA INFANTIL")

gg + 
  # Agrego la capa principal
  geom_sf(data = ageb_complemen,
          mapping = aes(
            fill = q
          ),
          color = "black",
          size = 0.1,
          alpha=.4,
          inherit.aes =FALSE
  ) +
  # Viridis color scale
  scale_fill_manual(values=c("#FCFF6C","#D7FFAB","#D89D6A","#6D454C")) + 
  #scale_fill_manual(values=c("#140F2D","#3F88C5","#F49D37","#D72638")) +
  
  # Utilizar un borde más grueso para los municipios
  geom_sf(
    data = mun_cdmx,
    fill = "transparent",
    color = "black",
    size = 0.75,
    alpha=0,
    inherit.aes =FALSE
  ) +
  
  #Etiquetas de municipios
  #geom_label(data = mun_jal, aes(X, Y, label = NOMBRE), size = 1.5, fontface = "bold")+ 
  
  # Capa de delitos
  geom_point(data=pornografia_inf, aes(x=lon, y=lat), 
             size=2, 
             alpha=0.7,
             inherit.aes = FALSE,
             color="black")+
  
  # Viridis color scale
  scale_color_manual(values=c("black")) +
  
  # Agregar títulos
  labs(x = NULL,
       y = NULL,
       title = "Mapa del delito: Pornografía infantil (79)",
       subtitle = "con víctimas mujeres, 2019.
       ",
       caption = "Fuente: Elaborado por @GatitosVsDesig con datos de ADIP y CPV-2010 de INEGI.",
       fill = "Marginación")+
  #color = "Margniación") +
  # Hacer un pequeño zoom
  coord_sf(
    xlim = c(-99.35, -98.94), 
    ylim = c(19.16,19.63), 
    expand = FALSE) +
  # Finalmente, agregar theme
  theme_map()

#Guardar el mapa
ggsave("puntos/pornografia_inf.png", width = 7)



#-> sumando feminicidios serían 217 -> homicidio de una mujer cada 33 horas

#Ejemplo: HOMICIDIO  -> 167    ->   un homicidio a mujer (no claf como feminicidio) cada  2 días

homicidio <- carp_invest %>%
  filter(Delito %in% c("HOMICIDIO CULPOSO", "HOMICIDIO CULPOSO POR ARMA DE FUEGO", "HOMICIDIO POR AHORCAMIENTO", 
                       "HOMICIDIO POR ARMA BLANCA","HOMICIDIO POR ARMA DE FUEGO", "HOMICIDIO POR GOLPES", "HOMICIDIOS INTENCIONALES (OTROS)"))


gg + 
  # Agrego la capa principal
  geom_sf(data = ageb_complemen,
          mapping = aes(
            fill = q
          ),
          color = "black",
          size = 0.1,
          alpha=.4,
          inherit.aes =FALSE
  ) +
  # Viridis color scale
  scale_fill_manual(values=c("#FCFF6C","#D7FFAB","#D89D6A","#6D454C")) + 
  #scale_fill_manual(values=c("#140F2D","#3F88C5","#F49D37","#D72638")) +
  
  # Utilizar un borde más grueso para los municipios
  geom_sf(
    data = mun_cdmx,
    fill = "transparent",
    color = "black",
    size = 0.75,
    alpha=0,
    inherit.aes =FALSE
  ) +
  
  #Etiquetas de municipios
  #geom_label(data = mun_jal, aes(X, Y, label = NOMBRE), size = 1.5, fontface = "bold")+ 
  
  # Capa de delitos
  geom_point(data=homicidio, aes(x=lon, y=lat), 
             size=2, 
             alpha=0.7,
             inherit.aes = FALSE,
             color="black")+
  
  # Viridis color scale
  scale_color_manual(values=c("black")) +
  
  # Agregar títulos
  labs(x = NULL,
       y = NULL,
       title = "Mapa del delito: Homicidios (167)",
       subtitle = "con víctimas mujeres, 2019.
       ",
       caption = "Fuente: Elaborado por @GatitosVsDesig con datos de ADIP y CPV-2010 de INEGI.
       Nota: Incluye Homicidio culposo, arma de fuego, por ahorcamiento, por arma blanca, por golpes, e intencionales (otros)",
       fill = "Marginación")+
  #color = "Margniación") +
  # Hacer un pequeño zoom
  coord_sf(
    xlim = c(-99.35, -98.94), 
    ylim = c(19.16,19.63), 
    expand = FALSE) +
  # Finalmente, agregar theme
  theme_map()

#Guardar el mapa
ggsave("puntos/homicidios.png", width = 7)






gg + 
  coord_cartesian() +
  #Capa delitos con hexagnos
  geom_hex(
    data=abuso, 
    aes(x=lon, y=lat), 
    # fill = cut(..value.., c(0, 100, 250, 500, 1000,
    #                         1500, 2000, 2500, Inf))),
    colour = NA, 
    bins = 30, 
    alpha = 0.35,
    inherit.aes =FALSE) 
  # Utilizar un borde más grueso para los municipios
geom_sf(
    data = mun_cdmx,
    fill = "transparent",
    color = "black",
    size = 0.75,
    alpha=.6,
    inherit.aes =FALSE
  ) +
  # Agrego la capa principal
  geom_sf(data = ageb_complemen,
          mapping = aes(
            fill = q
          ),
          color = "black",
          size = 0.1,
          alpha=.6,
          inherit.aes =FALSE
  ) +
  # Viridis color scale
  scale_fill_manual(values=c("#FCFF6C","#D7FFAB","#D89D6A","#6D454C")) +
  #scale_fill_manual(values=c("#140F2D","#3F88C5","#F49D37","#D72638")) +
  
  #Etiquetas de municipios
  #geom_label(data = mun_jal, aes(X, Y, label = NOMBRE), size = 1.5, fontface = "bold")+ 
  

  
  #Colores para hexagonos
  scale_fill_viridis(
    option = "viridis",
    name =  "",
    alpha = 0.8,
    begin = 0,
    end = 1,
    discrete = F, # discrete classes
    direction = -1 # oscuro es el más alto, claro/amarillo el más bajo
  ) +
  # Agregar títulos
  labs(x = NULL,
       y = NULL,
       title = "Mapa del delito: Abuso sexual",
       subtitle = "con víctimas mujeres, 2019.
       ",
       caption = "Fuente: Elaborado por @GatitosVsDesig con datos de ADIP y CPV-2010 de INEGI.",
       #fill = "Margniación",
       color = "Margniación") +
  # Hacer un pequeño zoom
  coord_sf(
    xlim = c(-99.35, -98.94), 
    ylim = c(19.16,19.63), 
    expand = FALSE) +
  # Finalmente, agregar theme
  theme_map()





 

