library(raster)
library(sf)
library(ggspatial)
library(sp)
library(ggplot2)
library(rnaturalearth)                                   #Continentes
library(rnaturalearthdata)                               #Continentes especifico

world        <- ne_countries(scale= "small", returnclass = "sf") # Continentes del mundo
world.SA     <- subset(world, continent=="Europe")     # Sur America
Russia       <- subset(world.SA, sovereignt == "Russia")  # Seleccionamos por Pais
Ukraine       <- subset(world.SA, sovereignt == "Ukraine") 
Russi     <- getData('GADM', country='Russia', level=1) %>%st_as_sf() 
Ukrain    <- getData('GADM', country='Ukraine', level=1) %>%st_as_sf()
BIELORRUSIA<- getData('GADM', country='Belarus', level=1) %>%st_as_sf()
Romania   <- getData('GADM', country='Romania', level=1) %>%st_as_sf()
Moldova   <- getData('GADM', country='Moldova', level=1) %>%st_as_sf()
Crimea     <- subset(Ukrain, NAME_1 == "Crimea") 

Ukrai    <- getData('GADM', country='Ukraine', level=0) %>%st_as_sf()
BIELORRUSI<- getData('GADM', country='Belarus', level=0) %>%st_as_sf()
Romani   <- getData('GADM', country='Romania', level=0) %>%st_as_sf()
Moldov   <- getData('GADM', country='Moldova', level=0) %>%st_as_sf()

Donets     <- subset(Ukrain, NAME_1 == "Donets'k")
Luhans     <- subset(Ukrain, NAME_1 == "Luhans'k")
Luhans_Per       = st_as_sfc(st_bbox(Luhans))


Per=ggplot()+
  geom_sf(data = world.SA, fill="white", color="white")+
  geom_sf(data = Ukrain, fill="gold", color="gold")+
  geom_sf(data = Luhans_Per, fill=NA, color="black")+
  coord_sf(xlim = c(-10,60), ylim = c(30,70),expand = FALSE)+
  theme_void()+
  theme(panel.background = element_rect(fill = "lightskyblue"))
Peru.grob  <- ggplotGrob(Per)
  
Mapas= ggplot()+
  geom_sf(data = Ukrain, fill="navajowhite1", color="gold")+
  geom_sf(data = Russi, fill="gray85", color="white")+
  geom_sf(data = Crimea, fill="gray85", color="white")+
  geom_sf(data = BIELORRUSIA, fill="gray85", color="white")+
  geom_sf(data = Romania , fill="gray85", color="white")+
  geom_sf(data = Moldova , fill="gray85", color="white")+
  geom_sf(data = Donets, fill="steelblue", color="white")+
  geom_sf(data = Luhans, fill="steelblue", color="white")+
  geom_sf(data = Ukrai, fill=NA, color="Black")+
  geom_sf(data = BIELORRUSI, fill=NA, color="Black")+
  geom_sf(data = Romani, fill=NA, color="Black")+
  geom_sf(data = Moldov, fill=NA, color="Black")+
  coord_sf(xlim = c(22.14045,43), ylim = c(44.38597,52.37503),expand = FALSE)+
  theme_void()+
  theme(panel.background = element_rect(fill = "lightsteelblue2"),
        panel.border = element_rect( color = "black", fill = NA, size = 1))+
  annotation_custom(Peru.grob, xmin = 39, xmax = 42.8, ymin =50, ymax=52.2)+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  annotate(geom = "text", x = 38, y = 51, label = "RUSIA",family = "serif", color = "Black", size = 4, face = "bold")+
  annotate(geom = "text", x = 29, y = 52, label = "BIELORRUSIA", family = "serif", color = "Black", size = 4, face = "bold")+
  annotate(geom = "text", x = 25, y = 46, label = "RUMANIA", family = "serif", color = "Black", size = 4, face = "bold")+
  annotate(geom = "text", x = 28, y = 47, label = "MOLDAVIA", family = "serif", color = "Black", size = 4, face = "bold")+
  annotate(geom = "text", x = 33, y = 49, label = "UCRANIA", family = "serif", color = "Black", size = 4, face = "bold")+
  annotate(geom = "text", x = 39, y = 49, label = "Lugansk", family = "serif", color = "Black", size = 4, face = "bold")+
  annotate(geom = "text", x = 38, y = 48, label = "Donetsk", family = "serif", color = "Black", size = 4, face = "bold")+
  ggtitle("")+
  labs(subtitle = "Ing. Gory Florez Castillo",
       caption = 'Elaborado en R')

ggsave("Mapas/Ucranea1.png", Mapas, width = 11.76, height = 8, 
       dpi = 900, type = "cairo-png")
  
  
  
  
  

