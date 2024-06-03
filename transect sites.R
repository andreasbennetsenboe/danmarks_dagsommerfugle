library(lubridate)
library(tidyverse)
library("geodata")
#library("sp")
library("sf")
library("rgdal")
#library("ggrepel")
library("raster")
library("rgeos")
#library(viridis)
#library(readxl)
install.packages("rgdal")


Dir.Base <- getwd()

denmark <- gadm(country = "DNK", level = 1, path = file.path(Dir.Base, 'Data'))

denmark.sf <- st_as_sf(denmark)

denmark.union <- st_union(denmark.sf)

fau.dist <- st_read(dsn= "faunistiskedistrikter.kml") %>%
  mutate(point = st_centroid(geometry)) %>%
  mutate(names = c("WJ", "SJ","EJ","NEJ","NWJ","B","F","NWZ","NEZ","SZ","LFM"))

coordinates(fau.dist$point)

data.15min <- st_read(dsn = "ebms_15.min.kml")

site.data <- read.csv("ebms_sites.csv") %>%
  dplyr::select(Spatial.Reference, length = "Overall.Length..m.") %>%
  drop_na() %>%
  filter(length > 100)

sites <- site.data %>%
  dplyr::select(Spatial.Reference) %>%
  separate(col = Spatial.Reference, into = c("N", "E"), sep = " ") %>%
  mutate(N = gsub(",$", "", N)) %>%
  mutate(N = gsub("[[:alpha:]]", "", N))%>%
  mutate(E = gsub("[[:alpha:]]", "", E)) %>%
  mutate(N = as.numeric(N),
         E = as.numeric(E))

ggplot(data.15min) +
  geom_sf() +
  stat_sf_coordinates()

ggplot()+
  geom_sf(data =  denmark.union, fill = "snow2", col = "snow3", lwd = 0.2, alpha=0.8)+
  geom_sf(data =  fau.dist, fill = NA, col = "snow4", lwd = 0.2, alpha=0.8)+
  geom_sf_text(data = fau.dist, 
               aes(label = names), 
               color = "black",
               fun.geometry = st_centroid)+
  #geom_point(aes(col = "red"))+
  #geom_sf(data = data.15min, 
  #           stat = "sf_coordinates",
  #        col = "blue", lwd = 2)+
  geom_point(data = sites,
             mapping = aes(x = E, y= N),
             fill= "#869EC2",
             col = "black",
             size = 1.5,
             shape = 21)+ #mørkere blå farve
  theme_classic() +
  labs(x = "", y="") +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank()
  ) + 
  theme(legend.position = c(0.83, 0.88))


