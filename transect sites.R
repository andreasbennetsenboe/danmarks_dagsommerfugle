library("sf")
library("terra")
library(tidyverse)
library(showtext)
library(viridis)

dk.species <- c("Aglais io","Gonepteryx rhamni","Anthocharis cardamines","Pieris napi","Pieris rapae",
                "Coenonympha pamphilus","Aglais urticae","Lycaena phlaeas","Celastrina argiolus",
                "Polyommatus icarus","Aricia agestis","Pieris brassicae","Araschnia levana","Cupido minimus",
                "Ochlodes sylvanus","Maniola jurtina","Melitaea cinxia","Boloria selene","Aporia crataegi",
                "Melitaea athalia","Issoria lathonia","Lycaena hippothoe","Callophrys rubi","Vanessa atalanta",
                "Fabriciana niobe","Cyaniris semiargus","Pararge aegeria","Thymelicus sylvestris",
                "Thymelicus lineola","Satyrinae","Thymelicus","Vanessa cardui","Erynnis tages","Pyrgus malvae",
                "Fabriciana adippe","Plebejus idas","Lycaena virgaureae","Favonius quercus","Satyrium w-album",
                "Argynnis","Hesperiidae","Argynnis paphia","Thecla betulae","Nymphalis antiopa","Lasiommata megera",
                "Sympetrum sanguineum","Colias hyale")

denmark <- readRDS(file= "DNK.rds")
denmark <- unwrap(denmark)

denmark.sf <- st_as_sf(denmark)

denmark.union <- st_union(denmark.sf)

fau.dist <- st_read(dsn= "faunistiskedistrikter.kml") %>%
  mutate(point = st_centroid(geometry)) %>%
  mutate(names = c("WJ", "SJ","EJ","NEJ","NWJ","B","F","NWZ","NEZ","SZ","LFM"))

#data.15min <- st_read(dsn = "ebms_15.min.kml")

site.data <- read.csv("ebms_sites.csv") %>%
  dplyr::select(Spatial.Reference, Transect.ID, length = "Overall.Length..m.") %>%
  drop_na() %>%
  filter(length > 100)

sites <- site.data %>%
  dplyr::select(Spatial.Reference, Transect.ID) %>%
  separate(col = Spatial.Reference, into = c("N", "E"), sep = " ") %>%
  mutate(N = gsub(",$", "", N)) %>%
  mutate(N = gsub("[[:alpha:]]", "", N))%>%
  mutate(E = gsub("[[:alpha:]]", "", E)) %>%
  mutate(N = as.numeric(N),
         E = as.numeric(E))

data.raw <- read.csv(file = "occurences.csv")

data1 <- data.raw %>%
  dplyr::select(1:3, count = Abundance.Count, date = "Date", species = "Preferred.Species.Name")%>%
  filter(species %in% dk.species) %>%
  mutate(date = dmy(date)) %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date)) 

data2 <- data1 %>%
  filter(year == 2024 & month == 5) %>%
  group_by(Transect.ID) %>%
  summarize(count=n_distinct(Transect.Sample.ID)) %>%
  full_join(y = sites, by = "Transect.ID") %>%
  drop_na()

data2023 <- data1 %>%
  filter(year == 2023) %>%
  group_by(Transect.ID) %>%
  summarize(count=n_distinct(Transect.Sample.ID)) %>%
  full_join(y = sites, by = "Transect.ID") %>%
  drop_na()

#ggplot(data.15min) +
#  geom_sf() +
#  stat_sf_coordinates()

font_add_google(name = "Amatic SC", family = "amatic-sc")

showtext_auto()

localities.map <- ggplot()+
  geom_sf(data =  denmark.union, 
          fill = "#f5f5f5", 
          col = "snow3", 
          lwd = 0.5
          )+
  geom_sf(data =  fau.dist, 
          fill = NA, 
          col = "snow4", 
          lwd = 0.5)+
  geom_sf_text(data = fau.dist, 
               aes(label = names), 
               color = "black",
               family = "amatic-sc", 
               fontface = "bold",
               size = 20,
               fun.geometry = st_centroid)+
  geom_point(data = data2023,
             mapping = aes(x = E, y= N),
             size = 1.5,
             col = "#63769E"
  )+
  theme_classic() +
  labs(x = "", y="")+
  #ggtitle("")+
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 100,
                              hjust = 0.5),
    legend.title = element_blank(),
    text = element_text(size = 60,
                        family = "amatic-sc", 
                        face = "bold")
  )
localities.map

effort.map <- ggplot()+
  geom_sf(data =  denmark.union, 
          fill = "#f5f5f5", 
          col = "snow3", 
          lwd = 0.2, 
          alpha=1)+
  geom_sf(data =  fau.dist, 
          fill = NA, 
          col = "snow4", 
          lwd = 0.2, 
          alpha=1)+
  geom_sf_text(data = fau.dist, 
               aes(label = names), 
               color = "black",
               family = "amatic-sc", 
               fontface = "bold",
               size = 20,
               fun.geometry = st_centroid)+
  geom_point(data = data2,
             mapping = aes(x = E, y= N, fill = count),
             size = 3,
             shape = 21
             )+
  scale_fill_viridis(
    option = "A"
  )+
  theme_classic() +
  labs(x = "", y="")+
  ggtitle("Antal ture pr. transekt")+
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 100,
                              hjust = 0.5),
    legend.title = element_blank(),
    text = element_text(size = 60,
                        family = "amatic-sc", 
                        face = "bold")
  )
effort.map

ggsave(filename = "maj_kort.jpg",
       plot = localities.map)

#geom_point(aes(col = "red"))+
#geom_sf(data = data.15min, 
#           stat = "sf_coordinates",
#        col = "blue", lwd = 2)+
