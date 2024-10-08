library(tidyverse)
library(mmtable2)
library(gt)
library(lubridate)
library(showtext)
library(grid)
library(gridExtra)
library("sf")
library("terra")
library(viridis)
library("ggpubr")
library("ggtext")

font_add_google(name = "Amatic SC", family = "amatic-sc")

showtext_auto()

denmark <- readRDS(file= "DNK.rds")
denmark <- unwrap(denmark)
denmark.sf <- st_as_sf(denmark)
denmark.union <- st_union(denmark.sf)

fau.dist <- st_read(dsn= "faunistiskedistrikter.kml") %>%
  mutate(point = st_centroid(geometry)) %>%
  mutate(names = c("WJ", "SJ","EJ","NEJ","NWJ","B","F","NWZ","NEZ","SZ","LFM"))

raw.15min.samples <- read.csv(file = "data/download.timed-count.sample.data.csv")

raw.15min.obs <- read.csv(file = "data/download.timed-count.occurences.csv")

raw.15min.obs.ss <- read.csv(file = "data/download.single-species-timed-count.occurences.csv")

raw.15min.samples$Geom <- st_as_sfc(raw.15min.samples$Geom, crs = 4326)

raw.15min.samples <- st_as_sf(raw.15min.samples)

centroids <- raw.15min.samples %>%
  st_centroid()

map <- ggplot()+
  geom_sf(data =  denmark.union, 
          fill = "#f5f5f5", 
          col = "snow3", 
          lwd = 0.3, 
          alpha=1)+
  geom_sf(data =  fau.dist, 
          fill = NA, 
          col = "snow4", 
          lwd = 0.3, 
          alpha=1)+
  geom_sf_text(data = fau.dist, 
               aes(label = names), 
               color = "black",
               family = "amatic-sc", 
               fontface = "bold",
               size = 20,
               fun.geometry = st_centroid)+
  geom_sf(data = centroids,
             fill = "#5F7394", #fill = visits),
             size = 1.5,
             shape = 21
  )+
  labs(x = "", y="")+
  theme_classic()+
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(family = "amatic-sc", 
                              face = "bold",
                              size = 100,
                              hjust = 0.5)
  )+
  theme(
    legend.title = element_blank(),
    legend.justification.inside = c(0.7,0.7),
    legend.text = element_text(size = 40,
                               family = "amatic-sc",
                               face = "bold"
    )
  )


ggsave(filename = "output/15min.map.jpg",
       plot = map,
       width = 8,
       height = 6)
