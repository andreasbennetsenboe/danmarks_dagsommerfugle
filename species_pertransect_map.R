library(tidyverse)
library(lubridate)
library(showtext)
library("sf")
library("terra")

year.of.interest <- 2025


sites <- read.csv("data/download.site.details.csv") %>%
  dplyr::select(Spatial.Reference, Transect.ID, Transect.Name, length = "Overall.Length..m.") %>%
  drop_na() %>%
  filter(length > 100) %>%
  dplyr::select(Spatial.Reference, Transect.Name) %>%
  separate(col = Spatial.Reference, into = c("N", "E"), sep = " ") %>%
  mutate(N = gsub(",$", "", N)) %>%
  mutate(N = gsub("[[:alpha:]]", "", N))%>%
  mutate(E = gsub("[[:alpha:]]", "", E)) %>%
  mutate(N = as.numeric(N),
         E = as.numeric(E))

visit.info <- read.csv(file = "data/download.species.occurences.from.transects.csv") %>%
  filter(Record.status == "Accepted" & Reliability == "1 Suitable conditions") %>%
  select(Transect.Name, sample = "Transect.Sample.ID", date = "Date") %>%
  mutate(date = dmy(date),
         Transect.Name = as.factor(Transect.Name)) %>%
  mutate(year = year(date)) %>%
  select(-date) %>%
  unique() %>%
  filter(year == year.of.interest) %>%
  group_by(Transect.Name) %>%
  summarize(
    visits = n()
  )

transect.data <- read.csv(file = "data/download.species.occurences.from.transects.csv") %>%
  filter(Record.status == "Accepted") %>%
  dplyr::select(1:3, Transect.Name, count = Abundance.Count, date = "Date", species = "Preferred.Species.Name") %>%
  filter(species %in% read_lines(file = "data/butterfly.taxons.txt")) %>%
  mutate(date = dmy(date),
         count = as.numeric(count)
  ) %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>%
  mutate(DOY = yday(date)) %>%
  filter(year == year.of.interest) %>%
  group_by(Transect.Name) %>%
  summarise(n_species = n_distinct(species)) %>%
  left_join(sites, by = "Transect.Name") %>%
  left_join(visit.info, by = "Transect.Name") %>%
  filter(visits >= 3)


#font_add_google(name = "Amatic SC", family = "amatic-sc")
#showtext_auto()

denmark <- readRDS(file= "DNK.rds")
denmark <- unwrap(denmark)
denmark.sf <- st_as_sf(denmark)
denmark.union <- st_union(denmark.sf)

fau.dist <- st_read(dsn= "faunistiskedistrikter.kml") %>%
  mutate(point = st_centroid(geometry)) %>%
  mutate(names = c("WJ", "SJ","EJ","NEJ","NWJ","B","F","NWZ","NEZ","SZ","LFM"))

effort.map <- ggplot()+
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
               size = 10,
               fun.geometry = st_centroid)+
  geom_point(data = transect.data,
             mapping = aes(x = E, y= N, col = n_species),
             size = 1.6,
             shape = 16
  ) +
  scale_color_viridis_c(option = "magma")+
  labs(x = "", y="")+
  ggtitle("Antal arter pr. transekt")+
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
  )#+ 
  #guides(colour = guide_legend(ncol = 1))

ggsave(filename = paste0("output/", year.of.interest, ".kort.jpg"),
       plot = effort.map,
       width = 8,
       height = 6)
