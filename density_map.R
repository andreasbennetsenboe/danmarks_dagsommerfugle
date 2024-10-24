library(tidyverse)
library(lubridate)
library(showtext)
library("sf")
library("terra")
library("wesanderson")
#library(viridis)

sites <- read.csv("data/download.site.details.csv") %>%
  dplyr::select(Spatial.Reference, Transect.ID, Transect.Name, transect.length = "Overall.Length..m.") %>%
  drop_na() %>%
  filter(transect.length > 100) %>%
  dplyr::select(Spatial.Reference, Transect.ID, transect.length) %>%
  separate(col = Spatial.Reference, into = c("N", "E"), sep = " ") %>%
  mutate(N = gsub(",$", "", N)) %>%
  mutate(N = gsub("[[:alpha:]]", "", N))%>%
  mutate(E = gsub("[[:alpha:]]", "", E)) %>%
  mutate(N = as.numeric(N),
         E = as.numeric(E))

transect.data <- read.csv(file = "data/download.species.occurences.from.transects.csv") %>%
  filter(Record.status == "Accepted") %>%
  dplyr::select(1:3, Transect.Name, count = Abundance.Count, date = "Date", species = "Preferred.Species.Name") %>%
  mutate(date = dmy(date)) %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>%
  mutate(DOY = yday(date)) %>%
  filter(year == 2024) %>%
  left_join(y = sites, by = "Transect.ID") %>%
  drop_na(N)

denmark <- readRDS(file= "DNK.rds")

denmark <- unwrap(denmark) %>%
  st_as_sf() %>%
  st_union() %>%
  st_transform(crs = 25832)

grid <- st_read(dsn = "data/UTM/dk_10km.shp") %>%
  st_transform(crs = 25832) %>%
  filter(st_intersects(., denmark, sparse = FALSE))

pal <- wes_palette("Zissou1", 6, type = "continuous")
my_colors <- c(pal[1], pal[3], pal[4], pal[5], pal[6])

#font_add_google(name = "Amatic SC", family = "amatic-sc")
#showtext_auto()

flyvetid <- data.frame(
  urticae       <- c("Aglais urticae",        "2024-06-01", "2024-09-30", "urticae"),
  cinxia        <- c("Melitaea cinxia",       "2024-05-15", "2024-06-30", "cinxia"),
  jurtina       <- c("Maniola jurtina",       "2024-06-05", "2024-08-20", "jurtina"),
  hyperantus    <- c("Aphantopus hyperantus", "2024-06-15", "2024-08-05", "hyperantus"),
  icarus.1gen   <- c("Polyommatus icarus",    "2024-05-15", "2024-06-01", "icarus.1gen"),
  icarus.2gen   <- c("Polyommatus icarus",    "2024-07-15", "2024-09-05", "icarus.2gen"),
  phlaeas.1gen  <- c("Lycaena phlaeas",       "2024-04-25", "2024-06-15", "phlaeas.1gen"),
  phlaeas.2gen  <- c("Lycaena phlaeas",       "2024-06-25", "2024-10-05", "phlaeas.2gen"),
  napi          <- c("Pieris napi",           "2024-04-15", "2024-09-15", "napi"),
  rapae         <- c("Pieris rapae",          "2024-04-15", "2024-09-15", "rapae"),
  atalanta      <- c("Vanessa atalanta",      "2024-05-15", "2024-10-15", "atalanta"),
  io            <- c("Aglais io",             "2024-04-01", "2024-08-31",  "io")
)

plot.list <- list()

for (i in 1:length(flyvetid)) {

species.data <- transect.data %>%
  select(Transect.Sample.ID, count, species, transect.length) %>%
  filter(species == flyvetid[[i]][1]) %>%
  select(-species) %>%
  group_by(Transect.Sample.ID, transect.length) %>%
  summarise(count = sum(count)) %>%
  mutate(density = count/transect.length*1000) %>%
  select(-count, -transect.length) %>%
  right_join(y = transect.data %>%
               select(Transect.Name, Transect.Sample.ID, date, N, E) %>%
               distinct(Transect.Name, Transect.Sample.ID, date, N, E), 
             by = "Transect.Sample.ID") %>%
  mutate(density = coalesce(density, 0))%>%
  filter(date >= ymd(flyvetid[[i]][2]) & date <= ymd(flyvetid[[i]][3])) %>%
  st_as_sf(coords = c("E", "N"), crs = 4326) %>%
  st_transform(crs = 25832)
  
grid.summary <- st_join(species.data, grid, join = st_within) %>%
  group_by(CELLCODE) %>%
  summarize(
    mean.density = mean(density)
  ) %>%
  st_drop_geometry() %>%
  mutate(mean.density = ifelse(mean.density == 0, NA, mean.density))

grid2 <- grid %>%
  left_join(x = grid.summary, by = "CELLCODE") %>%
  st_as_sf()

plot.list[[i]] <- ggplot()+
  geom_sf(data = denmark, 
          fill = "#f5f5f5", 
          col = "snow4",
          lwd = 0.3)+
  geom_sf(data = grid2, 
          mapping = aes(fill=mean.density),
          lwd = 0.3)+
  #scale_fill_viridis_c(option = "magma")+
  scale_fill_gradientn(colours = my_colors, na.value = NA) +
  theme_classic()+
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank()
    )+
  labs(fill = "Individer\npr. transekt\npr. tusind meter")+
  ggtitle(label = paste0(flyvetid[[i]][1]))

}


lapply(
  X = seq_along(plot.list),  
  FUN = function(i) {
    ggsave(
      filename = paste0("output/speciesmap.", flyvetid[[i]][4], ".jpg"),
      plot = plot.list[[i]],
      width = 8,
      height = 6
    )
  }
)

