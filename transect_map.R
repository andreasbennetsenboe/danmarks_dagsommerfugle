#library(tidyverse)
#library(lubridate)
#library(showtext)
library("sf")
#library("terra")

years <- c(2023, 2024, 2025)

####---- Data immport and wrangling ----####

transect.data <- read.csv(file = "data/download.species.occurences.from.transects.csv") %>%
  #filter(Record.status == "Accepted") %>%
  dplyr::select(1:3, Transect.Name, count = Abundance.Count, date = "Date", species = "Preferred.Species.Name") %>%
  mutate(date = dmy(date),
         count = as.numeric(count)
         ) %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>%
  mutate(DOY = yday(date)) %>%
  filter(year %in% years)


sites <- read.csv("data/download.site.details.csv") %>%
  dplyr::select(Spatial.Reference, Transect.ID, Transect.Name, length = "Overall.Length..m.") %>%
  drop_na() %>%
  filter(length > 100) %>%
  dplyr::select(Spatial.Reference, Transect.ID) %>%
  separate(col = Spatial.Reference, into = c("N", "E"), sep = " ") %>%
  mutate(N = gsub(",$", "", N)) %>%
  mutate(N = gsub("[[:alpha:]]", "", N))%>%
  mutate(E = gsub("[[:alpha:]]", "", E)) %>%
  mutate(N = as.numeric(N),
         E = as.numeric(E))

transect.over <- transect.data %>%
  #filter(year == 2024) %>%
  group_by(year, Transect.ID, Transect.Name) %>%
  summarise(visits = n_distinct(Transect.Sample.ID)) %>%
  filter(visits >= 3) %>%
  left_join(y = sites, by = "Transect.ID") %>%
  drop_na()

transect.over.wide <- transect.over %>%
  pivot_wider(names_from = year, values_from = visits)

transect.new.2024 <- transect.data %>%
  group_by(Transect.ID, Transect.Name, year) %>%
  summarise(visits = n_distinct(Transect.Sample.ID)) %>%
  pivot_wider(names_from = year, values_from = visits, values_fill = 0) %>%
  filter(`2023` == 0 & `2024` >= 3)

transect.2023.2024.2025 <- transect.data %>%
  group_by(Transect.ID, Transect.Name, year) %>%
  summarise(visits = n_distinct(Transect.Sample.ID)) %>%
  pivot_wider(names_from = year, values_from = visits, values_fill = 0) %>%
  #filter(`2023` >= 3 | `2024` >= 3) %>%
  left_join(y = sites, by = "Transect.ID") %>%
  drop_na() %>%
  filter(!(`2023` < 3 & `2024` < 3 & `2025` < 3)) %>%
  mutate(state = case_when(
    `2023` < 3 & `2024` < 3 & `2025` >= 3 ~ "1 year of data",
    `2023` >= 3 & `2024` < 3 & `2025` >= 3 ~ "2 years of data",
    `2023` < 3 & `2024` >= 3 & `2025` >= 3 ~ "2 years of data",
    `2023` >= 3 & `2024` >= 3 & `2025` >= 3 ~ "3 years of data",
    `2025` < 3 ~ "not qualified in 2025"
  ))

transect.2024 <- transect.data %>%
  group_by(Transect.ID, Transect.Name, year) %>%
  summarise(visits = n_distinct(Transect.Sample.ID)) %>%
  pivot_wider(names_from = year, values_from = visits, values_fill = 0) %>%
  filter(`2024` >= 3) %>%
  left_join(y = sites, by = "Transect.ID") %>%
  drop_na()

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
  geom_point(data = transect.2023.2024.2025,
             mapping = aes(x = E, y= N, color = state), #fill = visits),
             size = 1.6,
             shape = 16
  )+
  scale_color_manual(values = c("1 year of data" = "#B3CDE3",
                                "2 years of data" = "#6497B1",
                                "3 years of data" = "#03396C",
                                "not qualified in 2025" = "red"
                                )
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
    legend.position = c(0.8,0.8),
    #legend.justification.inside = c(0.7,0.7),
    legend.text = element_text(size = 40,
                               family = "amatic-sc",
                               face = "bold"
    )
  ) 
#guides(colour = guide_legend(ncol = 1))

effort.map

ggsave(filename = "output/2025.kort.over3.jpg",
       plot = effort.map,
       width = 8,
       height = 6)
  
