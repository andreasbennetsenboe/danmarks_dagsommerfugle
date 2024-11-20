#library(tidyverse)
#library(lubridate)
#library("sf")
#library("terra")

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

#Redlist species distribution

redlist.species <- read_lines(file = "data/redlist.species.txt")

ebms.data.list <- list()
distributions.list <- list()
suspect.observations.list <- list()

for (i in 1:length(redlist.species)) {
  
  ebms.data.list[[i]] <- test.data <- read.csv(file = "data/download.species.occurences.from.transects.csv") %>%
  filter(Record.status == "Accepted") %>%
  dplyr::select(Transect.ID,
                Transect.Sample.ID,
                count = "Abundance.Count", 
                date = "Date", 
                species = "Preferred.Species.Name"
                ) %>%
  filter(species == redlist.species[i]) %>%
  drop_na(count) %>%
  left_join(y = sites, by = "Transect.ID") %>%
  select(-count) %>%
  drop_na() %>%
  st_as_sf(coords = c("E", "N"), crs = 4258)

  distributions.list[[i]] <- distribution <- read.csv(file = "data/arter.redlist.csv", sep = ";") %>%
    filter(Systemoprindelse != "DOF/BirdLife Denmark - Observations from DOFbasen" & Usikkerhed..m. < 2000) %>%
    select(date = "Observationsdato", 
           species =  "Taxon.latinsk.navn", 
           N = "Lat", 
           E = "Long") %>%
    mutate(across(c(N, E), ~ gsub(",", ".", .))) %>%
    mutate(across(c(N, E), ~ parse_number(.))) %>%
    filter(species == redlist.species[i])  %>%
    st_as_sf(coords = c("E", "N"), crs = 4258) %>%
    st_buffer(dist = 3000) %>%
    st_union() %>%
    st_make_valid()
  
  suspect.observations.list[[i]] <- test.data[lengths(st_intersects(test.data, distribution)) == 0,]
  
}


#Restricted range species distribution

restricted.species <- read_lines(file = "data/restricted.species.txt")

ebms.data.list2 <- list()
distributions.list2 <- list()
suspect.observations.list2 <- list()

for (i in 1:length(restricted.species)) {
  
  ebms.data.list2[[i]] <- test.data <- read.csv(file = "data/download.species.occurences.from.transects.csv") %>%
    filter(Record.status == "Accepted") %>%
    dplyr::select(Transect.ID,
                  Transect.Sample.ID,
                  count = "Abundance.Count", 
                  date = "Date", 
                  species = "Preferred.Species.Name"
    ) %>%
    filter(species == restricted.species[i]) %>%
    drop_na(count) %>%
    left_join(y = sites, by = "Transect.ID") %>%
    select(-count) %>%
    drop_na() %>%
    st_as_sf(coords = c("E", "N"), crs = 4258)
  
  distributions.list2[[i]] <- distribution <- read.csv(file = "data/arter.restricted1.csv", sep = ";") %>%
    bind_rows(read.csv(file = "data/arter.restricted2.csv", sep = ";")) %>%
    filter(Systemoprindelse != "DOF/BirdLife Denmark - Observations from DOFbasen" & Usikkerhed..m. < 2000) %>%
    select(date = "Observationsdato", 
           species =  "Taxon.latinsk.navn", 
           N = "Lat", 
           E = "Long") %>%
    mutate(across(c(N, E), ~ gsub(",", ".", .))) %>%
    mutate(across(c(N, E), ~ parse_number(.))) %>%
    filter(species == restricted.species[i])  %>%
    st_as_sf(coords = c("E", "N"), crs = 4258) %>%
    st_buffer(dist = 7000) %>%
    st_union() %>%
    st_make_valid()
  
  suspect.observations.list2[[i]] <- test.data[lengths(st_intersects(test.data, distribution)) == 0,]
  
}

# Sylvestris range

sylvestris.ebms <- read.csv(file = "data/download.species.occurences.from.transects.csv") %>%
  filter(Record.status == "Accepted") %>%
  dplyr::select(Transect.ID,
                Transect.Sample.ID,
                count = "Abundance.Count", 
                date = "Date", 
                species = "Preferred.Species.Name"
  ) %>%
  filter(species == "Thymelicus sylvestris") %>%
  drop_na(count) %>%
  left_join(y = sites, by = "Transect.ID") %>%
  select(-count) %>%
  drop_na() %>%
  st_as_sf(coords = c("E", "N"), crs = 4258)

sylvestris.dist <- st_read(dsn = "data/sylvestris.gpkg") %>%
  st_transform(crs = 4258) %>%
  st_make_valid()
suspect.sylvestris <- sylvestris.ebms[lengths(st_intersects(sylvestris.ebms, sylvestris.dist)) == 0,]

for.review <- rbind(
  do.call(rbind, suspect.observations.list),
  do.call(rbind, suspect.observations.list2),
  suspect.sylvestris
)


####---- Illustration ----####

denmark <- readRDS(file= "DNK.rds") %>%
  unwrap() %>%
  st_as_sf() %>%
  st_union() %>%
  st_transform(crs = 4258)

redlist.speciesnr <- 2


ggplot()+
  geom_sf(data =  denmark, 
          fill = "#f5f5f5", 
          col = "snow3", 
          lwd = 0.3, 
          alpha=1)+
  geom_sf(data = distributions.list[[redlist.speciesnr]], col = "#00A08A", fill = "#00A08A", alpha = 0.2)+
  geom_sf(data = ebms.data.list[[redlist.speciesnr]], col = "black", size = 3)+
  geom_sf(data = suspect.observations.list[[redlist.speciesnr]], col = "#FF0000")+
  ggtitle(label = paste0(redlist.species[[redlist.speciesnr]]))+
  theme_classic()


restricted.speciesnr <- 2

ggplot()+
  geom_sf(data =  denmark, 
          fill = "snow2", 
          col = "snow4", 
          lwd = 0.3, 
          alpha=1)+
  geom_sf(data = distributions.list2[[restricted.speciesnr]], col = "#00A08A", fill = "#00A08A", alpha = 0.2)+
  geom_sf(data = ebms.data.list2[[restricted.speciesnr]], col = "black", size = 3)+
  geom_sf(data = suspect.observations.list2[[restricted.speciesnr]], col = "red")+
  ggtitle(label = paste0(restricted.species[[restricted.speciesnr]]))+
  theme_classic()

ggplot()+
  geom_sf(data =  denmark, 
          fill = "#f5f5f5", 
          col = "snow3", 
          lwd = 0.3, 
          alpha=1)+
  geom_sf(data = sylvestris.dist, col = "#00A08A", fill = "#00A08A", alpha = 0.2)+
  geom_sf(data = sylvestris.ebms, col = "black", size = 3)+
  geom_sf(data = suspect.sylvestris, col = "#FF0000")+
  ggtitle(label = "Thymelicus sylvestris")+
  theme_classic()
