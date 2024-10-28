 library("tidyverse")
 library("lubridate")
 library("wesanderson")
 library(showtext)
 
 ####---- Auxillary vectors and functions ----####
 dk.species <- c("Lycaena phlaeas",
                 "Lycaena virgaureae",
                 "Lycaena hippothoe",
                 
                 "Celastrina argiolus",
                 "Cupido minimus",
                 "Cyaniris semiargus",
                 "Plebejus argus",
                 "Plebejus idas",
                 "Agriades optilete",
                 "Polyommatus icarus",
                 "Polyommatus amandus",
                 "Aricia agestis",
                 "Aricia artaxerxes",
                 "Phengaris alcon",
                 "Phengaris arion",
                 
                 "Favonius quercus",
                 "Satyrium w-album",
                 "Callophrys rubi",
                 "Thecla betulae",
                 
                 "Maniola jurtina",
                 "Coenonympha pamphilus",
                 "Coenonympha tullia",
                 "Aphantopus hyperantus",
                 "Pararge aegeria",
                 "Lasiommata megera",
                 "Hipparchia semele",
                 
                 "Vanessa atalanta",
                 "Vanessa cardui",
                 "Aglais io",
                 "Aglais urticae",
                 "Polygonia c-album",
                 "Nymphalis antiopa",
                 "Nymphalis xanthomelas",
                 "Nymphalis polychloros",
                 "Limenitis camilla",
                 "Araschnia levana",
                 "Apatura iris",
                 "Apatura ilia",
                 
                 "Melitaea cinxia",
                 "Melitaea athalia",
                 "Melitaea diamina",
                 "Euphydryas aurinia",
                 
                 "Boloria selene",
                 "Boloria aquilonaris",
                 "Boloria euphrosyne",
                 "Issoria lathonia",
                 "Brenthis ino",
                 "Fabriciana niobe",
                 "Fabriciana adippe",
                 "Argynnis paphia",
                 "Speyeria aglaja",
                 
                 "Colias hyale",
                 "Colias crocea",
                 "Gonepteryx rhamni",
                 "Anthocharis cardamines",
                 "Pieris napi",
                 "Pieris rapae",
                 "Pieris brassicae",
                 "Aporia crataegi",
                 "Pontia edusa",
                 
                 "Ochlodes sylvanus",
                 "Thymelicus sylvestris",
                 "Thymelicus lineola",
                 "Erynnis tages",
                 "Pyrgus malvae",
                 "Pyrgus armoricanus",
                 "Hesperia comma",
                 "Carterocephalus silvicola",
                 "Heteropterus morpheus"
 )

 ####--- DATA ---####
 
 unique(transect.data$species)
 
 sites <- read.csv("data/download.site.details.csv") %>%
   dplyr::select(Spatial.Reference, Transect.ID, Transect.Name, transect.length = "Overall.Length..m.") %>%
   drop_na() %>%
   filter(transect.length > 100) %>%
   dplyr::select(Transect.ID, transect.length) %>%
   mutate(transect.length = as.numeric(transect.length),
          Transect.ID = as.factor(Transect.ID))

 
 transect.data <- read.csv(file = "data/download.species.occurences.from.transects.csv") %>%
   filter(Record.status == "Accepted") %>%
   dplyr::select(1:3, Transect.Name, count = Abundance.Count, date = "Date", species = "Preferred.Species.Name", "sample.ID" = Transect.Sample.ID) %>%
   drop_na(count) %>%
   filter(species %in% dk.species) %>%
   mutate(date = dmy(date)) %>%
   mutate(year = year(date),
          month = month(date),
          day = day(date),
          DOY = yday(date),
          week = week(date)
          ) %>%
   mutate(year.factor = as.factor(year),
          Transect.ID = as.factor(Transect.ID)) %>%
   filter(year.factor %in% c("2023", "2024")) %>%
   group_by(Transect.ID, sample.ID, week, year.factor) %>%
   summarise(count = sum(count)) %>% # transect.length bliver NA, left join senere via sample ID?
   left_join(y = sites, by = "Transect.ID") %>%
   drop_na() %>%
   mutate(density = count/transect.length*1000) %>%
   group_by(year.factor, week) %>%
   summarise(mean = mean(count)) %>%
   arrange(week, year.factor)
   
font_add_google(name = "Amatic SC", family = "amatic-sc")
showtext_auto()

 plot <- ggplot()+
   #geom_point(data = transect.data,
   #           mapping = aes(x = week, y = mean, col = year.factor))+
   geom_path(
     data = transect.data,
     mapping = aes(x = week, y = mean, col = factor(year.factor), group = year.factor)
   )+
   scale_color_manual(
    values = c("2023" = "#c3c2c7", 
               "2024" = "#63769e"
               )
    )+
   theme_classic()+
   xlab(label = "Uge")+
   ylab(label = "Individer")+
   labs(col = "Gennemsnitlige antal individer pr transektpr km")+
   theme(
     text = element_text(family = "amatic-sc", face = "bold"),
     axis.text = element_text(size = 50),
     axis.title = element_text(size = 50),
     legend.text = element_text(size = 50),
     legend.title = element_text(size = 50),
     legend.key.size = unit(0.5, "cm"),
     legend.position = "top"
   )
 
 plot
 
 ggsave(
   filename = paste0("output/phenology_graph.jpg"),
   plot = plot,
   width = 8,
   height = 6
   )
 
 

 