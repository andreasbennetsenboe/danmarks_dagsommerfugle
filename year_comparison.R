library(tidyverse)
library(lubridate)
library(showtext)
library(grid)
library(gridExtra)
library(viridis)
library("ggpubr")
library("ggtext")
library(ggbreak)

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


dk.resttax <- c("Pieridae", "Lycaenidae", "Hesperiidae","Thymelicus","Satyrinae","Argynnis")

mutate.dk <- function (x){
  x %>% mutate(species.dk = case_when(
    species == "Vanessa atalanta" ~ "Admiral",
    species == "Polyommatus icarus" ~ "Alm. Blåfugl",
    species == "Plebejus argus" ~ "Argusblåfugl",
    species == "Anthocharis cardamines" ~ "Aurora",
    species == "Favonius quercus" ~ "Blåhale",
    species == "Melitaea athalia" ~ "Brun Pletvinge",
    species == "Boloria selene" ~ "Brunlig Perlemorsommerfugl",
    species == "Agriades optilete" ~ "Bølleblåfugl",
    species == "Gonepteryx rhamni" ~ "Citronsommerfugl",
    species == "Aglais io" ~ "Dagpåfugleøje",
    species == "Polygonia c-album" ~ "Det Hvide C",
    species == "Satyrium w-album" ~ "Det Hvide W",
    species == "Lycaena virgaureae" ~ "Dukatsommerfugl",
    species == "Cupido minimus" ~ "Dværgblåfugl",
    species == "Cyaniris semiargus" ~ "Engblåfugl",
    species == "Brenthis ino" ~ "Engperlemorsommerfugl",
    species == "Aphantopus hyperantus" ~ "Engrandøje",
    species == "Phengaris alcon" ~ "Ensianblåfugl",
    species == "Plebejus idas" ~ "Foranderlig Blåfugl",
    species == "Pyrgus armoricanus" ~ "Fransk Bredpande",
    species == "Erynnis tages" ~ "Gråbåndet Bredpande",
    species == "Maniola jurtina" ~ "Græsrandøje",
    species == "Callophrys rubi" ~ "Grøn Busksommerfugl",
    species == "Pontia edusa" ~ "Grønbroget Kålsommerfugl",
    species == "Pieris napi" ~ "Grønåret Kålsommerfugl",
    species == "Colias hyale" ~ "Gul Høsommerfugl",
    species == "Thecla betulae" ~ "Guldhale",
    species == "Euphydryas aurinia" ~ "Hedepletvinge",
    species == "Limenitis camilla" ~ "Hvid Admiral",
    species == "Apatura ilia" ~ "Ilia",
    species == "Apatura iris" ~ "Iris",
    species == "Polyommatus amandus" ~ "Isblåfugl",
    species == "Argynnis paphia" ~ "Kejserkåbe",
    species == "Nymphalis polychloros" ~ "Kirsebærtakvinge",
    species == "Fabriciana niobe" ~ "Klitperlemorsommerfugl",
    species == "Hesperia comma" ~ "Kommabredpande",
    species == "Lycaena phlaeas" ~ "Lille Ildfugl",
    species == "Pieris rapae" ~ "Lille Kålsommerfugl",
    species == "Speyeria aglaja" ~ "Markperlemorsommerfugl",
    species == "Boloria aquilonaris" ~ "Moseperlemorsommerfugl",
    species == "Coenonympha tullia" ~ "Moserandøje",
    species == "Melitaea diamina" ~ "Mørk Pletvinge",
    species == "Aglais urticae" ~ "Nældens Takvinge",
    species == "Araschnia levana" ~ "Nældesommerfugl",
    species == "Melitaea cinxia" ~ "Okkergul Pletvinge",
    species == "Coenonympha pamphilus" ~ "Okkergul Randøje",
    species == "Colias crocea" ~ "Orange Høsommerfugl",
    species == "Boloria euphrosyne" ~ "Rødlig Perlemorsommerfugl",
    species == "Aricia agestis" ~ "Rødplettet Blåfugl",
    species == "Hipparchia semele" ~ "Sandrandøje",
    species == "Celastrina argiolus" ~ "Skovblåfugl",
    species == "Fabriciana adippe" ~ "Skovperlemorsommerfugl",
    species == "Pararge aegeria" ~ "Skovrandøje",
    species == "Thymelicus sylvestris" ~ "Skråstregbredpande",
    species == "Lycaena tityrus" ~ "Sort Ildfugl",
    species == "Aricia artaxerxes" ~ "Sortbrun Blåfugl",
    species == "Phengaris arion" ~ "Sortplettet Blåfugl",
    species == "Carterocephalus silvicola" ~ "Sortplettet Bredpande",
    species == "Aporia crataegi" ~ "Sortåret Hvidvinge",
    species == "Heteropterus morpheus" ~ "Spejlbredpande",
    species == "Pyrgus malvae" ~ "Spættet Bredpande",
    species == "Ochlodes sylvanus" ~ "Stor Bredpande",
    species == "Pieris brassicae" ~ "Stor Kålsommerfugl",
    species == "Issoria lathonia" ~ "Storplettet Perlemorsommerfugl",
    species == "Thymelicus lineola" ~ "Stregbredpande",
    species == "Papilio machaon" ~ "Svalehale",
    species == "Nymphalis antiopa" ~ "Sørgekåbe",
    species == "Vanessa cardui" ~ "Tidselsommerfugl",
    species == "Lasiommata megera" ~ "Vejrandøje",
    species == "Lycaena hippothoe" ~ "Violetrandet Ildfugl",
    species == "Nymphalis xanthomelas" ~ "Østlig Takvinge"
  ))
}

mutate.family <- function (x){
  x %>% mutate(family = case_when(
    species == "Vanessa atalanta" ~ "Nymphalidae",
    species == "Polyommatus icarus" ~ "Lycaenidae",
    species == "Plebejus argus" ~ "Lycaenidae",
    species == "Anthocharis cardamines" ~ "Pieridae",
    species == "Favonius quercus" ~ "Lycaenidae",
    species == "Melitaea athalia" ~ "Nymphalidae",
    species == "Boloria selene" ~ "Nymphalidae",
    species == "Agriades optilete" ~ "Lycaenidae",
    species == "Gonepteryx rhamni" ~ "Pieridae",
    species == "Aglais io" ~ "Nymphalidae",
    species == "Polygonia c-album" ~ "Nymphalidae",
    species == "Satyrium w-album" ~ "Lycaenidae",
    species == "Lycaena virgaureae" ~ "Lycaenidae",
    species == "Cupido minimus" ~ "Lycaenidae",
    species == "Cyaniris semiargus" ~ "Lycaenidae",
    species == "Brenthis ino" ~ "Nymphalidae",
    species == "Aphantopus hyperantus" ~ "Nymphalidae",
    species == "Phengaris alcon" ~ "Lycaenidae",
    species == "Plebejus idas" ~ "Lycaenidae",
    species == "Pyrgus armoricanus" ~ "Hesperiidae",
    species == "Erynnis tages" ~ "Hesperiidae",
    species == "Maniola jurtina" ~ "Nymphalidae",
    species == "Callophrys rubi" ~ "Lycaenidae",
    species == "Pontia edusa" ~ "Pieridae",
    species == "Pieris napi" ~ "Pieridae",
    species == "Colias hyale" ~ "Pieridae",
    species == "Thecla betulae" ~ "Lycaenidae",
    species == "Euphydryas aurinia" ~ "Nymphalidae",
    species == "Limenitis camilla" ~ "Nymphalidae",
    species == "Apatura ilia" ~ "Nymphalidae",
    species == "Apatura iris" ~ "Nymphalidae",
    species == "Polyommatus amandus" ~ "Lycaenidae",
    species == "Argynnis paphia" ~ "Nymphalidae",
    species == "Nymphalis polychloros" ~ "Nymphalidae",
    species == "Fabriciana niobe" ~ "Nymphalidae",
    species == "Hesperia comma" ~ "Hesperiidae",
    species == "Lycaena phlaeas" ~ "Lycaenidae",
    species == "Pieris rapae" ~ "Pieridae",
    species == "Speyeria aglaja" ~ "Nymphalidae",
    species == "Boloria aquilonaris" ~ "Nymphalidae",
    species == "Coenonympha tullia" ~ "Nymphalidae",
    species == "Melitaea diamina" ~ "Nymphalidae",
    species == "Aglais urticae" ~ "Nymphalidae",
    species == "Araschnia levana" ~ "Nymphalidae",
    species == "Melitaea cinxia" ~ "Nymphalidae",
    species == "Coenonympha pamphilus" ~ "Nymphalidae",
    species == "Colias croceus" ~ "Pieridae",
    species == "Boloria euphrosyne" ~ "Nymphalidae",
    species == "Aricia agestis" ~ "Lycaenidae",
    species == "Hipparchia semele" ~ "Nymphalidae",
    species == "Celastrina argiolus" ~ "Lycaenidae",
    species == "Fabriciana adippe" ~ "Nymphalidae",
    species == "Pararge aegeria" ~ "Nymphalidae",
    species == "Thymelicus sylvestris" ~ "Hesperiidae",
    species == "Lycaena tityrus" ~ "Lycaenidae",
    species == "Aricia artaxerxes" ~ "Lycaenidae",
    species == "Phengaris arion" ~ "Lycaenidae",
    species == "Carterocephalus silvicola" ~ "Hesperiidae",
    species == "Aporia crataegi" ~ "Pieridae",
    species == "Heteropterus morpheus" ~ "Hesperiidae",
    species == "Pyrgus malvae" ~ "Hesperiidae",
    species == "Ochlodes sylvanus" ~ "Hesperiidae",
    species == "Pieris brassicae" ~ "Pieridae",
    species == "Issoria lathonia" ~ "Nymphalidae",
    species == "Thymelicus lineola" ~ "Hesperiidae",
    species == "Papilio machaon" ~ "Papilionidae",
    species == "Nymphalis antiopa" ~ "Nymphalidae",
    species == "Vanessa cardui" ~ "Nymphalidae",
    species == "Lasiommata megera" ~ "Nymphalidae",
    species == "Lycaena hippothoe" ~ "Lycaenidae",
    species == "Nymphalis xanthomelas" ~ "Nymphalidae"
  ))
}

mutate.alt.tax <- function (x){
  x %>% mutate(alt.tax = case_when(
    species == "Vanessa atalanta" ~ "Nymphalinae",
    species == "Polyommatus icarus" ~ "Polyommatinae",
    species == "Plebejus argus" ~ "Polyommatinae",
    species == "Anthocharis cardamines" ~ "Pieridae",
    species == "Favonius quercus" ~ "Theclinae",
    species == "Melitaea athalia" ~ "Heliconiinae",
    species == "Boloria selene" ~ "Heliconiinae",
    species == "Agriades optilete" ~ "Polyommatinae",
    species == "Gonepteryx rhamni" ~ "Pieridae",
    species == "Aglais io" ~ "Nymphalinae",
    species == "Polygonia c-album" ~ "Nymphalinae",
    species == "Satyrium w-album" ~ "Theclinae",
    species == "Lycaena virgaureae" ~ "Lycaeninae",
    species == "Cupido minimus" ~ "Polyommatinae",
    species == "Cyaniris semiargus" ~ "Polyommatinae",
    species == "Brenthis ino" ~ "Heliconiinae",
    species == "Aphantopus hyperantus" ~ "Satyrinae",
    species == "Phengaris alcon" ~ "Polyommatinae",
    species == "Plebejus idas" ~ "Polyommatinae",
    species == "Pyrgus armoricanus" ~ "Hesperiidae",
    species == "Erynnis tages" ~ "Hesperiidae",
    species == "Maniola jurtina" ~ "Satyrinae",
    species == "Callophrys rubi" ~ "Theclinae",
    species == "Pontia edusa" ~ "Pieridae",
    species == "Pieris napi" ~ "Pieridae",
    species == "Colias hyale" ~ "Pieridae",
    species == "Thecla betulae" ~ "Theclinae",
    species == "Euphydryas aurinia" ~ "Heliconiinae",
    species == "Limenitis camilla" ~ "Limenitidinae",
    species == "Apatura ilia" ~ "Apaturinae",
    species == "Apatura iris" ~ "Apaturinae",
    species == "Polyommatus amandus" ~ "Polyommatinae",
    species == "Argynnis paphia" ~ "Heliconiinae",
    species == "Nymphalis polychloros" ~ "Nymphalinae",
    species == "Fabriciana niobe" ~ "Heliconiinae",
    species == "Hesperia comma" ~ "Hesperiidae",
    species == "Lycaena phlaeas" ~ "Lycaeninae",
    species == "Pieris rapae" ~ "Pieridae",
    species == "Speyeria aglaja" ~ "Heliconiinae",
    species == "Boloria aquilonaris" ~ "Heliconiinae",
    species == "Coenonympha tullia" ~ "Satyrinae",
    species == "Melitaea diamina" ~ "Heliconiinae",
    species == "Aglais urticae" ~ "Nymphalinae",
    species == "Araschnia levana" ~ "Nymphalinae",
    species == "Melitaea cinxia" ~ "Heliconiinae",
    species == "Coenonympha pamphilus" ~ "Satyrinae",
    species == "Colias croceus" ~ "Pieridae",
    species == "Boloria euphrosyne" ~ "Heliconiinae",
    species == "Aricia agestis" ~ "Polyommatinae",
    species == "Hipparchia semele" ~ "Satyrinae",
    species == "Celastrina argiolus" ~ "Polyommatinae",
    species == "Fabriciana adippe" ~ "Heliconiinae",
    species == "Pararge aegeria" ~ "Satyrinae",
    species == "Thymelicus sylvestris" ~ "Hesperiidae",
    species == "Lycaena tityrus" ~ "Lycaeninae",
    species == "Aricia artaxerxes" ~ "Polyommatinae",
    species == "Phengaris arion" ~ "Polyommatinae",
    species == "Carterocephalus silvicola" ~ "Hesperiidae",
    species == "Aporia crataegi" ~ "Pieridae",
    species == "Heteropterus morpheus" ~ "Hesperiidae",
    species == "Pyrgus malvae" ~ "Hesperiidae",
    species == "Ochlodes sylvanus" ~ "Hesperiidae",
    species == "Pieris brassicae" ~ "Pieridae",
    species == "Issoria lathonia" ~ "Heliconiinae",
    species == "Thymelicus lineola" ~ "Hesperiidae",
    species == "Papilio machaon" ~ "Papilionidae",
    species == "Nymphalis antiopa" ~ "Nymphalinae",
    species == "Vanessa cardui" ~ "Nymphalinae",
    species == "Lasiommata megera" ~ "Satyrinae",
    species == "Lycaena hippothoe" ~ "Lycaeninae",
    species == "Nymphalis xanthomelas" ~ "Nymphalinae"
  ))
}

redlist.join <- function (x){
  x %>% left_join(
    y = redlist <- read.csv(file = "data/redlist.csv", sep = ";") %>%
      select(species = "scientificName", redlist = "redlistCategory") %>%
      mutate(redlist = case_when(is.na(redlist)~"NA",
                                 TRUE ~ redlist)),
    by = "species"
  )
}
####---- Data prep ----####
transect.data.raw <- read.csv(file = "data/download.species.occurences.from.transects.csv")

transect.data <- read.csv(file = "data/download.species.occurences.from.transects.csv")%>%
  mutate(Date = dmy(Date))%>%
  mutate(Year = year(Date)) %>%
  filter(Record.status == "Accepted")

site.data <- read.csv("data/download.site.details.csv") %>%
  dplyr::select(Spatial.Reference, Transect.ID, Transect.Name, length = "Overall.Length..m.") %>%
  drop_na() %>%
  filter(length > 100)

transect.data.2023 <- transect.data %>%
  filter(Year == 2023) %>%
  group_by(Transect.ID) %>%
  summarise(visits = n_distinct(Transect.Sample.ID)) %>%
  left_join(site.data%>%
              select(Transect.ID, length), by ="Transect.ID") %>%
  drop_na() %>%
  mutate(walked = visits*length)

transect.meters.2023 <- sum(transect.data.2023$walked)


transect.data.2024 <- transect.data %>%
  filter(Year == 2024) %>%
  group_by(Transect.ID) %>%
  summarise(visits = n_distinct(Transect.Sample.ID)) %>%
  left_join(site.data%>%
              select(Transect.ID, length), by ="Transect.ID") %>%
  drop_na() %>%
  mutate(walked = visits*length)

transect.meters.2024 <- sum(transect.data.2024$walked)

sites <- site.data %>%
  dplyr::select(Spatial.Reference, Transect.ID) %>%
  separate(col = Spatial.Reference, into = c("N", "E"), sep = " ") %>%
  mutate(N = gsub(",$", "", N)) %>%
  mutate(N = gsub("[[:alpha:]]", "", N))%>%
  mutate(E = gsub("[[:alpha:]]", "", E)) %>%
  mutate(N = as.numeric(N),
         E = as.numeric(E))

transect.data1 <- transect.data.raw %>%
  filter(Transect.Name %in% site.data$Transect.Name) %>%
  dplyr::select(1:3, Transect.Name, count = Abundance.Count, date = "Date", species = "Preferred.Species.Name") %>%
  filter(species %in% dk.species) %>%
  mutate(date = dmy(date)) %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>%
  mutate(DOY = yday(date)) %>%
  mutate.dk() %>%
  mutate.family() %>%
  mutate.alt.tax() %>%
  filter(!(species %in% c("Heteropterus morpheus", "Boloria euphrosyne"))) %>%
  redlist.join()

mutate_facet <- function (x){
  x %>% mutate(facet = case_when(
    species.dk == "Græsrandøje" ~ 1,
    species.dk == "Engrandøje" ~ 1,
    species.dk == "Grønåret Kålsommerfugl" ~ 1,
    species.dk == "Lille Kålsommerfugl" ~ 1,
    species.dk == "Alm. Blåfugl" ~ 1,
    species.dk == "Nældens Takvinge" ~ 1,
    species.dk == "Stor Kålsommerfugl" ~ 1,
    species.dk == "Okkergul Randøje" ~ 1,
    species.dk == "Skråstregbredpande" ~ 1,
    species.dk == "Stregbredpande" ~ 1,
    species.dk == "Dagpåfugleøje" ~ 1,
    species.dk == "Admiral" ~ 1,
    species.dk == "Lille Ildfugl" ~ 1,
    species.dk == "Stor Bredpande" ~ 1,
    species.dk == "Okkergul Pletvinge" ~ 1,
    species.dk == "Aurora" ~ 1,
    species.dk == "Citronsommerfugl" ~ 1,
    species.dk == "Skovrandøje" ~ 1,
    species.dk == "Sortåret Hvidvinge" ~ 1,
    species.dk == "Foranderlig Blåfugl" ~ 1,
    species.dk == "Nældesommerfugl" ~ 1,
    species.dk == "Argusblåfugl" ~ 1,
    species.dk == "Storplettet Perlemorsommerfugl" ~ 1,
    species.dk == "Dværgblåfugl" ~ 1,
    species.dk == "Skovperlemorsommerfugl" ~ 1,
    species.dk == "Isblåfugl" ~ 1,
    TRUE ~ 2
  ))
}

facet1 <- transect.data1 %>%
  filter(year >= 2023) %>%
  filter(!(species.dk %in% c("Spejlbredpande", "Rødlig Perlemorsommerfugl"))) %>%
  drop_na(count) %>%
  group_by(species.dk, year, redlist) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  mutate_facet() %>%
  filter(facet == 1) 

facet1.corrected <- facet1 %>%
  mutate(walked = case_when(year == 2023 ~ transect.meters.2023,
                            year == 2024 ~ transect.meters.2024)) %>%
  mutate(count.corrected = count/walked*1000)

facet2 <- transect.data1 %>%
  filter(year >= 2023) %>%
  filter(!(species.dk %in% c("Spejlbredpande", "Rødlig Perlemorsommerfugl"))) %>%
  drop_na(count) %>%
  group_by(species.dk, year, redlist) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count)) %>%
  mutate_facet() %>%
  filter(facet == 2)  

facet2.corrected <- facet2 %>%
  mutate(walked = case_when(year == 2023 ~ transect.meters.2023,
                            year == 2024 ~ transect.meters.2024)) %>%
  mutate(count.corrected = count/walked*1000)

####---- Plotting ----####
font_add_google(name = "Amatic SC", family = "amatic-sc")

showtext_auto()

color_map <- c(
  "NA" = "#C1B5A5",   # Not Available
  "LC" = "#60C659",   # Least Concern
  "NT" = "#CCE226",   # Near Threatened
  "VU" = "#F9E814",   # Vulnerable
  "EN" = "#FC7F3F",   # Endangered
  "CR" = "#D81E05"   # Critically Endangered
)

facet1.plot <- ggplot(data = facet1, 
                      mapping = aes(y = count, 
                                    x = reorder(species.dk, +count), 
                                    fill = factor(year)))+
  geom_bar(stat = "identity",
           width = 0.7,
           size = 0.2,
           col = "black",
           position = position_dodge(preserve = "single")
  )+
  geom_text(aes(label = paste0(after_stat(y))),
            hjust=-0.5,
            position = position_dodge(width = 1),
            size = 12,
            family = "amatic-sc", 
            fontface = "bold"
  )+
  scale_fill_manual(values=c("2024" = "#63769e", "2023" = "#c3c2c7"))+
  coord_flip()+
  theme_minimal()+
  labs(x = "", y = "")+
  scale_y_continuous(limits = c(0,max(facet1$count)+20))+
  scale_x_discrete(labels = function(x) {
    sapply(seq_along(x), function(i) {
      condition <- unique(facet1[facet1$species.dk == x[i], "redlist"]) 
      condition <- as.character(condition)[1]
      color <- color_map[condition]
      paste0("<span style='color:black;'>", x[i], "</span> ", 
             "<span style='color:", color, ";'>(", condition, ")</span>")
    })
  })+
  theme(
    text = element_text(family = "amatic-sc", face = "bold"),
    axis.text.x = element_blank(),
    axis.text.y = element_markdown(size = 45),
    panel.grid = element_blank(),
    legend.text = element_text(size = 50),
    legend.title = element_text(size = 50)
  )+
  guides(fill=guide_legend(title="År"))

facet2.plot <- ggplot(data = facet2, 
                      mapping = aes(y = count, 
                                    x = reorder(species.dk, +count), 
                                    fill = factor(year)))+
  geom_bar(stat = "identity",
           width = 0.7,
           size = 0.2,
           col = "black",
           position = position_dodge(preserve = "single")
  )+
  geom_text(aes(label = paste0(after_stat(y))),
            hjust=-0.5,
            position = position_dodge(width = 1),
            size = 12,
            family = "amatic-sc", 
            fontface = "bold"
  )+
  scale_fill_manual(values=c("2024" = "#63769e", "2023" = "#c3c2c7"))+
  coord_flip()+
  theme_minimal()+
  labs(x = "", y = "")+
  scale_y_continuous(limits = c(0,max(facet2$count)+20))+
  scale_x_discrete(labels = function(x) {
    sapply(seq_along(x), function(i) {
      condition <- unique(facet2[facet2$species.dk == x[i], "redlist"]) 
      condition <- as.character(condition)[1]
      color <- color_map[condition]
      paste0("<span style='color:black;'>", x[i], "</span> ", 
             "<span style='color:", color, ";'>(", condition, ")</span>")
    })
  })+
  theme(
    text = element_text(family = "amatic-sc", face = "bold"),
    axis.text.x = element_blank(),
    axis.text.y = element_markdown(size = 45),
    panel.grid = element_blank(),
    legend.text = element_text(size = 50),
    legend.title = element_text(size = 50)
  )+
  guides(fill=guide_legend(title="År"))

# Adjusted plot 1
facet1.plot.adjusted <- ggplot(data = facet1.corrected, 
                      mapping = aes(y = count.corrected, x = reorder(species.dk, +desc(count.corrected)), fill = factor(year)))+
  geom_bar(stat = "identity",
           width = 0.6,
           col = "#495B73",
           position = position_dodge(preserve = "single")
  )+
  scale_fill_manual(values=c("2024" = "#63769e", "2023" = "#c3c2c7"))+
  theme_classic()+
  labs(x = "", y = "")+
  scale_y_continuous(limits = c(0, 14.5), breaks = seq(0, 14.5, by = 0.5)) +
  scale_y_break(c(5, 14)) +
  scale_y_break(c(2.5, 4.5)) +
  scale_x_discrete(labels = function(x) {
    sapply(seq_along(x), function(i) {
      condition <- unique(facet1.corrected[facet1.corrected$species.dk == x[i], "redlist"]) # Get condition
      condition <- as.character(condition)[1]
      color <- color_map[condition]  # Get corresponding color from map
      paste0("<span style='color:black;'>", x[i], "</span> ", 
             "<span style='color:", color, ";'>(", condition, ")</span>")
    })
  })+
  theme(
    text = element_text(family = "amatic-sc", face = "bold"),
    plot.title = element_text(size = 90,
                              hjust = 0.5,
                              margin=margin(10,0,15,0)),
    plot.subtitle = element_text(hjust = c(0.8, 0.2),
                                 size = 50),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_markdown(size = 15, angle = 45, hjust = 1),
    panel.grid = element_blank(),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20)
  )+
  guides(fill=guide_legend(title="År"))

# Adjusted plot 2
facet2.plot.adjusted <- ggplot(data = facet2.corrected, 
                               mapping = aes(y = count.corrected, x = reorder(species.dk, +desc(count.corrected)), fill = factor(year))) +
  geom_bar(stat = "identity",
           width = 0.6,
           col = "#495B73",
           position = position_dodge(preserve = "single")
  ) +
  scale_fill_manual(values = c("2024" = "#63769e", "2023" = "#c3c2c7")) +
  theme_classic() +
  labs(x = "", y = "") +
  scale_y_continuous(limits = c(0, 14.5), breaks = seq(0, 14.5, by = 0.5)) +
  scale_y_break(c(5, 14)) +
  scale_y_break(c(2.5, 4.5)) +
  scale_x_discrete(labels = function(x) {
    sapply(seq_along(x), function(i) {
      condition <- unique(facet2.corrected[facet2.corrected$species.dk == x[i], "redlist"]) # Get condition
      condition <- as.character(condition)[1]
      color <- color_map[condition]  # Get corresponding color from map
      paste0("<span style='color:black;'>", x[i], "</span> ", 
             "<span style='color:", color, ";'>(", condition, ")</span>")
    })
  }) +
  theme(
    text = element_text(family = "amatic-sc", face = "bold"),
    plot.title = element_text(size = 90,
                              hjust = 0.5,
                              margin = margin(10, 0, 15, 0)),
    plot.subtitle = element_text(hjust = c(0.8, 0.2),
                                 size = 50),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_markdown(size = 15, angle = 45, hjust = 1),
    panel.grid = element_blank(),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20)
  ) +
  guides(fill = guide_legend(title = "År"))






ggsave(filename = "output/facet1.plot.jpg",
       plot = facet1.plot,
       width = 10,
       height = 7)

ggsave(filename = "output/facet1.adjusted.plot.jpg",
       plot = facet1.plot.adjusted,
       width = 10,
       height = 7)

ggsave(filename = "output/facet2.plot.jpg",
       plot = facet2.plot,
       width = 10,
       height = 7)

ggsave(filename = "output/facet2.adjusted.plot.jpg",
       plot = facet2.plot.adjusted,
       width = 10,
       height = 7)
