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

####---- Species sorting vectors ----####
dk.species <- c("Lycaena phlaeas",
                "Lycaena virgaureae",
                "Lycaena hippothoe",
                "Lycaena titryrus",
                
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
                
                "Vanessa atalanta",
                "Vanessa cardui",
                "Aglais io",
                "Aglais urticae",
                "Nymphalis antiopa",
                "Limenitis camilla",
                "Araschnia levana",
                "Apatura iris",
                "Apatura ilia",
                
                "Melitaea cinxia",
                "Melitaea athalia",
                "Euphydryas aurinia",
                
                "Boloria selene",
                "Boloria euphrosyne",
                "Boloria aquilonaris",
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
                
                "Ochlodes sylvanus",
                "Thymelicus sylvestris",
                "Thymelicus lineola",
                "Heteropterus morpheus",
                "Erynnis tages",
                "Pyrgus malvae",
                "Pyrgus armoricanus"
)


dk.resttax <- c("Pieridae", "Lycaenidae", "Hesperiidae","Thymelicus","Satyrinae","Argynnis")

####---- Data immport and wrangling ----####

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
    species == "Colias croceus" ~ "Orange Høsommerfugl",
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

spot.data.raw <- read.csv(file = "data/allearter15min.csv")

spot.data1 <- spot.data.raw %>%
  dplyr::select(species = "Accepted.species.name", count = "Count", latitude = "Latitude", longitude = "Longitude", date = "Date", observer = "Recorder.name") %>%
  filter(species %in% dk.species) %>%
  mutate(date = dmy(date)) %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>%
  filter(year == 2024) %>%
  mutate.dk() %>%
  mutate.family() %>%
  mutate.alt.tax()

spot.data2 <- spot.data1 %>%
  group_by(species.dk) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count))

year_plot <- function(input, year) {
  
  ggplot(data = input, mapping = aes(y = count, x = reorder(species.dk, +count)))+
    geom_bar(stat = "identity",
             fill = "#869EC2",
             width = 0.6,
             col = "#495B73"
    )+
    geom_text(aes(label = paste0(after_stat(y))),
              hjust=-0.5,
              position = position_dodge(width = 1),
              size = 10,
              family = "amatic-sc", 
              fontface = "bold"
    )+
    coord_flip()+
    theme_minimal(
    )+
    labs(x = "",
         y = "",
         title = paste0("Observationer i ", year),
         subtitle = c(paste0("Antal observationer: ", sum(input$count)), 
                      paste0("Antal transektture: ", n_distinct(spot.data1$Transect.Sample.ID)))
    )+
    scale_y_continuous(limits = c(0,max(input$count)+20))+
    theme(
      text = element_text(family = "amatic-sc", face = "bold"),
      plot.title = element_text(size = 90,
                                hjust = 0.5,
                                margin=margin(10,0,15,0)),
      plot.subtitle = element_text(hjust = c(0.8, 0.2),
                                   size = 50),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 30),
      panel.grid = element_blank()
    )
  
}

spotplot.2024.year. <- year_plot(input = spot.data2, year = "2024")

ggsave(filename = "2024.species.15min.jpg",
       plot = spotplot.2024.year.,
       width = 10,
       height = 7)
