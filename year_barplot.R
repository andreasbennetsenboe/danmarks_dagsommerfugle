library("tidyverse")
library("lubridate")
library(showtext)


transect.data.raw <- read.csv(file = "data/download.species.occurences.from.transects.csv")

transect.data <- read.csv(file = "data/download.species.occurences.from.transects.csv")%>%
  mutate(Date = dmy(Date))%>%
  mutate(Year = year(Date)) %>%
  filter(Record.status == "Accepted")

site.data <- read.csv("data/download.site.details.csv") %>%
  dplyr::select(Spatial.Reference, Transect.ID, Transect.Name, length = "Overall.Length..m.") %>%
  drop_na() %>%
  filter(length > 100)%>%
  separate(col = Spatial.Reference, into = c("N", "E"), sep = " ") %>%
  mutate(N = gsub(",$", "", N)) %>%
  mutate(N = gsub("[[:alpha:]]", "", N))%>%
  mutate(E = gsub("[[:alpha:]]", "", E)) %>%
  mutate(N = as.numeric(N),
         E = as.numeric(E))

####---- Data import and wrangling ----####

redlist.join <- function (x){
  x %>% left_join(
    y = redlist <- read.csv(file = "data/redlist.csv", sep = ";") %>%
      select(species = "scientificName", redlist = "redlistCategory") %>%
      mutate(redlist = case_when(is.na(redlist)~"NA",
                                 TRUE ~ redlist)),
    by = "species"
  )
}

transect.data1 <- transect.data.raw %>%
  filter(Transect.Name %in% site.data$Transect.Name) %>%
  dplyr::select(1:3, Transect.Name, count = Abundance.Count, date = "Date", species = "Preferred.Species.Name") %>%
  filter(species %in% read_lines(file = "data/butterfly.species.txt")) %>%
  mutate(date = dmy(date)) %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>%
  mutate(DOY = yday(date)) %>%
  left_join(y = read.csv(file = "data/dk.species.names.txt"), by = "species")%>%
  left_join(y = read.csv(file = "data/dk.family.names.txt"), by = "species") %>%
  left_join(y = read.csv(file = "data/dk.alttax.names.txt"), by = "species") %>%
  redlist.join()%>%
  filter(!is.na(count)) %>%
  filter(year <= 2024) %>%
  group_by(species.dk) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count))

text.data <- transect.data1 %>%
  filter(count <= 100) %>%
  unite(col = data, species.dk, count, sep = " ")

figure.data <- transect.data1 %>%
  filter(count >= 100)

font_add_google(name = "Amatic SC", family = "amatic-sc")

showtext_auto()

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
              size = 13,
              family = "amatic-sc", 
              fontface = "bold"
    )+
    coord_flip()+
    theme_minimal(
    )+
    labs(x = "",
         y = "",
         #title = paste0("Observationer i ", year),
         #subtitle = c(paste0("Antal individer: ", sum(input$count)), 
         #             paste0("Antal transektture: ", n_distinct(transect.data1$Transect.Sample.ID)))
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
      axis.text.y = element_text(size = 40),
      panel.grid = element_blank()
    )
  
}

speciesplot.2024 <- year_plot(input = figure.data, year = "2024")

speciesplot.2024

writeLines(text = text.data$data, con = "output/species.totals.under.100.txt", sep = ", ")

ggsave(filename = "output/speciesplot.2024.jpg",
       plot = speciesplot.2024,
       width = 10,
       height = 7)

