library("tidyverse")
library("lubridate")
library(showtext)

sampling.year <- 2025

font_add_google(name = "Amatic SC", family = "amatic-sc")

showtext_auto()

redlist.join <- function (x){
  x %>% left_join(
    y = redlist <- read.csv(file = "data/redlist.csv", sep = ";") %>%
      select(species = "scientificName", redlist = "redlistCategory") %>%
      mutate(redlist = case_when(is.na(redlist)~"NA",
                                 TRUE ~ redlist)),
    by = "species"
  )
}

transect.data.raw <- read.csv(file = "data/download.species.occurences.from.transects.csv")

raw.15min.obs <- read.csv(file = "data/download.timed.count.occurences.csv") %>%
  select(date = "Date", count = "Count", species = "Accepted.species.name")

raw.15min.obs.ss <- read.csv(file = "data/download.single-species-timed-count.occurences.csv") %>%
  select(date = "Date", count = "Count", species = "Searched.for.species...accepted.name")

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

####---- Tidy transect data ----####

transect.data1 <- transect.data.raw %>%
  filter(Record.status == "Accepted") %>%
  filter(Transect.Name %in% site.data$Transect.Name) %>%
  dplyr::select(count = Abundance.Count, date = "Date", species = "Preferred.Species.Name") %>%
  filter(species %in% read_lines(file = "data/butterfly.species.txt")) %>%
  mutate(date = dmy(date)) %>%
  mutate(year = year(date)) %>%
  left_join(y = read.csv(file = "data/dk.species.names.txt"), by = "species")%>%
  filter(!is.na(count)) %>%
  filter(year == sampling.year) %>%
  group_by(species.dk) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count))

transect.text.data <- transect.data1 %>%
  filter(count < 100) %>%
  unite(col = data, species.dk, count, sep = " ")

transect.figure.data <- transect.data1 %>%
  filter(count >= 100)

transect.speciesplot <- year_plot(input = transect.figure.data, year = sampling.year)

transect.speciesplot

writeLines(text = transect.text.data$data, con = paste0("output/year_barplot/totals.under.100.transect.", sampling.year, ".txt"), sep = ", ")

ggsave(filename = paste0("output/year_barplot/speciesplot.transect.", sampling.year, ".jpg"),
       plot = transect.speciesplot,
       width = 10,
       height = 7)

####---- Tidy 15 min data ----####

data.15.min <- raw.15min.obs %>%
  filter(species %in% read_lines(file = "data/butterfly.species.txt")) %>%
  mutate(date = dmy(date)) %>%
  mutate(year = year(date)) %>%
  left_join(y = read.csv(file = "data/dk.species.names.txt"), by = "species")%>%
  filter(!is.na(count)) %>%
  filter(year == sampling.year) %>%
  group_by(species.dk) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count))

text.data.15min <- data.15.min %>%
  filter(count < 100) %>%
  unite(col = data, species.dk, count, sep = " ")

figure.data.15min <- data.15.min %>%
  filter(count >= 100)

speciesplot.15min <- year_plot(input = figure.data.15min, year = sampling.year)

speciesplot.15min

writeLines(text = text.data.15min$data, paste0(con = "output/year_barplot/totals.under.100.15min.", sampling.year, ".txt"), sep = ", ")

ggsave(filename = paste0("output/year_barplot/speciesplot.15min.", sampling.year, ".jpg"),
       plot = speciesplot.15min,
       width = 10,
       height = 7)

####---- Tidy all data ----####

all.data1 <- transect.data.raw %>%
  filter(Record.status == "Accepted") %>%
  filter(Transect.Name %in% site.data$Transect.Name) %>%
  dplyr::select(count = Abundance.Count, date = "Date", species = "Preferred.Species.Name") %>%
  bind_rows(raw.15min.obs, raw.15min.obs.ss) %>%
  filter(species %in% read_lines(file = "data/butterfly.species.txt")) %>%
  mutate(date = dmy(date)) %>%
  mutate(year = year(date)) %>%
  left_join(y = read.csv(file = "data/dk.species.names.txt"), by = "species")%>%
  filter(!is.na(count)) %>%
  filter(year == sampling.year) %>%
  group_by(species.dk) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count))

all.text.data <- all.data1 %>%
  filter(count < 100) %>%
  unite(col = data, species.dk, count, sep = " ")

all.figure.data <- all.data1 %>%
  filter(count >= 100)

all.speciesplot <- year_plot(input = all.figure.data, year = sampling.year)

all.speciesplot

writeLines(
  text = all.text.data$data,
  con = paste0("output/year_barplot/alldata.totals.under.100.", sampling.year, ".txt"),
  sep = ", "
  )

ggsave(
  filename = paste0("output/year_barplot/alldata.speciesplot.", sampling.year, ".jpg"),
  plot = all.speciesplot,
  width = 10,
  height = 7
  )
                     
