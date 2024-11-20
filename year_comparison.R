library(tidyverse)
library(lubridate)
library(showtext)
library(grid)
library(gridExtra)
library(viridis)
library("ggpubr")
library("ggtext")
library(ggbreak)

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
  filter(species %in% read_lines(file = "data/butterfly.species.txt")) %>%
  mutate(date = dmy(date)) %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>%
  mutate(DOY = yday(date)) %>%
  left_join(y = read.csv(file = "data/dk.species.names.txt"), by = "species")%>%
  left_join(y = read.csv(file = "data/dk.family.names.txt"), by = "species") %>%
  left_join(y = read.csv(file = "data/dk.alttax.names.txt"), by = "species") %>%
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

table <- facet1.corrected %>%
  bind_rows(facet2.corrected) %>%
  select(Art = "species.dk", År = "year", Tæthed = "count.corrected") %>%
  arrange(Art, År)

write.table(x = table, 
            file = "output/year.species.comparison.csv",
            dec = ",",
            row.names = FALSE)

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
