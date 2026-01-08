library("tidyverse")
library("lubridate")
library("wesanderson")
library(showtext)

####---- DATA ----####

sites <- read.csv("data/download.site.details.csv") %>%
  dplyr::select(Spatial.Reference, Transect.ID, Transect.Name, transect.length = "Overall.Length..m.") %>%
  drop_na() %>%
  filter(transect.length > 100) %>%
  dplyr::select(transectid = "Transect.ID", transect.length) %>%
  mutate(transect.length = as.numeric(transect.length),
         transectid = as.factor(transectid))


####--- Bias check ---####

visit.info <- read.csv(file = "data/download.species.occurences.from.transects.csv") %>%
  filter(Record.status == "Accepted" & Reliability == "1 Suitable conditions") %>%
  select(transectid = "Transect.ID", date = "Date") %>%
  mutate(date = dmy(date),
         transectid = as.factor(transectid)) %>%
  mutate(year = year(date)) %>%
  select(-date) %>%
  unique() %>%
  filter(year %in% c(2023, 2024, 2025)) %>%
  group_by(transectid) %>%
  summarize(
    visit_info = case_when(
      all(c(2023, 2024, 2025) %in% year) ~ "All",
      TRUE ~ NA_character_
    )
  )

####---- Analysis ----####

transect.data <- read.csv(file = "data/download.species.occurences.from.transects.csv") %>%
  filter(Record.status == "Accepted" & Reliability == "1 Suitable conditions") %>%
  dplyr::select(
    transectid = "Transect.ID",
    Transect.Name, 
    count = Abundance.Count, 
    date = "Date", 
    species = "Preferred.Species.Name", 
    sampleid = "Transect.Sample.ID") %>%
  drop_na(count) %>%
  filter(species %in% read_lines(file = "data/butterfly.taxons.txt")) %>%
  mutate(date = dmy(date)) %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date),
         DOY = yday(date),
         week = week(date)
  ) %>%
  mutate(year.factor = as.factor(year),
         transectid = as.factor(transectid)) %>%
  filter(week >= 14 & week <= 39) %>%
  filter(year.factor %in% c("2023","2024", "2025")) %>%
  group_by(transectid, sampleid, week, year.factor) %>%
  summarise(count = sum(count)) %>%
  left_join(y = sites, by = "transectid") %>%
  drop_na() %>%
  mutate(density = count/transect.length*1000) %>%
  left_join(y = visit.info, by = "transectid")

plot2.data <- transect.data %>%
  filter(visit_info == "All") %>%
  group_by(year.factor, week) %>%
  summarise(mean = mean(count),  
            sd=sd(count),  
            N=n(),  
            se=sd/sqrt(N),  
            upper_limit=mean+se,  
            lower_limit=mean-se,
            upper_limit2=mean+sd,
            lower_limit2=mean-sd) %>%
  arrange(week, year.factor) %>%
  filter(N >= 3)

year.avg <- plot2.data %>%
  filter(week >= 20 & week <= 39) %>% # starter senere pga. 2023
  group_by(year.factor) %>%
  summarise(mean = mean(mean))

font_add_google(name = "Amatic SC", family = "amatic-sc")
showtext_auto()


plot2 <- ggplot(data = plot2.data)+
  #geom_point(
  #   mapping = aes(x = week, 
  #                 y = mean, 
  #                 col = year.factor
  #                 )
  #   )+
  geom_ribbon(mapping = aes(x = week, 
                            y = mean,
                            ymin = lower_limit, 
                            ymax = upper_limit,
                            fill = year.factor
  ),
  alpha = 0.5,
  show.legend = FALSE
  )+
  geom_path(
    data = plot2.data,
    mapping = aes(x = week, 
                  y = mean, 
                  col = factor(year.factor), 
                  group = year.factor
    )
  )+
  scale_color_manual(  
    values = c(
      "2023" = "#D2D3D7",
      "2024" = "#B2B3B7",
      "2025" = "#63769e"
    )
  )+
  scale_fill_manual(
    values = c(
      "2023" = "#D2D3D7",
      "2024" = "#B2B3B7",
      "2025" = "#63769e"
    )
  )+
  theme_classic()+
  xlab(label = "Uge")+
  ylab(label = "Individer")+
  labs(col = "Gennemsnitlige antal individer pr transekt pr km")+
  theme(
    text = element_text(family = "amatic-sc", face = "bold"),
    axis.text = element_text(size = 50),
    axis.title = element_text(size = 50),
    legend.text = element_text(size = 50),
    legend.title = element_text(size = 50),
    legend.key.size = unit(0.5, "cm"),
    legend.position = "top"
  )

plot2

ggsave(
  filename = paste0("output/phenology_graph_threeyears.jpg"),
  plot = plot2,
  width = 8,
  height = 6
)



