library(tidyverse)
library(lubridate)
library(showtext)

####---- Plot ----#####

data.raw <-  read.csv(file = "data/download.species.occurences.from.transects.csv")

data.raw1 <- data.raw %>%
  filter(Record.status == "Accepted") %>%
  mutate(date = dmy(Date)) %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date))

data1 <- data.raw1 %>%
  dplyr::select(1:3, count = Abundance.Count, date, species = "Preferred.Species.Name", year, month) %>%
  filter(species %in% read_lines(file = "data/butterfly.species.txt")) %>%
  filter(year == 2024 & month == 9) %>%
  left_join(y = read.csv(file = "data/dk.species.names.txt"), by = "species")

data2 <- data1 %>%
  group_by(species.dk) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count))

font_add_google(name = "Amatic SC", family = "amatic-sc")

showtext_auto() 

month_plot <- function(input, month) {
  
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
         title = paste0("Observationer i ", month),
         subtitle = c(paste0("Antal observationer: ", sum(data2$count)), 
                      paste0("Antal transektture: ", n_distinct(data1$Transect.Sample.ID)))
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

monthplot <- month_plot(input = data2, month = "september")

ggsave(filename = "output/september.jpg",
       plot = monthplot,
       width = 10,
       height = 7)



