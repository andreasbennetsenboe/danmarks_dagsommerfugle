library("tidyverse")
library("lubridate")
library("wesanderson")
library(showtext)

visits.transects <- read.csv("data/download.sample.visit.information.from.transects.csv") %>%
  mutate(date = dmy(Date),
         sample = as.factor(Transect.Sample.ID)) %>%
  mutate(year = year(date),
         month = as.factor(month(date)),
         day = day(date),
         DOY = yday(date),
         week = week(date)
  ) %>%
  filter(year %in% c(2023, 2024, 2025)) %>%
  select(sample, date, month, year) %>%
  filter(month %in% c("4", "5", "6", "7", "8", "9")) %>%
  mutate(month = recode(month,
                        `4` = "April",
                        `5` = "Maj",
                        `6` = "Juni",
                        `7` = "Juli",
                        `8` = "August",
                        `9` = "September")
         ) %>%
  droplevels()

different.transects <- read.csv("data/download.sample.visit.information.from.transects.csv") %>%
  mutate(date = dmy(Date),
         sample = as.factor(Transect.Sample.ID)) %>%
  mutate(year = year(date),
         month = as.factor(month(date)),
         day = day(date),
         DOY = yday(date),
         week = week(date)
  ) %>%
  group_by(year, month) %>%
  summarise(
    counts = n(),
    different.transects = n_distinct(Transect.ID),
            
            ) %>%
  filter(year %in% c(2023, 2024, 2025)) %>%
  filter(month %in% c("4", "5", "6", "7", "8", "9")) %>%
  mutate(month = recode(month,
                        `4` = "April",
                        `5` = "Maj",
                        `6` = "Juni",
                        `7` = "Juli",
                        `8` = "August",
                        `9` = "September")
  ) %>%
  droplevels()

visits.15min <- read.csv(file = "data/download.timed.count.sample.data.csv") %>%
  mutate(date = dmy(Date),
         sample = as.factor(Visit.Sample.ID)) %>%
  mutate(year = year(date),
         month = as.factor(month(date)),
         day = day(date),
         DOY = yday(date),
         week = week(date)
  ) %>%
  filter(year %in% c(2023, 2024, 2025)) %>%
  select(sample, date, month, year) %>%
  filter(month %in% c("4", "5", "6", "7", "8", "9")) %>%
  mutate(month = recode(month,
                        `4` = "April",
                        `5` = "Maj",
                        `6` = "Juni",
                        `7` = "Juli",
                        `8` = "August",
                        `9` = "September")
  ) %>%
  droplevels()

#font_add_google(name = "Amatic SC", family = "amatic-sc")
#showtext_auto()

plot.visit.transect <- ggplot()+
  geom_bar(data = visits.transects,
           mapping = aes(x = month,
                         fill = factor(year)),
           position = position_dodge(),
           width = 0.6,
           col = "#495B73"
           ) +
  theme_classic()+
  theme(
    text = element_text(family = "amatic-sc", face = "bold"),
    axis.text.x = element_text(size = 50, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 50),
    axis.title = element_text(size = 50),
    legend.title = element_blank(),
    legend.text = element_text(size = 30)
  )+
  scale_x_discrete(drop = FALSE)+
  xlab(label = "Måned")+
  ylab(label = "Transektbesøg")

plot.visit.transect


plot.different.transect <- ggplot()+
  geom_bar(data = different.transects,
           mapping = aes(x = month,
                         y = different.transects,
                         fill = factor(year)),
           position = position_dodge(),
           width = 0.6,
           col = "#495B73",
           stat = "identity"
  ) +
  theme_classic()+
  theme(
    text = element_text(family = "amatic-sc", face = "bold"),
    axis.text.x = element_text(size = 50, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 50),
    axis.title = element_text(size = 50),
    legend.title = element_blank(),
    legend.text = element_text(size = 30)
  )+
  scale_x_discrete(drop = FALSE)+
  xlab(label = "Måned")+
  ylab(label = "Forskellige transekter")


plot.different.transect


plot.15min <- ggplot()+
  geom_bar(data = visits.15min,
           mapping = aes(x = month,
                         fill = factor(year)),
           position = position_dodge(),
           width = 0.6,
           col = "#495B73"
  ) +
  theme_classic()+
  theme(
    text = element_text(family = "amatic-sc", face = "bold"),
    axis.text.x = element_text(size = 50, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 50),
    axis.title = element_text(size = 50),
    legend.title = element_blank(),
    legend.text = element_text(size = 30)
  )+
  scale_x_discrete(drop = FALSE)+
  xlab(label = "Måned")+
  ylab(label = "15 min. tællinger")

plot.15min

ggsave(
  filename = paste0("output/transect_month_activity.jpg"),
  plot = plot.visit.transect,
  width = 8,
  height = 6
)

ggsave(
  filename = paste0("output/transect_month_activity2.jpg"),
  plot = plot.different.transect,
  width = 8,
  height = 6
)

ggsave(
  filename = paste0("output/15min_month_activity.jpg"),
  plot = plot.15min,
  width = 8,
  height = 6
)

