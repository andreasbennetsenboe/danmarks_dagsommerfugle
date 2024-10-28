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
  filter(year == 2024) %>%
  select(sample, date, month) %>%
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

visits.15min <- read.csv(file = "data/download.timed-count.sample.data.csv") %>%
  mutate(date = dmy(Date),
         sample = as.factor(Visit.Sample.ID)) %>%
  mutate(year = year(date),
         month = as.factor(month(date)),
         day = day(date),
         DOY = yday(date),
         week = week(date)
  ) %>%
  filter(year == 2024) %>%
  select(sample, date, month) %>%
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

plot.transect <- ggplot()+
  geom_bar(data = visits.transects,
           mapping = aes(x = month),
           fill = "#869EC2",
           width = 0.6,
           col = "#495B73"
           ) +
  theme_classic()+
  theme(
    text = element_text(family = "amatic-sc", face = "bold"),
    axis.text.x = element_text(size = 50, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 50),
    axis.title = element_text(size = 50)
  )+
  scale_x_discrete(drop = FALSE)+
  xlab(label = "Måned")+
  ylab(label = "Transektbesøg")


ggsave(
  filename = paste0("output/transect_month_activity.jpg"),
  plot = plot,
  width = 8,
  height = 6
)

plot.15min <- ggplot()+
  geom_bar(data = visits.15min,
           mapping = aes(x = month),
           fill = "#869EC2",
           width = 0.6,
           col = "#495B73"
  ) +
  theme_classic()+
  theme(
    text = element_text(family = "amatic-sc", face = "bold"),
    axis.text.x = element_text(size = 50, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 50),
    axis.title = element_text(size = 50)
  )+
  scale_x_discrete(drop = FALSE)+
  xlab(label = "Måned")+
  ylab(label = "Transektbesøg")


ggsave(
  filename = paste0("output/15min_month_activity.jpg"),
  plot = plot.15min,
  width = 8,
  height = 6
)
