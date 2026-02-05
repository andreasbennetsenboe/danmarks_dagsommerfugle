library(tidyverse)
library(lubridate)
library("sf")
library("terra")
library(openxlsx2)

years.of.interest <- c(2023, 2024, 2025)

kml <- st_read("data/spatial/transect.locations.kml") %>%
  mutate(site = sub("\\..*$", "", Name)) %>%
  group_by(site) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

full.sites <- read.csv(file = "data/spatial/full.sites.csv")

join <- full_join(kml, full.sites, by = c("site" = "Site.Code")) %>%
  select(
    name = "Site.Name",
    geometry,
    length = "Overall.Length..m.",
         )

visits.raw <- read.csv(file = "data/download.sample.visit.information.from.transects.csv") %>%
  filter(Transect.Name == "Vorup")
  
visits <- read.csv(file = "data/download.sample.visit.information.from.transects.csv") %>%
  select(Transect.Sample.ID, Transect.ID, Transect.Name, Date) %>%
  mutate(date = dmy(Date),
         Transect.ID = as.factor(Transect.ID)) %>%
  select(-Date) %>%
  mutate(year = year(date)) %>%
  filter(year %in% years.of.interest) %>%
  group_by(Transect.ID, Transect.Name, year) %>%
  summarise(visits = n()) %>%
  pivot_wider(names_from  = year,
              values_from = visits, 
              values_fill = 0
  ) %>%
  ungroup() %>%
  arrange(desc(`2025`), Transect.Name) %>%
  mutate(
    status = case_when(
      (`2023` > 0 | `2024` > 0) & `2025` == 0 ~ "inactive",
      TRUE ~ "active"
    )
  )

join2 <- left_join(
  join, visits, by = c("name" = "Transect.Name")
  ) %>%
  filter(length > 100) %>%
  drop_na() %>%
  filter(`2023` > 3 | `2024` > 3 | `2025` > 3)

st_write(join2, "output/transects.kml")

