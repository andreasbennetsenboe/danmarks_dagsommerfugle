library(tidyverse)
library(lubridate)
library(stringr)
#library(xlsx)

sampling.year <- 2025

# --- Visits from transects ---
data.t <- read.csv("data/download.sample.visit.information.from.transects.csv") %>%
  select(
    recorder.id   = "Recorded.by", 
    sample.id     = "Transect.Sample.ID",
    date          = "Date"
    ) %>%
  mutate(recorder.id = case_when(
    recorder.id == "Jakob Christensen, Sara Kande" ~ "Jakob Christensen og Sara Kande",
    TRUE ~ recorder.id
    )
    ) %>%
  separate_rows(recorder.id, sep = "\\s+og\\s+") %>%
  mutate(
    year        = year(dmy(date)),
    recorder.id = str_replace_all(recorder.id, "  ", " "),
    recorder.id = if_else(
      str_detect(recorder.id, ","),
      str_replace(recorder.id, "(.*),\\s*(.*)", "\\2 \\1"),
      recorder.id
    ),
    recorder.id = if_else(
      str_detect(recorder.id, "Skovsgaard"),
      str_replace_all(recorder.id, c("Mlj" = "MLJ", "mlj" = "MLJ", "tfn" = "TFN")),
      recorder.id
    ),
    recorder.id = if_else(recorder.id == "Lykke Niels", "Niels Lykke", recorder.id)
  ) %>% select(-date)
  

str(visits.transects)
unique(visits.transects$recorder.id)

data.t <- visits.transects %>%
  select(recorder.ID, `2023`, `2024`, `2025`)



# --- Timed count data ---
data.15 <- read.csv("data/download.timed.count.sample.data.csv") %>%
  select(
    date        = "Date",
    recorder.id = "Recorded.by",
    sample.id   = "Visit.Sample.ID"
  ) %>%
  mutate(date = dmy(date), 
         year = year(date)) %>%
  transmute(
    recorder.id = if_else(
      str_detect(recorder.id, ","),
      str_replace(recorder.id, "(.*),\\s*(.*)", "\\2 \\1"),
      recorder.id
    ),
    sample.id = sample.id,
    year = year
  )

data.15 <- read.csv("data/download.timed.count.sample.data.csv") %>%
  select(
    date        = "Date",
    recorder.id = "Recorded.by",
    sample.id   = "Visit.Sample.ID"
  ) %>%
  mutate(date = dmy(date), 
         year = year(date)) %>%
  transmute(
    recorder.id = if_else(
      str_detect(recorder.id, ","),
      str_replace(recorder.id, "(.*),\\s*(.*)", "\\2 \\1"),
      recorder.id
    ),
    sample.id = sample.id,
    year = year
  )

data.all <- bind_rows(
  data.t,
  data.15
) %>%
  filter(year %in% c(2023, 2024, 2025)) %>%
  group_by(recorder.id, year) %>%
  summarise(samples = n_distinct(sample.id), .groups = "drop") %>%
  pivot_wider(names_from  = year,
              values_from = samples,
              values_fill = 0
  )

write.xlsx(x = data.all, file ="output/volunteer_activity/volunteer_activity.xlsx")

