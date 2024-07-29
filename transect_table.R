library(tidyverse)
library("mmtable2")
library("gt")
library(lubridate)

transect_table <- function(file, transect.name) {
  
  transect.occurences <- read.csv(file = file) %>%
    filter(Transect.Name %in% transect.name) %>%
    mutate(Date = dmy(Date)) %>%
    separate(col = Section.Name, into = c("transects", "sections"), sep = " - ", remove = F) %>%
    select(transects.and.sections = "Section.Name", transects, dates = "Date", sections, species = "Preferred.Species.Name", count = "Abundance.Count")
  
  dates    <- unique(transect.occurences$dates)
  sections <- unique(transect.occurences$transects.and.sections)
  species  <- unique(transect.occurences$species)
  
  n.dates    <- n_distinct(dates)
  n.sections <- n_distinct(sections)
  n.species  <- n_distinct(species)
  
  skeleton <- tibble(
    dates =    rep(x = dates,    each =  n.sections*n.species),
    transects.and.sections = rep(x = sections, each =  n.species,                      times = n.dates),
    species =  rep(x = species,  times = n.dates*n.sections  )
  )
  
  table.data <- left_join(x = skeleton, y = transect.occurences, by = c("dates", "transects.and.sections", "species")) %>%
    select(-transects, -sections) %>%
    separate(col = transects.and.sections, into = c("transects", "sections"), sep = " - ", remove = F)
  
  mmtable(data = table.data,
          cells = count,
          table_name = transect.name)+
    {if(1<n_distinct(transect.occurences$transects))header_left(transects.and.sections) else 
    {if(1<n_distinct(transect.occurences$sections))header_left(sections)}}+
    header_left_top(dates)+
    header_top(species)+
    
    header_format(species, list(cell_text(style="italic",
                                          align = "left")))+
    table_format(locations=list(cells_body(rows = seq(from = 2, to = 2+(n.dates-1)*n.sections, by=n.sections))),
                 style = list(cell_borders(sides = "top", color = "grey")))+
    cells_format(cell_predicate = T, style = list(cell_text(align = "center")))
  
}

transect_table(file = "occurences.csv", transect.name = "Læsten Bakker 2024")
