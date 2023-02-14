library(tidyverse)
library(leaflet)
library(htmltools)
library(sf)
library(RColorBrewer)
library(gt)

state_map <- st_read("s_22mr22//s_22mr22.shp") %>%
  filter(NAME %in% census_2010$STATENAME)

census_2020_states <- read.csv("census_2020_states.csv") %>%
  select(STATENAME, P1_001N) %>%
  rename("population" = P1_001N) %>%
  mutate("seats" = 1)

repeat{
  census_2020_states$raw_ranking <- (census_2020_states$population) / sqrt(census_2020_states$seats * (census_2020_states$seats + 1))
  
  census_2020_states$ranking <- rank(census_2020_states$raw_ranking)
  
  census_2020_states <- census_2020_states %>%
    mutate("seats" = ifelse(ranking == 50, seats + 1, seats),
           "people_per_representative" = population / seats)
  
  census_2020_states$raw_ranking <- (census_2020_states$population) / sqrt(census_2020_states$seats * (census_2020_states$seats + 1))
  
  census_2020_states$ranking <- rank(census_2020_states$raw_ranking)
  
  if(sum(census_2020_states$seats) == 435){
    break
  }
}

seat_function <- function(number) {
  
  table <- read_csv("census_2020_states.csv") %>%
    select(STATENAME, P1_001N) %>%
    rename("population" = P1_001N) %>%
    mutate("seats" = 1)
  
  repeat{
    table$raw_ranking <- (table$population) / sqrt(table$seats * (table$seats + 1))
    
    table$ranking <- rank(table$raw_ranking)
    
    table <- table %>%
      mutate("seats" = ifelse(ranking == 50, seats + 1, seats),
             "people_per_representative" = population / seats)
    if(sum(table$seats) == number){
      break
    }
  }
  
  table %>%
    rename(
      "State" = STATENAME,
      "Seats" = seats,
      "Population" = population,
      "People Per Representative" = people_per_representative
    ) %>%
    select(State, Seats, Population, `People Per Representative`)
  
}

census_2020_states$raw_ranking <- (census_2020_states$population) / sqrt(census_2020_states$seats * (census_2020_states$seats + 1))

census_2020_states$ranking <- rank(census_2020_states$raw_ranking)

census_2020_states <- census_2020_states %>%
  mutate("seats" = ifelse(ranking == 50, seats + 1, seats),
         "people_per_representative" = population / seats)

census_2020_states$raw_ranking <- (census_2020_states$population) / sqrt(census_2020_states$seats * (census_2020_states$seats + 1))

census_2020_states$ranking <- rank(census_2020_states$raw_ranking)

census_2020_states <- census_2020_states[order(census_2020_states$raw_ranking, decreasing = TRUE),]

head(census_2020_states %>%
       rename(
         "State" = STATENAME,
         "Population" = population,
         "Seats" = seats,
         "Huntington-Hill Ranking" = raw_ranking
       ) %>% 
       select(State, Population, Seats, `Huntington-Hill Ranking`)) %>%
  gt() %>%
  fmt_number(
    columns = c(`Huntington-Hill Ranking`),
    use_seps = TRUE
    ) %>%
  fmt_number(
    columns = c(Population),
    decimals = 0,
    use_seps = TRUE
  )

exibble %>%
  gt() %>%
  fmt_number(
    columns = num,
    decimals = 3,
    use_seps = FALSE
  )




