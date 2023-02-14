library(tidyverse)

states <- read_csv("U.S. House Apportionment Spreadsheet.xlsx - U.S. House Apportionment.csv") %>%
  select(`U.S. State`, `Census Population (2010)`) %>%
  rename("state"= `U.S. State`,
         "population" = `Census Population (2010)`)

states_2020 <- read_csv("apportionment-2020-table01 (1).csv") %>%
  select(state, population)

states_2020$seats_hh_435 <- 1

repeat{
  states_2020$raw_ranking <- (states_2020$population) / sqrt(states_2020$seats_hh_435 * (states_2020$seats_hh_435 + 1))
  
  states_2020$ranking <- rank(states_2020$raw_ranking)
  
  states_2020 <- states_2020 %>%
    mutate("seats_hh_435" = ifelse(ranking == 50, seats_hh_435 + 1, seats_hh_435),
           "people_per_representative" = population / seats_hh_435)
  if(sum(states_2020$seats_hh_435) == 435){
    break
  }
}

