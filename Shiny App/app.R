library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(gt)

ui <- fluidPage(
    navbarPage(
      title = "Redistricting & Apportionment",
      tabPanel(title = "Abolished vs. High Growth Districts in 2010",
               mainPanel(leafletOutput("map1", width = "150%", height = "875px"))),
      tabPanel(title = "House Expansion",
               sidebarLayout(
                 sidebarPanel(
                   sliderInput(
                     "total_seats", "Number of Seats in the House",
                     min = 50, max = 1000, value = 435)
                 ),
                 mainPanel(gt_output("table"))
             ))
  )
)

server <- function(input, output) {
  
  seat_function <- function(number) {
    
    table1 <- table
    
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
  
  house_table <- seat_function(500)
  
  output$map1 <- renderLeaflet({
    leaflet() %>%
      addPolygons(data = map_2010_abolished, color = "#008EFF",
                  weight = 2,
                  popup = ~paste(paste("<b>", STATENAME, "District", DISTRICT, "</b>"),
                                 paste(""),
                                 paste("<b>", "White Population Percentage:", "</b>", white_percent),
                                 paste("<b>", "Black or African American Percentage:", "</b>", black_percent),
                                 paste("<b>", "American Indian or Alaska Native Percentage:", "</b>", am_indian_alskan_percent),
                                 paste("<b>", "Asian Percentage:", "</b>", asian_percent),
                                 paste("<b>", "Native Hawaiian and Other Pacific Islander Percentage:", "</b>", nat_hawaiian_pi_percent),
                                 paste("<b>", "Some Other Race Percentage:", "</b>", some_other_race_percent),
                                 paste("<b>", "Two or More Races Percentage:", "</b>", two_plus_race_percent),
                                 paste(""),
                                 paste("<b>", "Population Working in Construction:", "</b>", construction),
                                 paste("<b>", "Median Household Income:", "</b>", median_income),
                                 paste("<b>", "Region Type:", "</b>", Type),
                                 paste(""),
                                 paste("<b>", "Status:", "</b>", "Abolished District"),
                                 sep = "</br>")) %>%
      addPolygons(data = map_2010_high_growth, color = "#FF008E",
                  weight = 2,
                  popup = ~paste(paste("<b>", STATENAME, "District", DISTRICT, "</b>"),
                                 paste(""),
                                 paste("<b>", "White Population Percentage:", "</b>", white_percent),
                                 paste("<b>", "Black or African American Percentage:", "</b>", black_percent),
                                 paste("<b>", "American Indian or Alaska Native Percentage:", "</b>", am_indian_alskan_percent),
                                 paste("<b>", "Asian Percentage:", "</b>", asian_percent),
                                 paste("<b>", "Native Hawaiian and Other Pacific Islander Percentage:", "</b>", nat_hawaiian_pi_percent),
                                 paste("<b>", "Some Other Race Percentage:", "</b>", some_other_race_percent),
                                 paste("<b>", "Two or More Races Percentage:", "</b>", two_plus_race_percent),
                                 paste(""),
                                 paste("<b>", "Population Working in Construction:", "</b>", (construction)),
                                 paste("<b>", "Median Household Income:", "</b>", median_income),
                                 paste("<b>", "Region Type:", "</b>", Type),
                                 paste(""),
                                 paste("<b>", "Status:", "</b>", "High Growth District"),
                                 sep = "</br>")) %>%
      addTiles()
  })
  
  output$table <- render_gt(
    expr = house_table,
    height = px(600), width = px(600)
  )
  
}

shinyApp(ui = ui, server = server)