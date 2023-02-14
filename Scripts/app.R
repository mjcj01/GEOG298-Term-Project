library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(gt)

ui <- fluidPage(
  navbarPage(
    title = "Redistricting & Apportionment",
    tabPanel(title = "Abolished vs. High Growth Districts in 2010",
             mainPanel(leafletOutput("map1", width = "150%", height = "800px"))),
    tabPanel(title = "House Expansion",
             sidebarLayout(
               sidebarPanel(
                 numericInput(
                   "total_seats", "Number of Seats in the House", value = 435)
               ),
               mainPanel(dataTableOutput("table"))
             ))
  )
)

server <- function(input, output) {
  
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
  
  df1 <- reactive({
    
  })
  
  output$table <- renderDataTable({seat_function(input$total_seats)})
  
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
  
}

shinyApp(ui = ui, server = server)