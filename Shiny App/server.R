library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(gt)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  house_table <- seat_function(input$total_seats)
  
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
