library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(gt)

shinyUI(
  fluidPage(
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
)

