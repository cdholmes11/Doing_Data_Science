# Libraries
library(shiny)
library(magrittr)
library(tidyverse)
library(rsconnect)

# Data inport
beers_raw <- read.csv("Beers.csv", header = TRUE, encoding = "UTF-8")
breweries_raw <- read.csv("Breweries.csv", header = TRUE, encoding = "UTF-8")

# Data Merge
brew_beers <-
  left_join(beers_raw, breweries_raw, by = c("Brewery_id" = "Brew_ID")) %>%
  select(
    Name.x, Beer_ID, ABV, IBU, Brewery_id, Style, Ounces, Name.y, City,
    State
  ) %>%
  filter(!is.na(ABV) | !is.na(IBU)) %>%
  rename("beer_name" = "Name.x") %>%
  rename("brewery_name" = "Name.y")

# States Dataframe
states <- brew_beers %>%
  select(State) %>%
  distinct()
states <- states[order(states$State), ]

ui <- fluidPage(
  # Title
  fluidRow(
    column(12, 
           tags$h1("Beer Analysis")
    )
  ),
  
  # Side Panel for Inputs
  fluidRow(
    column(3,
           radioButtons(
             "dist", "Chart Type",
             c(
               "Histogram" = "hist",
               "Boxplot" = "box"
             )
           ),
           selectInput("select",
                       label = h3("Linear Regression Toggle"),
                       choices = c("On" = "on", "Off" = "off"),
                       selected = 1
           ),
           selectInput("select2",
                       label = h3("State Selection"),
                       choices = c("All", states),
                       selected = 1
           )       
    ),
    
    # Main Panel for Outputs
    column(9, 
           mainPanel(
             "ABV and IBU Charts",
             fluidRow(
               column(6, plotOutput(outputId = "dist_plot")),
               column(6, plotOutput(outputId = "dist_plot2")),
               column(6, plotOutput(outputId = "dist_plot3")),
               column(12, plotOutput(outputId = "dist_plot4"))
             )
           )
    ),
  ),
  fluidRow(
    tags$h1("GitHub Pages"),
    tags$a(
      href = "https://github.com/cdholmes11/cdholmes11.github.io",
      "If you want to see more, visit my GitHub Pages"
    )
  )
)