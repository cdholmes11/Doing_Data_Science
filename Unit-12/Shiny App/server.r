# Libraries
library(magrittr)
library(tidyverse)
library(ggplot2)

beers_raw <- read.csv("C:/Users/corey/OneDrive/Documents/GitHub/DDS-Project-1/Beers.csv", header = TRUE, encoding = "UTF-8")
breweries_raw <- read.csv("C:/Users/corey/OneDrive/Documents/GitHub/DDS-Project-1/Breweries.csv", header = TRUE, encoding = "UTF-8")

brew_beers <-
  left_join(beers_raw, breweries_raw, by = c("Brewery_id" = "Brew_ID")) %>%
  select(Name.x, Beer_ID, ABV, IBU, Brewery_id, Style, Ounces, Name.y, City,
         State) %>%
  filter(!is.na(ABV) | !is.na(IBU)) %>%
  rename("beer_name" = "Name.x") %>%
  rename("brewery_name" =  "Name.y")

states <- brew_beers %>% select(State) %>% distinct()
states <- states[order(states$State), ]


server <- function(input, output) {
  # Scatter Plot
  output$dist_plot3 <- renderPlot({
    if (input$select == "on") {
      if(input$select2 == "All") {
        brew_beers %>%
          ggplot(aes(x = IBU, y = ABV)) +
          geom_point() +
          geom_smooth(method = "lm") +
          theme_minimal()
      }
      else {
        brew_beers %>%
          filter(State == input$select2) %>%
          ggplot(aes(x = IBU, y = ABV)) +
          geom_point() +
          geom_smooth(method = "lm") +
          theme_minimal()        
      }
      
    }
    else {
      brew_beers %>%
        filter(State == input$select2) %>%
        ggplot(aes(x = IBU, y = ABV)) +
        geom_point() +
        theme_minimal()
    }
    
  })
  
  # ABV
  output$dist_plot <- renderPlot({
    
    dist <- switch(input$dist,
                   hist = geom_histogram(fill = "blue", color = "white"),
                   box = geom_boxplot(fill = "blue"),
    )
    
    brew_beers %>%
      filter(State == input$select2) %>%
      ggplot(aes(x = ABV)) +
      dist +
      ylab("ABV") +
      theme_minimal()
  })
  
  # IBU
  output$dist_plot2 <- renderPlot({
    
    dist <- switch(input$dist,
                   hist = geom_histogram(fill = "blue", color = "white"),
                   box = geom_boxplot(fill = "blue"),
    )
    
    brew_beers %>%
      filter(State == input$select2) %>%
      ggplot(aes(x = IBU)) +
      dist +
      ylab("IBU") +
      theme_minimal()
  })
  
  # Median ABV by State
  
  output$dist_plot4 <- renderPlot({
    median_state_abv <-
      brew_beers %>%
      filter(!is.na(ABV)) %>%
      group_by(State) %>%
      summarize(median_abv = median(ABV)) %>%
      as.data.frame()
    median_state_abv
    
    median_state_abv %>%
      ggplot(aes(
        x = reorder(State, -median_abv),
        y = median_abv * 100)
      ) +
      geom_bar(stat = "identity", fill = "#2a5769") +
      geom_text(aes(label = median_abv * 100),
                stat = "identity", vjust = -1, size = 3) +
      xlab("State") +
      ylab("Median ABV") +
      ggtitle("Median ABV by State") +
      theme_minimal() +
      theme(legend.position = "none")
  })
}