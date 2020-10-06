library(tidyverse)         # for graphing and data cleaning
library(ggplot2)
library(googlesheets4)     # for reading in data from google sheets
library(lubridate)         # for working with dates
library(shiny)
library(rsconnect)
theme_set(theme_minimal()) # you could choose a theme here
gs4_deauth()               # To not have to authorize each time

garden_harvest <- read_sheet("https://docs.google.com/spreadsheets/d/1DekSazCzKqPS2jnGhKue7tLxRU3GVL1oxi-4bEM5IWw/edit?usp=sharing") %>% 
  mutate(date = ymd(date))

ui <- fluidPage(
  checkboxGroupInput("tomVar", 
                     "Tomato Varieties [in order of total weight]", 
                     choices = c(`Amish Paste` = "Amish Paste", Volunteers = "volunteers", `Better Boy` = "Better Boy", 
                                 Grape = "grape", `Big Beef` = "Big Beef", `Bonny Best` = "Bonny Best", 
                                 `Mortgage Lifter` = "Mortgage Lifter", `Old German` = "Old German", 
                                 `Cherokee Purple` = "Cherokee Purple", `Black Krim` = "Black Krim", 
                                 `Jet Star` = "Jet Star", Brandywine = "Brandywine")),
  plotOutput(outputId = "timeplot")
)

server <- function(input, output) {
  output$timeplot <- renderPlot({
    garden_harvest %>%
      filter(vegetable == "tomatoes", 
             variety %in% input$tomVar) %>%
      group_by(variety) %>%
      mutate(cum_weight_lbs = sum(weight)/453.59237) %>%
      mutate(variety = fct_reorder(variety, 
                                   cum_weight_lbs,
                                   .desc = TRUE)) %>%
      ggplot(aes(x = date, y = weight), 
             show.legend = FALSE) +
      geom_smooth(aes(color = variety), 
                  se = FALSE, 
                  size = .5) +
      geom_point(aes(color = variety),
                 show.legend = FALSE) +
      labs(title = "Tomato Varieties' Growth over Time",
           subtitle = "by: Nicole Higgins",
           x = "Date",
           y = "Weight [lbs]") +
      theme(plot.title = element_text(size = 18, face = "bold"))
  })
}

shinyApp(ui = ui, server = server)