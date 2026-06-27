#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(dplyr)
library(teamcolors)
source("../../scripts/NBA_synergy_copy.R")

teams <- filter_shooting_data(player = 'Team')
tc <- teams %>% select(TEAM_NAME, TEAM_ABBREVIATION) %>% left_join(teamcolors::teamcolors %>% rename('TEAM_NAME' = name), by = 'TEAM_NAME')



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("NBA Synergy Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "stat",
        "Statistic",
        choices = c(
          "POINTS",
          "DRIVE",
          "CATCH_SHOOT",
          "PULL_UP",
          "PAINT",
          "POST",
          "ELBOW"
        ),
        selected = "POINTS"
      ),
      
      selectInput(
        "offense",
        "Offensive Team",
        choices = setNames(
          tc$TEAM_ABBREVIATION,
          tc$TEAM_NAME
        ),
        selected = "SAS"
      ),
      
      selectInput(
        "defense",
        "Defensive Team",
        choices = setNames(
          tc$TEAM_ABBREVIATION,
          tc$TEAM_NAME
        ),
        selected = "NYK"
      )
      
    ),
    
    mainPanel(
      plotOutput("ovd_plot", height = "700px")
    )
    
  )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  plot_data <- reactive({
    
    req(input$stat)
    req(input$offense)
    req(input$defense)
    
    chart_ovd(
      stat = input$stat,
      offense = input$offense,
      defense = input$defense
    )[[1]]
  })
  
  output$ovd_plot <- renderPlot({
    plot_data()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
