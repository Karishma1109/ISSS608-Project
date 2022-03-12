library(shiny)
library(bslib)
library(tidyverse)

imports <- read_csv('data\\SGImports.csv')
exports <- read_csv('data\\SGExports.csv')

# Combine imports and exports into 1 df
trade <- 

# Create functions outside that needs to be called more than once

ui <- fluidPage(
    theme = bs_theme(version = 5, bootswatch = "superhero"),
    # Application title
    titlePanel("Singapore Merchandise Trade"),
    # Add functions to create user interface into the tabs
    tabsetPanel(
      tabPanel(
        "Descriptive Analysis"
      ),
      tabPanel(
        "Comparative Analysis"
      ),
      tabPanel(
        "Time Series Clustering"
      )
    )
    
)

server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
