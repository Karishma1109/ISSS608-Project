#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


summaryUI <- function(id){
  fluidPage(
    titlePanel('Info'),
    tags$style(
      type = 'text/css', 
      '.bg-orange {background-color: #FF5A5F!important; }'
    ),
    fluidRow(
      infoBoxOutput(NS(id,'variableBox'), width = 3),
      infoBoxOutput(NS(id,'obBox'), width = 3),
      infoBoxOutput(NS(id,'numericBox'), width = 3)
    ),
    fluidRow(
      h3('Summary of Features'),
      column(5,
             DT::dataTableOutput(NS(id,'numeric'))))
  )
}

summaryServer <- function(id, trade){
  moduleServer(id, function(input, output, session){
    output$variableBox <- renderInfoBox({
      infoBox(
        'Trade Partners',
        value = tags$p(paste0(length(unique(trade$Country))),style = "font-size: 100%;"),
        icon = icon('globe'),
        color = 'navy',
        fill = TRUE
      )
    })
    
    output$obBox <- renderInfoBox({
      infoBox(
        'Data Set Features',
        value = tags$p(paste0("4 Variables",", 3969 Records"), style = "font-size: 100%;"),
        icon = icon('table'),
        color = 'navy',
        fill = TRUE
      )
    })    
    
    
    
    output$numericBox <- renderInfoBox({
      infoBox(
        'Time Period',
        value = tags$p(paste0(unique(trade$Year_Month[length(unique(trade$Year_Month))])," to ",(unique(trade$Year_Month)[1])),style = "font-size: 100%;"),
        icon = icon('calendar'),
        color = 'navy',
        fill = TRUE
      )
    })
    
    output$numeric <- DT::renderDataTable({
      
      skimDf <- trade %>%
        skim_without_charts()
      
      sum_n <-if ("numeric" %in% skimDf$skim_type){
        skimDf %>%
          yank('numeric') %>%
          select('skim_variable','n_missing','complete_rate',
                 'mean','sd','p0','p50','p100') %>%
          arrange(-n_missing) %>%
          mutate_if(is.numeric, round, digit = 2)}
      
      
    })
    
  })
}