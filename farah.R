#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#




library(shiny)
library(plotly)
library(tidyverse)
library(tools)
library(ggplot2)


exportsf <- read_csv("data/exportsf.csv")
importsf <- read_csv("data/importsf.csv")
combined <- rbind(exportsf,importsf) 
combined_spread <- combined[-c(1)]
combined_spread <- spread(combined_spread, Type, Value)

#date trend field
combined_spread$Datetrend <- as.Date(paste0(as.character(combined_spread$Time), ' 01'), format='%Y %B %d')

#country as Factor
combined_spread$Country<-as.factor(combined_spread$`Data Series`)

#graph 1
options(scipen=999)  # turn-off scientific notation like 1e+48

#sum of export during Covid Year based on country
combined_spread$NetEXport <-combined_spread$Export-combined_spread$Import

combined_spreadfiltered<-filter(combined_spread,Country == "Hong Kong" | Country == "America" | Country == "Malaysia" | Country == "Mainland China")

#prepared Data for % change in Exort
combined_spreadfilteredChangeExport<-filter(combined_spreadfiltered, Year != "2022")

#sorting based on months

combined_spreadfilteredChangeExport$Month = factor(combined_spreadfilteredChangeExport$Month, levels = month.abb)
# 
# 
# ggplot(combined_spreadfilteredChangeExport, aes(x=Datetrend, y= Export, group=Country)) +
#   geom_line(stat = "identity", na.rm = TRUE,aes(color=Country))+
#   ggtitle("Singapore's trade with Hong Kong, America, Malaysia, China ('000)") 
# 


# 
# ggplot(topcountries, aes(x=Month, y= Value)) +
#   geom_line(stat = "identity", na.rm = TRUE, )+
#   ggtitle("Singapore's trade with Hong Kong, America, Malaysia, China ('000)") +
#   facet_grid(Type ~ Year)





ui <- fluidPage(
  selectizeInput(
    inputId = "Countries",
    label = "Select a Country",
    choices = unique(combined_spreadfilteredChangeExport$Country),
    selected = "America",
    multiple = TRUE
  ),
  
  selectizeInput(
    inputId = "Year",
    label = "Select a Year",
    choices = unique(combined_spreadfilteredChangeExport$Year),
    selected = "2018",
    multiple = TRUE
  ),
  
  selectizeInput(
    inputId = "ExImport",
    label = "Select Meric Type",
    choices = c("EXPORT" = "Export",
                          "IMPORT" = "Import"),
    selected = "Import"),
  
  
selectInput(inputId = "xvariable",
            label = "x Variable:",
            choices = c("TREND" = "Datetrend"),
            selected = "Datetrend"),


selectInput(inputId = "Coloring",
            label = "Coloring",
            choices = c("Country" = "Country",
                        "Year"="Year"),
                        
            selected = "Country"),
  
  
  plotlyOutput(outputId = "Exportline")
  
)




server <- function(input, output){
  output$Exportline <- renderPlotly({
    
  
    p <- combined_spreadfilteredChangeExport %>%
    filter(Country %in% input$Countries) %>%
      filter(Year  %in% input$Year)%>%
      ggplot(aes_string(x = input$xvariable,
                           y = input$ExImport,
                           colour= input$Coloring
                           
                           )) +
      geom_line(stat = "identity", na.rm = TRUE) +
      xlab("Time") +
      ylab("Trade Value ($'000)")      +
      ggtitle ("Singapore Trade with selected countries in (SGD'000)")

    ggplotly(p)
    
  })
}



shinyApp(ui=ui, server = server)

















#---------------------------------------Dumbell plot below-------------------------

# 
# 
# dumbellsummary<-combined_spreadfilteredChangeExport %>% 
#   group_by(Country,Year) %>% 
# summarise(ExportYear = sum(Export)) %>% 
#   spread(key=Year, value=ExportYear)
# 
# 
# library(ggplot2)
# library(scales)
# theme_set(theme_classic())
# 
# dumbellsummary$left_label <- paste(dumbellsummary$Country, round(dumbellsummary$`2018`/100000),sep=", ")
# dumbellsummary$right_label <- paste(dumbellsummary$Country, round(dumbellsummary$`2022`/100000),sep=", ")
# dumbellsummary$class <- ifelse((dumbellsummary$`2018` - dumbellsummary$`2022`) < 0, "green", "red")
# 
# 
# p <- ggplot(dumbellsummary) + geom_segment(aes(x=1, xend=2, y=(`2018`/100000), yend=(`2022`/100000), col=class), size=.75, show.legend=F) + 
#   geom_vline(xintercept=1, linetype="dashed", size=.1) + 
#   geom_vline(xintercept=2, linetype="dashed", size=.1) +
#   scale_color_manual(labels = c("Up", "Down"), 
#                      values = c("green"="#00ba38", "red"="#f8766d")) +  # color of lines
#   labs(x="", y="Export Volume in Million") +  # Axis labels
#   xlim(.5, 2.5) + ylim(40,(1.1*(max(dumbellsummary$`2018`/100000, dumbellsummary$`2022`/100000))))  # X and Y axis limits
# 
# # Add texts
# p <- p + geom_text(label=dumbellsummary$left_label, y=dumbellsummary$`2018`/100000, x=rep(1, NROW(dumbellsummary)), hjust=1.1, size=3.5)
# p <- p + geom_text(label=dumbellsummary$right_label, y=dumbellsummary$`2022`/100000, x=rep(2, NROW(dumbellsummary)), hjust=-0.1, size=3.5)
# p <- p + geom_text(label="2018", x=1, y=1.1*(max(dumbellsummary$`2018`/100000, dumbellsummary$`2022`/100000)), hjust=1.2, size=5)  # title
# p <- p + geom_text(label="2022", x=2, y=1.1*(max(dumbellsummary$`2018`/100000, dumbellsummary$`2022`/100000)), hjust=-0.1, size=5)  # title
# 
# # Minify theme
# p + theme(panel.background = element_blank(), 
#           panel.grid = element_blank(),
#           axis.ticks = element_blank(),
#           axis.text.x = element_blank(),
#           panel.border = element_blank(),
#           plot.margin = unit(c(1,2,1,2), "cm"))
# 
# 
