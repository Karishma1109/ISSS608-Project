packages = c('rmarkdown','gganimate','shiny','tidyverse','shinydashboard','lubridate','plotly','leaflet','readr','crosstalk','shinythemes','sparkline','skimr','shinycssloaders','ggcorrplot','RColorBrewer','rgdal','vip','SnowballC','slam','zoo',"bslib", "ggplot2",
             "dtwclust", 'BBmisc', 'tibble', 'ggdendro',
             'rnaturalearth', 'rnaturalearthdata',
             'ggthemes', 'ggiraph', "shinyWidgets",'wordcloud')

for(p in packages)
{
  if(!require(p,character.only = T))
  {
    install.packages(p)
  }
  library(p,character.only = T)
}
trade <- read_csv("data/trade.csv")
imports <- read_csv("data/imports.csv")
imports <- imports[,c(2:4)]
exports <- read_csv("data/exports.csv")
exports <- exports[,c(2:4)]
singapore <- read_csv("data/singapore.csv")




# Remove all regions in data
imports.remove <- c("Total Merchandise Imports","America", "Asia", "Europe", "Oceania", 
                    "European Union", "Germany, Democratic Republic Of",
                    "Democratic People's Republic Of Korea", "Yemen Democratic",
                    "Other Countries In America", "Other Countries In Oceania",
                    "Commonwealth Of Independent States", "Africa")

exports.remove <- c("Total Merchandise Imports","America", "Asia", "Europe", "Oceania", 
                    "European Union", "Germany, Democratic Republic Of",
                    "Democratic People's Republic Of Korea", "Yemen Democratic",
                    "Other Countries In Oceania", "Africa")

# For clustering
imports_clust <- imports %>% 
  select(`Data Series`, Time, Imports) %>%
  pivot_wider(names_from = Time, values_from = Imports)

imports_clust <- imports_clust[c(1:(nrow(imports_clust)-15)),]

import.countries <- c("Total Merchandise Imports", "America", "Asia", "Europe", "Oceania",
                      "Africa", "European Union", "Belgium", "Denmark", "France",
                      "Germany", "Greece", "Ireland", "Italy", "Luxembourg",
                      "Netherlands", "United Kingdom", "Portugal", "Spain", "Austria",
                      "Finland", "Norway", "Sweden", "Switzerland", "Liechtenstein",
                      "Malta", "Germany, Democratic Republic Of", "Hungary", "Poland", "Estonia",
                      "Latvia", "Lithuania", "Slovenia", "Czech Republic", "Slovakia",
                      "Brunei", "Indonesia", "Malaysia", "Philippines", "Thailand",
                      "Myanmar", "Cambodia", "Laos", "Vietnam", "Japan",
                      "Hong Kong S.A.R.", "South Korea", "Taiwan", "Macao S.A.R", "China",
                      "Democratic People's Republic Of Korea", "Afghanistan", "Bangladesh", "India", "Maldives",
                      "Nepal", "Pakistan", "Sri Lanka", "Bahrain", "Cyprus",
                      "Iran", "Israel", "Jordan", "Kuwait", "Lebanon",
                      "Oman", "Qatar", "Saudi Arabia", "Syria", "United Arab Emirates",
                      "Yemen", "Yemen Democratic", "Canada", "Puerto Rico", "United States of America",
                      "Argentina", "Brazil", "Chile", "Colombia", "Ecuador",
                      "Mexico", "Paraguay", "Peru", "Uruguay", "Venezuela",
                      "Netherlands Antilles", "Panama", "The Bahamas", "Bermuda", "French Guiana",
                      "Grenada", "Guatemala", "Honduras", "Jamaica", "Saint Vincent and the Grenadines",
                      "Trinidad & Tobago", "Anguilla", "Other Countries In America", "Australia", "Fiji",
                      "Nauru", "New Caledonia", "New Zealand", "Papua New Guinea", "Cocos",
                      "French Southern and Antarctic Lands", "Norfolk Island", "Cook Islands", "French Polynesia", "Guam",
                      "Kiribati", "Niue", "Solomon Islands", "Tuvalu", "Wallis and Futuna",
                      "Federated States of Micronesia", "Palau", "South Sudan", "Other Countries In Oceania", "Commonwealth Of Independent States"
)

exports_clust <- exports %>% 
  select(`Data Series`, Time, Exports) %>%
  pivot_wider(names_from = Time, values_from = Exports)

exports_clust <- exports_clust[c(1:(nrow(exports_clust)-15)),]

export.countries <- c("Total Merchandise Imports", "America", "Asia", "Europe", "Oceania",
                      "Africa", "European Union", "Belgium", "Denmark", "France",
                      "Germany", "Greece", "Ireland", "Italy", "Luxembourg",
                      "Netherlands", "United Kingdom", "Portugal", "Spain", "Austria",
                      "Finland", "Sweden", "Switzerland", "Malta", "Germany, Democratic Republic Of", 
                      "Hungary", "Poland", "Estonia", "Latvia", "Lithuania", 
                      "Slovenia", "Czech Republic", "Slovakia", "Brunei", "Indonesia", 
                      "Malaysia", "Philippines", "Thailand", "Myanmar", "Cambodia", 
                      "Laos", "Vietnam", "Japan", "Hong Kong S.A.R.", "South Korea", 
                      "Taiwan", "Macao S.A.R", "China", "Democratic People's Republic Of Korea", "Afghanistan", 
                      "Bangladesh", "India", "Maldives", "Nepal", "Pakistan", 
                      "Sri Lanka", "Bahrain", "Cyprus", "Iran", "Israel", 
                      "Jordan", "Kuwait", "Lebanon", "Oman", "Qatar", 
                      "Saudi Arabia", "Syria", "United Arab Emirates", "Yemen", "Yemen Democratic", 
                      "Canada", "Puerto Rico", "United States of America", "Argentina", "Brazil", 
                      "Chile", "Colombia", "Ecuador", "Mexico", "Paraguay", 
                      "Peru", "Uruguay", "Venezuela", "Australia", "Fiji", 
                      "New Caledonia", "New Zealand", "Papua New Guinea", "French Polynesia", "Guam", 
                      "Solomon Islands", "Other Countries In Oceania"
)

# Date range for slider in app
date.range <- names(imports_clust)[-1]
# Reverse values to start with earliest date
date.range <- date.range[c(length(date.range):1)]

#Functions to be used
arrange.data <- function(df, countries){
  df$Countries <- countries
  df <- df[, c(ncol(df):2)]
  return(df) 
}

filter.data <- function(df, start_date, end_date){
  index.end <- grep(end_date, colnames(df))
  index.start <- grep(start_date, colnames(df))
  
  df <- df[c(index.start:index.end)]
  
  return(df)
}

convert_df <- function(df, countries){
  df <- data.frame(df)
  rownames(df) <- countries
  df <- df[-1,]
  return(df)
}

prep.data <- function(df){
  df <- df %>% 
    mutate_all(~as.numeric(as.character(.))) 
  
  clust.norm <- BBmisc::normalize(df, method="standardize")
  return(clust.norm)
}

clustering <- function(clust.norm, type, k, method){
  if (type == "PAM"){
    clust.result <- tsclust(clust.norm, type="partitional", k=k, distance="dtw", centroid="pam")
  }
  else{
    clust.result <- tsclust(clust.norm, type = "h", k = k, distance = "dtw", 
                            centroid=shape_extraction, control = hierarchical_control(method = method),
                            seed = 2022)
  }
  return(clust.result)
}

plot.clust <- function(clust.result, dataType, clustType){
  plot(clust.result, type = "sc") +
    xlab("Time") +
    ylab(dataType) +
    theme(
      panel.spacing.y = unit(0, 'lines'),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank()
    ) +
    ggtitle(label = paste("Clustering Results using", clustType, sep = " "))
}

plot.dendro <- function(clust.result, dataType){
  hcdata <- dendro_data(clust.result)
  
  hcdata_segments <- hcdata$segments
  
  # get terminal dendrogram segments
  hcdata_ends <- hcdata_segments %>%
    filter(yend == 0) %>% # filter for terminal dendrogram ends
    left_join(hcdata$labels, by = "x") 
  
  hcdata_ends$Clusters <- clust.result@cluster
  
  p <- ggplot() +
    geom_segment(data = hcdata_segments, 
                 aes(x=x, y=y, xend=xend, yend=yend)) +
    geom_segment(data = hcdata_ends,
                 aes(x=x, y=y.x, xend=xend, yend=yend, color = Clusters, text = label
                 )
    ) + # text aes is for plotly
    scale_y_reverse() +
    coord_flip() + 
    theme_bw() + 
    theme(legend.position = "none") + 
    ylab("Distance")  # flipped x and y coordinates for aesthetic reasons
  ggtitle(paste("Dendrogram Results for Hierarchical Clustering for", dataType, sep = " "),
  )
  
  ggplotly(p, tooltip = "text") 
}

plot.clustmap <- function(clust.final, dataType){
  world <- ne_countries(scale = "medium", returnclass = "sf") 
  clust.map <- merge(world, clust.final, by.x="admin", by.y="Countries")
  clust.map$Clusters <- as.factor(clust.map$Clusters)
  
  clust_worldmap <- ggplot(clust.map) + 
    geom_sf(data = world,
            fill = "white", colour = "#7f7f7f", size=0.5) +
    geom_sf_interactive(
      aes(fill = Clusters,
          tooltip = admin,
          data_id = admin)
    ) +
    theme_map() +
    ggtitle("DTW Time Series Clustering Results using Map",
            paste("Shows the clustering analysis of the different countries for", dataType, sep = " "))
  
  clust_worldmap <- girafe(code, 
                           ggobj = clust_worldmap,
                           options = list(opts_tooltip(use_fill = TRUE),
                                          opts_zoom(min = 1, max = 5),
                                          opts_toolbar(saveaspng = FALSE)))
  
  clust_worldmap
}
options(scipen=999)
# Filter out regions from table
imports_clust_2 <- arrange.data(imports_clust, import.countries)
remove_rows <- which(imports_clust_2$Countries %in% imports.remove)
import_clust_3 <- imports_clust_2[-remove_rows,]
import.countries.final <- import_clust_3$Countries

exports_clust_2 <- arrange.data(exports_clust, export.countries)
remove_rows <- which(exports_clust_2$Countries %in% imports.remove)
export_clust_3 <- exports_clust_2[-remove_rows,]
export.countries.final <- export_clust_3$Countries

ui <-
  dashboardPage(
    skin = 'black',
    dashboardHeader(title = "Singapore Trade"),         
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Introduction", tabName = "Overview", icon = icon('globe')),
        menuItem("Exploratory Analysis", tabName = "EDA", icon = icon('chart-bar')),
        menuItem("Time Series Clustering", tabName = "Clustering", icon = icon('chart-line'))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem("Overview",
                tabsetPanel(tabPanel("About",fluidPage(theme = shinytheme('journal'), 
                          titlePanel("PARTNERS THROUGH THE PANDEMIC"),
                          fluidRow( 
                                    column(width = 4,
                                           tags$img(src = 'trade.gif',width = 300, height = 200)),
                                    column(6,plotlyOutput(outputId = "fig", width = "50%")),
                            (column(width = 8,
                                          p("This is an interactive application built in R Shiny. 
                                            It aims to provide its users with a user-friendly interface to see how the imports, exports and balance of trade with countries across the globe varied for Singapore during the Coronavirus pandemic. 
                                            Additionally the application also provides the users to view clustering patterns for the countries in terms of their trade with Singapore during the Pandemic through Time-Series Clustering. 
                                            The data set used in this app was downloaded from the Department of Statistics, Singapore."),
                                    h3("Features"),
                                    h4("Tab-Item Overview"), 
                                    p("It has 2 tab panels: 
                                    
                                      'About' which briefly talks about the application 
                                       
                                      and 'Dataset Summary', which basically summarises the dataset used."),
                                    h4("Tab-Item Exploratory Analysis:"),
                                    p("It has 3 tab panels, 
                                       'Singapore' which which shows the overall import,export and balance of trade trend for Singapore for the period Jan 2018 to Jan 2022; 
                                       'Bubble Plot', is an animated scatter plot of Imports vs Exports for all the trading partner countries of Singapore 
                                       and 'Countries-Wise' tab, which lets users compare the trade flows for Imports, Exports, Net Imports/Balance of Trade, 
                                       Trade Volume across trading partner countries of Singapore"),
                                    h4("Tab-Item Time-Series Clustering:"),
                                    p("This tab allows users to cluster Singapore's trading partner countries
                                       using time-series clustering techniques to basically view the trends during the Coronavirus Pandemic.")
                                    
                                      ))
                            
                                          
                                   )
                          )
                )
        ,
        
          tabPanel("Dataset Summary",
                   summaryUI('summary')
                   
          ),
        ))
        ,
        tabItem("EDA",
                tabsetPanel(
                  tabPanel("Singapore",fluidPage(
                    titlePanel("Import-Export, Jan 2018 to Jan 2022 "),
                    
                    sidebarLayout(
                      sidebarPanel(column =1,
                        
                        # Add a slider selector for years to filter
                        sliderTextInput(inputId = "Time", "Select a Time-Interval", 
                                        singapore$Year_Month, 
                                        selected = singapore$Year_Month[c(1, length(singapore$Year_Month))])
                      
                        ),
                      mainPanel(
                        
                        plotlyOutput(outputId = "g", width = "100%"))))),
                  
                  tabPanel("Bubble Plot",fluidPage(
                    titlePanel("Import-Export across Countries, Jan 2018 to Jan 2022 "),
                    
                        plotlyOutput(outputId = "p", width = "40%"))),
                  
                  tabPanel("Country-Wise",fluidPage(
                      titlePanel("Trade Flows, Jan 2018 to Jan 2022 "),
                      
                      sidebarLayout(
                        sidebarPanel(column =2,
                    selectizeInput(
                      inputId = "Countries",
                      label = "Select Country(s)",
                      choices = unique(trade$Country),
                      selected = "Mainland China",
                      multiple = TRUE
                    ),
                    
                    selectizeInput(
                      inputId = "tradetype",
                      label = "Select Metric Type",
                      choices = c("Exports" = "Exports",
                                  "Imports" = "Imports",
                                  "Net_Imports" = "Net_Imports",
                                  "Trade_Volume" = "Trade_Volume"),
                      selected = "Imports")),
                    
                    
                           mainPanel(
                             
                             plotlyOutput(outputId = "Exportline"))
                  ))))),
                  
        tabItem("Clustering",
                
                tabPanel(
                  "Clustering",
                  sidebarLayout(
                    sidebarPanel(
                      width = 3,
                      selectInput(inputId = "dataType",
                                  label = "Trade Type:",
                                  choices = c("Imports" = "Imports",
                                              "Exports" = "Exports"
                                  ),
                                  selected = "Imports"),
                      sliderTextInput(
                        inputId = "dateSlider",
                        label = "Date Range:",
                        choices = date.range,
                        selected = date.range[c(1, length(date.range))]
                      ),
                      uiOutput(
                        outputId = "countryInput"
                      ),
                      numericInput(
                        inputId = "nClusters",
                        label = "Number of Clusters:",
                        min = 2,
                        max = 15,
                        step = 1,
                        value = 6
                      ),
                      selectInput(
                        inputId = "clustType",
                        label = "Clustering Method:",
                        choices = c("PAM", "Hierarchical"),
                        selected = "PAM"
                      ),
                      uiOutput(
                        outputId = 'disableHmethod'
                      ),
                      actionButton(
                        inputId = "submitChanges",
                        label = "Apply Changes"
                      )
                    ),
                    mainPanel(
                      plotOutput("clustPlot"),
                      girafeOutput(
                        outputId = "worldMap"
                      ),
                      plotlyOutput(
                        outputId = "dendroPlot"
                      )
                    )
                  )
                )))))

server <- function(input, output, session) {
  output$fig <- renderPlotly({
    fig <- singapore %>%
      plot_ly( x= ~Year_Month,
               y = ~Trade_Volume,
               frame = ~Year_Month, 
               hoverinfo = "text",
               type = 'scatter',
               mode = 'markers',
               
               marker =list(size = 30, sizemode = "diameter", color = 'maroon')
      )
    fig <- fig %>%
      layout(title = 'Trade Volume, Singapore (Jan 2018-Jan 2022)', xaxis = list(
        range= c(0,50), showticklabels = FALSE, showgrid = FALSE),yaxis = list( title = "Trade Volume", showgrid = FALSE),autosize = T, width = 600, height = 300)
    
    
    fig <- fig %>%
      animation_opts(
        500, easing = 'linear', redraw = FALSE
      )%>%animation_slider(hide = FALSE)
    
  fig
    
  })
  
  summaryServer('summary', trade[c("Country","Year_Month","Imports","Exports","Net_Imports","Trade_Volume")])
  
  output$out_year <- renderText(input$Time)
  
  
  output$g <- renderPlotly({
    
    singapore <- singapore[singapore$Year_Month>=input$Time[1] & singapore$Year_Month<=input$Time[2],]
    singapore$Year_Month <- as.character(singapore$Year_Month)
    p <- ggplot(singapore, aes(x = factor(Year_Month,level = unique(Year_Month)), text = paste0("Time: ", Year_Month))) +  geom_bar(singapore, mapping = aes(y=Exports, fill = "green"), stat = "identity")
    
    p <- p + geom_bar(singapore, mapping = aes(y=Imports,  fill = "red" ), stat = "identity")
    
    
    
    p <- p+geom_line(singapore, mapping = aes(x = factor(Year_Month,level = unique(Year_Month)), y=BalanceOfTrade, group = 1))
    
    p <- p +labs(title = "Singapore Trade Trend Jan (2018 to Jan 2022)", 
                 
                 subtitle = "Balance of Trade is positive",
                 x = 'Year Month', 
                 y ='Imports                   Exports')+
      scale_y_continuous(breaks = seq(-60000000000,60000000000,10000000000), labels=c("60B","50B","40B", "30B","20B","10B","0","10B","20B", "30B", "40B","50B","60B"))
    
    p <- p + theme_bw() + theme(legend.position = "none",plot.title = element_text(size=10), 
                                plot.subtitle = element_text(size=9), axis.title = element_text(size=8, hjust=0.5), 
                                axis.text.x=element_text(size=6, vjust=0.5, angle = 45),  axis.text.y=element_text(size=6, vjust=0.5))+
      scale_fill_manual(values=c("#00CCCC", "#990033"))
    
    p <- ggplotly(p, tooltip = ("text"))
    
  })
  
  
  
  output$p <- renderPlotly({
    fig <- trade %>%
      plot_ly(
        x = ~Exports, 
        y = ~Imports, 
        size = ~Trade_Volume, 
        color = ~Balance, 
        frame = ~Year_Month, 
        text = ~Country, 
        hoverinfo = "text",
        type = 'scatter',
        mode = 'markers',
        colors = c('maroon','darkturquoise'),
        marker =list(opacity = 0.9,sizemode = "diameter")
      )
    fig <- fig %>%
      layout(legend=list(title=list(text='Balance of Trade')),autosize = F, width = 1000, height = 450)
    fig <- fig %>%
      animation_opts(
        1000, easing = 'linear', redraw = FALSE, transition = 100
      )%>%animation_slider(hide = FALSE)
  })
  output$Exportline <- renderPlotly({
    
   if (input$tradetype == "Imports"){
     trade <- trade %>%
       filter(Country %in% input$Countries)
     p1 <- ggplot(trade, aes(x=Year_Month, y=Imports, group = 1,color = Country)) + geom_line()
   }else if(input$tradetype == "Exports"){
     trade <- trade %>%
       filter(Country %in% input$Countries)
     p1 <- ggplot(trade, aes(x=Year_Month, y=Exports, group = 1,color = Country)) + geom_line()
   }else if(input$tradetype == "Net_Imports"){
      trade <- trade %>%
        filter(Country %in% input$Countries)
      p1 <- ggplot(trade, aes(x=Year_Month, y=Net_Imports, group = 1,color = Country)) + geom_line()
    }else{
      trade <- trade %>%
        filter(Country %in% input$Countries)
      p1 <- ggplot(trade, aes(x=Year_Month, y=Trade_Volume, group = 1,color = Country)) + geom_line()
    }
  
   p <- p1 +labs(title = "Trade Trends (2018 to Jan 2022)", 
                
                subtitle = "Comparison between Countries",
                x = 'Year Month', 
                y ='Trade Type')
      p <- p + theme_bw() + theme(plot.title = element_text(size=10), 
                               plot.subtitle = element_text(size=9), axis.title = element_text(size=8, hjust=0.5), 
                               axis.text.x=element_text(size=6, vjust=0.5, angle = 45),  axis.text.y=element_text(size=6, vjust=0.5))
   
   ggplotly(p, tooltip = ("text"))
    
  })
  
  # Only show select input for Hierarchical control method when selected
  output$disableHmethod <- renderUI({
    if (input$clustType == "Hierarchical"){
      selectInput(
        inputId = "hierControl",
        label = "Hierarchical Control Method:",
        choices = c("complete", "single", "average",
                    "ward.D", "ward.D2"),
        selected = "complete"
      )
    }
    else{
      return(NULL)
    }
  })
  
  # Only enable when countries list is >= 5
  observe({
    shinyjs::toggleState("submitChanges", length(input$countryFilter) >= 5)
  })
  
  # Show list of countries based on imports/exports
  output$countryInput <- renderUI({
    if (input$dataType == "Imports"){
      country.list <- import_clust_3$Countries
    }
    else{
      country.list <- export_clust_3$Countries
    }
    selectInput(inputId = "countryFilter",
                label = "Select Countries:",
                choices = country.list,
                multiple = TRUE,
                selected = NULL
    )
  })
  
  # Get data out for clustering based on user input
  a <- reactiveValues()
  
  observeEvent(
    input$submitChanges,
    {
      if(input$dataType == "Imports"){
        clust_2 <- import_clust_3
      }
      else{
        clust_2 <- export_clust_3
      }
      
      clust_3 <- filter.data(clust_2, input$dateSlider[1], input$dateSlider[2])
      clust_4 <- convert_df(clust_3, clust_2$Countries)
      
      # Filter out countries
      choose_rows <- which(row.names(clust_4) %in% input$countryFilter)
      clust_5 <- clust_4[choose_rows,]
      
      #Normalise data
      clust.norm <- prep.data(clust_5)
      
      #Do clustering
      if (input$clustType == "Hierarchical"){
        control <- input$hierControl
      }
      else{
        control <- ""
      }
      clust.result <- clustering(clust.norm, input$clustType, input$nClusters, control)
      
      
      a$clust_result <- clust.result
      a$filtered_countries <- row.names(clust_5)
    }
  )
  
  output$clustPlot <- renderPlot({
    input$submitChanges
    req(a$clust_result)
    req(a$filtered_countries)
    
    isolate(plot.clust(a$clust_result, input$dataType, input$clustType))
  })
  
  output$worldMap <- renderGirafe({
    input$submitChanges
    req(a$clust_result)
    req(a$filtered_countries)
    
    clust.final <- isolate(data.frame(Countries=a$filtered_countries, Clusters=a$clust_result@cluster))
    isolate(plot.clustmap(clust.final, input$dataType))
  })
  
  # Only show dendrogram for Hierarchical
  output$dendroPlot <- renderPlotly({
    input$submitChanges
    req(a$clust_result)
    req(a$filtered_countries)
    
    if (input$clustType == "Hierarchical"){
      isolate(plot.dendro(a$clust_result, input$dataType))
    }
    else{
      return(NULL)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)