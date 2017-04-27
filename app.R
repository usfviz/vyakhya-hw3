rm(list=ls())
cat("\014")

load_package <- function(package_name){
  if(package_name %in% rownames(installed.packages()) == FALSE) {install.packages(package_name)}
}

load_package('d3heatmap'); library(d3heatmap)
load_package('shiny'); library(shiny)
load_package('pairsD3'); library(pairsD3)
load_package('parcoords'); library(parcoords)
load_package('githubinstall'); library(githubinstall)
if('parcoords' %in% rownames(installed.packages()) == FALSE) {devtools::install_github("timelyportfolio/parcoords")}
library(parcoords)

fb <- read.csv('dataset_Facebook.csv', sep = ";",header=T)
colnames(fb) <- c("Total_likes","Type","Category","Post_Month","Post_Weekday","Post_Hour","Paid","Total_Reach","Total_Impressions","Users","Consumers","Consumptions","impressions","reach","liked_engaged","comment","like","share","interactions")

  ui <- fluidPage(
    titlePanel("Facebook Dataset - Visualizations"),
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("vars", "Select variables:",
                           c("Total_likes", "Users", "Consumers", "Consumptions", "comment", "like", "share", "interactions"), 
                           selected = c("like","comment","share","Users"))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Heat Map", 
                   h1("Facebook - Heat Map"),
                   d3heatmapOutput("heatmap"),
                   textOutput("Selected")
          ),
          
          tabPanel("Scatterplot Matrix", 
                   h1("Facebook - Scatter Plot Matrix"),
                   selectInput("group", "Color by", c("Type", "Category", "Post_Weekday", "Paid")),
                   pairsD3Output("scatterPlot", width = "900px",height="900px")
          ),
          tabPanel("Parallel Coordinates", 
                   h1("Facebook - Parallel Coordinates"),
                   selectInput("groupPar", "Color by", c("Type", "Category", "Post_Weekday", "Paid")),
                   parcoordsOutput("parcoords", width = "900px", height = "900px" ))
        )
      )
    )
  )
  # Server logic
  server <- function(input, output) {

    output$heatmap <- renderD3heatmap({
      df <- aggregate(. ~ Post_Month, data = fb[,c(input$vars,'Post_Month')], FUN = sum)
      row.names(df) <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
      df <-within(df, rm(Post_Month))
      d3heatmap(
        df,
        scale="column",
        dendrogram = "none",
        colors = "Blues",
        xaxis_height = 150
      )
    })
    
    output$scatterPlot <- renderPairsD3({
      pairsD3(fb[,input$vars],group=fb[[input$group]])
    })
    
    output$parcoords = renderParcoords({
      parcoords(
        fb[,c(input$groupPar, input$vars)]
        , rownames = FALSE
        , brushMode = "1d"
        , color = list(
          colorScale = htmlwidgets::JS(sprintf(
            'd3.scale.ordinal().range(%s).domain(%s)'
            ,jsonlite::toJSON(RColorBrewer::brewer.pal(4,'Set1'))
            ,jsonlite::toJSON(as.character(unique(fb[[input$groupPar]])))
          ))
          ,colorBy = input$groupPar
        )
      )
    })
  }
  
  # Complete app with UI and server components
  shinyApp(ui, server)