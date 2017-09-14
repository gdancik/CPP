library(shiny)
library(shinyBS)
library(wordcloud2)
library(shinydashboard)
library(shinyjs)

shinyUI(
  
  dashboardPage(
    
    
    
    dashboardHeader(title = "DCAST"),
 
    dashboardSidebar(width = 350,
       includeCSS('ecsu.css'),
      tags$head(tags$style(HTML(
        "#console { max-height:15vh; max-width:50vh; overflow:auto; }"
      ))),
    
      useShinyjs(),  
      fileInput("fileSelect", "Select Files", multiple=TRUE),
      verbatimTextOutput("console"),
      actionButton("analyzeFiles", "Analyze Files"),
      hr(),
      actionButton("btnViewClusters", "View Clusters", disabled = "disabled")
      
    ), 
    
    dashboardBody(
    
  fluidRow(
    
    bsModal("clusterModal", "Document clustering", "btnViewClusters", 
            plotOutput("clusters"),
            size = "large"
    )      
  
  ),
 
    fluidRow(
      column(6, tableOutput("cluster1Table")),
      column(6, tableOutput("cluster2Table"))
    )
  )
)
)