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
      actionButton("btnAnalyzeFiles", "Analyze Files", disabled = "disabled"),
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
        hidden(
          div(
            id = "globalTitle",
            h2("Global Term Frequency")
          )
        ),
        DT::dataTableOutput("globalTable")
      ),
      
      fluidRow(
        hidden(
          div(
            id = "datatables",
            br(),
            h2("Cluster Term Frequency"),
            tabsetPanel(
              useShinyjs(),
              selected = "Cluster 1",
              tabPanel("Cluster 1", DT::dataTableOutput("cluster1Table")),
              tabPanel("Cluster 2", DT::dataTableOutput("cluster2Table"))
            )
          )
        )
      )
    )
  )
)