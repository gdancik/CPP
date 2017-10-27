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
      fileInput("fileSelect", "Select Files", multiple=TRUE, width = "100%"),
      
      div(style="display:inline-block; width: 45%",
        actionButton("btnAnalyzeFiles", "Analyze Files", disabled = "disabled", width = "100%")
      ),
      div(style="display:inline-block; width: 45%",
        actionButton("btnViewClusters", "View Clusters", disabled = "disabled", width = "100%")
      ), 

      hr(),
      
      verbatimTextOutput("console"),
      
      selectizeInput("geneInput", label = "Enter a Gene ID", choices = NULL),
      actionButton("btnGeneSearch", "Search")
      
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
      ),
      
      fluidRow(
        dataTableOutput("queryResults")
      ),
      
      fluidRow(
        shiny::verbatimTextOutput("articleTitles")
      )
    )
  )
)