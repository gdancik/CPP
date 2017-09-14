library(shiny)
library(wordcloud2)

shinyUI(
  
  fluidPage(
    
  title = "DCAST",
  sidebarLayout(
 
    sidebarPanel(
      tags$head(tags$style(HTML(
        "#console { max-height:15vh; max-width:50vh; overflow:auto; }"
      ))),
      
      fileInput("fileSelect", "Select Files", multiple=TRUE),
      verbatimTextOutput("console"),
      actionButton("analyzeFiles", "Analyze Files")
    ), 
    
    mainPanel(
    
  hr(),
  

  fluidRow(
    
    column(12, align="center",
           plotOutput("clusters")
    )

  ),
 
    fluidRow(
      column(6, tableOutput("cluster1Table")),
      column(6, tableOutput("cluster2Table"))
    )
  )
)
)
)