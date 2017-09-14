library(shiny)
library(wordcloud2)

shinyUI(fluidPage(
  title = "DCAST",
  tags$head(tags$style(HTML(
    "#console { max-height:15vh; max-width:50vh; overflow:auto; }"
  ))),
  hr(),
  
  
  fluidRow(
    
    column(6, align="center",
           fileInput("fileSelect", "Select Files", multiple=TRUE),
           verbatimTextOutput("console"),
           actionButton("analyzeFiles", "Analyze Files")
           ),
    column(6, align="center",
           plotOutput("clusters")
           )

  ),
  fluidRow(
    
    column(6,
           tableOutput("cluster1Table")
           ),
    column(6,
           tableOutput("cluster2Table")
           )
    
  )

))
