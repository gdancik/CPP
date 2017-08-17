library(shiny)
library(wordcloud2)

shinyUI(fluidPage(
  title = "Abstract WordCloud",
  wordcloud2Output('wordCloud'),
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
           sliderInput('size', "Size",
                       min=0.0, max=1, value = 0.5,
                       step=0.1))

  )

))
