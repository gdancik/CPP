library(shiny)
library(wordcloud2)

shinyUI(fluidPage(
  
  title = "Abstract WordCloud",
  
  wordcloud2Output('wordCloud'),
  
  hr(),
  
  fluidRow(
    column(12, align="center",
           sliderInput('size', "Size",
                       min=0.0, max=1, value = 0.5,
                       step=0.1))

  )

))
