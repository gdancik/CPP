library(shiny)
library(shinyBS)
library(wordcloud2)
library(shinydashboard)
library(shinyjs)

shinyUI(
  
  dashboardPage(
    
    dashboardHeader(title = "DCAST"),
 
    dashboardSidebar(width = 250,
       includeCSS('ecsu.css'),
      tags$head(tags$style(HTML(
        "#console { max-height:15vh; max-width:50vh; overflow:auto; }"
      ))),
    
      useShinyjs(),  
      
      selectizeInput("geneInput", label = "Enter a Gene ID", choices = NULL),
      actionButton("btnGeneSearch", "Search"), 
      actionButton("btnMeshFilter", "Filter")
      
    ), 
    
    dashboardBody(
      

    #  fluidRow(

    #    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
    #         HTML("<br><br>"),
    #         HTML("<div class=\"progress\" style=\"height:25px !important\"><div class=\"progress-bar progress-bar-striped active\" role=\"progressbar\" aria-valuenow=\"100\" aria-valuemin=\"0\" aria-valuemax=\"100\" style=\"width:100%\">
    #                                                      <span id=\"bar-text\"><b><font size=\"+1.5\">Loading, please wait...</font></b></span></div></div>")
    #    )
    #  ),
      
      
      htmlOutput("x_value"),
      
      
      fluidRow(
        shiny::column(width = 6,
          tabsetPanel(
            tabPanel("Mesh Graph", plotOutput("MeshGraph", click = "MeshGraph_click", 
                                              hover = hoverOpts(id = "MeshGraph_hover", 
                                                                delay = 100,
                                                                delayType = "debounce"))),
            tabPanel("MeSH Summary", dataTableOutput("meshResults")),
            tabPanel("MeSH Tree", htmlOutput("meshHierarchy"))
          )
        ),
        
        #shiny::column(width = 12,
                      #htmlOutput("articles"))
        shiny::column(width = 3),
        shiny::column(width = 3, dataTableOutput("articleTable"))
      ),
      
      fluidRow(
        shiny::column(width = 12,
          htmlOutput("articles")
        )
      )
    )
  )
)
