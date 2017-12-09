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
      
      selectizeInput("geneInput", label = "Enter a Gene ID", choices = NULL),
      actionButton("btnGeneSearch", "Search"), 
      actionButton("btnMeshFilter", "Filter")
      
    ), 
    
    dashboardBody(
      
      fluidRow(

      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
             HTML("<br><br>"),
             HTML("<div class=\"progress\" style=\"height:25px !important\"><div class=\"progress-bar progress-bar-striped active\" role=\"progressbar\" aria-valuenow=\"100\" aria-valuemin=\"0\" aria-valuemax=\"100\" style=\"width:100%\">
                                                          <span id=\"bar-text\"><b><font size=\"+1.5\">Loading, please wait...</font></b></span></div></div>")
      ), 

        bsModal("clusterModal", "Document clustering", "btnViewClusters", 
                plotOutput("clusters"),
                size = "large"
        )      
        
      ),
      
      fluidRow(
        shiny::column(width = 6,
          tabsetPanel(
            tabPanel("MeSH Summary", dataTableOutput("queryResults")),
            tabPanel("MeSH Tree", htmlOutput("meshHierarchy"))
          )
        ),
        shiny::column(width = 6)
      ),
      
      
      fluidRow(
        shiny::column(width = 6),
        shiny::column(width = 6)
      )
    )
  )
)
