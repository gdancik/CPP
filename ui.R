library(shiny)
library(shinyBS)
library(wordcloud2)
library(shinydashboard)
library(shinyjs)


SHOW.PUBMED <- FALSE

source("addDeps.R")

shinyUI(
  
  dashboardPage(

    dashboardHeader(title = uiOutput("shinyTitle"), titleWidth = 350),

     
    dashboardSidebar(width = 250,
 
      includeCSS('www/ecsu.css'),
      HTML("<link href='https://fonts.googleapis.com/css?family=Courgette' rel='stylesheet' type='text/css'>"),
                     
      tags$head(tags$style(HTML(
        "#console { max-height:15vh; max-width:50vh; overflow:auto; }"
      ))),
    
      
      selectizeInput("geneInput", label = "Enter a Gene ID", choices = NULL),
      actionButton("btnGeneSearch", "Search"), 
      radioButtons("rbMeshLimits", "Filters:",
                   c("cancer-related" = "cancer",
                     "none" = "none"), selected = "cancer")
    ), 
    
    dashboardBody(
      
      useShinyjs(),
      
        fluidRow(shiny::column(width = 6,
                               conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                HTML("<div class=\"progress\" style=\"position: fixed;  width: 35%; height:25px; !important\">
                                                     <div class=\"progress-bar progress-bar-striped active\" role=\"progressbar\" aria-valuenow=\"100\" aria-valuemin=\"0\" aria-valuemax=\"100\" style=\"opacity: 1; width:100%\" !important>
                                                     <span id=\"bar-text\"><b>Loading, please wait...</b></span>
                                                     </div></div>")
                                                )),
               shiny::column(width = 6,
                    htmlOutput("x_value"))
        ),
      
      
      tags$style(HTML("
    .tabbable > .nav > li[class=active] > a {
           background-color: maroon;
                      color: white;
    }
    .tabbable > .nav > li > a[data-value='hide'] {color: maroon;}
    
    
  ")),
      fluidRow(
        shiny::column(id = "colSummary", width = 6,
          tabsetPanel(id = "tspSummary",
            tabPanel("Mesh Graph", 
                     div(style = "height: 450px; overflow-y: scroll",
                          plotOutput("MeshGraph",  click = "MeshGraph_click"#, 
                                  #hover = hoverOpts(id = "MeshGraph_hover", 
                                  #                              delay = 300,
                                  #                              delayType = "throttle")
                                 ))
                     ),
            tabPanel("MeSH Summary", dataTableOutput("meshResults")),
            tabPanel("MeSH Tree", htmlOutput("meshHierarchy")),
            tabPanel("Gene Summary", dataTableOutput("geneResults")),
            tabPanel("Hide", value = "hide")
          )
        ),
        
        #shiny::column(width = 12,
                      #htmlOutput("articles"))
        
        if (!SHOW.PUBMED) {
            #shiny::column(width = 3)
            shiny::column(id = "colPubs", width = 6, 
                          uiOutput("articleHeader"),
                          dataTableOutput("articleTable"))
        } else {
          shiny::column(id = "colPubs", width = 6, 
                        uiOutput("articleHeader"),
                        uiOutput("articles"))
        }
      )
    )
  )
)
