library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyjs)


SHOW.PUBMED <- FALSE

source("addDeps.R")

shinyUI(
  
  dashboardPage(title = "Cancer Publication Portal",

    dashboardHeader(title = uiOutput("shinyTitle"), titleWidth = 350),
    
    dashboardSidebar(width = 250,
 
      includeCSS('www/ecsu.css'),
      HTML("<link href='https://fonts.googleapis.com/css?family=Courgette' rel='stylesheet' type='text/css'>"),
                     
      tags$head(tags$style(HTML(
        "#console { max-height:15vh; max-width:50vh; overflow:auto; }"
      ))),
    
      
      selectizeInput("geneInput", label = "Enter a Gene ID", choices = NULL),
      actionButton("btnGeneSearch", "Search"), 
      radioButtons("rbDiseaseLimits", "Disease Filters:",
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
      
      
      
      fluidRow(
        shiny::column(id = "colSummary", width = 6,
          navbarPage(" ",id = "tspSummary",
                navbarMenu("Diseases",
                        tabPanel("Disease Graph", id = "tabDiseaseGraph", 
                          div(style = "height: 450px; overflow-y: scroll",
                          plotOutput("DiseaseGraph",  click = "DiseaseGraph_click"#, 
                                  #hover = hoverOpts(id = "DiseaseGraph_hover", 
                                  #                              delay = 300,
                                  #                              delayType = "throttle")
                                 ))
                        ),
                        tabPanel("Disease Summary", DT::dataTableOutput("diseaseResults")),
                        tabPanel("Disease Tree", htmlOutput("diseaseHierarchy"))
                  ),
                
                tabPanel("Chemicals", DT::dataTableOutput("chemResults")),
                tabPanel("Gene Co-occurrences", DT::dataTableOutput("geneResults")),
                tabPanel("Hide Summaries", value = "hide")
          )
        ),

        tags$style(HTML("

              /* 'hide' tab */
              .navbar-nav > li > a[data-value='hide'] {color: maroon !important;}
                        
              /* Top level tabs */
              .navbar-default .navbar-nav > .active > a, .navbar-default .navbar-nav > .active > a:focus, .navbar-default .navbar-nav > .active > a:hover {
                        color: white;
                        background-color: maroon
              }
                        
                        
                        
              .navbar-default .navbar-nav > li > a:hover {
                        color: darkred;
              }
                        
                        
              .navbar-default .navbar-nav > li > a {
                        color: darkblue;
              }
                        
                        
              /* 2nd level (Disease menu) tabs */
              .navbar-default .navbar-nav > li > ul > li[class='active'] > a {
                        color: white;
                        background-color: maroon
              }
                        
              .navbar-default .navbar-nav > li > ul > li > a {
                        color: darkblue;
              }
                        
                        
              .navbar-default .navbar-nav > .active > li > ul > li > a, .navbar-default .navbar-nav > .active > li > ul > li > a:focus, .navbar-default .navbar-nav > .active > li > ul > li > a:hover {
                        color: white;
                        background-color: maroon
              }
                        
              .navbar-default .navbar-nav > li >ul > li > a:hover {
                        color: darkred;
              }
                        
")),
        
                
        #shiny::column(width = 12,
                      #htmlOutput("articles"))
        
        if (!SHOW.PUBMED) {
            #shiny::column(width = 3)
            shiny::column(id = "colPubs", width = 6, 
                          uiOutput("articleHeader"),
                          DT::dataTableOutput("articleTable"))
        } else {
          shiny::column(id = "colPubs", width = 6, 
                        uiOutput("articleHeader"),
                        uiOutput("articles"))
        }
      )
    )
  )
)
