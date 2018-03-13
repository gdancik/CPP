library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyjs)


SHOW.PUBMED <- FALSE

source("addDeps.R")

shinyUI(
  

  navbarPage(
    title = 'Cancer Publication Portal',
    tabPanel('Summaries',   
             
      includeCSS('www/ecsu.css'),
      HTML("<link href='https://fonts.googleapis.com/css?family=Courgette' rel='stylesheet' type='text/css'>"),
             
      tags$head(tags$style(HTML(
        "#console { max-height:15vh; max-width:50vh; overflow:auto; }"
      ))),
             
      # input / filter row
      fluidRow(
        shiny::column(width = 3, 
          div(style = "display:inline-block;width:70%",
            selectizeInput("geneInput", label = "Select a Gene", choices = NULL)
          ),
          div (style = "display:inline-block; width: 20%",
            actionButton("btnGeneSearch", "Search")
          )
        ),
        shiny::column(width = 2,
          radioButtons("rbDiseaseLimits", "Article Limits:",
          c("cancer-related articles" = "cancer", "none" = "all articles"), selected = "cancer")),
      
        shiny::column(width=2,
          selectInput("filterDisease", "Disease Filters", c("Choose one" = "", 1:5), multiple = TRUE, selectize = TRUE)
        ),
        shiny::column(width=2,
                      selectInput("filterChem", "Chem Filters", c("Choose one" = "", 1:5), multiple = TRUE, selectize = TRUE)
        ),
        shiny::column(width=2,
                      selectInput("filterGenes", "Gene Filters", c("Choose one" = "", 1:5), multiple = TRUE, selectize = TRUE)
        )
      ),
        hr(),
      useShinyjs(),     
             
      fluidRow(shiny::column(width = 12,
          conditionalPanel(condition="$('html').hasClass('shiny-busy')",
              HTML("<div class=\"progress\" style=\"position: fixed;  width: 100%; height:25px; !important\">
                    <div class=\"progress-bar progress-bar-striped active\" role=\"progressbar\" aria-valuenow=\"100\" aria-valuemin=\"0\" aria-valuemax=\"100\" style=\"opacity: 1; width:100%\" !important>
                    <span id=\"bar-text\"><b>Loading, please wait...</b></span>
                   </div></div>")
          ))
      
      ),
      br(),br(),
      
      fluidRow(
        shiny::column(width = 4,
                      tabsetPanel(type = "tabs",
                          tabPanel("Disease Graph", id = "tabDiseaseGraph", 
                              div(style = "height: 450px; overflow-y: scroll",
                              plotOutput("DiseaseGraph",  click = "DiseaseGraph_click"#, 
                                                          #hover = hoverOpts(id = "DiseaseGraph_hover", 
                                                          #                              delay = 300,
                                                          #                              delayType = "throttle")
                              ))
                          ),
                          tabPanel("Disease Summary", DT::dataTableOutput("diseaseResults"))
                          #tabPanel("Disease Tree", htmlOutput("diseaseHierarchy"))
                                  
                      )
        ),
        shiny::column(width = 4,
                      tabsetPanel(type = "tabs",
                          tabPanel("Chemicals", DT::dataTableOutput("chemResults"))
                      )
        ),
        shiny::column(width = 4,
                      tabsetPanel(type = "tabs",
                          tabPanel("Gene Co-occurrences", DT::dataTableOutput("geneResults"))
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
                        
                        "))
      )),
    
    tabPanel('Articles',        
             
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
  
