library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)

source("addDeps.R")




commonStyles <- list(
  includeCSS('www/ecsu.css'),
  HTML("<link href='https://fonts.googleapis.com/css?family=Courgette' rel='stylesheet' type='text/css'>"),
  
  tags$style(HTML("
                               
                  /* color table selections */
                  table.dataTable tr.selected td, table.dataTable td.selected {
                  background-color: maroon !important;
                  color: white
                  }
                  
                  /* 'hide' tab */
                  #sidebar, .navbar {
                  background-image: linear-gradient(#04519b,#044687 60%,#033769);
                  color:#FFF;
                  }
                  
                  
                  .navbar-header > .navbar-brand {
                  font-family:Courgette;
                  color:#FFF;
                  }
                  
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
                  color: white;
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
  
)

# common header for all pages
commonHeader <- list(
  
  # input / filter row
  fluidRow(
    shiny::column(width = 2, 
                  div(style = "display:inline-block;width:70%",
                      selectizeInput("geneInput", label = "Select a Gene", choices = NULL)
                  ),
                  div (style = "display:inline-block; width: 20%",
                       actionButton("btnGeneSearch", "Search", style="position:relative; bottom: 12px; height: 35px; width 66px;")
                  )
    ),
    shiny::column(width = 2,
                  radioButtons("rbDiseaseLimits", "Article Limits:",
                               c("Cancer-related articles only" = "cancer", "All articles" = "none"), selected = "cancer")),
    
    shiny::column(width=2,
                  selectInput("filterDisease", "Disease Filters", choices = NULL, multiple = TRUE, selectize = TRUE)
    ),
    shiny::column(width=2,
                  selectInput("filterChem", "Chem Filters", choices = NULL, multiple = TRUE, selectize = TRUE)
    ),
    shiny::column(width=2,
                  selectInput("filterMutations", "Mutation Filters", choices = NULL, multiple = TRUE, selectize = TRUE)
    ),    
    
    shiny::column(width=2,
                  selectInput("filterGenes", "Additional Gene Filters", choices = NULL, multiple = TRUE, selectize = TRUE)
    )
  ),
  hr(style = "padding: 0px; margin: 0px"),
 
  fluidRow(shiny::column(width = 12,
                         conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                          HTML("<div class=\"progress\" style=\"position: fixed;  width: 100%; height:25px; !important\">
                                               <div class=\"progress-bar progress-bar-striped active\" role=\"progressbar\" aria-valuenow=\"100\" aria-valuemin=\"0\" aria-valuemax=\"100\" style=\"opacity: 1; width:100%\" !important>
                                               <span id=\"bar-text\"><b>Loading, please wait...</b></span>
                                               </div></div>")
                                          ))
           
                         ),
  br(),br(), commonStyles
)


# add a tab panel with given title, table, and graph,
# may want to consider setting click = "CancerGraph_click", etc on graph
addTabPanel <- function(title, tableId, graphId = NULL) {
  tabPanel(title, 
           fluidRow(
             shiny::column(width = 5,
                           withSpinner(DT::dataTableOutput(tableId), type = 3,
                                       color.background = "white")
             ),
             if (!is.null(graphId)) {
               shiny::column(width = 7,
                             withSpinner(plotOutput(graphId), type = 3, 
                                         color.background = "white")
               )
             }
           )
  )
}

shinyUI(


  navbarPage(
   
        
    title = 'Cancer Publication Portal', id = "headerNavBarPage", header = commonHeader,
   
    addTabPanel('Cancer Types', "cancerSummaryTable", "cancerGraph"),
    addTabPanel('Treatments', "paResults"),
    addTabPanel("Diseases", "diseaseResults"),
    addTabPanel('Chemicals', 'chemResults'),
    addTabPanel("Mutations", "mutationResults"),
    addTabPanel('Genes', 'geneResults'),
    
    
    tabPanel('Articles',         
             useShinyjs(),  
             fluidRow(
               shiny::column(width = 6 
               ),
               shiny::column(width = 3,
                             bsButton(inputId = "btnPubTator", label = "Load/Refresh PubTator Results", style = "info")
               ),
               shiny::column(width = 3,
                             div(align = "right",
                                 bsButton("btnPubTatorGo", label = "View Results in PubTator", style = "danger")
                             )
               )
             ),
             
             fluidRow(
               shiny::column(id = "colPubs", width = 3, 
                             DT::dataTableOutput("articleTable")),
               shiny::column(id = "colPubs", width = 9, 
                             uiOutput("articles")
               )
             )
    ),
    tabPanel("Log", verbatimTextOutput("log")
             )
    
    
  ) 
  )
               

