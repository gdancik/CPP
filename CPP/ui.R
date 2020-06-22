
lib.loc <- NULL
lib.loc <- "/Users/dancikg/RESEARCH/research_easternct/work/GenePubViewer/DCAST/Github/CPP/lib"

library(shinyBS)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders, lib.loc = lib.loc)
library(plotly)
library(rclipboard)

source("addDeps.R")
source("ui-about.R")
source("modals.R")

jsCode <- "shinyjs.setReadOnly = function(id) {
  document.getElementById(id).setAttribute('readonly', 'readonly');
}"

#jsCode <- "shinyjs.setAttribute = function(id, attr, value){alert('hi')};"

commonStyles <- list(
  includeCSS('www/ecsu.css'),
  HTML("<link href='https://fonts.googleapis.com/css?family=Courgette' rel='stylesheet' type='text/css'>"),
  HTML("<link rel='stylesheet' href='https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css' integrity='sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u' crossorigin='anonymous'>"),
  tags$style(HTML("
                               
                  /* color table selections */
                  table.dataTable tr.selected td, table.dataTable td.selected {
                  background-color: maroon !important;
                  color: white
                  }
                  
              .blue-button {
                background-image: linear-gradient(#04519b,#044687 60%,#033769);
                color:#FFF;
              }

              .width100 {
                width: 100%;
              }

              .hide {
                //display:none;
                height: 20px;
              }


              .noclick {
                pointer-events: none;
              }

              .noclick tr {
                  color: lightgrey;
              }

                  /* 'hide' tab */
                  #sidebar, .navbar {
                  background-image: linear-gradient(#04519b,#044687 60%,#033769);
                  color:#FFF;
                  }
                  
                  td, td {
                    font-size:80%;
                  }

                 .modal-open {
                      overflow-y: auto;
                      height: 100px;
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
                  
                #summaryRow {
                  position:sticky;
                  position:-webkit-sticky;
                  top:0;
                  //background:white;
                  //z-index:1000;
                }
          
                .shiny-notification {
                    position: fixed;                    
                    top: 15%;
                    left: 65%;
                    font-size: 1.3em;
                    padding-left: 20px;
                    padding-right: 20px;
                    padding-top: 10px; 
                    padding-bottom: 10px;
                    border-style: solid;
                    border-color: black;
                    background-color: #f2dede;
                    opacity: 1;
                    font-weight: bold;
                }

                .graph-button {
                  font:bold 12px verdana;
                  width:60%;
                  position: relative;
                  bottom: 220px;
                  margin: 10px 32% 0 28%;
                  height: 50px;
                  background-image: linear-gradient(#04519b,#044687 60%,#033769);
                  color:#FFF;
                }

                  ")),

              #modal styles  
              tags$head(tags$style("button.close, #welcomeModal button.cancel, #cancerTypeSetupModal button.cancel {display:none}")),
              tags$head(tags$style("#cancerTypeDiv {min-height: 20em; max-height: 30em; overflow-y: auto;}"))
  
)

# common header for all pages
commonHeader <- list(

  welcomeModal, 
  filterModal,
  # graphSetupModalTerm,
  # graphSetupModalChem,
  cancerTypeSetupModal,
  # 
  # summary row
  fluidRow(id = "summaryRow",
    #shiny::column(width = 2,
    #              radioButtons("rbDiseaseLimits", "Article Limits:",
    #                           c("Cancer-related only" = "cancer", "All articles" = "none"), selected = "cancer")),

    shiny::column(width=2,
                  actionButton("btnNewSearch", "New Gene Search", class = "blue-button")
    ),
    shiny::column(width =10 ,
        htmlOutput("summaryHeader")              
    ),
    br()

  ),
  
  
  hr(style = "padding: 0px; margin: 0px"),
 
  fluidRow(shiny::column(width = 12,
             div(style = "position: relative; width: 100%; height: 10px", 
                conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                  HTML("<div class=\"progress\" style=\"position: fixed;  width: 100%; height:50px; z-index:1000; !important\">
                      <div class=\"progress-bar progress-bar-striped active\" role=\"progressbar\" aria-valuenow=\"100\" aria-valuemin=\"0\" aria-valuemax=\"100\" style=\"opacity: 1; width:100%\" !important>
                      </br>
                          <span id=\"bar-text\" style = \"font-size:2em\"><b>Loading, please wait...</b></span>
                      </div>
                    </div>")
                )
              ))
           ),
  br(), 
  commonStyles
)


# add a tab panel with given title, table, and graph,
# may want to consider setting click = "CancerGraph_click", etc on graph
addTabPanel <- function(title, tableId, graphId = NULL) {
  
  tabPanel(title, 

           fluidRow(div(style = "height: 300px",
             shiny::column(width = 5,
                           #withSpinner(
                             DT::dataTableOutput(tableId), type = 3,
                                       color.background = "white"
                            # )
             ),
             if (!is.null(graphId)) {
               shiny::column(7, 
                             if (graphId == "cancerGraph") {
                               withSpinner(plotOutput(graphId), type = 3, 
                                           color.background = "white")  
                             } else {
                               withSpinner(plotlyOutput(graphId), type = 3, 
                                           color.background = "white")  
                             },
                             # graph setup buttons
                              if (graphId == "cancerTermGraph") {
                                actionButton("btnGenerateGraphCancerTerm", "Generate graph", class = "graph-button") 
                              } else if (graphId == "chemGraph") {
                                actionButton("btnGenerateGraphChem", "Generate graph", class = "graph-button")
                              } else if (graphId == "mutGraph") {
                                actionButton("btnGenerateGraphMut", "Generate graph", class = "graph-button")
                              } else if (graphId == "geneGraph") {
                                actionButton("btnGenerateGraphGene", "Generate graph", class = "graph-button")
                              } else if (graphId == 'multiGeneGraph') {
                                actionButton("btnGenerateGraphMultiGene", "Generate graph", class = "graph-button")
                              }
               )
             }
           ))
  )
}


articlesPanel <- function() {
  tabPanel('Articles',         
           
           rclipboardSetup(),
           
           br(),
           fluidRow(
             shiny::column(
               width = 5,
               h4("Display your articles by creating a collection in PubTator")
             ),
             shiny::column(width = 4,
                          # actionButton(inputId = "btnPubTator", label = "Load/Refresh PubTator Results", 
                          #              class = "blue-button")
                          uiOutput("clip")
             )
           ),
           fluidRow(
             shiny::column(
               width = 12,
               p("Directions: The ", 
                 a(href = "https://www.ncbi.nlm.nih.gov/CBBresearch/Lu/Demo/PubTator/", 
                 target="_blank", "PubTator"), 
                 "website is displayed below. Click the", em("Create a new collection"), "link to create a new PubTator collection.",
                 "Click the above button to copy your PMIDs and paste into the Collection PMID list box.",
                 "Give your collection a name and click the submit button. Your collection will now appear in the Collection list.", 
                 "Click the collection name to view the results."  
                ), 
                hr(style="background-color:darkred; height:1px; margin:10px;")
              )
            ),
           fluidRow(
             #shiny::column(id = "colPubs", width = 2, 
             #                DT::dataTableOutput("articleTable")),
             shiny::column(id = "colPubs", width = 12,
                                uiOutput("articles")
             )
           )
  )
}

addDownloadsTabPanel <- function(title) {
  tabPanel(title,
        br(),
          
        h3("Click a button below to download PMIDS and summary results as a csv file."),
        br(),
 
        fluidRow(
          column(3,
        
        downloadButton("downloadPMIDs", "Download PMIDs",
                       class = 'blue-button width100'), br(),br(),
 
        downloadButton("downloadCancerTypesData", "Download Cancer Type Summary",
                       class = 'blue-button width100'), br(),br(),
        
        downloadButton("downloadCancerTermsData", "Download Cancer Terms Summary",
                       class = 'blue-button width100')
          ), column(3,
        
        downloadButton("downloadDrugTreatmentsData", "Download Drugs Summary",
                       class = 'blue-button width100'), br(),br(),
                             
        downloadButton("downloadMutationsData", "Download Mutations Summary",
                       class = 'blue-button width100'), br(),br(),
        
        downloadButton("downloadGenesData", "Download Genes Summmary",
                       class = 'blue-button width100')
          )
        )
  )
}



shinyUI(
  
  navbarPage(title = 'Cancer Publication Portal',
             id = "headerNavBarPage", 
    
    tabPanel("Home",
          
             tags$head(HTML(
               "<!-- Global site tag (gtag.js) - Google Analytics -->
                <script async src='https://www.googletagmanager.com/gtag/js?id=UA-62826724-4'></script>
               <script>
               window.dataLayer = window.dataLayer || [];
               function gtag(){dataLayer.push(arguments);}
               gtag('js', new Date());
               
               gtag('config', 'UA-62826724-4');
               </script>
               "
             )),
             
          commonHeader, 
          fluidRow(column(style='border-right: 1px solid',width = 12,
          tabsetPanel(id = "MainPage",
            #addTabPanel('Cancer Types', "cancerSummaryTable"),
            #addTabPanel('Treatments', "paResults"),
            addTabPanel("Cancer Types", "diseaseResults", "cancerGraph"),
            addTabPanel("Selected Genes", "multiGeneResults", "multiGeneGraph"),
            addTabPanel("Cancer Terms", "cancerTermResults", "cancerTermGraph"),
            addTabPanel('Drugs', 'chemResults', "chemGraph"),
            addTabPanel("Mutations", "mutationResults", "mutGraph"),
            addTabPanel("Additional Genes", "geneResults", "geneGraph"),
            addDownloadsTabPanel("Download"),
            tabPanel("Articles", articlesPanel())

          )))#, # end tabsetPanel and 1st row
          #fluidRow(column(width = 12, articlesPanel() ) )# end second row (articles panel)
          ), # end Portal Panel
      tabAbout,
    
    # activate shinyJS
    useShinyjs(),
    shinyjs::extendShinyjs(text = jsCode)
    
      #logPanel()
  ) # end navbarPage
) # end shinyUI
               

