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

