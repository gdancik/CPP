# add a tab panel with given title, table, and graph,
# may want to consider setting click = "CancerGraph_click", etc on graph
addTabPanel <- function(title, tableId, graphId = NULL, statTableId = NULL) {
  
      tt <- list(fluidRow(div(style = "height: 300px",
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
           
          if (is.null(statTableId)) {
            return(tabPanel(title, tt))
          }    
      
          tabPanel(title, 
                   br(),
                   tabsetPanel(
                     tabPanel('Summary', tt),
                     tabPanel('Full table', 
                              fluidRow(div(style = "height: 300px",
                                           column(width = 1),
                                           shiny::column(width = 10,
                                                         DT::dataTableOutput(statTableId), type = 3,
                                                         color.background = "white"
                                           )
                              )),
                              
                              fluidRow(column(width=1),
                                       column(width = 10,
                                              HTML('<br><b>Term enrichment in selected articles</b>. Analysis assesses whether terms are
                                    more likely than chance to appear in the selected article abstracts. 
                                                   <i>Frequency</i> is the number of articles with the specified term;
                                                   <i>n</i> is the number of selected articles;
                                                   <i>Proportion</i> is the proportion of selected articles with the corresponding term, rounded
                                                      to 3 decimal places.
                                                   Similar values are reported for <i>N</i> = total number of articles in the database.
                                                   The <i>Score</i> is equal to <i>Proportion</i> / <i>Proportion_N</i> and is the
                                                   fold enrichment of the term. 
                                                   P-values are calculated according to the hypergeometric distribution, and rounded to 2 
                                                    decimal places. <br><br>')
                                       )
                             ))
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

articlesPanel <- function() {
  tabPanel('Articles',         
           
      div(style = 'margin-left:5%; margin-right:5%',
           
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
                 a(href = "https://www.ncbi.nlm.nih.gov/research/pubtator/", 
                   target="_blank", "PubTator Central"), 
                 "website is displayed below. To view your selected articles, first click the", em("Collection Manager"), "icon (on the top right of the PubTator page, next to", em("TUTORIAL", .noWS ="after"), ") to create a new PubTator collection.",
                 "Click the", em("New Collection"), "button. Then click the above button to copy your PMIDs and paste these into the Collection PMID text box on the PubTator page.",
                 "Click on the", em("Update"), "button, and then click on", em("Show"), "to view your articles.",
                 strong("Note: "), "if you prefer, or if the page below is not working, you can access PubTator directly from this link: ",
                 a(href = "https://www.ncbi.nlm.nih.gov/research/pubtator/", target="_blank", "https://www.ncbi.nlm.nih.gov/research/pubtator/", ".")
               ), 
               hr(style="background-color:darkred; height:1px; margin:10px;")
             )
           ),br(),
           fluidRow(
             #shiny::column(id = "colPubs", width = 2, 
             #                DT::dataTableOutput("articleTable")),
             shiny::column(id = "colPubs", width = 12,
                           uiOutput("articles")
             )
           )
      )
  )
}

