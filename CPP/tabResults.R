#################################
# 
#################################


commonResults <- list(
  
  # summary row
  fluidRow(id = "summaryRow",
           #shiny::column(width = 2,
           #              radioButtons("rbDiseaseLimits", "Article Limits:",
           #                           c("Cancer-related only" = "cancer", "All articles" = "none"), selected = "cancer")),
           
           shiny::column(width=1),
           shiny::column(width =10 ,
                         htmlOutput("summaryHeader")              
           ),
           br()
           
  ),
  
  hr(style = "padding: 0px; margin: 0px"),
  
  fluidRow(shiny::column(width = 12,
                         # div(style = "position: relative; width: 100%; height: 10px", 
                         #     conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         #                      
                         #                      ))
                         #     ),
    div(style = "position: relative; width: 100%; height: 10px",
        HTML("<div id = 'progress-bar-results' class=\"progress\" style=\"position: fixed;  width: 100%; height:50px; z-index:1000; !important\">
                                                   <div class=\"progress-bar progress-bar-striped active\" role=\"progressbar\" aria-valuenow=\"100\" aria-valuemin=\"0\" aria-valuemax=\"100\" style=\"opacity: 1; width:100%\" !important>
             </br>
             <span id=\"bar-text\" style = \"font-size:2em\"><b>Loading, please wait...</b></span>
             </div>
             </div>")
        ))
    ),                     
  
  br()
                         )


tabResults <- tabPanel("Results",
                       commonResults,
                       # actionButton('btnLaunchCancerTypes', 'Click',
                       #              `data-toggle`="modal",
                       #              `data-target`="#cancerTypeSetupModal",
                       #              style = "display:none;"),
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
)


