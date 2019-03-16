# from shinyGEO, includes cancel button with applyID
formatBSModal<-function (id, title, trigger, applyID, ..., size, applyText = "Update filters") 
{

  if (!missing(size)) {
    if (size == "large") {
      size = "modal-lg"
    }
    else if (size == "small") {
      size = "modal-sm"
    }
    size <- paste("modal-dialog", size)
  }
  else {
    size <- "modal-dialog"
  }
  bsTag <- shiny::tags$div(class = "modal sbs-modal fade", 
                           id = id, tabindex = "-1", `data-sbs-trigger` = trigger, 
                           shiny::tags$div(class = size, 
                                           shiny::tags$div(class = "modal-content", 
                                                           shiny::tags$div(class = "modal-header", 
                                                                           shiny::tags$button(type = "button",  class = "close", `data-dismiss` = "modal", shiny::tags$span(shiny::HTML("&times;"))), 
                                                                           shiny::tags$h4(class = "modal-title", title)
                                                           ), 
                                                           shiny::tags$div(class = "modal-body", list(...)), 
                                                           shiny::tags$div(class = "modal-footer", 
                                                                           shiny::tags$button(type = "button", class = "btn btn-default", `data-dismiss` = "modal", "Cancel"),
                                                                           actionButton(applyID, applyText, class = "btn-primary")    
                                                           )      
                                           )
                           )
  )
  #htmltools::attachDependencies(bsTag, shinyBSDep)
}


welcomeModal <-  bsModal("welcomeModal",HTML("<i>Cancer Publication Portal</i>"), trigger = "btnNewSearch", size = "large",
      p(strong("Instructions:"), "This is a beta version of a Cancer Publication Portal for summarizing and searching cancer-related literature.",
        "To start, select a gene and click Search to summarize cancer publications for that gene. When the results are displayed, you can click on a row in any 
        table to further filter the results. Filters can be removed by removing the term from the 
        drop down box."),
      p("Feedback is welcome, and can be sent to dancikg@easternct.edu"),
      
      hr(class = "blue-button", style="height: 2px"),
      
      fluidRow(
        column(2, style="padding-right:0px",
          selectizeInput("geneInput", label = "Select a gene", choices = NULL)
        ),
        column(2,style = "vertical-align:middle; padding-left:0px",
           HTML("<label class = 'control-label' style='visibility:hidden'>Hello</label>"),
           div(
                actionButton("btnGeneSearch", "Summarize Cancer Articles", class = "blue-button")
           )
        )
      )
        
  )


filterModal <- formatBSModal("filterModal", "Remove filters", "btnRemoveFilters", "btnSaveFilters",

  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
        HTML("<div class=\"progress\" style=\"z-index:1000; position: fixed; width: 95%; height:25px; !important\">
                                               <div class=\"progress-bar progress-bar-striped active\" role=\"progressbar\" aria-valuenow=\"100\" aria-valuemin=\"0\" aria-valuemax=\"100\" style=\"opacity: 1; width:100%\" !important>
                         <span id=\"modal-bar-text\"><b>Applying filters, please wait...</b></span>
                                               </div></div>")
  ),
                         
                         
  fluidRow(column(12,
     HTML("<p>To remove a filter, delete the term from the dropdown menus below. Then click the 'Update filters' button to apply your changes.</p><br>")
    )),
    fluidRow(
      shiny::column(width=4,
                  selectInput("filterDisease", "Disease Filters", choices = NULL, multiple = TRUE, selectize = TRUE)
      ),
      shiny::column(width=4,
                    selectInput("filterCancerTerms", "Cancer Term Filters", choices = NULL, multiple = TRUE, selectize = TRUE)
      ),
      shiny::column(width=4,
                  selectInput("filterChem", "Chem Filters", choices = NULL, multiple = TRUE, selectize = TRUE)
      )
    ),
    fluidRow(
      shiny::column(width=4,
                  selectInput("filterMutations", "Mutation Filters", choices = NULL, multiple = TRUE, selectize = TRUE)
      ),    
      shiny::column(width=4,
                  selectInput("filterGenes", "Additional Gene Filters", choices = NULL, multiple = TRUE, selectize = TRUE)
      )
    ), size = "large"
)

# modals for graph settings
graphSetupModalTerm <- formatBSModal("graphSetupModalTerm", "Cancer Terms Graph Settings", "btnGraphSetupTerm", "btnUpdateGraphTerm",

                      fluidRow(column(12, 
                            HTML("<p>Selection boxes below show default graph settings (15 most frequent terms in 10 most frequent cancers) or your\
                                    previous customized settings. <br><br>Tune the graph by choosing among all mentioned cancer types and terms for the gene. \
                                    You can add a term by clicking on its selection box and choosing from the dropdown menu below \
                                    (you can start typing the term to find it in the list).\
                                    You can also remove current selection by deleting the term in the selection box. <br><br>\
                                    When you are done, click the 'Update graph' button to apply your changes.\
                                 We do not recommend choosing more than 10 cancer types and more than 15 cancer terms. </p><br>")
                             )),
                      actionButton("defaultCterms", "Restore Defaults", class = "btn btn-info"),
                      actionButton("clearCterms", "Clear", class = "btn btn-secondary"),
                      hr(class = "blue-button", style="height: 2px"),
                      fluidRow(
                        column(6, style="padding-right:0px",
                               selectInput("ctype", "Select cancer types: *", choices = NULL, multiple = TRUE, selectize = TRUE)
                             ),
                        column(6, style="padding-right:0px",
                               selectInput("termType", "Select cancer terms: *", choices = NULL, multiple = TRUE, selectize = TRUE)
                        )
                      ), size = "large", applyText = "Update graph"
                   )

graphSetupModalChem <- formatBSModal("graphSetupModalChem", "Chemicals Graph Settings", "btnGraphSetupChem", "btnUpdateGraphChem",
                                     
                        fluidRow(column(12, 
                            HTML("<p>Selection boxes below show default graph settings (15 most frequent chemicals in 10 most frequent cancers) or your\
                                  previous customized settings. <br><br>Tune the graph by choosing among all mentioned cancer types and chemicals for the gene. \
                                  You can add a term by clicking on its selection box and choosing from the dropdown menu below \
                                  (you can start typing the term to find it in the list).\
                                  You can also remove current selection by deleting the term in the selection box. <br><br>\
                                  When you are done, click the 'Update graph' button to apply your changes.\
                                  We do not recommend choosing more than 10 cancer types and more than 15 chemicals. </p><br>")
                               )),
                       actionButton("defaultChems", "Restore Defaults", class = "btn btn-info"),
                       actionButton("clearChems", "Clear", class = "btn btn-secondary"),
                       hr(class = "blue-button", style="height: 2px"),
                       fluidRow(
                         column(6, style="padding-right:0px",
                                selectInput("ctypeChem", "Select cancer types: *", choices = NULL, multiple = TRUE, selectize = TRUE)
                               ),
                         column(6, style="padding-right:0px",
                                selectInput("chems", "Select chemicals: *", choices = NULL, multiple = TRUE, selectize = TRUE)
                               )
                        ), size = "large", applyText = "Update graph"
                    )


graphSetupModalMut <- formatBSModal("graphSetupModalMut", "Mutations Graph Settings", "btnGraphSetupMut", "btnUpdateGraphMut",
                                     
                        fluidRow(column(12, 
                            HTML("<p>Selection boxes below show default graph settings (15 most frequent mutations in 10 most frequent cancers) or your\
                                  previous customized settings. <br><br>Tune the graph by choosing among all mentioned cancer types and mutations for the gene. \
                                  You can add a term by clicking on its selection box and choosing from the dropdown menu below \
                                  (you can start typing the term to find it in the list).\
                                  You can also remove current selection by deleting the term in the selection box. <br><br>\
                                  When you are done, click the 'Update graph' button to apply your changes.\
                                  We do not recommend choosing more than 10 cancer types and more than 15 mutations. </p><br>")
                              )),
                            actionButton("defaultMuts", "Restore Defaults", class = "btn btn-info"),
                            actionButton("clearMuts", "Clear", class = "btn btn-secondary"),
                            hr(class = "blue-button", style="height: 2px"),
                            fluidRow(
                                 column(6, style="padding-right:0px",
                                    selectInput("ctypeMut", "Select cancer types: *", choices = NULL, multiple = TRUE, selectize = TRUE)
                                 ),
                                 column(6, style="padding-right:0px",
                                    selectInput("muts", "Select mutations: *", choices = NULL, multiple = TRUE, selectize = TRUE)
                                 )
                             ), size = "large", applyText = "Update graph"
                        )