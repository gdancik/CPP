source("progress.R")

# from shinyGEO, includes cancel button with applyID; cancelID is optional id of cancel button
formatBSModal<-function (id, title, trigger, applyID, ..., size, applyText = "Update filters", 
                         cancelID = NULL, escape = TRUE) 
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
  t <- bsTag <- shiny::tags$div(class = "modal sbs-modal fade", `data-backdrop` = 'static',
                           id = id, tabindex = "-1", `data-sbs-trigger` = trigger, 
                           shiny::tags$div(class = size, 
                                           shiny::tags$div(class = "modal-content", 
                                                           shiny::tags$div(class = "modal-header", 
                                                                           shiny::tags$button(type = "button",  class = "close", `data-dismiss` = "modal", shiny::tags$span(shiny::HTML("&times;"))), 
                                                                           shiny::tags$h4(class = "modal-title", title)
                                                           ), 
                                                           shiny::tags$div(class = "modal-body", list(...)), 
                                                           shiny::tags$div(class = "modal-footer", 
                                                                           shiny::tags$button(id = cancelID, type = "button", class = "btn btn-default cancel", `data-dismiss` = "modal", "Cancel"),
                                                                           actionButton(applyID, applyText, class = "btn-primary")    
                                                           )      
                                           )
                           )
  )
  if (!escape) {
    t$attribs$`data-keyboard`= "false" 
  }
  t
  #htmltools::attachDependencies(bsTag, shinyBSDep)
}


welcomeModal <-  bsModal("welcomeModal",HTML("<i>Cancer Publication Portal</i>"), trigger = "btnNewSearch", size = "large",
      p(strong("Instructions:"), "Welcome to the Cancer Publication Portal for summarizing and searching cancer-related literature.",
        "To start, select a gene and click the 'Summarize cancer types' button to select the cancer types you are interested in.
        After selecting the cancer types of interest, articles will be summarized based on drugs, cancer terms, mutations, and additional genes
        mentioned in article titles and abstracts. Additional filters can be applied by clicking on any of the tables."),
        
      fluidRow(column(12,
               progressDiv('welcomeModalProgress', 'welcomeModal-bar-text', "Summarizing cancer types, please wait..."))),


      hr(class = "blue-button", style="height: 2px"),
      HTML("<p>Note: <i>CPP</i> uses official gene symbols. Check your genes or lookup invalid symbols from 
             <a href = 'https://www.genenames.org/tools/multi-symbol-checker/'>genenames.org</a>.</p>"),
      
      fluidRow(
        column(2, style="padding-right:0px",
          selectizeInput("geneInput", label = "Select a gene", choices = NULL)
        ),
        column(2,style = "vertical-align:middle; padding-left:0px",
           HTML("<label class = 'control-label' style='visibility:hidden'>Hello</label>"),
           div(
                actionButton("btnGeneSearch", "Summarize cancer types", class = "blue-button")
           )
        )
      ), 
      fluidRow(
        h4('- OR- ', style = 'padding-left:15px')
      ),
      fluidRow(
        br(),
        column(4, style = "padding-right:0px",
               textAreaInput('multiGeneInput', label = 'Select multiple genes',
                              value = "",
                              placeholder = 'Enter multiple genes, separated by spaces or one per line', rows = 10,
                              resize = "none")
        ),
        
        column(4, style = "padding-right:0px",
               
               textAreaInput('invalidGeneOutput', label = 'Invalid genes', rows = 10,
                             resize = "none"))#,
        #column(2),
      ), fluidRow(
        column(4, style = "padding-right:0px", 
               div(width = "100%",
                 actionButton("btnMultiGeneSearch", "Process multiple genes", class = "blue-button",
                              style = "width:100%;", disabled = "dfdisabled")
               )
        ), column(4, style = "padding-right:0px;padding-top: 7px;padding-bottom: 7px;text-align:center",
                  htmlOutput('multiGeneMsg')
        )
      ) 
  )

# prevent outside click of modal to close it
welcomeModal$attribs$`data-keyboard`= "false" 
welcomeModal$attribs$`data-backdrop`= "static"

# set class of cancel button
a <- welcomeModal$children[[1]]$children[[1]]$children[[3]]$children[[1]]$attribs
a$class <- paste(a$class, "cancel")
welcomeModal$children[[1]]$children[[1]]$children[[3]]$children[[1]]$attribs <- a
welcomeModal$children[[1]]$children[[1]]$children[[3]]$children[[1]]$attribs$id <- 'btnWelcomeCancel'
rm(a)


multiGeneSummaryModal <- formatBSModal('multiGeneSummary', "Processing multiple genes", 
                                 trigger = 'btnMultiGeneSearch', 'btnMultiGeneCancerSummary',
    br(),
    p("The table below shows the results..."), br(),
    
    fluidRow(
      column(6,
          withSpinner(DT::dataTableOutput("multiGeneSummaryTable"), type = 3,
                                             color.background = "white")
      )
    
    ), applyText = "Summarize cancer types", cancelID = "cancelMultiGeneSummary", size = "large"
)
                                 
                                 




## variables / functions for filterModal
selectionTypeChoices <- rev(c("ALL selected terms" = "all",
                              "ANY selected terms" = "any"))

filterModal <- formatBSModal("filterModal", "Remove filters", "btnRemoveFilters", "btnSaveFilters", applyText = "Apply filters",

  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
        HTML("<div class=\"progress\" style=\"z-index:1000; position: fixed; width: 95%; height:25px; !important\">
                                               <div class=\"progress-bar progress-bar-striped active\" role=\"progressbar\" aria-valuenow=\"100\" aria-valuemin=\"0\" aria-valuemax=\"100\" style=\"opacity: 1; width:100%\" !important>
                         <span id=\"modal-bar-text\"><b>Applying filters, please wait...</b></span>
                                               </div></div>")
  ),
                         
                         
  fluidRow(column(12,
     HTML("<p>To remove a filter, delete the term from the dropdown menus below. Then click the 'Apply filters' button to apply your changes.</p><br>
           <p> To clear all filters, click the 'Clear filters' button then the 'Apply filters' button to apply your changes.
          </br><hr>" 
     ))),
    

    fluidRow(
      shiny::column(width=4,
                    selectInput("filterCancerTerms", "Selected cancer terms", choices = NULL, multiple = TRUE, selectize = TRUE)            
      ),
      shiny::column(width=4,
                    selectInput("filterCancerTermsType", "Search for", choices = selectionTypeChoices, selected = "any")
      )
    ),
    
    fluidRow(
      shiny::column(width=4,
                    selectInput("filterChem", "Selected drugs", choices = NULL, multiple = TRUE, selectize = TRUE)           
      ), 
      shiny::column(width=4,
                    selectInput("filterChemType", "Search for", choices = selectionTypeChoices, selected = "any")
      )
    ),
    
    fluidRow(  
      shiny::column(width=4,
                    selectInput("filterMutations", "Selected mutations", choices = NULL, multiple = TRUE, selectize = TRUE)                    
      ),
      shiny::column(width=4,
                    selectInput("filterMutationsType", "Search for", choices = selectionTypeChoices, selected = "any")
      )
    ),
  
    fluidRow(
      shiny::column(width=4,
                    selectInput("filterGenes", "Additional selected genes", choices = NULL, multiple = TRUE, selectize = TRUE)            
      ),
      shiny::column(width=4,
                    selectInput("filterGenesType", "Gene filter type", choices = selectionTypeChoices, selected = "any")
      )
  ) , size = "large", cancelID = "btnCancelFilter"
)

filterModal$children[[1]]$children[[1]]$children[[3]]
if (!filterModal$children[[1]]$children[[1]]$children[[3]]$attribs$class == 'modal-footer') {
  stop("filterModal structure has changed")
}
filterModal$children[[1]]$children[[1]]$children[[3]]$children[[3]] <- 
  actionButton('btnClearFilters', 'Clear filters', class = 'btn-danger')




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


cancerTypeSetupModal <- formatBSModal("cancerTypeSetupModal", "Select cancer types", "", "btnSelectCancerType", 
                                      fluidRow(column(12,
                                            uiOutput('cancerTypeSummaryHeader'),          
                                            progressDiv('cancerTypeProgress', 'cancerSelection-bar-text'),          
                                              HTML("<p>Select your desired cancer types by clicking on the table or by using the drop down below. 
                                                           When you are finished, click the 'Retrieve' button to retrieve summaries of your results. </br></br>\
                                                           </p>")
                                                      )),
                                      fluidRow(
                                        
                                        column(6,
                                               withSpinner(DT::dataTableOutput("cancerSelectionTable"), type = 3,
                                                           color.background = "white")
                                               ),
                                        column(1),
                                        
                                        column(5, 
                                               
                                               fluidRow(
                                               conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                                                      style="padding-right:0px",
                                                      div(id = "cancerTypeDiv",
                                                          selectizeInput("cancerType", "Selected cancer types: ", choices = NULL, multiple = TRUE,
                                                                      options = list(placeholder = "(leave blank to search all articles)") ),
                                                            htmlOutput("cancerSelectionMsg")
                                                      )
                                               ))
                                        )
                                      ),
                                      
                                      size = "large", applyText = "Retrieve summaries for all cancer types", 
                                      cancelID = "btnCancelCancerType", escape = FALSE
)


showProgress <- function(msg = NULL) {
  if (!is.null(msg)) {
    shinyjs::html("cancerSelection-bar-text", msg)
  }
  shinyjs::removeClass(id = 'cancerTypeProgress', class = 'hide')
}

hideProgress <- function() {
  shinyjs::addClass(id = 'cancerTypeProgress', class = 'hide')
}


refreshNotification <- function() {
  showNotification("Your filters have changed!",
                   div(hr(class = 'blue-button', style="height: 2px; margin-top: 10px; margin-bottom: 10px;"),
                       div(style = 'width: 100%; display:inline-block; text-align:center',
                           actionButton("btnRefresh", "View/Update", class = 'blue-button', style = 'width: 45%;'),
                           HTML("&nbsp;&nbsp;"),
                           actionButton("btnCancelRefresh", "Cancel", class = 'btn-danger', style = 'width: 45%;')
                       )
                   ), id = "refreshNotification", duration = NULL,  closeButton = FALSE)
}
