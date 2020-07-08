source("progress.R")

########################################################################
# original formatBS modal from shinyGEO
# includes cancel button with applyID; cancelID is optional id of cancel button
# toggleModal does not work bc modal does not have right R class; but still
# used for filterModal
########################################################################
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
  t <- bsTag <- shiny::tags$div(class = "modal sbs-modal fade", #`data-backdrop` = 'static',
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

## variables / functions for filterModal
selectionTypeChoices <- rev(c("ALL selected terms" = "all",
                              "ANY selected terms" = "any"))

filterModal <- formatBSModal("filterModal", "Remove filters", "btnTest", #"btnRemoveFilters",
                             "btnSaveFilters", applyText = "Apply filters",

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

########################################################
# common modals
########################################################

formatBSModal <- function(id, title, trigger, ..., size, cancelID, actionID = NULL, actionLabel = NULL) {
        x <- bsModal(id, title, trigger, ..., size = size)
        x$children[[1]]$children[[1]]$children[[3]]$children[[1]]$attribs$id <- cancelID
        #x$children[[1]]$children[[1]]$children[[3]]$children[[1]]$attribs$disabled <- ""
        x$children[[1]]$children[[1]]$children[[3]]$children[[1]]$children[[1]] <- "Cancel"        
        if (!is.null(actionID)) {
           x$children[[1]]$children[[1]]$children[[3]]$children[[2]] <- 
              actionButton(actionID, actionLabel, 
                    class = 'btn-primary cancel', `data-dismiss` = 'modal')  
        }
        
        x
}

cancerTypeSetupModal <- formatBSModal("cancerTypeSetupModal", "Select cancer types", "notrigger", 
          fluidRow(column(12,
              uiOutput('cancerTypeSummaryHeader'),          
              progressDiv('cancerTypeProgress', 'cancerSelection-bar-text'),          
              HTML("<p>Select your desired cancer types by clicking on the table or by using the drop down 
                    below. Remove selected cancer types by clicking on the table or deleting them from the 
                    drop down. All cancer types will be summarized if none are selected. You may also upload your cancer types from a file, and Download your selected 
                    cancer types for future use. When you are finished, click the 'Retrieve' button to retrieve summaries of your results. </br></br>\
              </p>")
          )),
          fluidRow(column(6,
              withSpinner(DT::dataTableOutput("cancerSelectionTable"), type = 3,
                color.background = "white")
          ),column(1),column(5, 
          
          fluidRow(
            conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                style="padding-right:0px", 
                div(id = "cancerTypeDiv",
                  selectizeInput("cancerType", "Selected cancer types:", choices = NULL, multiple = TRUE,
                            options = list(placeholder = "(leave blank to search all articles)")),
                  downloadLink('saveCancerTypes', 'Save selected cancer types to file'),
                  htmlOutput("cancerSelectionMsg")
                )),
      
            fluidRow(
              fileInput("cancerTypeFile", "Upload cancer types (must include MeshIDs in first column)",
                accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
            ))
          )
        )),

        size = "large", cancelID = "btnCancelCancerType",
        actionID = "btnSelectCancerType", 
        actionLabel = "Submit")

commonModals <- list(cancerTypeSetupModal, filterModal)

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
