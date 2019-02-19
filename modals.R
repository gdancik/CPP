# from shinyGEO, includes cancel button with applyID
formatBSModal<-function (id, title, trigger, applyID, ..., size) 
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
                                                                           actionButton(applyID, "Update filters", class = "btn-primary")    
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

