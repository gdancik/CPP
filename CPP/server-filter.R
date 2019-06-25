##########################################################################################
# Functions to handle filterModal
##########################################################################################

observeEvent(input$filterModal,{
  shinyjs::disable("btnSaveFilters") 
  setSelectInput(session, "filterDisease", diseaseSummary$selectedID, diseaseSummary$selectedTerm)
  setSelectInput(session, "filterChem", chemSummary$selectedID, chemSummary$selectedTerm)
  setSelectInput(session, "filterMutations", mutationSummary$selectedID, mutationSummary$selectedTerm)
  setSelectInput(session, "filterCancerTerms", cancerTermSummary$selectedID, cancerTermSummary$selectedTerm)
  setSelectInput(session, "filterGenes", geneSummary$selectedID, geneSummary$selectedTerm)

})



# observer to enable / disable Save Filter button
observe({

    # return TRUE if x includes values that y does not
    f <- function(x,y) {
        s <- setdiff(x,y)
        length(s) > 0
    }

    # check whether any filters have changed
    l <- vector("logical", 5)
    l[1] <- f(diseaseSummary$selectedID, input$filterDisease)
    l[2] <- f(chemSummary$selectedID, input$filterChem)
    l[3] <- f(mutationSummary$selectedID, input$filterMutations)
    l[4] <- f(cancerTermSummary$selectedID, input$filterCancerTerms)
    l[5] <- f(geneSummary$selectedID, input$filterGenes)

    # enable or disable Save Filter button as appropriate
    if (any(l)) {
        shinyjs::enable("btnSaveFilters")
    } else {
        shinyjs::disable("btnSaveFilters")
    }
})

# updates filters based on filter values 'f' and summary table with name 's_name'
updateFilters <- function(f,s_name) {

    x <- get(s_name)
    cat("filters: ", f, "\n")
    cat("summary: ", x$selectedID, "\n")

    if (setequal(x$selectedID,f)) {
        return (FALSE) 
    }
 
    if (is.null(f)) {
        isolate(x$selectedID <- NULL)
        isolate(x$selectedTerm <- NULL)
    } else {
        cat("updating to: ", f, "\n")  

        g <- grep(f, x$selectedID)
    
        isolate(x$selectedID <- f)
        isolate(x$selectedTerm <- x$selectedTerm[g])
    }

    isolate(assign(s_name, x, env = .GlobalEnv))
    return(TRUE)
}


resetRefreshPending <- function() {
          chemSummary$refreshPending <- NULL
          mutationSummary$refreshPending <- NULL
          cancerTermSummary$refreshPending <- NULL
          geneSummary$refreshPending <- NULL
}

observeEvent(input$btnSaveFilters, {

      f <- vector("logical", 5)
      f[[1]] <- updateFilters(input$filterDisease, "diseaseSummary")
      f[[2]] <- updateFilters(input$filterChem, "chemSummary")
      f[[3]] <- updateFilters(input$filterMutations, "mutationSummary")
      f[[4]] <- updateFilters(input$filterCancerTerms, "cancerTermSummary")
      f[[5]] <- updateFilters(input$filterGenes, "geneSummary")

      if (any(f)) {
          catn("\n\nAPPLYING FILTER NOW...")
          removeNotification('refreshNotification')
          disableTableClicks()
          cat("closing modal...\n")
          toggleModal(session, "filterModal")
          cat("respond to selection ...\n")
          
          resetRefreshPending()

          respondToSelectionDrill()
          enableTableClicks() 
      }

})

