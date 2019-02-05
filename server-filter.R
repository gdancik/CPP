##########################################################################################
# Functions to handle filterModal
##########################################################################################

observeEvent(input$filterModal,{
  cat("entering modal\n")
  setSelectInput(session, "filterDisease", diseaseSummary$selectedID, diseaseSummary$selectedTerm)
  setSelectInput(session, "filterChem", chemSummary$selectedID, chemSummary$selectedTerm)
  setSelectInput(session, "filterMutations", mutationSummary$selectedID, mutationSummary$selectedTerm)
  setSelectInput(session, "filterCancerTerms", cancerTermSummary$selectedID, cancerTermSummary$selectedTerm)
  setSelectInput(session, "filterGenes", geneSummary$selectedID, geneSummary$selectedTerm)

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
        x$selectedID <- NULL
        x$selectedTerm <- NULL
    } else {
        cat("updating to: ", f, "\n")  

        g <- grep(f, x$selectedID)
    
        x$selectedID <- f
        x$selectedTerm <- x$selectedTerm[g]
    }

    assign(s_name, x, env = .GlobalEnv)
    return(TRUE)
}

# let's enable button only if there is a change
observeEvent(input$saveFilters, {

      f <- vector("logical", 5)
      f[[1]] <- updateFilters(input$filterDisease, "diseaseSummary")
      f[[2]] <- updateFilters(input$filterChem, "chemSummary")
      f[[3]] <- updateFilters(input$filterMutations, "mutationSummary")
      f[[4]] <- updateFilters(input$filterCancerTerms, "cancerTermSummary")
      f[[5]] <- updateFilters(input$filterGenes, "geneSummary")

      if (any(f)) {
          cat("closing modal...\n")
          toggleModal(session, "filterModal")
          cat("respond to selection ...\n")
          respondToSelectionDrill() 
      }

})

