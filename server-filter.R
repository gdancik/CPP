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


