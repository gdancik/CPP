##########################################################################################
# Functions to handle cancerTypeSetupModal
##########################################################################################

# when modal opens following gene search
observeEvent(input$cancerTypeSetupModal,{
  a <- cancerSelectionSummary$selected1
  catn("open modal, set selected to: ", cancerSelectionSummary$selected1)
  updateSelectInput(session, "cancerType", choices = cancerSelectionChoices(), 
                    selected = a)
  
  if (is.null(diseaseSummary$dat)) {
    shinyjs::enable('btnSelectCancerType')
  }
})


# when modal opens from select cancer button
observeEvent(input$btnSelectCancerType, {
  
  cat("searching for gene btnSelectCancertype selection...\n")
  disableTableClicks()
  shinyjs::disable('btnSelectCancerType')
  showProgress()
  
  shinyjs::removeClass(id = "btnWelcomeCancel", class = "cancel") 
  shinyjs::removeClass(id = "btnCancelCancerType", class = "cancel") 
  shinyjs::removeClass(id = "btnCancelFilter", class = "cancel") 
  
  cat("selected types: ", input$cancerType, "\n")
  ids <- getChildMeshIDsForSelectedCancers()
  cat("children: ", ids, "\n")
  
  isolate(cancerSelectionSummary$selected1 <- input$cancerType)
  isolate(cancerSelectionSummary$selected2 <- ids)
  
  meshIDs <- unique(c(input$cancerType, ids))
  
  genes <- selected$geneID
  
  con = dbConnect(MariaDB(), group = "CPP")
  if (!is.null(meshIDs)) {
    pmidList$pmids_initial = getCancerPMIDsbyMeshID(con, cleanseList(genes), cleanseList(meshIDs))
  } else {
    pmidList$pmids_initial <- getPMIDs("PubGene", "GeneID", genes, con, NULL)
  }
  
  dbDisconnect(con)
  
  cat("done getting cancer IDs\n")
  
  respondToSelectionDrill()
  
  toggleModal(session, "cancerTypeSetupModal",  toggle = "close")
  hideProgress()
  enableTableClicks()
  
})

# ## update btnSelectCancerType based on current selection
observe({
  label <- "Retrieve summaries for selected cancer types"
  if (is.null(input$cancerType)) {
    label <- "Retrieve summaries for all cancer types"
  }
  updateActionButton(session, "btnSelectCancerType", label)

  if (is.null(diseaseSummary$dat)) {
    return()
  }
  
  if (setequal(input$cancerType, cancerSelectionSummary$selected1)) {
    shinyjs::disable('btnSelectCancerType')
  } else {
    shinyjs::enable('btnSelectCancerType')
  }

})





