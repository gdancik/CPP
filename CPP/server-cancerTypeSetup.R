##########################################################################################
# Functions to handle cancerTypeSetupModal
##########################################################################################
observeEvent(input$cancerTypeSetupModal,{
  
  updateSelectInput(session, "cancerType", choices = cancerSelectionChoices(), 
                    selected = cancerSelectionSummary$selected1)
})


# Select Cancer Type button
observeEvent(input$btnSelectCancerType, {
  
  cat("open modal from btnSelectCancertype selection...\n")
  toggleModal(session, "cancerTypeSetupModal",  toggle = "close")
  
  shinyjs::removeClass(id = "btnWelcomeCancel", class = "cancel") 
  shinyjs::removeClass(id = "btnCancelCancerType", class = "cancel") 
  shinyjs::removeClass(id = "btnCancelFilter", class = "cancel") 
  
  cat("selected types: ", input$cancerType, "\n")
  ids <- getChildMeshIDsForSelectedCancers()
  cat("children: ", ids, "\n")
  
  
  isolate(cancerSelectionSummary$selected1 <- input$cancerType)
  isolate(cancerSelectionSummary$selected2 <- ids)
  
  meshIDs <- unique(c(input$cancerType, ids))
  
  if (!is.null(meshIDs)) {
    con <- dbConnect(MariaDB(), group = "CPP")
    pmidList$pmids_initial = getCancerPMIDsbyMeshID(con, cleanse(input$geneInput), cleanseList(meshIDs))
    dbDisconnect(con)
  }
  
  cat("done getting cancer IDs\n")
  
  respondToSelectionDrill()
  
  
})

