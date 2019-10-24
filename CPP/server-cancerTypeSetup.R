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
  
  reset('cancerTypeFile')
  
})

# ## update btnSelectCancerType based on current selection
observe({
  label <- "Retrieve summaries for selected cancer types"
  if (is.null(input$cancerType)) {
    label <- "Retrieve summaries for all cancer types"
    catn('hide')
    shinyjs::hide('saveCancerTypes')
  } else {
    shinyjs::show('saveCancerTypes')
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



observeEvent(input$cancerTypeFile, {
  cat("look at file")
  if (is.null(input$cancerTypeFile)) {
    return()
  }
  types <- read.csv(input$cancerTypeFile$datapath, nrows = 1000, header = FALSE, as.is = 1)
  if (ncol(types) > 2) {
    shinyjs::alert('Invalid format: gene file should have Mesh IDs in first column')
    return()
  }
  
  choices <- cancerSelectionChoices()
  
  if (!any(types$V1 %in% choices)) {
    shinyjs::alert('File does not contain any cancer types for current selection')
  }
  
  updateSelectInput(session, "cancerType", choices = choices, 
                    selected = types$V1)
  
  #updateTextAreaInput(session, 'multiGeneInput', value = paste0(genes$V1, collapse = "\n"))
  
})




