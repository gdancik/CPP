##########################################################
# Handle a New Search
##########################################################
observeEvent(input$btnNewSearch,{
  
  selectedSingle = selectedMulti = ""
  
  if (length(selected$geneID) == 1) {
    selectedSingle = selected$geneID
  } else {
    selectedMulti <- paste0(selected$geneSymbol, collapse = '\n')
  }
  
  updateSelectizeInput(session, "geneInput", choices = geneIDs, selected = selectedSingle, server = TRUE)
  

  
  updateTextAreaInput(session, "multiGeneInput", value = selectedMulti)

  shinyjs::disable("btnGeneSearch")
  shinyjs::removeClass(id = 'welcomeModal', class = 'hide')
})


geneID_to_symbol <- function(id) {
  GeneTable$SYMBOL[GeneTable$GeneID %in% id]      
}

##########################################################
# Handle Single Gene Search
##########################################################

observeEvent(input$geneInput, {
  if (is.null(input$geneInput) | input$geneInput == "") {
    shinyjs::disable("btnGeneSearch")
    return()
  }
  
  if (selectedGeneLength() > 1) {
    shinyjs::enable("btnGeneSearch")
    return()
  }
  
  symbol <- geneID_to_symbol(input$geneInput)
  msg <- paste0("prev: ", symbol, " curr: ", selected$geneSymbol )
  
  #shinyjs::alert(msg)
  if (!is.null(selected$geneSymbol) && symbol == selected$geneSymbol) {
    shinyjs::disable("btnGeneSearch")
  } 
  else {
    shinyjs::enable("btnGeneSearch")
  }

})


# on single gene search
observeEvent(input$btnGeneSearch,{
  
  cat("clicked btnGeneSearch, geneInput = ", input$geneInput, "\n")
  
  if (CONFIG$AUTO.RUN) {
    cat("updating..")
    
    #updateSelectizeInput(session, "geneInput", choices = geneIDs, selected =10 , server = TRUE)
    
    CONFIG$AUTO.RUN <- FALSE
    toggleModal(session, "welcomeModal", toggle = "close")
    return()
  }
  
  # do nothing if no valid gene is selected
  if (is.null(input$geneInput) | input$geneInput == "") return()
  if (!is.null(selected$geneSymbol) && 
      input$geneInput == selected$geneSymbol) {
    return()
  }
  
  cancerSummaryByGenes(input$geneInput)
  
  shinyjs::addClass(id = "btnCancelCancerType", class = "cancel")
  
})


# on multi gene search
observeEvent(input$btnMultiGeneSearch,{

  cat("clicked btnMultiGeneSearch, genes = ", input$multiGeneInput, "\n")

  genes <- getGenesFromMultiGeneInput()

  n <- length(genes$ids)
  
  if (n == 0) {
    return()
  } else if (n > 500) {
    msg <- paste0('Your list contains ', n, ' valid genes.\n')
    msg <- paste0(msg, '\nPlease restrict your list to no more than 500 genes.')
    shinyjs::alert(msg)
    return()
  } 
  
  cancerSummaryByGenes(genes$ids)
  shinyjs::addClass(id = "btnCancelCancerType", class = "cancel")

})


# process and display cancerSummary for specified geneIDs
cancerSummaryByGenes <- function(geneID) {

  reset("cancerType")
  resetReactiveValues()

  isolate(selected$geneID <- geneID)  
  isolate(selected$geneSymbol <- geneID_to_symbol(geneID))
  
  #wait()
  shinyjs::removeClass('welcomeModalProgress', 'hide')
  validSearch <- getCancerTypes()
  
  if (!validSearch) {
    shinyjs::addClass('welcomeModalProgress', 'hide')
    resetReactiveValues()
    shinyjs::addClass(id = "btnWelcomeCancel", class = "cancel")
    return()
  }
  
  
  toggleMenus(TRUE)

  shinyjs::addClass('welcomeModalProgress', 'hide')
  toggleModal(session, "welcomeModal", toggle = "close")      
  toggleModal(session, "cancerTypeSetupModal", toggle = "open")   

  output$cancerTypeSummaryHeader <- renderUI({
    HTML(
      paste0("<p style = 'font-size:1.1em'>Search for gene <b style='color:red'>", selectedGeneName(),
           "</b> found <b>",isolate(nrow(pmidList$pmids_initial)), "</b> articles.</p>")
    )
  })

  displayCancerSelectionSummary(cancerSelectionSummary$dat, NULL, NULL)

}

##########################################################
# Handle Multiple Gene Search
##########################################################

observeEvent(input$multiGeneInput, {
  delay(500, validateGenes())
  
}, ignoreInit = TRUE, ignoreNULL = TRUE)


# returns list containing
#   invalid gene symbols and valid ids, or NULL if nothing is entered
getGenesFromMultiGeneInput <- function() {
  
  genes <- strsplit(trimws(input$multiGeneInput), split = '\\s+')[[1]]
  
  if (length(genes) == 0) {
    return(NULL)
  }
  
  genes_lower <- tolower(genes)
  m <- match(genes_lower, GeneTable$lower)
  invalid <- genes[is.na(m)] 
  ids <- GeneTable$GeneID[m[!is.na(m)]]
  
  list(ids = ids, invalid = invalid)

}

validateGenes <- reactive({
  
  genes <- getGenesFromMultiGeneInput() 
  
  if (is.null(genes)) {
    shinyjs::disable('btnMultiGeneSearch')
    shinyjs::hide('invalidGeneOutput')
    output$multiInvalidGeneMsg <- renderUI({})
    return(NULL)
  }
  
  if (length(genes$invalid) > 0) {
    shinyjs::show('invalidGeneOutput')
    updateTextAreaInput(session, 'invalidGeneOutput', value = paste(genes$invalid, collapse = '\n'))
    js$setReadOnly('invalidGeneOutput')
    output$multiInvalidGeneMsg <- renderUI({
      HTML("<span style = 'color:red'> Invalid genes detected</span>")
    })
  } else {
    output$multiInvalidGeneMsg <- renderUI({})
    shinyjs::hide('invalidGeneOutput')
  }
  
 
  if (length(genes$ids) == 0 || setequal(genes$ids, selected$geneID)) {
      shinyjs::disable('btnMultiGeneSearch')
  } else {
      shinyjs::enable('btnMultiGeneSearch')
  }
  
})
