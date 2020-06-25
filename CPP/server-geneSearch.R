##########################################################
# Handle a New Search
##########################################################

geneID_to_symbol <- function(id) {
  GeneTable$SYMBOL[GeneTable$GeneID %in% id]      
}

##########################################################
# Handle Single Gene Search
##########################################################

observeEvent(input$geneInput, {
  catn('observeEvent geneInput...')
  if (is.null(input$geneInput) | input$geneInput == "") {
    catn("disable...")
    shinyjs::disable("btnGeneSearch")
    catn("done...")
    return()
  }
  
  if (selectedGeneLength() > 1) {
    catn("enable...")
    shinyjs::enable("btnGeneSearch")
    catn("done...")
    return()
  }
  
  catn('get symbol and msg...')
  symbol <- geneID_to_symbol(input$geneInput)
  msg <- paste0("prev: ", symbol, " curr: ", selected$geneSymbol )
  catn('done')
  
  #shinyjs::alert(msg)
  if (!is.null(selected$geneSymbol) && symbol == selected$geneSymbol) {
    catn('disable...')
    shinyjs::disable("btnGeneSearch")
    catn('done')
  } 
  else {
    catn('enable btnGeneSearch...')
    shinyjs::enable("btnGeneSearch")
    catn('done...')
  }
  
}, ignoreInit = TRUE)


# on single gene search
observeEvent(input$btnGeneSearch,{
  cat("clicked btnGeneSearch, geneInput = ", input$geneInput, "\n")
  
  # do nothing if no valid gene is selected
  if (is.null(input$geneInput) | input$geneInput == "") return()
  if (!is.null(selected$geneSymbol) && 
      input$geneInput == selected$geneSymbol) {
    return()
  }
  
  updateTextAreaInput(session, "multiGeneInput", value = "")
  
  commonGeneSearch(input$geneInput)
  
  #cancerSummaryByGenes(input$geneInput)
  
  #shinyjs::addClass(id = "btnCancelCancerType", class = "cancel")
  #shinyjs::enable('btnSelectCancerType')
  shinyjs::disable('btnGeneSearch')
  
  
}, ignoreInit = TRUE)


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
  
  commonGeneSearch(genes$ids)
  updateSelectizeInput(session, "geneInput", choices = geneIDs, 
                                            selected = "", server = TRUE)

}, ignoreInit = TRUE)


commonGeneSearch <- function(gid) {
  cancerSummaryByGenes(gid)
  
  shinyjs::addClass(id = "btnCancelCancerType", class = "cancel")
  shinyjs::enable('btnSelectCancerType')
}

# process and display cancerSummary for specified geneIDs
cancerSummaryByGenes <- function(geneID) {

  reset("cancerType")
  resetReactiveValues()
  NEW_SEARCH <<- TRUE

  isolate(selected$geneID <- geneID)  
  isolate(selected$geneSymbol <- geneID_to_symbol(geneID))
  
  shinyjs::removeClass('welcomeModalProgress', 'hide')
  
  validSearch <- getCancerTypes()
  
  
  if (is.null(validSearch)) {
    shinyjs::addClass('welcomeModalProgress', 'hide')
    resetReactiveValues()
    shinyjs::addClass(id = "btnWelcomeCancel", class = "cancel")
    return()
  }
  
  toggleMenus(TRUE)

  shinyjs::addClass('welcomeModalProgress', 'hide')
  
  updateNavbarPage(session, "headerNavBarPage", "Results")
  

  catn('set cancerTypeSummaryHeader...')
  output$cancerTypeSummaryHeader <- renderUI({
    HTML(
      paste0("<p style = 'font-size:1.1em'>Search for gene <b style='color:red'>", selectedGeneName(),
           "</b> found <b>",isolate(nrow(pmidList$pmids_initial)), "</b> articles.</p>")
    )
  })

  catn('displayCancerSelectionSummary')
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


observeEvent(input$multiGeneFile, {
  catn('observeEvent multiGeneFile...')
  if (is.null(input$multiGeneFile)) {
    return()
  }
  genes <- read.csv(input$multiGeneFile$datapath, nrows = 1000, header = FALSE, as.is = 1)
  if (ncol(genes) > 1) {
    shinyjs::alert('Invalid format: gene file should have a single column')
    return()
  }
  updateTextAreaInput(session, 'multiGeneInput', value = paste0(genes$V1, collapse = "\n"))
  
}, ignoreInit = TRUE)
