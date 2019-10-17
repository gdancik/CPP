##########################################################
# Handle a New Search
##########################################################
observeEvent(input$btnNewSearch,{
  updateSelectizeInput(session, "geneInput", choices = geneIDs, selected = selected$geneID, server = TRUE)
  updateSelectizeInput(session, "multiGeneInput", choices = geneIDs, selected = selected$geneID, server = TRUE)
  shinyjs::disable("btnGeneSearch")
  shinyjs::removeClass(id = 'welcomeModal', class = 'hide')
})


geneID_to_symbol <- function(id) {
  GeneTable$SYMBOL[GeneTable$GeneID == id]      
}

##########################################################
# Handle Single Gene Search
##########################################################

observeEvent(input$geneInput, {
  if (is.null(input$geneInput) | input$geneInput == "") {
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


# on gene search
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
  
  reset("cancerType")
  resetReactiveValues()
  
  selected$geneID <- input$geneInput  
  selected$geneSymbol <- geneID_to_symbol(input$geneInput)
  
  cat("get cancer types now...\n")
  #wait()
  shinyjs::removeClass('welcomeModalProgress', 'hide')
  getCancerTypes()
  toggleMenus(TRUE)
  
  shinyjs::addClass('welcomeModalProgress', 'hide')
  toggleModal(session, "welcomeModal", toggle = "close")      
  toggleModal(session, "cancerTypeSetupModal", toggle = "open")   
  
  
  
  output$cancerTypeSummaryHeader <- renderUI({
    HTML(
      paste0("<p style = 'font-size:1.1em'>Search for gene <b style='color:red'>", selected$geneSymbol,
             "</b> found <b>",isolate(nrow(pmidList$pmids_initial)), "</b> articles.</p>")
    )
  })
  
  
  displayCancerSelectionSummary(cancerSelectionSummary$dat, NULL, NULL)
  
})


##########################################################
# Handle Multiple Gene Search
##########################################################

observeEvent(input$multiGeneInput, {
  delay(500, validateGenes())
  
}, ignoreInit = TRUE, ignoreNULL = TRUE)


getGenesFromInput <- function() {
  strsplit(input$multiGeneInput, split = '\\s+')[[1]]
}

validateGenes <- reactive({
  genes <- getGenesFromInput() 
  if (length(genes) == 0) {
    shinyjs::disable('btnMultiGeneSearch')
    shinyjs::hide('invalidGeneOutput')
    output$multiGeneMsg <- renderUI({})
    return(NULL)
  }
  
  genes_lower <- tolower(genes)
  m <- match(genes_lower, GeneTable$lower)
  
  print(m)
  
  if (anyNA(m)) {
    shinyjs::show('invalidGeneOutput')
    updateTextAreaInput(session, 'invalidGeneOutput', value = paste(genes[is.na(m)], collapse = '\n'))
    js$setReadOnly('invalidGeneOutput')
    output$multiGeneMsg <- renderUI({
      HTML("<span style = 'color:red'> Invalid genes detected</span>")
    })
  } else {
    shinyjs::hide('invalidGeneOutput')
    output$multiGeneMsg <- renderUI({
      HTML("<span style = 'color:darkgreen'> All genes are valid</span>")
    })
  }
  
  if (all(is.na(m))) {
      shinyjs::disable('btnMultiGeneSearch')
  } else {
      shinyjs::enable('btnMultiGeneSearch')
  }
    
})

observeEvent(input$btnMultiGeneSearch, {
  
  genes <- getGenesFromInput() 
  genes_lower <- tolower(genes)
  m <- match(genes_lower, GeneTable$lower)
  
  ids <- GeneTable$GeneID[m]
  cat("Gene IDs: ", ids, "\n")
  
  con = dbConnect(MariaDB(), group = "CPP")
  t <- getGeneSummaryForSelectedGeneIDs(ids, con)
  dbDisconnect(con)
  
  missing <- genes[is.na(m)]
  
  if (length(missing) > 0) {
    d <- data.frame(0, missing,0)
    colnames(d) <- colnames(t)
    t<- rbind(t, d)
  }
  
  output$multiGeneSummaryTable <- DT::renderDataTable(datatable(t[,-1],rownames = FALSE,
                                                        options = list(paging = FALSE, scrollY = 300),
                                                        selection = "none"
                                                      ))
}) 


