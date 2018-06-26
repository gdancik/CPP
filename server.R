# Cancer Publication Portal

library(shiny)
library(DT) # requires development version for single row selection with datatables
library(DBI)
library(RMySQL)
library(ggplot2)

DEBUG <<- FALSE


if (!DEBUG) {
# comment out for debugging
  cat <- function(...){invisible()}
  print <- function(...){invisible()}
}

source("functions.R", local = TRUE)
source("setResults.R", local = TRUE)


# some genes have duplicate IDs...we should combine, for now, remove
library(dplyr)



shinyServer(function(input, output, session) {

  observe({
    output$log <- renderText(logFile$log)
    
  })
  
  output$shinyTitle <- renderText("Cancer Publication Portal")
  
  source("server-reactives.R", local = TRUE)
  source("server-articles.R", local = TRUE)
  source("sql_functions.R", local = TRUE)
  source("server-GeneTable.R", local = TRUE)
  source("server-updateTables.R", local = TRUE)
  source("server-tableClicks.R", local = TRUE)
  
  # disable drop downs on startup
  shinyjs::disable("filterDisease")
  shinyjs::disable("filterChem")
  shinyjs::disable("filterMutations")
  shinyjs::disable("filterGenes")
  
  
  # set home page results to NULL (otherwise you will see spinner)
  output$cancerSummaryTable <- renderDataTable(NULL)
  output$cancerGraph <- renderPlot(NULL)
  
  toggleMenus <-function(show) {
    
    f <- shinyjs::hide
    if (show) {
      f <- shinyjs::show
    }
    
    f("headerNavBarPage")
    f("tabSetDisease")
    f("tabSetChemicals")
    f("tabSetGenes")
    f("filterDisease")
    f("filterChem")
    f("filterMutations")
    f("filterDisease")
    f("filterGenes")
    
  }
    
  toggleMenus(FALSE)
  
  
    # on initial search
    observeEvent(
      {input$btnGeneSearch
      input$rbDiseaseLimits},{

        if (is.null(input$geneInput) | input$geneInput == "") return()
        
        resetReactiveValues()
        
        selected$geneSymbol <- GeneTable$SYMBOL[GeneTable$GeneID == input$geneInput]
        
        respondToSelectionDrill()
        toggleMenus(TRUE)
        
    })
    
  
    # returns intersection of x and y but if x is NULL return y
    intersectIgnoreNULL <- function(x,y) {
      if (is.null(x)) {
        return(y)
      }
      intersect(x,y)
    }
    
    
  ###################################################################################################    
  # This is the main function that drives db queries, and works as follows:
  # 1) if cancer-specific, get list of cancer-specific PMIDS if not already set
  # 2) get PMIDs based on Mesh selection (limited to current PMID list)
  # 3) get PMIDs based on Chem selection (limited to current PMID list)
  # 4) get PMIDs based on Gene selection (limited to current PMID list)
  # 5) get intersection of (2) - (4) producing list of PMIDs matching search criteria
  # 6) get summaries for Mesh terms, Chemicals, and Genes
  ###################################################################################################    
    respondToSelectionDrill <- function() {
      cat("respondToSelectionDrill\n")
      
      if (is.null(input$geneInput)) {
        cat("NULL\n")
      }
      num <- 0
      p1 <- list(PMID=NULL); p2 <- list(PMID=NULL); p3 <- list(PMID=NULL)
      
      cat("getting connection...\n")
      con = dbConnect(MySQL(), group = "CPP")
      
      cat("got connection\n")
      
      # get cancer PMIDs if specified, restrict to gene #
      if (is.null(pmidList$pmids_initial) & input$rbDiseaseLimits == "cancer") {
        cat("getting cancer IDs for ", input$geneInput, "\n")
        pmidList$pmids_initial = getCancerPMIDs(con, cleanse(input$geneInput))
      }
      
      
      cat("done getting cancer IDs\n")
      pmids <- pmidList$pmids_initial$PMID
      
      cat("pmids = ", pmids, "\n")
      # get PMIDs for gene selection
      genes <- c(input$geneInput, geneSummary$selectedID)
      
      
      shinyjs::html("bar-text", "Retrieving Articles for Selected Genes, please wait...")
      p3 <- getPMIDs("PubGene", "GeneID", genes, con, pmids)
      
      
      pmids <- intersectIgnoreNULL(pmids, p3$PMID)
      
      # get PMIDS for Mesh Selection
      if (!is.null(diseaseSummary$selectedID)) {
          shinyjs::html("bar-text", "Retrieving Articles for Selected Diseases, please wait...")
          cat("Disease selection, geting PMIDS for: ", diseaseSummary$selectedID, "\n")
          p1 <- getPMIDs("PubMesh", "MeshID", diseaseSummary$selectedID, con, pmids)
          pmids <- intersectIgnoreNULL(pmids, p1$PMID)
      }
      
      # get PMIDs for Chem selection
      if (!is.null(chemSummary$selectedID)) {
        shinyjs::html("bar-text", "Retrieving Articles for Selected Chemicals, please wait...")
        cat("Chem selection, getting PMIDS for: ", chemSummary$selectedID, "\n")
        p2 <- getPMIDs("PubChem", "meshID", chemSummary$selectedID, con, pmids)
        pmids <- intersectIgnoreNULL(pmids, p2$PMID)
      }
      
      # get PMIDs for Mutation selection
      if (!is.null(mutationSummary$selectedID)) {
        shinyjs::html("bar-text", "Retrieving Articles for Selected Mutations, please wait...")
        cat("\n\n==========================================\nMutation selection, getting PMIDS for: ", mutationSummary$selectedID, "\n")
        p2 <- getPMIDs("PubMut", "MutID", mutationSummary$selectedID, con, pmids)
        cat("\n\n======================================\n\nnumber of articles with selected mutation = ", nrow(p2))
        pmids <- intersectIgnoreNULL(pmids, p2$PMID)
      }
      
      
      if (length(pmids) == 0) {
        dbDisconnect(con)
        cat("NO RESULTS -- SHOULD BE CHECKED!")
        return()
      }
      
      cat("updating summaries...\n")
      
      getSummaries("Pharmacological Substances", con, getChemSummaryByPMIDs, pmids, session, paSummary, pa = TRUE)
      getSummaries("Related Diseases", con, getMeshSummaryByPMIDs, pmids, session, diseaseSummary, "filterDisease")
      getSummaries("Related Chemicals", con, getChemSummaryByPMIDs, pmids, session, chemSummary, "filterChem")
      getSummaries("Related Mutations", con, getMutationSummaryByPMIDs, pmids, session, mutationSummary, "filterMutations")

      
      # update geneSummary
      shinyjs::html("bar-text", "Retrieving Related Genes, please wait...")
      geneSummary$dat <- getGeneSummaryByPMIDs(pmids, con)
      setGeneResults(session, geneSummary$dat, geneSummary)
      #setResults(session, geneSummary$dat, geneSummary, "filterGenes")
  
      dbDisconnect(con)
      
      #update PMIDs
      pmidList$pmids <- data.frame(PMID = pmids)
      
      if (is.null(pmidList$pmids_initial)) {
        pmidList$pmids_initial <- pmidList$pmids
      }
      
    }
    
    
    # re-query when selection changes
    observe( {
      
      selections <- list(diseaseSummary$selectedID, geneSummary$selectedID, chemSummary$selectedID, mutationSummary$selectedID)
      
      if (all(sapply(selections, is.null))) {
        return()
      }
      
      
      respondToSelectionDrill()
      

    })
    
    
    output$test <- renderText({
        HTML("<h2> how are you? </h2>")
    })

})
