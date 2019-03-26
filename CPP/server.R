# Cancer Publication Portal

library(shiny)
library(DT) # requires development version for single row selection with datatables
library(DBI)
library(RMariaDB)
library(ggplot2) # need development version for plotly (for horizontal bar)
library(rclipboard)

#install_version("plotly", version = "4.6.0", repos = "http://cran.us.r-project.org")
library(plotly) # need development version 
library(stringr)
library(shinycssloaders)

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
  source("sql_functions_contingency.R", local = TRUE)
  source("server-GeneTable.R", local = TRUE)
  source("server-updateTables.R", local = TRUE)
  source("server-tableClicks.R", local = TRUE)
  source("stackedBarGraphs.R", local = TRUE)
  source("server-download.R", local = TRUE)
  source("server-filter.R", local = TRUE)
  source("server-graphSetup.R", local = TRUE)
  
  # disable drop downs on startup
  shinyjs::disable("filterDisease")
  shinyjs::disable("filterChem")
  shinyjs::disable("filterMutations")
  shinyjs::disable("filterGenes")
  shinyjs::disable("filterCancerTerms")
  toggleModal(session, "welcomeModal")
  
  # set home page results to NULL (otherwise you will see spinner)
  output$cancerSummaryTable <- renderDataTable(NULL)
  output$cancerGraph <- renderPlot(NULL)
  
#  lastTab <<- "Home"
  
  toggleMenus <-function(show) {
    
    f <- shinyjs::hide
    if (show) {
      f <- shinyjs::show
    }
    
    f("MainPage")
    #f("headerNavBarPage")
    #f("tabSetDisease")
    #f("tabSetChemicals")
    #f("tabSetGenes")
    #f("filterDisease")
    #f("filterChem")
    #f("filterMutations")
    #f("filterDisease")
    #f("filterGenes")
    
  }
    
  toggleMenus(FALSE)
  
  # Javascript to trigger analysis when modal is closed
  shinyjs::runjs("
        var numSearches = 0
        $('#welcomeModal').on('hidden.bs.modal', function () {
            numSearches += 1;
            Shiny.onInputChange('testInput', numSearches);
        });")
  
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
  
  observeEvent(input$btnNewSearch,{
    updateSelectizeInput(session, "geneInput", choices = geneIDs, selected = selected$geneID, server = TRUE)
    shinyjs::disable("btnGeneSearch")
  })
  
  observeEvent(input$btnGeneSearch,{

      if (is.null(input$geneInput) | input$geneInput == "") return()
    
      if (!is.null(selected$geneSymbol) && 
          input$geneInput == selected$geneSymbol) {
            return()
      }
    
      triggers$newSearch <- TRUE      
      toggleModal(session, "welcomeModal", toggle = "close")
        
        
  })
  
    # on initial search
    observeEvent(
      {input$testInput
      #input$rbDiseaseLimits
        },{
        if (!triggers$newSearch) {
          if (is.null(selected$geneSymbol)) {
            shinyjs::alert("Please select a gene from the drop down menu to start")
            #toggleModal(session, "welcomeModal")
          }
          return()
        }
        resetReactiveValues()
      
        selected$geneID <- input$geneInput  
        selected$geneSymbol <- geneID_to_symbol(input$geneInput)
        respondToSelectionDrill()
        toggleMenus(TRUE)
        triggers$newSearch <- FALSE
        
    })
    
    geneID_to_symbol <- function(id) {
      GeneTable$SYMBOL[GeneTable$GeneID == id]      
    }
  
    # returns intersection of x and y but if x is NULL return y
    intersectIgnoreNULL <- function(x,y) {
      if (is.null(x)) {
        return(y)
      }
      intersect(x,y)
    }
    
   # creates HTML formatted string of currently selected filters
   createFilterString <- function() {
     l <- list("Cancer Types" = diseaseSummary,
               "Drugs" = chemSummary,
               Mutations = mutationSummary,
               CancerTerms = cancerTermSummary,
               "Additional Genes" = geneSummary)
     s <- sapply(l, function(x) !is.null(x$selectedID))
     f <- names(which(s))
     filterString <- ""
     for (i in f) {
       label <- "selectedTerm"
       if (i == "Mutations") {
         label <- "selectedID"
       }
       if (filterString!= "") {
         filterString = paste0(filterString, "; ")
       }
       filterString <- paste0(filterString, "<b style='font-style:italic'>", i, "</b>: ", paste0(l[[i]][[label]], collapse = ", "))
     }
     return(filterString)
   }
    
    
   output$summaryHeader <- renderUI({
     if (is.null(selected$geneSymbol)) {
       return()
     }
     x <- paste0("Search for gene <b style='color:red'>", selected$geneSymbol,
                 "</b> found <b>",nrow(pmidList$pmids), "</b> articles.")

     l <- list("Cancer Types" = diseaseSummary,
               "Drugs" = chemSummary,
               Mutations = mutationSummary,
               CancerTerms = cancerTermSummary,
               "Additional Genes" = geneSummary)
     s <- sapply(l, function(x) !is.null(x$selectedID))
     filterString <- createFilterString()
     
     if (filterString != "") {
       x <- gsub("found", "with additional filters found", x)
       x <- paste0(x, "</br>Current filters 
                   (<a href = '#' id = 'btnRemoveFilters' data-toggle='modal'
                      data-target='#filterModal'>Remove</a>): ")
        x <- paste0(x, filterString)
     }
     
     
     x<- paste0("<span style='font-size:1.1em'>", x, "</span>")
                 
     HTML(x)
    })
    
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
      
      # hide/clear articles
    
      if (is.null(input$geneInput)) {
        cat("NULL\n")
      }
      num <- 0
      p1 <- list(PMID=NULL); p2 <- list(PMID=NULL); p3 <- list(PMID=NULL)
      
      cat("getting connection...\n")
      con = dbConnect(MariaDB(), group = "CPP")
      
      cat("got connection\n")
      
      # get cancer PMIDs if specified, restrict to gene #
      #if (is.null(pmidList$pmids_initial) & input$rbDiseaseLimits == "cancer") {
      #  cat("getting cancer IDs for ", input$geneInput, "\n")
      #  pmidList$pmids_initial = getCancerPMIDs(con, cleanse(input$geneInput))
      #}
      
      
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
          cat("Drill Down Disease selection, geting PMIDS for: ", diseaseSummary$selectedID, "\n")
          #scan(what = character())
          
          # look at selected MeshIDs one at a time, because we need to 
          # consider child mesh IDs for each
          
          for (meshID in diseaseSummary$selectedID) {
              #cat("MeshID is: ", meshID, "....enter to continue\n")
              #scan(what = character())
              p1 <- getPMIDs("PubMesh", "MeshID", 
                         getChildMeshIDs(con, meshID), 
                         con, pmids, ids.AND = FALSE)
              pmids <- intersectIgnoreNULL(pmids, p1$PMID)
          }
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
      
      # get PMIDs for Cancer Term Selection
      if (!is.null(cancerTermSummary$selectedID)) {
        shinyjs::html("bar-text", "Retrieving Articles for Selected CancerTerms, please wait...")
        cat("\n\n==========================================\nCancer Term selection, getting PMIDS for: ", cancerTermSummary$selectedID, "\n")
        p2 <- getPMIDs("PubCancerTerms", "TermID", cancerTermSummary$selectedID, con, pmids)
        cat("\n\n======================================\n\nnumber of articles with selected mutation = ", nrow(p2))
        pmids <- intersectIgnoreNULL(pmids, p2$PMID)
      }
      
      if (length(pmids) == 0) {
        dbDisconnect(con)
        cat("NO RESULTS -- SHOULD BE CHECKED!")
        return()
      }
      
      cat("updating summaries...\n")
      
      #getSummaries("Pharmacological Substances", con, getChemSummaryByPMIDs, pmids, session, paSummary, pa = TRUE)
      getSummaries("Related Diseases", con, getMeshSummaryByPMIDs, pmids, session, diseaseSummary, "filterDisease")
      ids <- diseaseSummary$selectedID
      terms <- diseaseSummary$selectedTerm
      getSummaries("Related Chemicals", con, getChemSummaryByPMIDs, pmids, session, chemSummary, "filterChem", pa = TRUE)
      getSummaries("Related Mutations", con, getMutationSummaryByPMIDs, pmids, session, mutationSummary, "filterMutations")
      getSummaries("Related Cancer Terms", con, getCancerTermSummaryByPMIDs, pmids, session, cancerTermSummary, "filterCancerTerms")
  
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
    # GD: why do we need this?
"    observe( {
      
      selections <- list(diseaseSummary$selectedID, geneSummary$selectedID, chemSummary$selectedID, 
                         mutationSummary$selectedID, cancerTermSummary$selectedID)
      
      if (all(sapply(selections, is.null))) {
        return()
      }
      
      respondToSelectionDrill()
      

    })"
    
    
    output$test <- renderText({
        HTML("<h2> how are you? </h2>")
    })

})
