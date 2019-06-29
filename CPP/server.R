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

CONFIG <- list(
  DEBUG = TRUE,
  DEFAULT.GENE = "AGL",
  AUTO.RUN = FALSE
)

if (!CONFIG$DEBUG) {
# comment out for debugging
  cat <- function(...){invisible()}
  print <- function(...){invisible()}
}


wait <- function() {
  cat("Press a key to continue...")
  scan(what = character(), n = 1)
}

catn <- function(...) {
  cat(..., '\n')
}

source("functions.R", local = TRUE)
source("setResults.R", local = TRUE)
source("progress.R", local = TRUE)

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
  source("server-cancerTypeSetup.R", local = TRUE)
  
  # disable drop downs on startup
  shinyjs::disable("filterDisease")
  shinyjs::disable("filterChem")
  shinyjs::disable("filterMutations")
  shinyjs::disable("filterGenes")
  shinyjs::disable("filterCancerTerms")
  
  # add logPanel if in debug mode
  if (CONFIG$DEBUG) {
    appendTab("headerNavBarPage", 
              tabPanel("Log", verbatimTextOutput("log"))  )
  }
  
  # set up welcome modal and auto run if specified
  toggleModal(session, "welcomeModal", toggle = "open")
  if (CONFIG$AUTO.RUN) {
    shinyjs::click("btnGeneSearch")
  }
  
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
  
  # shinyjs::runjs("
  #                if (navigator.userAgent.indexOf('Chrome') == -1) {
  #                   alert('For the best user experience, we recommend using the Google Chrome browser, available at: http://www.google.com/chrome/');
  #                }
  # ")
  
  
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
    shinyjs::removeClass(id = 'welcomeModal', class = 'hide')
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
          displayCancerSelectionSummary(cancerSelectionSummary$dat, NULL, NULL)
          
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

    
#################################################################
# HTML formatted strings for filters and cancer types
#   createFilterString uses diseaseSummary, etc, so keep
#   here
#################################################################

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
    
   
   createCancerString <- function(max_num = NULL) {
      cancerIDs <- cancerSelectionSummary$selected1
      if (is.null(cancerIDs)) {
        return(NULL)
      }
      m <- length(cancerIDs)
      if (!is.null(max_num)) {
          m <- min(max_num, length(cancerIDs))
      }
      
      i <- match(cancerIDs[1:m], cancerSelectionSummary$dat$MeshID)    
      cancers <- cancerSelectionSummary$dat$Term[i]
      
      if (length(cancerIDs) > m) {
        cancers[m] <- " ..."
      }
      
      cancers <- paste0(cancers, collapse = "; ")
      cancers
   }
   
    
   output$summaryHeader <- renderUI({
     if (is.null(selected$geneSymbol)) {
       return()
     }
     x <- paste0("Search for gene <b style='color:red'>", selected$geneSymbol,
                 "</b> found <b>",nrow(pmidList$pmids), "</b> articles.")
     
     cancers <- createCancerString(4)
     if (is.null(cancers)) {
          x <- gsub("found", "for any cancer type found", x)
          cancers <- "</br>Selected cancers: (none selected)"
     } else {
       x <- gsub("found", "for selected cancer types found", x)
       cancers <- paste0("</br><b>Selected cancers</b>: ", cancers)
     }
     
     
     # add cancer modal
     cancers <- paste0(cancers, " (<a href = '#' id = 'linkCancerTypeSetup' data-toggle='modal'
                 data-target='#cancerTypeSetupModal'>View/Change</a>) ")
     
     x <- paste0(x, cancers)
     
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
  # 2) get PMIDs based on Mesh selection (limited to current PMID list) (NO LONGER DONE)
  # 3) get PMIDs based on Chem selection (limited to current PMID list)
  # 4) get PMIDs based on Gene selection (limited to current PMID list)
  # 5) get intersection of (2) - (4) producing list of PMIDs matching search criteria
  # 6) get summaries for Mesh terms, Chemicals, and Genes
  ###################################################################################################    
    respondToSelectionDrill <- function() {
      
      disableTableClicks()
      
      cat("respondToSelectionDrill\n")
    
      resetReactive(diseaseSummary)
      
      # hide/clear articles
    
      if (is.null(input$geneInput)) {
        cat("NULL\n")
      }
      num <- 0
      p1 <- list(PMID=NULL); p2 <- list(PMID=NULL); p3 <- list(PMID=NULL)
      
      cat("getting connection...\n")
      con = dbConnect(MariaDB(), group = "CPP")
      cat("got connection\n")
      
      pmids <- pmidList$pmids_initial$PMID
      catn("initial pmids:")
      catn(pmids)
      
      cat("pmids = ", pmids, "\n")
      # get PMIDs for gene selection
      genes <- c(input$geneInput, geneSummary$selectedID)
    
      if (length(genes) > 1) {
        setProgressBarText("Retrieving Articles for Selected Genes, please wait...")
        p3 <- getPMIDs("PubGene", "GeneID", genes, con, pmids)
        pmids <- intersectIgnoreNULL(pmids, p3$PMID)
      }
      
      
      # get PMIDs for Chem selection
      if (!is.null(chemSummary$selectedID)) {
        setProgressBarText("Retrieving Articles for Selected Chemicals, please wait...")
        cat("Chem selection, getting PMIDS for: ", chemSummary$selectedID, "\n")
        p2 <- getPMIDs("PubChem", "meshID", chemSummary$selectedID, con, pmids)
        pmids <- intersectIgnoreNULL(pmids, p2$PMID)
      }
      
      # get PMIDs for Mutation selection
      if (!is.null(mutationSummary$selectedID)) {
        setProgressBarText("Retrieving Articles for Selected Mutations, please wait...")
        cat("\n\n==========================================\nMutation selection, getting PMIDS for: ", mutationSummary$selectedID, "\n")
        p2 <- getPMIDs("PubMut", "MutID", mutationSummary$selectedID, con, pmids)
        cat("\n\n======================================\n\nnumber of articles with selected mutation = ", nrow(p2))
        pmids <- intersectIgnoreNULL(pmids, p2$PMID)
      }
      
      # get PMIDs for Cancer Term Selection
      if (!is.null(cancerTermSummary$selectedID)) {
        setProgressBarText("Retrieving Articles for Selected CancerTerms, please wait...")
        cat("\n\n==========================================\nCancer Term selection, getting PMIDS for: ", cancerTermSummary$selectedID, "\n")
        p2 <- getPMIDs("PubCancerTerms", "TermID", cancerTermSummary$selectedID, con, pmids)
        cat("\n\n======================================\n\nnumber of articles with selected mutation = ", nrow(p2))
        pmids <- intersectIgnoreNULL(pmids, p2$PMID)
      }
      
      if (length(pmids) == 0) {
        #dbDisconnect(con)
        pmidList$pmids <- data.frame(PMID = pmids)
        cat("NO RESULTS -- SHOULD BE CHECKED!")
        # don't stop here because this is now possible
        # if user updates cancer types but keeps filters
        
        pmids <- "NULL"
      }
      
      cat("updating summaries...\n")
      
      catn("get mesh summary...\n")
      #getSummaries("Pharmacological Substances", con, getChemSummaryByPMIDs, pmids, session, paSummary, pa = TRUE)

      getSummaries("Cancer Types", con, getMeshSummaryByPMIDs, pmids, session, diseaseSummary, "filterDisease")
      cat("we got disease summaries")

      # TO DO: make optional
      
    
      
      if (!is.null(cancerSelectionSummary$selected1)) {
          meshIDs <- c(cancerSelectionSummary$selected1, cancerSelectionSummary$selected2)
          m <- match(meshIDs, diseaseSummary$dat$MeshID)
          diseaseSummary$dat <- diseaseSummary$dat[m,]
      }
        
        
            
      #ids <- diseaseSummary$selectedID
      #terms <- diseaseSummary$selectedTerm
    
      # skip Neoplasms by Site and Neoplasms
      if (nrow(diseaseSummary$dat) > 0) {
        #skipIDs <- c('D009370', 'D009371', 'D009369')
        skipIDs <- c('D009370', 'D009371')  # keep Neoplasms
        diseaseSummary$dat <- diseaseSummary$dat [!diseaseSummary$dat$MeshID %in% skipIDs,]
      }
      getSummaries("Related Chemicals", con, getChemSummaryByPMIDs, pmids, session, chemSummary, "filterChem", pa = TRUE)
      getSummaries("Related Mutations", con, getMutationSummaryByPMIDs, pmids, session, mutationSummary, "filterMutations")
      getSummaries("Related Cancer Terms", con, getCancerTermSummaryByPMIDs, pmids, session, cancerTermSummary, "filterCancerTerms")
  
      # update geneSummary
      setProgressBarText("Retrieving Related Genes, please wait...")
      catn("genes = ", genes)
      catn('input = ', selected$geneID)
      tmp <- getGeneSummaryByPMIDs(pmids, con)
      catn("tmp = ")
      print(head(tmp))
      
      m <- match(selected$geneSymbol, tmp$Symbol)
      geneSummary$dat <- tmp[-m,]
      
      setGeneResults(session, geneSummary$dat, geneSummary)
      #setResults(session, geneSummary$dat, geneSummary, "filterGenes")
  
      dbDisconnect(con)
      
      #update PMIDs
      if (pmids == 'NULL') {
        pmids <- NULL
      }
      pmidList$pmids <- data.frame(PMID = pmids)
      
      if (is.null(pmidList$pmids_initial)) {
        pmidList$pmids_initial <- pmidList$pmids
      }
      
      # select current tab to update graphs on current page
      updateCurrentStackedGraph()
      enableTableClicks()
      
    }
   
    output$test <- renderText({
        HTML("<h2> how are you? </h2>")
    })
    
    # gets cancer types following initial gene selection
    getCancerTypes <- function() {
      
      if (is.null(input$geneInput)) {
        return()
      }
      
      # get PMIDs for gene selection
      genes <- c(input$geneInput)
      setProgressBarText("Retrieving Cancer Types for the selected Gene, please wait...")
      
      # get PMIDs for selected gene
      con = dbConnect(MariaDB(), group = "CPP")
      p3 <- getPMIDs("PubGene", "GeneID", genes, con, NULL)
      pmids <- p3$PMID
      
      if (length(pmids) == 0) {
        dbDisconnect(con)
        cat("NO RESULTS -- SHOULD BE CHECKED!")
        return()
      }
      
      # get Mesh IDs
      mesh <- getMeshSummaryByPMIDs(pmids, con)
      
      # get Tree info
      qry <- paste0("select MeshID, TreeID from MeshTerms where MeshID IN ",
                    cleanseList(mesh$MeshID),
                    " AND MeshTerms.TreeID LIKE \"C04.%\";"
      )
      
      
      cancerSelectionSummary$tree_ids <- dbGetQuery(con, qry)
      
      dbDisconnect(con)
      
     
      
      
      # skip Neoplasms by Histologic types, Neoplasms by Site, and Neoplasms
      skipIDs <- c('D009370', 'D009371', 'D009369')
      skipIDs <- c('D009370', 'D009371')
      mesh <- mesh [!mesh$MeshID %in% skipIDs,]
      
      cancerSelectionSummary$dat <- mesh
      
      
      #update PMIDs
      pmidList$pmids <- data.frame(PMID = pmids)
      
      if (is.null(pmidList$pmids_initial)) {
        pmidList$pmids_initial <- pmidList$pmids
      }
      
    }

})
