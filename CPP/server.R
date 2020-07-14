# Cancer Publication Portal

lib.loc <- NULL
lib.loc <- "/Users/dancikg/RESEARCH/research_easternct/work/GenePubViewer/DCAST/Github/CPP/lib"


library(shiny, lib.loc = lib.loc)  # needs shiny_1.2.0

library(V8)
library(DT) # requires development version for single row selection with datatables
library(DBI)
library(RMariaDB)
library(ggplot2) # need development version for plotly (for horizontal bar)
library(rclipboard)

#install_version("plotly", version = "4.6.0", repos = "http://cran.us.r-project.org")
library(plotly) # need development version 
library(stringr)
library(shinycssloaders, lib.loc = lib.loc)


CONFIG <- list(
  DEBUG = FALSE,
  DEFAULT.GENE = "AGL"
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
source("tabResults.R", local = TRUE)

# some genes have duplicate IDs...we should combine, for now, remove
library(dplyr)

shinyServer(function(input, output, session) {

  hideTab('headerNavBarPage', 'Results')
  
  
  observe({
    catn('observe the logFile...')
    output$log <- renderText(logFile$log)
  })

  observeEvent(input$btnTest, {
    catn("click it", input$btnTest)
    #toggleModal(session, "testmodal", "open")
    #toggleModal(session, "cancerTypeSetupModal", "open")
    #showTab('headerNavBarPage', 'Results')
    #toggleModal(session, "filterModal", "open")
  })
  
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
  source("server-cancerTypeSetup.R", local = TRUE)
  source("server-geneSearch.R", local = TRUE)

  #clearStackedGraphs(FALSE)
  
  # disable drop downs on startup
  # shinyjs::disable("filterDisease")
  # shinyjs::disable("filterChem")
  # shinyjs::disable("filterMutations")
  # shinyjs::disable("filterGenes")
  # shinyjs::disable("filterCancerTerms")
  
  
  # add logPanel if in debug mode
  if (CONFIG$DEBUG) {
    appendTab("headerNavBarPage", 
              tabPanel("Log", verbatimTextOutput("log"))  )
  }
  
  
  # set up welcome modal and auto run if specified
#  toggleModal(session, "welcomeModal", toggle = "open")
  
  shinyjs::hide('invalidGeneOutput')
  
  # set home page results to NULL (otherwise you will see spinner)
  output$cancerSummaryTable <- renderDataTable(NULL)
  output$cancerGraph <- renderPlot(NULL)
  
#  lastTab <<- "Home"
  
  #updateSelectInput(session, 'testing', choices = GeneTable$SYMBOL)
  
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
   createFilterString <- function(newlines = FALSE) {
     l <- list("Cancer Types" = list(diseaseSummary, "any"),
               "Drugs" = list(chemSummary, savedFilterValues$filterChemType),
               Mutations = list(mutationSummary, savedFilterValues$filterMutationsType),
               CancerTerms = list(cancerTermSummary,savedFilterValues$filterCancerTermsType),
               "Additional Genes" = list(geneSummary, savedFilterValues$filterGenesType)
     )
               
     s <- sapply(l, function(x) !is.null(x[[1]]$selectedID))
     f <- names(which(s))
     filterString <- ""
     
     get_desc <- function(x, i, label) {
       x <- x[[i]]
       if (length(x[[1]][[label]]) == 1) {
         return(i)
       }
       paste0(i, "(", x[[2]], ")")
     }
     
     for (i in f) {
       label <- "selectedTerm"
       if (i == "Mutations") {
         label <- "selectedID"
       }
       if (filterString!= "") {
         if (newlines) {
           filterString = paste0(filterString, "\n")
         } else {
           filterString = paste0(filterString, "; ")
         }
       }
       filterString <- paste0(filterString, "<b style='font-style:italic'>", get_desc(l, i, label), "</b>: ", 
                              paste0(l[[i]][[1]][[label]], collapse = ", "))
     }
     
     
     #catn('filterString: ', filterString)
     #wait()
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
     x <- paste0("Search for gene <b style='color:red'>", selectedGeneName(),
                 "</b> found <b>",nrow(pmidList$pmids), "</b> articles.")
     
     if (selectedGeneLength() > 1) {
       showTab('MainPage', 'Selected Genes')
     } else {
       hideTab('MainPage', 'Selected Genes')
     }
     
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
                 data-target='#cancerTypeSetupModal'>view/change</a>) ")
     
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
                      data-target='#filterModal'>remove/options</a>): ")
        x <- paste0(x, filterString)
     } else {
       # x <- paste0(x, "</br>No filters selected  
       #             (<a href = '#' id = 'btnFilterOptions' data-toggle='modal'
       #             data-target='#filterModal2'>Filter options</a>): ")
     }
     
     x<- paste0("<span style='font-size:1.1em'>", x, "</span>")
                 
     HTML(x)
    })
    
   
   # if no results, update pmidList, return TRUE, and generate alert
   noResults <- function(x) {
     if (is.null(x) || length(x) == 0) {
       shinyjs::alert("No articles match your current filters. Modify your filters and search again")
       pmidList$pmids <- data.frame(PMID = x)
       return(TRUE)
     }
     return(FALSE)
   }
   
   shinyjs::onclick('linkViewMultiGene',{
     
     print("")
     print('click')
     print("")
     
     con = dbConnect(MariaDB(), group = "CPP")
     multiGeneSummary$dat <- getGeneSummaryForSelectedGeneIDs(selected$geneID, con, pmidList$pmids$PMID)
     dbDisconnect(con)  
     
     # add 0 results?
      
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
      
      cat("\n\nrespondToSelectionDrill\n")
  
      resetReactive(diseaseSummary)
      diseaseSummary$dat <- NULL
      
      clearStackedGraphs()
      resetSummaryData()
      
      # hide/clear articles
    
      num <- 0
      p1 <- list(PMID=NULL); p2 <- list(PMID=NULL); p3 <- list(PMID=NULL)
      
      cat("getting connection...\n")
      con = dbConnect(MariaDB(), group = "CPP")
      cat("got connection\n")
      
      pmids <- pmidList$pmids_initial$PMID
      
      if (is.null(pmids)) {
        stop("pmids should not be NULL in drill down")
      }
      
      catn("initial pmids:")
      catn(pmids)
      
      cat("pmids = ", pmids, "\n")
      # get PMIDs for gene selection
      
      catn("filter genes type: ", input$filterGenesType)
      catn("filter mut type: ", input$filterMutationsType)
      catn("filter chem type: ", input$filterChemType)
      catn("filter cancer terms type: ", input$filterCancerTermsType)
      
      genes <- c(geneSummary$selectedID)
      
      catn("in drill down, genes = ", genes)
    
      if (length(genes) > 0) {
        setProgressBarText("Retrieving Articles for Selected Genes, please wait...")
        p3 <- getPMIDs("PubGene", "GeneID", genes, con, pmids, savedFilterValues$filterGenesType)
        pmids <- intersectIgnoreNULL(pmids, p3$PMID)
        if (noResults(pmids)) {
          return()
        }
      }
      
      
      # get PMIDs for Mutation selection
      if (!is.null(mutationSummary$selectedID)) {
        setProgressBarText("Retrieving Articles for Selected Mutations, please wait...")
        cat("\n\n==========================================\nMutation selection, getting PMIDS for: ", mutationSummary$selectedID, "\n")
        p2 <- getPMIDs("PubMut", "MutID", mutationSummary$selectedID, con, pmids, savedFilterValues$filterMutationsType)
        cat("\n\n======================================\n\nnumber of articles with selected mutation = ", nrow(p2))
        pmids <- intersectIgnoreNULL(pmids, p2$PMID)
        if (noResults(pmids)) {
          return()
        }
      }
      
      
      # get PMIDs for Cancer Term Selection
      if (!is.null(cancerTermSummary$selectedID)) {
        setProgressBarText("Retrieving Articles for Selected CancerTerms, please wait...")
        cat("\n\n==========================================\nCancer Term selection, getting PMIDS for: ", cancerTermSummary$selectedID, "\n")
        p2 <- getPMIDs("PubCancerTerms", "TermID", cancerTermSummary$selectedID, con, pmids, savedFilterValues$filterCancerTermsType)
        cat("\n\n======================================\n\nnumber of articles with selected mutation = ", nrow(p2))
        pmids <- intersectIgnoreNULL(pmids, p2$PMID)
        if (noResults(pmids)) {
          return()
        }
      }
      
      # get PMIDs for Chem selection
      if (!is.null(chemSummary$selectedID)) {
        setProgressBarText("Retrieving Articles for Selected Chemicals, please wait...")
        cat("Chem selection, getting PMIDS for: ", chemSummary$selectedID, "\n")
        p2 <- getPMIDs("PubChem", "meshID", chemSummary$selectedID, con, pmids, savedFilterValues$filterChemType)
        pmids <- intersectIgnoreNULL(pmids, p2$PMID)
        if (noResults(pmids)) {
          return()
        }
      }
      
      if (length(pmids) == 0) {
        #dbDisconnect(con)
        pmidList$pmids <- data.frame(PMID = pmids)
        cat("NO RESULTS -- SHOULD BE CHECKED!")
        # don't stop here because this is now possible
        # if user updates cancer types but keeps filters
        return()
      }
      
      pmidList$pmids <- data.frame(PMID = pmids)
      
      dbDisconnect(con)
      
      #getSummaries("Cancer Types", con, getMeshSummaryByPMIDs, pmids, session, diseaseSummary, "filterDisease")
      #formatCancerSummaries()
      
      updateCancerTypesSummary()
      
      
      
      if (is.null(pmidList$pmids_initial)) {
        pmidList$pmids_initial <- pmidList$pmids
      }
      
      page <- isolate(input$MainPage)
      catn('page is: ', page)
      
      updateTabsetPanel(session, 'MainPage', 'Articles')
      updateTabsetPanel(session, 'MainPage', page)
      
    }
   
    output$test <- renderText({
        HTML("<h2> how are you? </h2>")
    })
    
    # gets cancer types following initial gene selection
    # but limit to 200000 PMIDs
    # return TRUE if successful or FALSE if exceeds limit or no results found
    getCancerTypes <- function(limit = 200000) {
      
      # get PMIDs for gene selection
      genes <- selected$geneID
      setProgressBarText("Retrieving Cancer Types for the selected Gene, please wait...")
      
      # get PMIDs for selected gene
      con = dbConnect(MariaDB(), group = "CPP")
      p3 <- getPMIDs("PubGene", "GeneID", genes, con, NULL)
      pmids <- p3$PMID
      
      n <- length(pmids)
        
      if (n == 0) {
        dbDisconnect(con)
        cat("NO RESULTS -- SHOULD BE CHECKED!")
        return(NULL)
      } else if (n > limit) {
        dbDisconnect(con)
        msg <- paste0('Limit exceeded: your gene list has >200,000 results.\n\n',
                      'Reduce the number of genes in your list and search again.')
        shinyjs::alert(msg)
        return(NULL)
      }
      
      # get Mesh IDs
      mesh <- getMeshSummaryByPMIDs(pmids, con)
      
      # get Tree info
      qry <- paste0("select MeshID, TreeID from MeshTerms where MeshID IN ",
                    cleanseList(mesh$MeshID),
                    " AND MeshTerms.TreeID LIKE \"C04.%\";"
      )
      
      
      cancerSelectionSummary$tree_ids <- dbGetQuery(con, qry)
      
      #tree_ids <- cancerSelectionSummary$tree_ids
      
      # skip Neoplasms by Histologic types, Neoplasms by Site, and Neoplasms
      skipIDs <- c('D009370', 'D009371', 'D009369')
      skipIDs <- c('D009370', 'D009371')
      
      mesh <- mesh [!mesh$MeshID %in% skipIDs,]
      
      cancerSelectionSummary$dat <- mesh
      
      getStatSummaries(con, cancerSelectionSummary, "MeshID", length(pmids), "MeshCounts")
      dbDisconnect(con)
      
      #update PMIDs
      pmidList$pmids <- data.frame(PMID = pmids)
      
      if (is.null(pmidList$pmids_initial)) {
        pmidList$pmids_initial <- pmidList$pmids
      }
      
    }

    
    # format cancer summaries by restricting to selected types and removing
    # Neoplasms by site, etc
    formatCancerSummaries <- function() {
      if (!is.null(cancerSelectionSummary$selected1)) {
        meshIDs <- unique(c(cancerSelectionSummary$selected1, cancerSelectionSummary$selected2))
        m <- match(meshIDs, diseaseSummary$dat$MeshID)
        m <- m[!is.na(m)]
        diseaseSummary$dat <- diseaseSummary$dat[m,]
      }
      
      # skip Neoplasms by Site and Neoplasms
      if (nrow(diseaseSummary$dat) > 0) {
        #skipIDs <- c('D009370', 'D009371', 'D009369')
        skipIDs <- c('D009370', 'D009371')  # keep Neoplasms (unknown)
        diseaseSummary$dat <- diseaseSummary$dat [!diseaseSummary$dat$MeshID %in% skipIDs,]
      }
    }
    
    observeEvent(input$headerNavBarPage, {
      catn("observing headerNavBarPage")
      if (input$headerNavBarPage == "Results" && 
          input$MainPage == "Cancer Types") {
          updateCancerTypesSummary()
      }
    })
    
    observeEvent(input$MainPage , {
      catn('observing Main Page...')
      cat('clicked on: ', input$MainPage, '\n')
      
      disableTableClicks()
      
      if (input$MainPage == "Cancer Types") {
        
        cat("updating...\n")
        updateCancerTypesSummary()
        
      } else if (input$MainPage == "Selected Genes" && length(selected$geneID) > 1) {
        
          updateSelectedGenesSummary()
        
      } else if (input$MainPage == "Cancer Terms") {
        
          updateCancerTermsSummary()
        
      } else if (input$MainPage == "Drugs") {
        
          updateChemicalSummary()
        
      } else if (input$MainPage == "Mutations") {
        
          updateMutationSummary()
        
      } else if (input$MainPage == "Additional Genes") {
        
          updateAdditionalGenesSummary()
        
      }
    
      enableTableClicks()
      
    }, ignoreInit = TRUE)

    

updateCancerTypesSummary <- function() {
  if (!is.null(diseaseSummary$dat)) {
    return()
  }
  
  pmids <- pmidList$pmids$PMID
  con = dbConnect(MariaDB(), group = "CPP")
  getSummaries("Cancer Types", con, getMeshSummaryByPMIDs, pmids, session, diseaseSummary, "filterDisease")
  formatCancerSummaries()
  getStatSummaries(con, diseaseSummary, "MeshID", length(pmids), "MeshCounts")
  dbDisconnect(con)
  
  #toggleStackedGraphButtons(graph = "btnGenerateGraphCancerTypes")
}
    

updateSelectedGenesSummary <- function() {
  if (!is.null(multiGeneSummary$dat)) {
    return()
  }
  
  pmids <- pmidList$pmids$PMID
  con = dbConnect(MariaDB(), group = "CPP")
  multiGeneSummary$dat <- getGeneSummaryForSelectedGeneIDs(selected$geneID, con, pmidList$pmids$PMID)
  dbDisconnect(con)
  toggleStackedGraphButtons(graph = "btnGenerateGraphMultiGene")
  
}

updateCancerTermsSummary <- function() {
  if (!is.null(cancerTermSummary$dat)) {
    return()
  }
  
  pmids <- pmidList$pmids$PMID
  con = dbConnect(MariaDB(), group = "CPP")
  getSummaries("Related Cancer Terms", con, getCancerTermSummaryByPMIDs, pmids, session, cancerTermSummary, "filterCancerTerms")

  getStatSummaries(con, cancerTermSummary, "TermID", length(pmids), "CancerTermCounts")
  
  dbDisconnect(con)
  toggleStackedGraphButtons(graph = "btnGenerateGraphCancerTerm")
  
}
    
updateChemicalSummary <- function() {
  if (!is.null(chemSummary$dat)) {
    return()
  }
  
  pmids <- pmidList$pmids$PMID
  con = dbConnect(MariaDB(), group = "CPP")
  getSummaries("Related Chemicals", con, getChemSummaryByPMIDs, pmids, session, chemSummary, "filterChem", pa = TRUE)
  getStatSummaries(con, chemSummary, "MeshID", length(pmids), "ChemCounts")
  dbDisconnect(con)
  toggleStackedGraphButtons(graph = "btnGenerateGraphChem")
}

updateMutationSummary <- function() {
  if (!is.null(mutationSummary$dat)) {
    return()
  }
  pmids <- pmidList$pmids$PMID
  con = dbConnect(MariaDB(), group = "CPP")
  getSummaries("Related Mutations", con, getMutationSummaryByPMIDs, pmids, session, mutationSummary, "filterMutations")
  getStatSummaries(con, mutationSummary, "MutID", length(pmids), "MutCounts")
  dbDisconnect(con)
  toggleStackedGraphButtons(graph = "btnGenerateGraphMut")
  
}
    
updateAdditionalGenesSummary <- function() {
  
  if (!is.null(geneSummary$dat)) {
    return()
  }
  
  pmids <- pmidList$pmids$PMID
  con = dbConnect(MariaDB(), group = "CPP")
  setProgressBarText("Summarizing Gene Frequencies, please wait...")
  tmp <- getGeneSummaryByPMIDs(pmids, con)
  m <- match(selected$geneSymbol, tmp$Symbol)
  geneSummary$dat <- tmp[-m[!is.na(m)],]
  
  getStatSummaries(con, geneSummary, "GeneID", length(pmids), "GeneCounts")
  dbDisconnect(con)
  toggleStackedGraphButtons(graph = "btnGenerateGraphGene")
}

})
