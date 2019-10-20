# server-reactives.R


logFile <- reactiveValues(log = NULL)
updateLog <- function(logFile, ...) {
  l = list(...)
  isolate(logFile$log <- paste0(logFile$log, paste0(unlist(l), collapse = ""), sep = "\n"))
}

# basic reactive structure for storing disease, chem, etc results tables
# set not.refreshed to TRUE when selectedID is outdated (i.e., because of
# change in table selection)
createReactiveTable <- function(...) {
  reactiveValues(dat = NULL, selectedID = NULL,
                 selectedTerm = NULL, graphData = NULL, refreshPending = NULL, ...)
}


# reactives to deal with results tables
diseaseSummary <- createReactiveTable(initialized = NULL)
chemSummary <- createReactiveTable()
#paSummary <- createReactiveTable()
mutationSummary <- createReactiveTable()
geneSummary <- createReactiveTable()
multiGeneSummary <- reactiveValues(dat = NULL)
cancerTermSummary <- createReactiveTable()


savedFilterValues <- reactiveValues(filterGenesType = "any",filterChemType = "any",
                                    filterMutationsType = "any", filterCancerTermsType = "any")
                                    
# reactive holding original cancer summary and corresponding tree IDs 
cancerSelectionSummary <- reactiveValues(dat = NULL, tree_ids = NULL, ids2 = NULL, highlightPending = NULL,
                                         selected1 = NULL, selected2 = NULL)



# reactive for currently selected gene symbol
selected <- reactiveValues(geneID = NULL, geneSymbol = NULL)

# returns the number of selected genes
selectedGeneLength <- reactive({
  return(length(selected$geneID))
})

selectedGeneName <- reactive({
  
  print("getting name..")
  n <- length(selected$geneSymbol)
  cat('n =', n, '\n')
  
  if (n <= 5) {
    return(paste0(selected$geneSymbol, collapse = ", "))
  }
  
  genes <- paste0(selected$geneSymbol[1:3], collapse = ", ")
  return(paste0(genes, " and ", n-3, " more genes"))
})

# reactive for pmids:
#   pmids - current pmids to be displayed
#   pmids_initial - pmids on initial search used to limit further queries
pmidList <- reactiveValues(pmids = NULL, pmids_initial = NULL)

# resets reactive values in 'x' to NULL
resetReactive <- function(x, val = NULL) {
  for (n in names(x)) {
    x[[n]] <- val
  }
}

resetReactiveValues <- function() {
  
  cat("RESETTING REACTIVES...\n")
  
  logFile$log <- NULL
  resetReactive(savedFilterValues, "any")
  resetReactive(selected)
  resetReactive(diseaseSummary)
  resetReactive(pmidList)
  resetReactive(geneSummary)
  resetReactive(chemSummary)
  resetReactive(mutationSummary)
  resetReactive(cancerTermSummary)
  resetReactive(multiGeneSummary)
  resetReactive(cancerSelectionSummary)
  
  #resetReactive(paSummary)
}

resetSummaryData <-function() {
# reactives to deal with results tables
  diseaseSummary$dat <- NULL
  chemSummary$dat <- NULL
  mutationSummary$dat <- NULL
  geneSummary$dat <- NULL
  cancerTermSummary$dat <- NULL
  multiGeneSummary$dat <- NULL
}
