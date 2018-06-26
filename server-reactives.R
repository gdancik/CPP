# server-reactives.R


logFile <- reactiveValues(log = NULL)
updateLog <- function(logFile, ...) {
  l = list(...)
  isolate(logFile$log <- paste0(logFile$log, paste0(unlist(l), collapse = ""), sep = "\n"))
}


# basic reactive structure for storing disease, chem, etc results tables
createReactiveTable <- function() {
  reactiveValues(dat = NULL, selectedID = NULL,
                 selectedTerm = NULL, graphData = NULL)
}


# reactives to deal with results tables
diseaseSummary <- createReactiveTable()
chemSummary <- createReactiveTable()
paSummary <- createReactiveTable()
mutationSummary <- createReactiveTable()
geneSummary <- createReactiveTable()

# reactive for currently selected gene symbol
selected <- reactiveValues(geneSymbol = NULL)


# reactive for pmids:
#   pmids - current pmids to be displayed
#   pmids_initial - pmids on initial search used to limit further queries
pmidList <- reactiveValues(pmids = NULL, pmids_initial = NULL)


# resets reactive values in 'x' to NULL
resetReactive <- function(x) {
  for (n in names(x)) {
    x[[n]] <- NULL
  }
}

resetReactiveValues <- function() {
  
  logFile$log <- NULL
  selected$geneSymbol <- NULL
  
  resetReactive(diseaseSummary)
  resetReactive(pmidList)
  resetReactive(geneSummary)
  resetReactive(chemSummary)
  resetReactive(mutationSummary)
  resetReactive(paSummary)
}


