# server-reactives.R

diseaseSummary <- reactiveValues(dat = NULL, uniqueDat = NULL, selectedID = NULL, 
                                 selectedTerm = NULL, graphData = NULL)
chemSummary <- reactiveValues(dat = NULL, uniqueDat = NULL, selectedID = NULL, 
                              selectedTerm = NULL)

paSummary <- reactiveValues(dat = NULL, uniqueDat = NULL, selectedID = NULL, 
                              selectedTerm = NULL)

geneSummary <- reactiveValues(dat = NULL, selectedID = NULL, selectedTerm = NULL)

selected <- reactiveValues(geneSymbol = NULL)

# pmids is current pmids that will be displayed; pmids_initial is pmids on initial search and is used to limit 
# subsequent searches
pmidList <- reactiveValues(pmids = NULL, pmids_initial = NULL)

lastTab <- reactiveValues(tab = NULL)

resetReactiveValues <- function() {
  diseaseSummary$dat <- NULL
  diseaseSummary$uniqueDat <- NULL
  diseaseSummary$selectedID <- NULL 
  diseaseSummary$selectedTerm <- NULL
  diseaseSummary$graphData <- NULL
  pmidList$pmids <- NULL
  pmidList$pmids_initial <- NULL
  geneSummary$dat <- NULL
  geneSummary$selectedID <- NULL
  geneSummary$selectedTerm <- NULL
  chemSummary$dat <- NULL
  chemSummary$selectedID <- NULL
  chemSummary$selectedTerm <- NULL
  
  paSummary$dat <- NULL
  paSummary$selectedID <- NULL
  paSummary$selectedTerm <- NULL
  
  selected$geneSymbol <- NULL
  
}


clearSelectedGene <- function() {
  geneSummary$selectedID <- NULL
  geneSummary$selectedTerm <- NULL
}

clearSelectedDisease <- function() {
  diseaseSummary$selectedID <- NULL
  diseaseSummary$selectedTerm <- NULL
  diseaseSummary$graphData <- NULL
}

clearSelectedChem <- function() {
  chemSummary$selectedID <- NULL
  chemSummary$selectedTerm <- NULL
}

clearSelectedPa <- function() {
  paSummary$selectedID <- NULL
  paSummary$selectedTerm <- NULL
}