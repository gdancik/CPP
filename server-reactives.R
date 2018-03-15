# server-reactives.R

diseaseSummary <- reactiveValues(dat = NULL, uniqueDat = NULL, selectedID = NULL, 
                                 selectedTerm = NULL, hoverID = NULL)
chemSummary <- reactiveValues(dat = NULL, uniqueDat = NULL, selectedID = NULL, 
                              selectedTerm = NULL, hoverID = NULL)
geneSummary <- reactiveValues(dat = NULL, selectedID = NULL, selectedTerm = NULL)

pmidList <- reactiveValues(pmids = NULL)

lastTab <- reactiveValues(tab = NULL)

resetReactiveValues <- function() {
  diseaseSummary$dat = NULL
  diseaseSummary$uniqueDat = NULL
  diseaseSummary$selectedID = NULL 
  diseaseSummary$selectedTerm = NULL
  diseaseSummary$hoverID = NULL
  pmidList$pmids <- NULL
  geneSummary$dat <- NULL
  geneSummary$selectedID <- NULL
  geneSummary$selectedTerm <- NULL
  chemSummary$dat <- NULL
  chemSummary$selectedID <- NULL
  chemSummary$selectedTerm <- NULL
  
}


clearSelectedGene <- function() {
  geneSummary$selectedID <- NULL
  geneSummary$selectedTerm <- NULL
}

clearSelectedDisease <- function() {
  diseaseSummary$selectedID <- NULL
  diseaseSummary$selectedTerm <- NULL
}

clearSelectedChem <- function() {
  chemSummary$selectedID <- NULL
  chemSummary$selectedTerm <- NULL
}