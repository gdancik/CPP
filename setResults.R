

# updates the selectInput with given id, choices, and selected;
# ifi choices is NULL, disable and set choices to "NONE"
setSelectInput <- function(session, id, choices, selected) {
  if (is.null(choices)) {
    updateSelectInput(session, id, choices = "NONE")
    shinyjs::disable(id)
  } else {
    updateSelectInput(session, id, choices = choices,
                      selected = selected)
    shinyjs::enable(id)
  }
  
}

# sets unique disease results and updates drop down
setDiseaseResults <-function(session, res, diseaseSummary) {
  cat("setting disease results...")
  cat("terms = ", diseaseSummary$selectedTerm, "\n")
 
  isolate({
  diseaseSummary$uniqueDat <- res
  #cat("done\n")
  
  choices <- diseaseSummary$selectedID
  if (!is.null(choices)) {
    names(choices) <- diseaseSummary$selectedTerm
  }
  cat("updating drop down with selected = ", diseaseSummary$selectedID, "and choices = ", choices, "\n")
  
  setSelectInput(session, "filterDisease", choices, diseaseSummary$selectedID)
  })
}


# sets unique disease results and updates drop down
setDrugResults <-function(session, res, paSummary) {
  #cat("setting chem results...")
  
  isolate({
    paSummary$uniqueDat <- res
    # cat("done\n")
    
    choices <- paSummary$selectedID
    if (!is.null(choices)) {
      names(choices) <- paSummary$selectedTerm
    }
    
    #setSelectInput(session, "filterChem", choices, chemSummary$selectedID)
  })
}

# sets unique disease results and updates drop down
setChemResults <-function(session, res, chemSummary) {
  #cat("setting chem results...")
  
  isolate({
    chemSummary$uniqueDat <- res
   # cat("done\n")
    
    choices <- chemSummary$selectedID
    if (!is.null(choices)) {
      names(choices) <- chemSummary$selectedTerm
    }
     
    setSelectInput(session, "filterChem", choices, chemSummary$selectedID)
  })
}

# sets unique disease results and updates drop down
setGeneResults <-function(session, res, geneSummary) {
  #cat("setting gene results...")
  
  isolate({
    geneSummary$dat <- res
   # cat("done\n")
  
    #choices <- geneSummary$dat$Symbol
    #choices <- gsub("\r", "", choices)
    
  #  cat("selected genes = ", geneSummary$selectedTerm, "\n")
    
    setSelectInput(session, "filterGenes", geneSummary$selectedTerm, geneSummary$selectedTerm)
    
  })
}
