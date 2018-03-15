



# sets unique disease results and updates drop down
setDiseaseResults <-function(session, res, diseaseSummary) {
  #cat("setting disease results...")
 
  isolate({
  diseaseSummary$uniqueDat <- res
  #cat("done\n")
  choices <- diseaseSummary$uniqueDat$MeshID
  names(choices) <- diseaseSummary$uniqueDat$Term
  
  cat("updating drop down with selected = ", diseaseSummary$selectedID, "\n")
  updateSelectInput(session, "filterDisease", choices = choices,
                    selected = diseaseSummary$selectedID)
  })
}


# sets unique disease results and updates drop down
setChemResults <-function(session, res, chemSummary) {
  #cat("setting chem results...")
  
  isolate({
    chemSummary$uniqueDat <- res
   # cat("done\n")
    choices <- chemSummary$uniqueDat$MeshID
    names(choices) <- chemSummary$uniqueDat$Term
    
    updateSelectInput(session, "filterChem", choices = choices,
                      selected = chemSummary$selectedID)
  })
}

# sets unique disease results and updates drop down
setGeneResults <-function(session, res, geneSummary) {
  #cat("setting gene results...")
  
  isolate({
    geneSummary$dat <- res
   # cat("done\n")
  
    choices <- geneSummary$dat$Symbol
    choices <- gsub("\r", "", choices)
    
  #  cat("selected genes = ", geneSummary$selectedTerm, "\n")
    
    updateSelectInput(session, "filterGenes", choices = choices,
                      selected = geneSummary$selectedTerm)
  })
}
