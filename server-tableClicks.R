# server-tableClicks.R

# update current selections - compare current selections with previous, and return
# updated vector (we may add or remove selections, but should not double count)
updateCurrentSelections <- function(current, previous) {
  
  if (setequal(current, previous)) {
    return (NULL)
  }
  
  return (current)
}


###########################################################################
# click on diseaseSummary table or drop-down
###########################################################################

# update diseaseSummary reactive
updateSelectedMeshIDs <- function(ids, tblSummary) {
  
  isolate({
    cat("\ncall updateSelectedMeshIDs\n")
    cat("new ids = ", ids, "\n")
    cat("selected ids = ", tblSummary$selectedID, "\n")
    
    selections <- updateCurrentSelections(ids, tblSummary$selectedID)
    cat("getting selections = ", selections ,"\n\n")
    
    if (!is.null(selections)) {
      tblSummary$selectedID <- selections
      m <- match(selections, tblSummary$uniqueDat$MeshID)
      tblSummary$selectedTerm <-as.character(tblSummary$uniqueDat$Term)[m]
    }
    
  })
}


# respond to drop down change
observe({
  cat("dropdown selected: ", input$filterDisease, "\n")
  updateSelectedMeshIDs(input$filterDisease, diseaseSummary)
})


# respond to table selection (or deselection) 
observeEvent(
  input$diseaseResults_rows_selected, {
    
    s = input$diseaseResults_rows_selected
    selectedMeshIDs <- diseaseSummary$uniqueDat$MeshID[s]
    cat("table click -- selected MeshIDs are: ", selectedMeshIDs, "\n")
    
    updateSelectedMeshIDs(selectedMeshIDs, diseaseSummary)
    
  })



###########################################################################
# click on chemSummary table or drop-down
###########################################################################



# respond to drop down change
observe({
  cat("dropdown selected: ", input$filterChem, "\n")
  updateSelectedMeshIDs(input$filterChem, chemSummary)
})



# respond to table selection (or deselection) 
observeEvent(
  input$chemResults_rows_selected, {
    
    s = input$chemResults_rows_selected
    selectedMeshIDs <- chemSummary$uniqueDat$MeshID[s]
    cat("table click -- selected MeshIDs are: ", selectedMeshIDs, "\n")
    
    updateSelectedMeshIDs(selectedMeshIDs, chemSummary)
    
  })

###########################################################################
# click on geneSummary table or drop-down (need to update)
###########################################################################


# record click from geneSumary Table 
observeEvent(input$geneResults_rows_selected, {
  s = input$geneResults_rows_selected
  cat("selected gene: ", s, "\n")
  if (length(s) > 0) {
    print(geneSummary$dat[s,])
  }
  if (s== 1) {
    return()
  }
  gene <- geneSummary$dat[s,2]
  cat("gene = ", gene, "\n")
  gg <- geneSymbolToID(gene, GeneTable)
  geneSummary$selectedID <- append(geneSummary$selectedID, gg$ID)
  geneSummary$selectedTerm <- append(geneSummary$selectedTerm, gg$Symbol)
  cat("set selected term to: ", gg$Symbol,"\n")
  
})


