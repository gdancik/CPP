# server-tableClicks.R (respond to table clicks and drop-down changes)

# update current selections - compare current selections with previous, and return
# updated vector (we may add or remove selections, but should not double count)
updateMeshSelections <- function(current, previous) {
  
  if (setequal(current, previous)) {
    return (NULL)
  }
  
  return (current)
}


# update Mesh IDs for Disease (tblSummary = diseaseSummary) or Chemicals (tblSummary = ChemSummary)
updateSelectedMeshIDs <- function(ids, tblSummary) {
  
  isolate({
    cat("\ncall updateSelectedMeshIDs\n")
    cat("new ids = ", ids, "\n")
    cat("selected ids = ", tblSummary$selectedID, "\n")
    
    selections <- updateMeshSelections(ids, tblSummary$selectedID)
    cat("getting selections = ", selections ,"\n\n")
  
    if (!is.null(selections))  {
      # if selections have changed, update
      tblSummary$selectedID <- selections
      m <- match(selections, tblSummary$uniqueDat$MeshID)
      tblSummary$selectedTerm <-as.character(tblSummary$uniqueDat$Term)[m]
    } else if (is.null(ids) & !is.null(tblSummary$selectedID)) {
      # the user has canceled all filters
      tblSummary$selectedTerm <- NULL
      tblSummary$selectedID <- NULL
      respondToSelectionDrill()
    }
    
  })
}




###########################################################################
# click on diseaseSummary table or drop-down
###########################################################################

# needed for table click resulting in no selection
observe({
  selected <- input$diseaseResults_rows_selected
  #return()
  cat("on observe, selected = ", selected, "\n")
  if (is.null(selected)) {
    if (is.null(diseaseSummary$selectedID)) {
      return()
    }
    diseaseSummary$selectedID <- NULL
    diseaseSummary$selectedTerm <- NULL
    respondToSelectionDrill()
  }
})



# respond to drop down change
observe({
  x <- input$filterDisease # need this for trigger
  updateSelectedMeshIDs(input$filterDisease, diseaseSummary)
})

# respond to table selection (or deselection) 
observeEvent(
  input$diseaseResults_rows_selected, {

    s = input$diseaseResults_rows_selected
    cat("table click, s = ", s, "\n")
    selectedMeshIDs <- diseaseSummary$uniqueDat$MeshID[s]
    cat("table click -- selected MeshIDs are: ", selectedMeshIDs, "\n")
    
    cat("call updateSelectedMeshIDs\n")
    updateSelectedMeshIDs(selectedMeshIDs, diseaseSummary)
        
  })


# respond to graph selection (or deselection) 

# TO DO: allow graph selection: this triggers observe for no table selection and results
# in refresh; multiple selections also do not work
if (0) {
observeEvent(
  input$DiseaseGraph_click$y, {
    
    s = input$DiseaseGraph_click$y
      
      lvls <- levels(diseaseSummary$graphData$Term)
    
      cat("click on: ", s, "\n")
      name <- lvls[round(s)]
      m <- match(name, diseaseSummary$graphData$Term)
      selectedMeshIDs <- diseaseSummary$graphData$MeshID[s]
      cat("selected: ", selectedMeshIDs, ", ", name, "\n")

    updateSelectedMeshIDs(selectedMeshIDs, diseaseSummary)
    
    
  })

}


###########################################################################
# click on chemSummary table or drop-down
###########################################################################

# needed for table click resulting in no selection
observe({
  selected <- input$chemResults_rows_selected
  if (is.null(selected)) {
    if (is.null(chemSummary$selectedID)) {
      return()
    }
    chemSummary$selectedID <- NULL
    chemSummary$selectedTerm <- NULL
    respondToSelectionDrill()
  }
})


# respond to drop down change
observe({
  x <- input$filterChem # need this to trigger
  updateSelectedMeshIDs(input$filterChem, chemSummary)
})


# respond to table selection (or deselection) 
observeEvent(
  input$chemResults_rows_selected, {
    
    s = input$chemResults_rows_selected
    selectedMeshIDs <- chemSummary$uniqueDat$MeshID[s]
  #  cat("table click -- selected MeshIDs are: ", selectedMeshIDs, "\n")
    
    updateSelectedMeshIDs(selectedMeshIDs, chemSummary)
    
  })

###########################################################################
# click on geneSummary table or drop-down (need to update)
###########################################################################

# needed for table click resulting in no selection -- 
# should handle no selection only
observe({

    #if (is.null(selected$geneSymbol)) {
    #  return()
    #}
  

  if (is.null(input$geneResults_rows_selected)) {
    if (is.null(geneSummary$selectedID)) {
      if (!is.null(selected$geneSymbol)) {
        displayGeneSummaryTable()
      }
      return()
    }
    #geneSummary$selectedID <- NULL
    #geneSummary$selectedTerm <- NULL
    #respondToSelectionDrill()
  } else if (length(input$geneResults_rows_selected) == 1) {
    gene <- geneSummary$dat[input$geneResults_rows_selected,2]
    cat("gene gene = ", gene, "\n")
    cat("sel sel = ", selected$geneSymbol, "\n")
    if (gene == selected$geneSymbol && !is.null(geneSummary$selectedID)) {
      geneSummary$selectedID <- NULL
      geneSummary$selectedTerm <- NULL
      respondToSelectionDrill()
    }
  }
  
  
})



updateGeneSelections <- function(gene, geneSummary, GeneTable) {

  isolate({  
    cat("current selection = ", gene, "\n")
    cat("previous selection = ", geneSummary$selectedTerm, "\n")
    selections <- updateMeshSelections(gene, geneSummary$selectedTerm)
    cat("selections = ", selections, "\n")
  
   
    if (!is.null(selections)) {
      gg <- geneSymbolToID(selections, GeneTable)
      gg$Symbol <- gsub("\r", "", gg$Symbol)
      geneSummary$selectedID <- gg$ID
      geneSummary$selectedTerm <- gg$Symbol
      cat("set selected term to: ", gg$Symbol,"\n")
    } else if (is.null(gene) & !is.null(geneSummary$selectedID)) {
      # the user has canceled all filters
      geneSummary$selectedTerm <- NULL
      geneSummary$selectedID <- NULL
      respondToSelectionDrill()
    }
    
  })
  
}


# respond to drop down change
observe({
    x <- input$filterGenes # need this for trigger
    updateGeneSelections(input$filterGenes, geneSummary, GeneTable)
})

# record click from geneSumary Table 
observeEvent(input$geneResults_rows_selected, {
  s = input$geneResults_rows_selected
  cat("selected gene: ", s, "\n")
  if (length(s) > 0) {
    print(geneSummary$dat[s,])
  }
  
  
  gene <- geneSummary$dat[s,2]
  gene <- gsub("\r", "", gene)
  
  cat("selected gene: ", gene, "\n")
  # if main gene was de-selected, just return
  if (!selected$geneSymbol %in% gene) {
    displayGeneSummaryTable()
    return()
  }
  
  cat("selected gene = ", gene, "\n")
  cat("current input = ", input$geneInput, "\n")
  
  gene = setdiff(gene, selected$geneSymbol)
  cat("remove input$geneInput, selected gene = ", gene, "\n")
  
  if (length(gene) == 0) {
    return()
  }
  
  updateGeneSelections(gene, geneSummary, GeneTable)
  
})


