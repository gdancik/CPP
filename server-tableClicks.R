# server-tableClicks.R (respond to table clicks and drop-down changes)

# update current selections - compare current selections with previous, and return
# updated vector (we may add or remove selections, but should not double count)
updateMeshSelections <- function(current, previous) {
  if (setequal(current, previous)) {
    return (NULL)
  }
  return (current)
}


# Given a set of 'ids', the table reactive will be updated
#     ids = the currently selected ids
#     tblSummary = the reactive object with table stored in tblSummary$dat
#     colName1 = the column name where the 'ids' are stored
#     colName2 = set to NULL or optional column name where 'terms' are stored
updateSelectedMeshIDs <- function(ids, tblSummary, colName1 = "MeshID", colName2 = "Term") {
  
  isolate({
    cat("\ncall updateSelectedMeshIDs\n")
    
    selections <- updateMeshSelections(ids, tblSummary$selectedID)
    cat("getting selections = ", selections ,"\n\n")
  
    if (!is.null(selections))  {
      # if selections have changed, update
      tblSummary$selectedID <- selections
      if (!is.null(colName2)) {
          m <- match(selections, tblSummary$dat[[colName1]])
          tblSummary$selectedTerm <-as.character(tblSummary$dat[[colName2]])[m]
      }
      respondToSelectionDrill()
    } else if (is.null(ids) & !is.null(tblSummary$selectedID)) {
      # the user has canceled all filters
      cat("setting Term and ID to NULL, then respond to selection..\n")
      tblSummary$selectedTerm <- NULL
      tblSummary$selectedID <- NULL
      respondToSelectionDrill()
    }
    
  })
}


########################################################
# respond to drop down changes
########################################################
observe({input$filterDisease
         updateSelectedMeshIDs(input$filterDisease, diseaseSummary)})
observe({input$filterChem
         updateSelectedMeshIDs(input$filterChem, chemSummary)})
observe({input$filterGenes
         updateGeneSelections(input$filterGenes, geneSummary, GeneTable)})
observe({input$filterMutations
        updateSelectedMeshIDs(input$filterMutations, mutationSummary, "MutID", NULL)})





########################################################
# respond to table selection or de-selection
########################################################
observeEvent(input$diseaseResults_rows_selected, {
             updateSelectedMeshIDs(diseaseSummary$dat$MeshID[input$diseaseResults_rows_selected], diseaseSummary) 
  })

observeEvent(input$chemResults_rows_selected, 
             updateSelectedMeshIDs(chemSummary$dat$MeshID[input$chemResults_rows_selected], chemSummary)  )

observeEvent(input$mutationResults_rows_selected, {
          updateSelectedMeshIDs(mutationSummary$dat$MutID[input$mutationResults_rows_selected], mutationSummary, "MutID", NULL)
})

#####################################################################
# respond to table de-selection resulting in no selection
#####################################################################


checkNoSelection <- function(selected, resTable) {
  if (is.null(selected)) {
    if (is.null(resTable$selectedID)) {
      return()
    }
    resTable$selectedID <- NULL
    resTable$selectedTerm <- NULL
    respondToSelectionDrill()
  }
}


# process possible no selection for diseaseTable
observe({ selected <- input$diseaseResults_rows_selected
          checkNoSelection(selected, diseaseSummary)
})

# process possible no selection for chemTable
observe({ selected <- input$chemResults_rows_selected
          checkNoSelection(selected, chemSummary)
})

# process possible no selection for mutationTable
observe({ selected <- input$mutationResults_rows_selected
          checkNoSelection(selected,  mutationSummary)
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







# record click from geneSumary Table 
observeEvent(input$geneResults_rows_selected, {
  s = input$geneResults_rows_selected
  cat("selected gene: ", s, "\n")
  if (length(s) > 0) {
    print(geneSummary$dat[s,])
  }
  
  
  gene <- geneSummary$dat[s,2]
  
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






##########################################################################################
# Not used
##########################################################################################

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


