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
  cat("update selected Mesh IDs, currently selected = ", ids, "\n")
  
  
  isolate({
    cat("\ncall updateSelectedMeshIDs\n")
    
    selections <- updateMeshSelections(ids, tblSummary$selectedID)
    cat("getting selections = ", selections ,"\n\n")
  
    if (!is.null(selections))  {
      # if selections have changed, update
      #cat("\nselections have changed\noriginal:", ids, "\n")
      #cat("new: ", selections, "\n")
      #scan(what = character())
      tblSummary$selectedID <- selections
      if (!is.null(colName2)) {
          m <- match(selections, tblSummary$dat[[colName1]])
          #keep <- !is.na(m)
          keep <- 1:length(m)
          tblSummary$selectedID <- selections[keep]
          tblSummary$selectedTerm <-as.character(tblSummary$dat[[colName2]])[m[keep]]
          if (any(is.na(tblSummary$selectedTerm))) {
            cat("we have an NA, ", tblSummary$selectedTerm, "\n")
           # scan(what = character())
          }
      }
      
      respondToSelectionDrill()
    } else if (is.null(ids) & !is.null(tblSummary$selectedID)) {
      # the user has canceled all filters
      cat("setting Term and ID to NULL, then respond to selection..\n")
      tblSummary$selectedTerm <- NULL
      tblSummary$selectedID <- NULL
      resetReactive(diseaseSummary)
      respondToSelectionDrill()
    }
    
  })
}

#############################################################
# respond to table selection or de-selection
#############################################################

#################################################
# For diseaseSummary, open model if user clicks
#################################################
observe( {
           input$diseaseResults_rows_selected
          
            if (is.null(diseaseSummary$dat)) {
              cat("diseaseSummary$dat is NULL\n")
              return()
            } 
  
  
            cat("diseaseSummary$dat is not NULL\n")
              
            initialized <- isolate(diseaseSummary$initialized)
            cat("initialized: ", initialized, "\n")
            if (is.null(initialized)) {
                initialized <- 1
                isolate(diseaseSummary$initialized <- initialized)
                return()
            } else if (initialized == 1) {
                initialized <- 2
                isolate(diseaseSummary$initialized <- initialized)
                return()
            }
            
            # if no rows selected
            if (is.null(input$diseaseResults_rows_selected)) {
              cat("no rows selected\n")
              cat("selected1: ", cancerSelectionSummary$selected1, "\n")
               isolate(displayCancerSelectionSummary(diseaseSummary$dat, cancerSelectionSummary$selected1, cancerSelectionSummary$selected2, "diseaseResults"))
               cat("toggle modal from no rows selected...\n")
              # toggleModal(session, "cancerTypeSetupModal",  toggle = "open")
               catn("click link")
               shinyjs::click('#cancerTypeSetupModal')
               
               return()
            } else {
              
                
                ids <- diseaseSummary$dat$MeshID[input$diseaseResults_rows_selected]          
                
                # if user didn't actually click, then return
                correctIDs <- intersect(diseaseSummary$dat$MeshID, cancerSelectionSummary$selected1)
                cat("indices: ", input$diseaseResults_rows_selected, "\n")
                cat("selected ids = ", ids, "\n")
                cat("selected1 = ", cancerSelectionSummary$selected1, "\n")
                cat("correctIDs: ", correctIDs, "\n")
              
                if (setequal(correctIDs, ids)) {
                    cat("returning...\n")
                    return()
                }
              
                isolate(displayCancerSelectionSummary(diseaseSummary$dat, cancerSelectionSummary$selected1, cancerSelectionSummary$selected2, "diseaseResults"))
                cat("toggle modal from user...\n")
                #shinyjs::click('#cancerTypeSetupModal')
                toggleModal(session, "cancerTypeSetupModal",  toggle = "open")
                return()
            }
  })

observeEvent(input$chemResults_rows_selected, {
             #catn("chem table, rows selected...")
             #wait()
             updateSelectedMeshIDs(chemSummary$dat$MeshID[input$chemResults_rows_selected], chemSummary)  
})

observeEvent(input$mutationResults_rows_selected, {
          updateSelectedMeshIDs(mutationSummary$dat$MutID[input$mutationResults_rows_selected], mutationSummary, "MutID", NULL)
})

observeEvent(input$cancerTermResults_rows_selected, {
  cat("clicked: ", cancerTermSummary$dat$TermID[input$cancerTermResults_rows_selected], "\n")
  updateSelectedMeshIDs(cancerTermSummary$dat$TermID[input$cancerTermResults_rows_selected], cancerTermSummary, "TermID", "Term")
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
#observe({ selected <- input$diseaseResults_rows_selected
#          checkNoSelection(selected, diseaseSummary)
#})

# process possible no selection for chemTable
# if table has no rows, we don't want to process
# as a selection, so ignore 
observe({ selected <- input$chemResults_rows_selected
          if (!is.null(chemSummary$dat) && nrow(chemSummary$dat) == 0) {
            return()
          }
          checkNoSelection(selected, chemSummary)
})

# process possible no selection for mutationTable
observe({ selected <- input$mutationResults_rows_selected
        if (!is.null(mutationSummary$dat) && nrow(mutationSummary$dat) == 0) {
          return()
        }
        checkNoSelection(selected,  mutationSummary)
})

# process possible no selection for cancerTermsTable
observe({ 
  if (!is.null(cancerTermSummary$dat) && nrow(cancerTermSummary$dat) == 0) {
    return()
  }
  selected <- input$cancerTermResults_rows_selected
  checkNoSelection(selected, cancerTermSummary)
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
      cat("responding to selection drill")
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
      #save(selections, GeneTable, file = "genes.RData")
      geneSummary$selectedID <- gg$ID
      geneSummary$selectedTerm <- gg$Symbol
      print(gg)
      #scan(what = "character") 
      cat("set selected term to: ", gg$Symbol,"\n")
    } else if (is.null(gene) & !is.null(geneSummary$selectedID)) {
      # the user has canceled all filters
      geneSummary$selectedTerm <- NULL
      geneSummary$selectedID <- NULL
    }
    respondToSelectionDrill()
    
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
  
  cat("selected gene = ", gene, "\n")
  cat("current input = ", selected$geneSymbol, "\n")
 
  #gene <- c(gene, selected$geneSymbol) 
 
  symbol <- trimws(geneID_to_symbol(input$geneInput))
  gene <- setdiff(gene, symbol) 

  if (length(gene) < 1) {
    return()
  }
  
  updateGeneSelections(gene, geneSummary, GeneTable)
  
})
