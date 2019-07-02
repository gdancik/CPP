##########################################################################################
# Functions to handle filterModal
##########################################################################################


# this gets current table selections if they have changed, based on 
# 'refreshPending'. We cannot just look at table; this is because
# the row indices 'i' will not be correct if the table has not
# been refreshed (because user did not click on tab yet)
getTableSelections <- function(tbl, i, id.col, term.col = NULL) {
  
  if (is.null(tbl$refreshPending)) {
    catn("no refresh, use previous...")
    return(list(choices = tbl$selectedID, terms = tbl$selectedTerm))
  }
  
  if (is.null(i) || length(i) == 0) {
    return(NULL)
  }
  ids <- tbl$dat[[id.col]][i]
  terms <- NULL
  if (!is.null(term.col)) {
    terms <- tbl$dat[[term.col]][i]
  }
  list(choices = ids, terms = terms)
}





# updates the selectInput with given id, choices (ids), and terms;
# if choices is NULL, disable and set choices to "NONE"
setSelectInput <- function(session, id, choices, terms) {
  if (is.null(choices)) {
    updateSelectInput(session, id, choices = "NONE")
    shinyjs::hide(id)
    shinyjs::hide(paste0(id, "Type"))
  } else {
    if (!is.null(terms)) {
      names(choices) <- terms
    }
    
    updateSelectInput(session, id, choices = choices,
                      selected = choices)
    shinyjs::show(id)
    if (length(choices) > 1) {
      shinyjs::show(paste0(id, "Type"))
    } else {
      shinyjs::hide(paste0(id, "Type"))
    }
  }
}




observeEvent(input$filterModal,{
  #shinyjs::disable("btnSaveFilters") 
  
  #setSelectInput(session, "filterDisease", diseaseSummary$selectedID, diseaseSummary$selectedTerm)
  chem <- getTableSelections(chemSummary, input$chemResults_rows_selected, 'MeshID', 'Term')
  
  ct <- getTableSelections(cancerTermSummary, input$cancerTermResults_rows_selected, 'TermID', 'Term')
  print(ct)
  
  mut <- getTableSelections(mutationSummary, input$mutationResults_rows_selected, 'MutID')
  print(mut)
  
  
  gene <- getTableSelections(geneSummary, input$geneResults_rows_selected, 'Symbol')
  print(gene)
  if (!is.null(gene$choices) && is.null(gene$terms)) {
    gene$terms <- gene$choices
    gene$choices <- as.integer(geneSymbolToID(gene$terms, GeneTable)$ID)
  }
  
  print(gene)
  
  
  setSelectInput(session, "filterChem", chem$choices, chem$terms)
  setSelectInput(session, "filterMutations", mut$choices, mut$terms)
  setSelectInput(session, "filterCancerTerms", ct$choices, ct$terms)
  
  catn("setting input, gene choices: ", gene$choices)
  catn("setting input, gene terms: ", gene$terms)
  setSelectInput(session, "filterGenes", gene$choices, gene$terms)

  # setSelectInput(session, "filterChem", chemSummary$selectedID, chemSummary$selectedTerm)
  # setSelectInput(session, "filterMutations", mutationSummary$selectedID, mutationSummary$selectedTerm)
  # setSelectInput(session, "filterCancerTerms", cancerTermSummary$selectedID, cancerTermSummary$selectedTerm)
  # setSelectInput(session, "filterGenes", geneSummary$selectedID, geneSummary$selectedTerm)
  # 
  
  

})




observeEvent(input$btnClearFilters, {
  catn("clicked clearFilters")
  setSelectInput(session, "filterDisease", NULL, NULL)
  setSelectInput(session, "filterChem", NULL, NULL)
  setSelectInput(session, "filterMutations", NULL, NULL)
  setSelectInput(session, "filterCancerTerms", NULL, NULL)
  setSelectInput(session, "filterGenes", NULL, NULL)
  
  #shinyjs::click('btnSaveFilters')
  
})

# observer to enable / disable Save Filter button
# observe({
# 
#     # return TRUE if x includes values that y does not
#     f <- function(x,y) {
#         s <- setdiff(x,y)
#         length(s) > 0
#     }
# 
#     # check whether any filters have changed
#     l <- vector("logical", 5)
#     l[1] <- f(diseaseSummary$selectedID, input$filterDisease)
#     l[2] <- f(chemSummary$selectedID, input$filterChem)
#     l[3] <- f(mutationSummary$selectedID, input$filterMutations)
#     l[4] <- f(cancerTermSummary$selectedID, input$filterCancerTerms)
#     l[5] <- f(geneSummary$selectedID, input$filterGenes)
# 
#     # enable or disable Save Filter button as appropriate
#     if (any(l)) {
#          catn("on update, enabling button")
#         shinyjs::enable("btnSaveFilters")
#     } else {
#         catn("on update, disabling button")
#         shinyjs::disable("btnSaveFilters")
#     }
# })


saveFilters <- function(f, s_name, col1, col2, GeneTable = NULL) {
  x <- get(s_name)
  
  if (setequal(x$selectedID,f)) {
    catn("no change to: ", s_name)
    return (FALSE) 
  }
  
  if (is.null(f)) {
    isolate(x$selectedID <- NULL)
    isolate(x$selectedTerm <- NULL)
  } else {
    cat("updating to: ", f, "\n")  
    
    m <- match(f, x$dat[[col1]])
    
    if (is.null(GeneTable)) {
      isolate(x$selectedID <- f)
      isolate(x$selectedTerm <- x$dat[[col2]][m])
    } else {
      x$selectedID <- f
      x$selectedTerm <- geneIDToSymbol(f, GeneTable)
    }
  }
  
  catn("assigning the following to: ", s_name)
  catn("selectedID: ", x$selectedID)
  catn("selectedTerm: ", x$selectedTerm)
  isolate(assign(s_name, x, env = .GlobalEnv))
  return(TRUE)
}


resetRefreshPending <- function() {
          chemSummary$refreshPending <- NULL
          mutationSummary$refreshPending <- NULL
          cancerTermSummary$refreshPending <- NULL
          geneSummary$refreshPending <- NULL
}


saveFilterValuesAndCheck <- function(n) {
  isolate({
    if (setequal(savedFilterValues[[n]], input[[n]])) {
      return(FALSE)
    }
  
    savedFilterValues[[n]] <- input[[n]]
    return(TRUE)
  })
}




observeEvent(input$btnSaveFilters, {

      f1 <- vector("logical", 4)
      f1[[1]] <- saveFilterValuesAndCheck('filterGenesType')
      f1[[2]] <- saveFilterValuesAndCheck('filterChemType')
      f1[[3]] <- saveFilterValuesAndCheck('filterMutationsType')
      f1[[4]] <- saveFilterValuesAndCheck('filterCancerTermsType')
      
      f2 <- vector("logical", 4)
      f2[[1]] <- saveFilters(input$filterChem, "chemSummary", "MeshID", "Term")
      f2[[2]] <- saveFilters(input$filterMutations, "mutationSummary", "MutID", "MutID")
      f2[[3]] <- saveFilters(input$filterCancerTerms, "cancerTermSummary", "TermID", "Term")
      f2[[4]] <- saveFilters(input$filterGenes, "geneSummary", "Symbol", NULL, GeneTable)
      
      if (any(f1) || any(f2)) {
          catn("\n\nAPPLYING FILTER NOW...")
          removeNotification('refreshNotification')
          disableTableClicks()
          cat("closing modal...\n")
          toggleModal(session, "filterModal", toggle = "close")
          cat("respond to selection ...\n")
          resetRefreshPending()
          respondToSelectionDrill()
          enableTableClicks() 
      } else {
        resetRefreshPending()
        invalidateAllSummaryTables()
        toggleModal(session, "filterModal", toggle = "close")
      }

})


# selectionTypeChoices <- rev(c("ALL selected terms" = "all",
#                               "ANY selected terms" = "any"))

shinyjs::onclick('btnCancelFilter', {
  updateSelectInput(session, 'filterCancerTermsType', 'Cancer term filter type', choices = selectionTypeChoices, selected = savedFilterValues$filterCancerTermsType)
  updateSelectInput(session, "filterChemType", "Drug filter type", choices = selectionTypeChoices, selected = savedFilterValues$filterChemType)
  updateSelectInput(session, "filterMutationsType", "Mutation filter type", choices = selectionTypeChoices, selected = savedFilterValues$filterMutationsType )
  updateSelectInput(session, "filterGenesType", "Gene filter type", choices = selectionTypeChoices, selected = savedFilterValues$filterGenesType )
  if (refreshPending()) {
    refreshNotification()
  }
})


