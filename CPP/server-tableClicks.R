# server-tableClicks.R (respond to table clicks and drop-down changes)

# apply addClass or removeClass to all table ids
# 'f' should be shinyjs::addClass or shinyjs::removeClass
toggleTableClicks <- function(f) {
  
  f(id = 'diseaseResults', class = 'noclick')
  f(id = 'cancerSelectionTable', class = 'noclick')
  f(id = 'chemResults', class = 'noclick')
  f(id = 'mutationResults', class = 'noclick')
  f(id = 'cancerTermResults', class = 'noclick')
  f(id = 'geneResults', class = 'noclick')
  
}

enableTableClicks <- function() {
      toggleTableClicks(shinyjs::removeClass)
}

disableTableClicks <- function() {
    toggleTableClicks(shinyjs::addClass)
}

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
            wait()
           # scan(what = character())
          }
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


# Given a set of 'ids', the table reactive will be updated
#     ids = the currently selected ids
#     tblSummary = the reactive object with table stored in tblSummary$dat
#     colName1, colName2- optional columns to get terms from IDs
#     if terms is specified, this will be used to to assign terms
#     Note: only update IDs if there is a change
#           ids are assigned to tblSummary$selectedIDs
#           corresponding terms (from tblSummary$dat) assigned to tblSummary$selectedTerms

#assign geneIDs to tblSummary$selectedIDs

updateSelectedMeshIDs2 <- function(ids, tblSummary, colName1 = "MeshID", 
                                   colName2 = "Term", terms = NULL) {
  
  isolate({
    selections <- ids
    if (length(selections) == 0) {
      selections <- NULL
    }
    
    catn("\ncall updateSelectedMeshIDs2 with selections:", selections)
    catn("tblSummary$selectedID: ", tblSummary$selectedID)
    catn("tblSummary$selectedTermS: ", tblSummary$selectedTerm)
   
    # if no change, return
    if (setequal(selections, tblSummary$selectedID)) {
      catn("no change, returning...")
      return()
    }
    
    if (!is.null(selections))  {
      tblSummary$selectedID <- selections
      if (!is.null(terms)) {
        tblSummary$selectedTerm <- terms
      } else if (!is.null(colName2)) {
        m <- match(selections, tblSummary$dat[[colName1]])
        tblSummary$selectedTerm <-as.character(tblSummary$dat[[colName2]])[m]
        if (any(is.na(tblSummary$selectedTerm))) {
          cat("we have an NA for a selected term, ", tblSummary$selectedTerm, "\n")
          catn("this should be checked")
        }
      }
    } else if (is.null(selections) & !is.null(tblSummary$selectedID)) {
      # the user has canceled all filters
      catn("setting selected terms and IDs to NULL")
      tblSummary$selectedTerm <- NULL
      tblSummary$selectedID <- NULL
    }
  })
}


#############################################################
# respond to click on cancer type table
#############################################################
shinyjs::onclick('diseaseResults', {
  toggleModal(session, "cancerTypeSetupModal",  toggle = "open")
})

#################################################
# For diseaseSummary, open model if user clicks
#################################################
# observe( {
#            input$diseaseResults_rows_selected
#           
#             if (is.null(diseaseSummary$dat)) {
#               cat("diseaseSummary$dat is NULL\n")
#               return()
#             } 
#   
#   
#             cat("diseaseSummary$dat is not NULL\n")
#               
#             initialized <- isolate(diseaseSummary$initialized)
#             cat("initialized: ", initialized, "\n")
#             if (is.null(initialized)) {
#                 initialized <- 1
#                 isolate(diseaseSummary$initialized <- initialized)
#                 return()
#             } else if (initialized == 1) {
#                 initialized <- 2
#                 isolate(diseaseSummary$initialized <- initialized)
#                 return()
#             }
#             
#             # if no rows selected
#             if (is.null(input$diseaseResults_rows_selected)) {
#               cat("no rows selected\n")
#               cat("selected1: ", cancerSelectionSummary$selected1, "\n")
#                isolate(displayCancerSelectionSummary(diseaseSummary$dat, cancerSelectionSummary$selected1, cancerSelectionSummary$selected2, "diseaseResults"))
#                cat("toggle modal from no rows selected...\n")
#               # toggleModal(session, "cancerTypeSetupModal",  toggle = "open")
#                catn("click link")
#                shinyjs::click('#cancerTypeSetupModal')
#                
#                return()
#             } else {
#               
#                 
#                 ids <- diseaseSummary$dat$MeshID[input$diseaseResults_rows_selected]          
#                 
#                 # if user didn't actually click, then return
#                 correctIDs <- intersect(diseaseSummary$dat$MeshID, cancerSelectionSummary$selected1)
#                 cat("indices: ", input$diseaseResults_rows_selected, "\n")
#                 cat("selected ids = ", ids, "\n")
#                 cat("selected1 = ", cancerSelectionSummary$selected1, "\n")
#                 cat("correctIDs: ", correctIDs, "\n")
#               
#                 if (setequal(correctIDs, ids)) {
#                     cat("returning...\n")
#                     return()
#                 }
#               
#                 isolate(displayCancerSelectionSummary(diseaseSummary$dat, cancerSelectionSummary$selected1, cancerSelectionSummary$selected2, "diseaseResults"))
#                 cat("toggle modal from user...\n")
#                 #shinyjs::click('#cancerTypeSetupModal')
#                 toggleModal(session, "cancerTypeSetupModal",  toggle = "open")
#                 return()
#             }
#   })
# 

######################################################################
# returns TRUE if any summary selections need to be refreshed
######################################################################
refreshPending <- function() {
  l <- list(cancerTermSummary, chemSummary, mutationSummary, geneSummary)
  s <- sapply(l, `[[`, 'refreshPending' )
  any(unlist(s))
}

# ##################################################################
# # generate expression used to observe table clicks
# # compares selected ids (in colname1) with tblSummary$selectedID
# #################################################################
tableClickExpression <- function(inTblSummary, rows_selected,
                                 colname1 = quote(MeshID),
                                 selectedID = quote(selectedID)){
  substitute(
    {
      catn("click")
      tblSummary <- isolate(inTblSummary)
      if (is.null(tblSummary)) {
        return()
      }

      catn("summary table:")
      head(tblSummary$dat)
      catn("selected row #s: ", input$rows_selected)
      ids <- tblSummary$dat$colname1[input$rows_selected]
      catn("selected ids: ", ids)
      catn("currently selected ids: ", tblSummary$selectedID)

      # if filter has not changed, return
      if (setequal(ids, tblSummary$selectedID[tblSummary$selectedID %in% tblSummary$dat$colname1])) {
        tblSummary$refreshPending <- NULL
        if (!refreshPending()) {
          removeNotification('refreshNotification')
        }
        return()
      } else {
        tblSummary$refreshPending <- TRUE
        refreshNotification()
      }
    }, list(inTblSummary = inTblSummary, rows_selected = rows_selected,
            colname1 = colname1, selectedID = selectedID)
  )
}


########################################################
# set observers to handle table clicks
########################################################

# Drugs
observeEvent(input$chemResults_rows_selected, {
  tableClickExpression(quote(chemSummary), quote(chemResults_rows_selected))
  }, handler.quoted = TRUE, ignoreNULL = FALSE, ignoreInit = TRUE)

# cancerTermSummary
observeEvent(input$cancerTermResults_rows_selected, {
  tableClickExpression(quote(cancerTermSummary), quote(cancerTermResults_rows_selected), colname1 = quote(TermID))
  }, handler.quoted = TRUE, ignoreNULL = FALSE, ignoreInit = TRUE)

# mutationSummary
observeEvent(input$mutationResults_rows_selected, {
  tableClickExpression(quote(mutationSummary), quote(mutationResults_rows_selected), colname1 = quote(MutID))
}, handler.quoted = TRUE, ignoreNULL = FALSE, ignoreInit = TRUE)

observeEvent(input$geneResults_rows_selected, {
  tableClickExpression(quote(geneSummary), quote(geneResults_rows_selected), colname1 = quote(Symbol), selectedID = quote(selectedTerm))
}, handler.quoted = TRUE, ignoreNULL = FALSE, ignoreInit = TRUE)


# filterChanged <-function(ids, selected, msg = NULL) {
#   catn()
#   catn(msg)
#   catn("selected ids: ", ids)
#   catn("currently selected ids: ", selected)
#   ret <- !setequal(ids, selected)
#   catn('returning: ', ret)
#   ret
# }
# 
# all_rows <- reactive({
#   list(ct =  input$cancerTermResults_rows_selected,
#        chem = input$chemResults_rows_selected,
#        mut = input$mutationResults_rows_selected,
#        gene = input$geneResults_rows_selected
#        )
# })
# 
# observeEvent( all_rows(), {
#     
#     catn('***************************')
#     print(all_rows())
#     catn("cancer Term table click!")
#     catn("rows selected: ", input$cancerTermResults_rows_selected)
#     catn("id selected: ", cancerTermSummary$dat$Term[input$cancerTermResults_rows_selected])
#     catn('***************************')
#     #wait()
# })
# 
# observeEvent(input$cancerTermResults_rows_selected, {
#   catn('changed ct rows selected:', input$cancerTermResults_rows_selected)
#   
#   wait()
# }, ignoreNULL = FALSE, ignoreInit = TRUE)
# 
# # observer to handle all table selections (except diseaseSummary)
# observe({
#   
#   if (is.null(chemSummary$dat) || is.null(cancerTermSummary$dat) ||
#       is.null(mutationSummary$dat) || is.null(geneSummary$dat)) {
#       return()
#   }
# 
#   catn('current tab is: ', input$MainPage)
#   
#   dat <- cancerTermSummary$dat
#   rows_selected <- input$cancerTermResults_rows_selected
#   selectedID <- cancerTermSummary$selectedID
#   catn("dat")
#   print(head(dat))
#   catn("previously selected: ", selectedID, ":", cancerTermSummary$selectedTerm)
#   catn('table selection: ', rows_selected, "; ", dat$TermID[rows_selected], ":", dat$Term[rows_selected])
#   #save(dat, rows_selected, selectedID, file = "cancerTermSummary.RData")
#   #wait()
#   
#   changed <- FALSE
# 
#   tab <- input$MainPage
#   if (tab == "Drugs") {
#     chemSummary$refreshed <- 1
#   } else if (tab == "Mutations") {
#     mutationSummary$refreshed <- 1
#   } else if (tab == "Cancer Terms") {
#     cancerTermSummary$refreshed <- 1
#   } else if (tab == "Genes") {
#     geneSummary$refreshed <- 1
#   }
#     
#   l <- list('chem' = chemSummary, 'mut' = mutationSummary, 'ct' = cancerTermSummary, 'genes' = geneSummary)
# 
#   sapply(1:length(l), function(i,l) catn(names(l)[i], ": ", l[[i]]$refreshed), l = l)
#   #wait()
#       
#   if (!is.null(chemSummary$refreshed) && filterChanged(chemSummary$dat$MeshID[input$chemResults_rows_selected], chemSummary$selectedID, 'chemSummary')) {
#     changed <- TRUE
#   } else if (!is.null(cancerTermSummary$refreshed) && filterChanged(cancerTermSummary$dat$TermID[input$cancerTermResults_rows_selected], cancerTermSummary$selectedID, 'cancerTermSummary')) {
#     changed <- TRUE    
#   } else if (!is.null(mutationSummary$refreshed) && filterChanged(mutationSummary$dat$MutID[input$mutationResults_rows_selected], mutationSummary$selectedID, 'mutationSummary')) {
#     changed <- TRUE    
#   } else if (!is.null(geneSummary$refreshed) && filterChanged(geneSummary$dat$Symbol[input$geneResults_rows_selected], geneSummary$selectedTerm, 'geneSummary')) {
#     changed <- TRUE
#   }
#   
#   if (changed) {
#     showNotification("Your filters have changed!", 
#                      div(hr(class = 'blue-button', style="height: 2px; margin-top: 10px; margin-bottom: 10px;"), 
#                          div(style = 'width: 100%; display:inline-block; text-align:center',
#                              actionButton("btnRefresh", "Update", class = 'blue-button', style = 'width: 45%;'),
#                              HTML("&nbsp;&nbsp;"),
#                              actionButton("btnCancelRefresh", "Cancel", class = 'btn-danger', style = 'width: 45%;')
#                          )
#                      ), id = "refreshNotification", duration = NULL,  closeButton = FALSE)
#     
#   } else {
#     removeNotification('refreshNotification')
#   }
#   
# })
# 

#################################################
# refresh filters - update selections and drill
#################################################
shinyjs::onclick('btnRefresh', {
  catn("REFRESH THE RESULTS!!")
  
  toggleModal(session, 'filterModal', toggle = 'open')
  shinyjs::enable("btnSaveFilters")
  removeNotification(id = 'refreshNotification')
  
  # catn("we have selected: ", input$chemResults_rows_selected)
  
  # disableTableClicks()
  # 
  # 
  # updateSelectedMeshIDs2(cancerTermSummary$dat$TermID[input$cancerTermResults_rows_selected], 
  #                       cancerTermSummary, "TermID", "Term")
  # 
  # updateSelectedMeshIDs2(chemSummary$dat$MeshID[input$chemResults_rows_selected], chemSummary)  
  # 
  # updateSelectedMeshIDs2(mutationSummary$dat$MutID[input$mutationResults_rows_selected], mutationSummary, "MutID", NULL)
  # 
  # terms <- geneSummary$dat$Symbol[input$geneResults_rows_selected]
  # ids <- geneSymbolToID(terms, GeneTable)$ID
  # updateSelectedMeshIDs2(ids, geneSummary, NULL, NULL, terms = terms)
  # 
  # removeNotification(id = 'refreshNotification')
  # disableTableClicks()
  # 
  # resetRefreshPending()
  # respondToSelectionDrill()
  # 
  # enableTableClicks()

  })

#################################################
# cancel filters - this resets summary tables
# to remove current selections
#################################################

invalidateAllSummaryTables <- function() {
  invalidateSummaryTable <- function(tbl) {
    tmp <- tbl$dat
    tbl$dat <- 4
    tbl$dat <- tmp
  }
  invalidateSummaryTable(chemSummary)
  invalidateSummaryTable(mutationSummary)
  invalidateSummaryTable(geneSummary)
  invalidateSummaryTable(cancerTermSummary)
}

shinyjs::onclick('btnCancelRefresh', {
  catn("Cancel refresh!!")
  removeNotification(id = 'refreshNotification')
  invalidateAllSummaryTables()
  
  
})

