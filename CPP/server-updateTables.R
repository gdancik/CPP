# server-updateTables.R

#update geneTable
observe ({
  
  if (is.null(geneSummary$dat)) {
    return()
  }
  displayGeneSummaryTable()
})


displayGeneSummaryTable <- function() {
  
  selected <- c(selected$geneSymbol, geneSummary$selectedTerm)
  cat("update gene summary, selected = ", selected, "\n")
  m <- match(selected, geneSummary$dat$Symbol)
  
  isolate({
    selection = list(mode = "multiple", selected = m, target = "row")
    cat("selected gene rows = ", m, "\n")
  })
  output$geneResults <- DT::renderDataTable(datatable(geneSummary$dat, rownames = FALSE,
                                                      selection = selection,
                                                      options = list(paging = FALSE, scrollY = 300)))
  
}


##################################################################
# update PMID table
##################################################################
# observe ({
#   if (is.null(pmidList$pmids)) {
#     return()
#   }
#   o <- order(pmidList$pmids$PMID, decreasing = TRUE)
#   output$articleTable <- DT::renderDataTable(DT::datatable(pmidList$pmids[o,,drop = FALSE], rownames = FALSE,
#                                                            selection = "none",
#                                                            options = list(lengthChange = FALSE, 
#                                                                           searching = FALSE,
#                                                                           pageLength = 10, scrollY = 300)))
# })


##################################################################
# update disease, chem, pa tables, Cancer Term
##################################################################

# generic function to display table with current selection
updateTable <- function(resTable, columnName, tableID) {
  
  if (is.null(resTable$dat)) {
    return()
  }
  
  x <- resTable$dat
  
  isolate({
    m <- match(resTable$selectedID, resTable$dat[[columnName]])
    selection = list(mode = "multiple", selected = m, target = "row")
  })
  
  output[[tableID]] <- 
          DT::renderDataTable(datatable(x, rownames = FALSE, 
                                selection = selection,
                                options = list(paging = FALSE, scrollY = 300#,
                                       #  columnDefs = list(list(targets = -1, visible = FALSE))
                                        )
                                ) 
          )
}

#observe(updateTable(paSummary, "MeshID", "paResults"))
#observe(updateTable(diseaseSummary, "MeshID", "diseaseResults"))
observe(updateTable(chemSummary, "MeshID", "chemResults"))
observe(updateTable(mutationSummary, "MutID", "mutationResults"))
observe(updateTable(cancerTermSummary, "TermID", "cancerTermResults"))

observe(
  displayCancerSelectionSummary(diseaseSummary$dat, cancerSelectionSummary$selected1, cancerSelectionSummary$selected2, "diseaseResults")
)




##################################################################
# update cancer graph
##################################################################
observe({
  cat("in cancer plot observe...\n")
  
  if (!is.null(diseaseSummary$dat)) {
    
    # TO DO: delete commented block below (commented out 6/19) 
    # con = dbConnect(MariaDB(), group = "CPP")
    # 
    # qry <- paste0("select MeshID, TreeID from MeshTerms where MeshID IN ", 
    #               cleanseList(diseaseSummary$dat$MeshID),
    #               " AND MeshTerms.TreeID LIKE \"C04.%\";"
    # )
    # 
    # cancerTerms <- dbGetQuery(con, qry)
    # dbDisconnect(con)
    # 
    # x <- subset(diseaseSummary$dat, MeshID %in% cancerTerms$MeshID)
    # 
    
    x <- diseaseSummary$dat
    x$Frequency <- as.double(x$Frequency)

    output$cancerGraph <- renderPlot({
      
      # put levels in sorted order for plotting
      x$Term <- factor(x$Term, levels = x$Term[order(x$Frequency)])
      
      xx <- subset(x, Term%in% rev(levels(x$Term))[1:min(10,nrow(x))])
      title <- paste("Cancer-related publications mentioning", selected$geneSymbol)
      if (nrow(xx)!=nrow(x)) {
        title <- paste(title, " (top 10 shown)")
      }
      
      l <- list(diseaseSummary$selectedID, chemSummary$selectedID, geneSummary$selectedID,
           cancerTermSummary$selectedID)
      if (!all(sapply(l, is.null))) {
        title <- paste(title, "\n(summary includes additional filters)")
      }
      
      
      ggplot(xx, aes(Term, Frequency, fill = Term)) + geom_bar(stat = "identity") +
        coord_flip() + theme_linedraw() + theme(legend.position = "none") + xlab("") + ylab("# of articles") +
        ggtitle(title) +
        theme(plot.title = element_text(face = "bold"),  
              axis.title = element_text(face = "bold"),
              axis.text = element_text(face = "bold")) +
              scale_x_discrete(labels = function(x) str_wrap(x, width = 15))
    })
    
    output$cancerSummaryTable <- DT::renderDataTable(datatable(x, rownames = FALSE, selection = "none",
                                                               options = list(paging = FALSE, scrollY = 300)))
  }
})


############################################################
# cancerSelectionTable
############################################################
# observe({
#   x<- cancerSelectionSummary$dat
#   cat("observe1....\n")
#   #scan(what = character(), n = 1)
#   output$cancerSelectionTable <- DT::renderDataTable(datatable(cancerSelectionSummary$dat, rownames = FALSE, selection = "multiple",
#                                                                options = list(paging = FALSE, scrollY = 300)))
# })


#############################################################
### cancerSelectionSummary
#############################################################

# ids1 = selectedIDs,
# ids2 = child IDs that will be colored

displayCancerSelectionSummary <- function(dat, ids1, ids2, outputID = "cancerSelectionTable") {
  
  cat("\n\n")
  cat("displaying: ", outputID, "\n")
  cat("ids1 = ", ids1, "\n")
  cat("ids2 = ", ids2, "\n")
  cat("\n\n")
  
  if (is.null(dat)) {
    return()
  }
  
  #wait()
  
  selection = list(mode = "multiple", selected = NULL, target = "row")
  
  if (!is.null(ids1)) {
    m <- match(ids1, dat$MeshID)
    selection$selected = m
  }
  
  x <- dat
  # if (is.null(x)) {
  #   output[[outputID]] <- DT::renderDataTable(data.frame())
  #   return()
  # }
  # 
  
  x <- mutate(x, color = 0)
  x$color[x$MeshID %in% ids2] <- 1
  
  # set selection, and hide the 'color' column
  dt <- datatable(x, rownames = FALSE,
                    selection = selection,
                    options = list(paging = FALSE, scrollY = 300,
                                   columnDefs = list(list(targets = 3, visible = FALSE))
                                   )
                  )
    
  # formatting table
  dt <- dt %>% formatStyle("color", target = "row", 
                           backgroundColor = styleEqual(c("0","1"), c("white", "pink"))
                           )
  
  #output$cancerSelectionTable <- DT::renderDataTable(dt)
  output[[outputID]] <- DT::renderDataTable(dt)
}

cancerSelectionChoices <- function() {
  choices <- as.list(cancerSelectionSummary$dat$MeshID)
  names(choices) <- cancerSelectionSummary$dat$Term
  choices  
} 


# observe any changes to the table and update
observe ({
  m <- input$cancerSelectionTable_rows_selected
  selected <- NULL
  if (!is.null(m)) {
    selected <- cancerSelectionSummary$dat$MeshID[m]  
  }
  
  updateSelectInput(session, "cancerType", choices = cancerSelectionChoices(), selected = selected)
})

# updates cancerSelectionSummary$ids2 for new IDs
# returns TRUE if IDs have changed
updateChildIDsForSelectedCancers <- function(){
  cat("\nupdate child IDs...\n")
  ids <- getChildMeshIDsForSelectedCancers()
  cat("ids = ", ids, "\n")
  cat("ids2 = ", cancerSelectionSummary$ids2, "\n\n")
  
  msg <- NULL
  ret <- FALSE
  
  # if ids have changed
  if (!setequal(ids, cancerSelectionSummary$ids2)) {
    ret <- TRUE
    
    if (length(ids) != 0) {
         msg <- HTML("<br/><b>Note: </b>", "Above selections will include additional cancer types</br> 
         highlighted in pink in the table (<a id = 'highlight' href = '#highlight'>refresh</a>).")
         cancerSelectionSummary$highlightPending <- TRUE
    } else {
         msg <- NULL
    }
    output$cancerSelectionMsg <- renderUI({
      msg
    })
    cancerSelectionSummary$ids2 <- ids
  }
  
  return(ret)
}



# change in drop down -- update table if different
observeEvent(input$cancerType,{
  # refreshTable if drop down and table selections differ
   refreshTable <- !setequal(input$cancerType, cancerSelectionSummary$dat$MeshID[input$cancerSelectionTable_rows_selected])
   
   newChildIDs <- updateChildIDsForSelectedCancers()
   
   # refresh table because user has changed drop down
   if (refreshTable) {
     if (newChildIDs) {
       cat("\n\nCLEARING HIGHLIGHTS...\n")
       cancerSelectionSummary$highlightPending <- TRUE
       displayCancerSelectionSummary(cancerSelectionSummary$dat, input$cancerType, NULL)  
     } else {
       displayCancerSelectionSummary(cancerSelectionSummary$dat, input$cancerType, cancerSelectionSummary$ids2)  
     }
     return()
   }
  
  # otherwise the drop down change is from a table selection 
  # so if child ids have changed, removed highlights
  cat("updating child IDs...\n")
   if (newChildIDs & is.null(cancerSelectionSummary$highlightPending)) {
     cat("\n\nCLEARING HIGHLIGHTS...\n")
     cancerSelectionSummary$highlightPending <- TRUE
     displayCancerSelectionSummary(cancerSelectionSummary$dat, input$cancerType, NULL)
   }
   
})

# handle drop down when no selection
observe({
  if (is.null(input$cancerType)) {
    cat("NO SELECTION FOR CANCER TYPE!\n")
    cancerSelectionSummary$ids2 <- NULL
    cancerSelectionSummary$highlightPending <- NULL
    displayCancerSelectionSummary(cancerSelectionSummary$dat, NULL, NULL)
  }
  
})

# get child MeshIDs from input$cancerType
getChildMeshIDsForSelectedCancers <- function() {
   
    catn("get childmesh ids for selected cancers...")

    # get tree info for selected ids (my_trees) and unselected (other_trees)
    trees <- cancerSelectionSummary$tree_ids
    
    my_trees <- trees %>% dplyr::filter(MeshID %in% input$cancerType)
    other_trees <- trees %>% dplyr::filter(!MeshID %in% input$cancerType)
    
    # for each MeshID we have selected, search corresponding tree IDs
    s <- split(my_trees$TreeID, my_trees$MeshID)
    s <- sapply(s, paste0, ".", collapse = "|")   # Note: '.' is not literal but that's okay
    
    terms <- cancerSelectionSummary$dat[match(names(s), cancerSelectionSummary$dat$MeshID),]$Term
    names(s) <- terms
    
    more <- NULL
    
    for (id in s) {
      g <- grep(id, other_trees$TreeID) 
      if (length(g) > 0) {
        terms <- cancerSelectionSummary$dat[cancerSelectionSummary$dat$MeshID%in%other_trees$MeshID[g],]$MeshID
        more <- c(more, terms)        
      }
    }
    
    more
        
}
  

onclick("highlight", {
  cat("clicked on highlight\n")
  cat("ids2: ", cancerSelectionSummary$ids2, "\n")
  
  cancerSelectionSummary$highlightPending <- NULL
  
  displayCancerSelectionSummary(cancerSelectionSummary$dat, input$cancerType, cancerSelectionSummary$ids2)
  
  output$cancerSelectionMsg <- renderUI({

    if (is.null(cancerSelectionSummary$ids2)) {
      return("")
    }
    HTML("<br/><b>Note: </b>", "Above selections will include additional cancer types</br> 
         highlighted in pink in the table.")
  })
  
})



