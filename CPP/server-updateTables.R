# server-updateTables.R

#update geneTable
observe ({
  catn('observe for displayGeneSummaryTable...')
  
  if (is.null(geneSummary$dat)) {
    return()
  }
  displayGeneSummaryTable()
  
  updateStatTable("statGeneTable", geneSummary$stat)
})


displayGeneSummaryTable <- function() {
  
  #selected <- c(selected$geneSymbol, geneSummary$selectedTerm)
  selected <-  geneSummary$selectedTerm
  
  cat("update gene summary, selected = ", selected, "\n")
  m <- match(selected, geneSummary$dat$Symbol)
  
  isolate({
    selection = list(mode = "multiple", selected = m, target = "row")
    cat("selected gene rows = ", m, "\n")
  })

  isolate({
  output$geneResults <- DT::renderDataTable(datatable(geneSummary$dat, rownames = FALSE,
                                                      selection = selection,
                                                      options = list(paging = FALSE, scrollY = 300,
                                                                     scrollX = TRUE)))
  cat("selected rows: ", input$geneResults_rows_selected)
  })

}

# update multiGeneSummaryTable
observe({
  catn('observe for multiGeneSummaryTable...')
  t <- multiGeneSummary$dat
  
  if (is.null(t)) {
    return()
  }
  
  output$multiGeneResults <- DT::renderDataTable(datatable(t[,-1],rownames = FALSE,
                                                              options = list(paging = FALSE, scrollY = 300),
                                                              selection = "none"
  ))

})

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
 
  catn("in updateTable with", columnName, " and ", tableID)

  #wait()

  if (is.null(resTable$dat)) {
    output[[tableID]] <- NULL
    return()
  }
  
  x <- resTable$dat
  
  isolate({
    m <- match(resTable$selectedID, resTable$dat[[columnName]])
    selection = list(mode = "multiple", selected = m, target = "row")
  })
 
 isolate({ 
  output[[tableID]] <- 
          DT::renderDataTable(datatable(x, rownames = FALSE, 
                                selection = selection,
                                options = list(paging = FALSE, scrollY = 300#,
                                       #  columnDefs = list(list(targets = -1, visible = FALSE))
                                        )
                                ) 
          )

  catn("selected rows: ", input[[tableID]]$rows_selected)
  catn('*********************')

 })
}

updateStatTable <- function(tableID, x) {
  if (is.null(x)) {
    return (NULL)
  }
  output[[tableID]] <- 
      DT::renderDataTable(datatable(x, rownames = FALSE, selection = "none", 
                                options = list(paging = FALSE, scrollY = 300#,
                                               #  columnDefs = list(list(targets = -1, visible = FALSE))
                                )) %>%
                formatStyle(c(3,6,8), `border-right` = "solid 2px")
      )
}



observe({
  catn('observe for update chem summary table...')
  updateTable(chemSummary, "MeshID", "chemResults")
  updateStatTable("statChemTable", chemSummary$stat)
})

observe({
  catn('observe for update mutation summary table...')
  updateTable(mutationSummary, "MutID", "mutationResults")
  updateStatTable("statMutTable", mutationSummary$stat)
  
})

observe({
  catn('observe for update cancerTerm summary table...')
  updateTable(cancerTermSummary, "TermID", "cancerTermResults")
  updateStatTable("statCancerTermTable", cancerTermSummary$stat)
})

observe({
  catn('observe for cancerSelectionSummary...')
  displayCancerSelectionSummary(diseaseSummary$dat, cancerSelectionSummary$selected1, cancerSelectionSummary$selected2, "diseaseResults")
  updateStatTable("statCancerTable", diseaseSummary$stat)
})



##################################################################
# update cancer graph
##################################################################
observe({
  cat("in cancer plot observe...\n")
  
  #wait()
  if (!is.null(diseaseSummary$dat)) {
  
    x <- diseaseSummary$dat
    x$Frequency <- as.double(x$Frequency)
    
    output$cancerGraph <- renderPlot({
      
      # put levels in sorted order for plotting
      #x$Term <- factor(x$Term, levels = x$Term[order(x$Frequency)])
      
      # use table order
      x$Term <- factor(x$Term, levels = rev(x$Term))
      
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
    
   # output$cancerSummaryTable <- DT::renderDataTable(datatable(x, rownames = FALSE, selection = "none",
                          #                                     options = list(paging = FALSE, scrollY = 300)))
  } else {
    # catn("render NULL table")
    # output$cancerSummaryTable <- DT::renderDataTable(iris)
     output$cancerGraph <- NULL
  }
  print("done cancer plot observe")
})


#############################################################
### cancerSelectionSummary
#############################################################

# ids1 = selectedIDs,
# ids2 = child IDs that will be colored
# for 'cancerSelectionTable', set/allow multiple selections;
#    otherwise color code 'selected' rows but do not enable
#    selection
displayCancerSelectionSummary <- function(dat, ids1, ids2, outputID = "cancerSelectionTable") {
  
  cat("\n\n")
  cat("displaying: ", outputID, "\n")
  cat("ids1 = ", ids1, "\n")
  cat("ids2 = ", ids2, "\n")
  cat("\n\n")
  
  if (is.null(dat)) {
    if (outputID != "cancerSelectionTable") {
      output[[outputID]] <- NULL
    }
    return()
  }
  
  selection = list(mode = "multiple", selected = NULL, target = "row")
  
  x <- dat

  x <- mutate(x, color = 0)
  x$color[x$MeshID %in% ids2] <- 1
  
  # set selection
  if (!is.null(ids1)) {
    m <- match(ids1, dat$MeshID)
    selection$selected = m
    
    if (outputID != 'cancerSelectionTable') {
      x$color[m] <- 2
      selection <- 'none'
    }
    
  } else if (outputID != 'cancerSelectionTable') {
    selection <- 'none'
  }

  # set selection, and hide the 'color' column
  dt <- datatable(x, rownames = FALSE,
                    selection = selection,
                    options = list(paging = FALSE, scrollY = 300,
                                   columnDefs = list(list(targets = 5, visible = FALSE))
                                   )
                  )
    
  # formatting table
  dt <- dt %>% formatStyle("color", target = "row", 
                           backgroundColor = styleEqual(c("0","1", "2"), c("white", "pink", "maroon")),
                           color = styleEqual(c("0","1", "2"), c("black", "black", "white"))
                           )
  
  #output$cancerSelectionTable <- DT::renderDataTable(dt)
  output[[outputID]] <- DT::renderDataTable(dt)
  
   # catn("setting ", outputID, '...')
   # print(head(x))
   # wait()
  
}

cancerSelectionChoices <- reactive({
  choices <- as.list(cancerSelectionSummary$dat$MeshID)
  names(choices) <- cancerSelectionSummary$dat$Term
  choices  
})


#observe any changes to the table and update
observe ({
  catn('observe cancerSelectionTable_rows_selected')
  m <- input$cancerSelectionTable_rows_selected
  selected <- NULL
  if (!is.null(m)) {
    selected <- cancerSelectionSummary$dat$MeshID[m]
  }

  catn('UPDATE')
  # print(head(cancerSelectionSummary$dat))
  # wait()
  
  
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
    
    cancerSelectionSummary$ids2 <- ids
  }
  
  output$cancerSelectionMsg <- renderUI({
    msg
  })
  
  return(ret)
}



# change in drop down -- update table if different
observeEvent(input$cancerType,{
    catn('observeEvent cancerType...')
    if (is.null(input$cancerType)) {
      cat("NO SELECTION FOR CANCER TYPE!\n")
      cancerSelectionSummary$ids2 <- NULL
      cancerSelectionSummary$highlightPending <- NULL
      displayCancerSelectionSummary(cancerSelectionSummary$dat, NULL, NULL)
      return()
    }


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

}, ignoreNULL = FALSE, ignoreInit = TRUE)



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



