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
observe ({
  if (is.null(pmidList$pmids)) {
    return()
  }
  o <- order(pmidList$pmids$PMID, decreasing = TRUE)
  output$articleTable <- DT::renderDataTable(DT::datatable(pmidList$pmids[o,,drop = FALSE], rownames = FALSE,
                                                           selection = "none",
                                                           options = list(lengthChange = FALSE, 
                                                                          searching = FALSE,
                                                                          pageLength = 100, scrollY = 300)))
})


##################################################################
# update disease, chem, pa tables
##################################################################

# generic function to display table with current selection
updateTable <- function(resTable, columnName, tableID) {
  m <- match(resTable$selectedID, resTable$dat[[columnName]])
  isolate({
    selection = list(mode = "multiple", selected = m, target = "row")
  })
  output[[tableID]] <- DT::renderDataTable(datatable(resTable$dat, rownames = FALSE, 
                                                     selection = selection,
                                                     options = list(paging = FALSE, scrollY = 300)))
}

observe(updateTable(paSummary, "MeshID", "paResults"))
observe(updateTable(diseaseSummary, "MeshID", "diseaseResults"))
observe(updateTable(chemSummary, "MeshID", "chemResults"))
observe(updateTable(mutationSummary, "MutID", "mutationResults"))


##################################################################
# update cancer graph
##################################################################
observe({
  cat("in cancer plot observe...\n")
  
  if (!is.null(diseaseSummary$dat)) {
    
    con = dbConnect(MySQL(), group = "CPP")
    
    qry <- paste0("select MeshID, TreeID from MeshTerms where MeshID IN ", 
                  cleanseList(diseaseSummary$dat$MeshID),
                  " AND MeshTerms.TreeID LIKE \"C04.%\";"
    )
    
    cancerTerms <- dbGetQuery(con, qry)
    dbDisconnect(con)
    
    
    
    x <- subset(diseaseSummary$dat, MeshID %in% cancerTerms$MeshID)
    
    # put levels in sorted order for plotting
    x$Term <- factor(x$Term, levels = x$Term[order(x$Frequency)])
    
    
    output$cancerGraph <- renderPlot({
      
      xx <- subset(x, Term%in% rev(levels(x$Term))[1:min(10,nrow(x))])
      title <- paste("Cancer-related publications mentioning", selected$geneSymbol)
      if (nrow(xx)!=nrow(x)) {
        title <- paste(title, " (top 10 shown)")
      }
      
      if (!is.null(input$filterDisease) | !is.null(input$filterChem) | !is.null(input$filterGenes)) {
        title <- paste(title, "\n(summary includes additional filters)")
      }
      
      ggplot(xx, aes(Term, Frequency, fill = Term)) + geom_bar(stat = "identity") +
        coord_flip() + theme_linedraw() + theme(legend.position = "none") + xlab("") + ylab("# of articles") +
        ggtitle(title) +
        theme(plot.title = element_text(face = "bold"),  
              axis.title = element_text(face = "bold"),
              axis.text = element_text(face = "bold")) +
              scale_x_discrete(labels = function(x) str_wrap(x, width = 15))
    }, height = 600)
    
    output$cancerSummaryTable <- DT::renderDataTable(datatable(x, rownames = FALSE, selection = "none",
                                                               options = list(paging = FALSE, scrollY = 500)))
  }
})



####################################################################################
# Not used
####################################################################################

# update MeshTerms graph
# To do: no guarantee selected will be displayed if > 10 with same frequency

observe({
  cat("in plot observe...\n")
  
  if (!is.null(diseaseSummary$dat)) {
    output$DiseaseGraph <- renderPlot({
      cat("rendering plot...\n")
      
      # put levels in sorted order for plotting
      diseaseSummary$dat$Term <- factor(diseaseSummary$dat$Term, levels = diseaseSummary$dat$Term[order(diseaseSummary$dat$Frequency)])
      
      # get top 10 levels
      x <- levels(diseaseSummary$dat$Term)
      x <- rev(x)
      m2 <- min(10, length(x))
      
      x <- x[1:m2]
      
      x <- subset(diseaseSummary$dat, Term %in% x)
      
      # update levels to remove any no longer included
      x$Term <- factor(x$Term)
      
      colors <- rep("darkblue", nrow(x))
      print(diseaseSummary$selectedID)
      
      if (!is.null(diseaseSummary$selectedID)) {
        m <- match(diseaseSummary$selectedID, x$MeshID)
        term <- x$Term[m]
        m <- match(term, levels(x$Term))
        colors[m] <- "darkred"
      }
      
      diseaseSummary$graphData <- x
      
      ggplot(x, aes(Term, Frequency)) + geom_bar(fill = colors, stat = "identity") +
        coord_flip()
    }, height = max(450, min(10, nrow(diseaseSummary$dat))*26))
  }
})



