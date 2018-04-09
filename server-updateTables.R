# server-updateTables.R

#update geneSummary
observe ({
  
  if (is.null(geneSummary$dat)) {
    return()
  }
  displayGeneSummaryTable()
})


displayGeneSummaryTable <- function() {
  geneSummary$dat$Symbol <- gsub("\r", "", geneSummary$dat$Symbol)
  
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

# update PMID table
observe ({
  output$articleTable <- DT::renderDataTable(DT::datatable(pmidList$pmids, rownames = FALSE,
                                                           selection = "none", 
                                                           options = list(lengthChange = FALSE)))
})


# update MeshTerms table and graph
observe ({
  
  m <- match(diseaseSummary$selectedID, diseaseSummary$uniqueDat$MeshID)
  cat("click, m = ", m, "\n")
  isolate({
    selection = list(mode = "multiple", selected = m, target = "row")
  
  
    output$diseaseResults <- DT::renderDataTable(datatable(diseaseSummary$uniqueDat, rownames = FALSE, 
                                                         selection = selection,
                                                         options = list(paging = FALSE, scrollY = 300)))
  
    #output$diseaseHierarchy <- renderUI(HTML(displayMesh(diseaseSummary$dat$TreeID,
    #                                                   diseaseSummary$dat$Frequency)))
  
  })
  cat("...done observe\n")
})

# update MeshTerms graph
# To do: no guarantee selected will be displayed if > 10 with same frequency

observe({
  cat("in plot observe...\n")
  
  if (!is.null(diseaseSummary$uniqueDat)) {
    output$DiseaseGraph <- renderPlot({
      cat("rendering plot...\n")
      
      # put levels in sorted order for plotting
      diseaseSummary$uniqueDat$Term <- factor(diseaseSummary$uniqueDat$Term, levels = diseaseSummary$uniqueDat$Term[order(diseaseSummary$uniqueDat$Frequency)])
      
      # get top 10 levels
      x <- levels(diseaseSummary$uniqueDat$Term)
      x <- rev(x)
      m2 <- min(10, length(x))
      
      x <- x[1:m2]
      
      x <- subset(diseaseSummary$uniqueDat, Term %in% x)
      
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
    }, height = max(450, min(10, nrow(diseaseSummary$uniqueDat))*26))
  }
})


# update cancer graph
observe({
  cat("in cancer plot observe...\n")
  
  if (!is.null(diseaseSummary$uniqueDat)) {
    
    con = dbConnect(MySQL(), group = "CPP")
    
    qry <- paste0("select MeshID, TreeID from MeshTerms where MeshID IN ", 
                  cleanseList(diseaseSummary$uniqueDat$MeshID),
                  " AND MeshTerms.TreeID LIKE \"C04.%\";"
    )
    
    cancerTerms <- dbGetQuery(con, qry)
    dbDisconnect(con)
    
    
    
    x <- subset(diseaseSummary$uniqueDat, MeshID %in% cancerTerms$MeshID)
    
    # put levels in sorted order for plotting
    x$Term <- factor(x$Term, levels = x$Term[order(x$Frequency)])
    
    
    output$CancerGraph <- renderPlot({
      
      xx <- subset(x, Term%in% rev(levels(x$Term))[1:min(20,nrow(x))])
      title <- paste("Cancer-related publications mentioning", selected$geneSymbol)
      if (nrow(xx)!=nrow(x)) {
        title <- paste(title, " (top 20 shown)")
      }
      
      if (!is.null(input$filterDisease) | !is.null(input$filterChem) | !is.null(input$filterGenes)) {
        title <- paste(title, "\n(summary includes additional filters)")
      }
      
      ggplot(xx, aes(Term, Frequency, fill = Term)) + geom_bar(stat = "identity") +
        coord_flip() + theme(legend.position = "none") + xlab("") + ylab("# of articles") +
        ggtitle(title) +
          theme(plot.title = element_text(face = "bold"),  
                axis.title = element_text(face = "bold"),
                axis.text = element_text(face = "bold"))
    }, height = 600)
    
    output$cancerSummaryTable <- DT::renderDataTable(datatable(x, rownames = FALSE, selection = "none",
                                                           options = list(paging = FALSE, scrollY = 500)))
  }
})



# update chem table
observe ({
  m <- match(chemSummary$selectedID, chemSummary$uniqueDat$MeshID)
  isolate({
    selection = list(mode = "multiple", selected = m, target = "row")
  })
  output$chemResults <- DT::renderDataTable(datatable(chemSummary$uniqueDat, rownames = FALSE, 
                                                      selection = selection,
                                                      options = list(paging = FALSE, scrollY = 300)))
})
