# server-updateTables.R

#update geneSummary
observe ({
  
  if (is.null(geneSummary$dat)) {
    return()
  }
  geneSummary$dat$Symbol <- gsub("\r", "", geneSummary$dat$Symbol)
  m <- match(geneSummary$selectedTerm, geneSummary$dat$Symbol)
  
  isolate({
    selection = list(mode = "multiple", selected = m, target = "row")
    cat("selected gene rows = ", m, "\n")
  })
  output$geneResults <- DT::renderDataTable(datatable(geneSummary$dat, rownames = FALSE,
                                                      selection = selection,
                                                      options = list(paging = FALSE, scrollY = 300)))
})

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
  
})

# update MeshTerms graph
# To do: no guarantee selected will be displayed if > 10 with same frequency
observe({
  if (!is.null(diseaseSummary$uniqueDat)) {
    output$DiseaseGraph <- renderPlot({
      #cat("rendering plot...\n")
      
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
    }, height = max(450, nrow(x)*26))
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
