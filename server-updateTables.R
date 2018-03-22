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
  output$articleTable <- DT::renderDataTable(DT::datatable(pmidList$pmids, rownames = FALSE))
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
