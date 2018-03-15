# server-updateTables.R

#update geneSummary
observe ({
  output$geneResults <- DT::renderDataTable(datatable(geneSummary$dat, rownames = FALSE,
                                                      selection = "single",
                                                      options = list(paging = FALSE, scrollY = 300)))
})

# update PMID table
observe ({
  output$articleTable <- DT::renderDataTable(DT::datatable(pmidList$pmids, rownames = FALSE))
})


# update MeshTerms table and graph
observe ({
  
  m <- match(diseaseSummary$selectedID, diseaseSummary$uniqueDat$MeshID)
  isolate({
    selection = list(mode = "multiple", selected = m, target = "row")
  })
  
  output$diseaseResults <- DT::renderDataTable(datatable(diseaseSummary$uniqueDat, rownames = FALSE, 
                                                         selection = selection,
                                                         options = list(paging = FALSE, scrollY = 300)))
  
  output$diseaseHierarchy <- renderUI(HTML(displayMesh(diseaseSummary$dat$TreeID,
                                                       diseaseSummary$dat$Frequency)))
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
