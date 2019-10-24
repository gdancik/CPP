
writeDownloadHeader <- function(header, file) {
  f <- createFilterString(newlines = TRUE)
  cancers <- createCancerString()

  if (is.null(cancers)) {
    cancers <- "(none)"
  }
  cancers <- paste0("Any from selected cancers: ", cancers)
  
  
  # if (f == "") {
  #   f <- paste0("\"# Summary for ", selected$geneSymbol, " (no filters)\"")
  # } else {
  #   f <- gsub("<.+?>", "", f)
  #   f <- paste0("\"# Summary for ", selected$geneSymbol, " with filters: ", f, "\"")
  # }
  
  h <- paste0("# ", header, "\n\"# Selected gene: ", selectedGeneName(), "\"")
  cancers <- paste0("\"# ", cancers, "\"") 
  f <- gsub("<.+?>", "", f)
  
  write(h, file)
  write(cancers, file, append = TRUE)
  
  if (f == "") {
    return()
  }
  
  catn("f = ")
  catn(f)
  
  f <- strsplit(f, "\n")[[1]]
  f2 <- paste0("\"# ", f, "\"")
  write(f2, file, append = TRUE)
  write("\n", file, append = TRUE)
  
}


output$saveCancerTypes <- downloadHandler(
  "selectedCancerTypes.csv",
  content = function(file) {
    choices <- cancerSelectionChoices()
    
    m <- match(input$cancerType, choices)
    choices <- choices[m]
    write.table(data.frame(unlist(choices), names(choices), row.names = NULL), file, sep = ",", row.names = FALSE, col.names = FALSE)  
    
    msg <- paste0("Cancer types saved to your download directory")
    showNotification(msg, duration = 4, closeButton = TRUE,
                     id = "fileSaveNotification", type = "message")
    
  },
  contentType = "text/csv"
)

output$downloadPMIDs <- downloadHandler(
      "pmids.csv",
      content = function(file) {
        writeDownloadHeader("PMID list", file)
        write.table(pmidList$pmids, file, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)  
      },
      contentType = "text/csv"
)

output$downloadCancerTypesData <- downloadHandler(
      "cancerTypesSummary.csv",
      content = function(file) {
        updateCancerTypesSummary()
        writeDownloadHeader("Cancer Type Summary", file)
        write.table(diseaseSummary$dat, file, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)  
      },
      contentType = "text/csv"
)


output$downloadCancerTermsData <- downloadHandler(
  "cancerTerms.csv",
  content = function(file) {
    updateCancerTermsSummary()
    writeDownloadHeader("Cancer Terms Summary", file)
    write.table(cancerTermSummary$dat, file, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)  
  },
  contentType = "text/csv"
)


output$downloadDrugTreatmentsData <- downloadHandler(
  "drugSummary.csv",
  content = function(file) {
    updateChemicalSummary()
    writeDownloadHeader("Drug Summary", file)
    write.table(chemSummary$dat, file, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)  
  },
  contentType = "text/csv"
)

output$downloadMutationsData <- downloadHandler(
  "mutationSummary.csv",
  content = function(file) {
    updateMutationSummary()
    writeDownloadHeader("Mutation Summary", file)
    write.table(mutationSummary$dat, file, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)  
  },
  contentType = "text/csv"
)

output$downloadGenesData <- downloadHandler(
  "geneSummary.csv",
  content = function(file) {
    updateAdditionalGenesSummary()
    writeDownloadHeader("Gene Summary", file)
    write.table(geneSummary$dat, file, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)  
  },
  contentType = "text/csv"
)




