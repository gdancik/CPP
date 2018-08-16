
writeDownloadHeader <- function(file) {
  f <- createFilterString()
  
  if (f == "") {
    f <- paste0("\"# Summary for ", selected$geneSymbol, " (no filters)\"")
  } else {
    f <- gsub("<.+?>", "", f)
    f <- paste0("\"# Summary for ", selected$geneSymbol, " with filters -- ", f, "\"")
  }
  
  write(f, file)
}


output$downloadCancerTypesData <- downloadHandler(
      "cancerTypesSummary.csv",
      content = function(file) {
        writeDownloadHeader(file)
        write.table(diseaseSummary$dat, file, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)  
      },
      contentType = "text/csv"
)

output$downloadDrugTreatmentsData <- downloadHandler(
  "drugSummary.csv",
  content = function(file) {
    writeDownloadHeader(file)
    write.table(chemSummary$dat, file, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)  
  },
  contentType = "text/csv"
)

output$downloadMutationsData <- downloadHandler(
  "mutationSummary.csv",
  content = function(file) {
    writeDownloadHeader(file)
    write.table(mutationSummary$dat, file, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)  
  },
  contentType = "text/csv"
)

output$downloadGenesData <- downloadHandler(
  "geneSummary.csv",
  content = function(file) {
    writeDownloadHeader(file)
    write.table(geneSummary$dat, file, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)  
  },
  contentType = "text/csv"
)




