# server-articles

observeEvent ( input$articleTable_rows_current, {
  shinyjs::show("btnPubTator")
})

srcPubTator <- reactive ({
  if (!is.null(pmidList$pmids)) {
    pmids <- pmidList$pmids$PMID[input$articleTable_rows_current]
    pmids <- paste0(pmids, "[uid]")
    pmids <- paste0(pmids, collapse = " or ")
    src <- paste0("https://www.ncbi.nlm.nih.gov/CBBresearch/Lu/Demo/PubTator/index.cgi?searchtype=PubMed_Search&query=", pmids)
    return(src)
  }
  return (NULL)
})

observeEvent ( input$btnPubTator,{
  
   src <- srcPubTator()
   
   if (!is.null(src)) {
     shinyjs::hide("btnPubTator")
      output$articles <- renderUI({
          my_test <- tags$html(tags$iframe(id = "iframeid", src=src, style = "width:100%;", height = 800))
          print(my_test)
          my_test
      })
    }
  }
)

observeEvent ( input$btnPubTatorGo, {
  src <- srcPubTator()
  cat("src = ", src)
  if (!is.null(src)) {
    js <- paste0("window.open('", src, "', '_blank');")
#    browseURL(URLencode(src))
    runjs(js) 
  }
})


