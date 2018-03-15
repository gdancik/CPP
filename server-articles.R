# server-articles

observe ({
  if (!is.null(pmidList$pmids)) {
    num <- min(10,nrow(pmidList$pmids))
    pmids <- pmidList$pmids$PMID[1:num]
    print(pmids)
    pmids <- paste0(pmids, "[uid]")
    pmids <- paste0(pmids, collapse = " or ")
    #src <- paste0("https://www.ncbi.nlm.nih.gov/pubmed/?term=", pmids)
    src <- paste0("https://www.ncbi.nlm.nih.gov/CBBresearch/Lu/Demo/PubTator/index.cgi?searchtype=PubMed_Search&query=", pmids)
    #src <- paste0("https://www.livejournal.com/")    
    output$articles <- renderUI({
      my_test <- tags$html(tags$iframe(id = "iframeid", src=src, style = "width:100%;", height = 600))
      print(my_test)
      my_test
    })
    
    output$articleHeader <- renderUI({
      div(
        div(style="float: left; width = 50",
            a("Go to PubTator", href = src, target = "_blank")
        ), 
        shinyjs::hidden(div(id ="linkShowSummaries", style = "display: in-line block", 
                            HTML("&nbsp; | &nbsp;"),
                            a("(Show Summaries)", href = "#", id = "togglePubs", style = "color: maroon")
        ))
      )
    })
  } else {
    
    output$articleHeader <- renderUI({
      # a(href = "#", id = "togglePubs", "(expand)")
    })  
  }
  
})

