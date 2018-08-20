# server-articles

shinyjs::hide("pagesNav")

srcPubTator <- reactive ({
  if (!is.null(pmidList$pmids)) {
    pmids <- pmidList$pmids$PMID[pmidList$pagePMIDS]
    pmids <- paste0(pmids, "[uid]")
    pmids <- paste0(pmids, collapse = " or ")
    src <- paste0("https://www.ncbi.nlm.nih.gov/CBBresearch/Lu/Demo/PubTator/index.cgi?searchtype=PubMed_Search&query=", pmids)
    
    if (!is.null(src)) {
      output$articles <- renderUI({
        my_test <- tags$html(tags$iframe(id = "iframeid", src=src, style = "width:100%;", height = 800))
        print(my_test)
        my_test
      })
    }
  }
  
  return (NULL)
})

getPages <- function(pmids){
  
  pages <- ceiling(length(pmids)/10)
  return(pages)
  
}

pageHandler <- function(direction = "NULL"){
  
  observe({
    if(!is.null(pmidList$pages)){
      pages = pmidList$pages
      if(pmidList$currentPage == 1){
        shinyjs::disable("btnPageBackward")
      }
      else if(pmidList$currentPage == pages){
        shinyjs::disable("btnPageForward")
      }
      else{
        shinyjs::enable("btnPageBackward")
        shinyjs::enable("btnPageForward")
      }
    }
    else{
      return()
    }
    
  })
  
  switch(direction,
         forward={
           pmidList$currentPage <- pmidList$currentPage + 1
         },
         backward={
           pmidList$currentPage <- pmidList$currentPage - 1
         })
  
  output$pageNumbers <- renderText({
    paste("Page", pmidList$currentPage, "of", pmidList$pages)
  })
  
  
  page <- pmidList$currentPage
  
  if(page != pmidList$pages){
    beg <- (page * 10) - 9
    end <- (page * 10)
    pmidList$pagePMIDS <- c(beg:end)
  }
  else{
    beg <- (page * 10) - 9
    end <- (length(pmidList$pmids$PMID) %% 10) + (beg - 1)
    pmidList$pagePMIDS <- c(beg:end)
  }
  
  output$articleResults <- renderText({
    beg <- pmidList$pagePMIDS[1]
    end <- beg+length(pmidList$pagePMIDS)-1
    paste("Showing articles", beg, " to ", end, " of", length(pmidList$pmids$PMID))
  })
  
  srcPubTator()
  
}



observeEvent( input$btnPageForward, {
  
  pageHandler("forward")
  
})

observeEvent( input$btnPageBackward, {
  
  pageHandler("backward")
  
})

observeEvent ( input$btnPubTator,{
  
  pmidList$pages <- getPages(pmidList$pmids$PMID)
  pmidList$currentPage <- 1
  shinyjs::show("pagesNav")
  pageHandler()
  
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



