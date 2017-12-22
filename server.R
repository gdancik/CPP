library(shiny)
library(data.table)
library(DT) # requires development version for single row selection with datatables
library(DBI)
library(RMySQL)
library(ggplot2)

source("functions.R", local = TRUE)



shinyServer(function(input, output, session) {

  
  # clear hover on mouse out of Mesh Graph
  onevent("mouseout", "MeshGraph", {
    meshSummary$hoverID <- NULL
  })
  
  observeEvent( input$btnMeshFilter,{
    cat("current hover = ", input$MeshGraph_hover$y)
  })
  
  observe ({
  if (!is.null(pmidList$pmids)) {
  output$articles <- renderUI({
    
    return(NULL)
    num <- min(10,nrow(pmidList$pmids))
    pmids <- pmidList$pmids$PMID[1:num]
    print(pmids)
    pmids <- paste0(pmids, "[uid]")
    pmids <- paste0(pmids, collapse = " or ")
    src <- paste0("https://www.ncbi.nlm.nih.gov/pubmed/?term=", pmids)
    
    #my_test <- tags$iframe(src="https://www.ncbi.nlm.nih.gov/pubmed/?term=1%5Buid%5D+or+10%5Buid%5D", height=600, width=535)
   # my_test <- tags$iframe(src="https://www.ncbi.nlm.nih.gov/pubmed/", style = "width:100%", height = 600)
    my_test <- tags$iframe(src=src, style = "width:100%", height = 600)
    print(my_test)
    my_test
  })
  }
  
  })
  

  
  
  #############################################################
  # Analyze processed files and display wordcloud
  #############################################################

  meshSummary <- reactiveValues(dat = NULL, uniqueDat = NULL, selectedID = NULL, 
                                 selectedTerm = NULL, hoverID = NULL)
  pmidList <- reactiveValues(pmids = NULL)
  
  resetReactiveValues <- function() {
    meshSummary$dat = NULL
    meshSummary$uniqueDat = NULL
    meshSummary$selectedID = NULL 
    meshSummary$selectedTerm = NULL
    meshSummary$hoverID = NULL
    pmidList$pmids <- NULL
  }
  
  
  
  # record hover from Mesh Graph (store in meshSummary reactive)
  observeEvent(input$MeshGraph_hover$x, {
    cat("selected = ", input$MeshGraph_hover$x)
    # get Mesh from selected graph
    if (!is.null(input$MeshGraph_hover$x)) {
      lvls <- levels(meshSummary$uniqueDat$Term)
      name <- lvls[round(input$MeshGraph_hover$y)]
      m <- match(name, meshSummary$uniqueDat$Term)
      meshSummary$hoverID <- meshSummary$uniqueDat$MeshID[m]
    }
  })
  
  
  # record click from Mesh Graph (store in meshSummary reactive)
  observeEvent(input$MeshGraph_click$y, {
    # get Mesh from selected graph
    if (!is.null(input$MeshGraph_click$x)) {
      lvls <- levels(meshSummary$uniqueDat$Term)
      name <- lvls[round(input$MeshGraph_click$y)]
      cat("You've selected <code>", name, "</code>\n")
      m <- match(name, meshSummary$uniqueDat$Term)
      meshSummary$selectedID <- meshSummary$uniqueDat$MeshID[m]
      meshSummary$selectedTerm <- as.character(meshSummary$uniqueDat$Term)[m]
    }
  })
  
  # record click from Mesh Table (store in meshSummary reactive)
  observeEvent(input$meshResults_rows_selected, {
    s = input$meshResults_rows_selected
    cat("selected: ", s, "\n")
    if (length(s) > 0) {
      print(meshSummary$uniqueDat[s,])
    }
    meshSummary$selectedID <- meshSummary$uniqueDat$MeshID[s]
    meshSummary$selectedTerm <- as.character(meshSummary$uniqueDat$Term)[s]
  })
  
  

  # queries mesh terms - currently stores results in 
  # meshSummary$dat and meshSummary$uniqueDat
  retrieveMeshTerms <- function(meshID = NULL) {
    con = dbConnect(MySQL(), dbname = "dcast", user = "root", password = "password")
    
    # query MeSH terms
    query = paste("SELECT count(MeshTerms.MeshID), MeshTerms.MeshID, MeshTerms.Term, MeshTerms.TreeID from MeshTerms")
    
    query <- paste(query, "INNER JOIN PubMesh ON MeshTerms.MeshID = PubMesh.MeshID",
                   "INNER JOIN PubGene ON PubMesh.PMID = PubGene.PMID",
                   "WHERE PubGene.GeneID = ", input$geneInput)
    
    if (!is.null(meshID)) {
      query <- paste0(query, " AND MeshTerms.MeshID = ", meshID)
    }
    
    query <- paste(query,  "AND TreeID LIKE 'C04.%' GROUP BY MeshTerms.MeshID, MeshTerms.Term, MeshTerms.TreeID",
                   "ORDER BY count(MeshTerms.MeshID) desc")
    
    
    print(query)
    meshSummary$dat <- dbGetQuery(con, query)
    dbDisconnect(con)
    
    if (!is.null(meshSummary$dat)) {
      colnames(meshSummary$dat)[1] = "Frequency"
    }
    
    rownames(meshSummary$dat) = NULL
    
    meshSummary$uniqueDat <- unique(meshSummary$dat[,1:3])
    
  }
  
  # retreives articles for specific gene and (optional) meshID
  retrieveArticles <- function(meshID = NULL) {
    con = dbConnect(MySQL(), dbname = "dcast", user = "root", password = "password")
  
    # query PMIDs
    query = paste("SELECT distinct(PubGene.PMID) from PubGene", 
                  "INNER JOIN PubMesh ON PubGene.PMID = PubMesh.PMID",
                  "WHERE PubGene.GeneID = ", input$geneInput)
    
    # add MeSH filter if term is selected
    if (!is.null(meshID)) {
      query <- paste0(query, " AND MeshID = ", meshID)
    }
    
    query <- paste(query, "ORDER BY PubGene.PMID DESC")
    
    print("pmid query: ")
    print(query)
    pmids <- dbGetQuery(con, query)
    dbDisconnect(con)
    rownames(pmids) <- NULL
    pmidList$pmids <- pmids
  
  }
  
    geneIDs = c(1, 2, 3, 9, 153,2261, 178)
    
    updateSelectizeInput(session, "geneInput", choices = geneIDs, selected = 178, server = TRUE)

    
    # on initial search
    observeEvent(input$btnGeneSearch,{
      resetReactiveValues()
      retrieveMeshTerms()
      retrieveArticles()
    })
    
    # update PMID list when Selected Mesh Term changes
    observe( {
      if (is.null(meshSummary$selectedID)) {
        return()
      }
      retrieveArticles(paste0("'", meshSummary$selectedID, "'"))
    })
    
    
    # update PMID table
    observe ({
      output$articleTable <- renderDataTable(datatable(pmidList$pmids, rownames = FALSE))
    })
    
    
    # update MeshTerms table and graph
    observe ({
      output$meshResults <- renderDataTable(datatable(meshSummary$uniqueDat, rownames = FALSE, 
                                                      selection = "single",
                                                      options = list(paging = FALSE, scrollY = 300)))
    
      output$meshHierarchy <- renderUI(HTML(displayMesh(meshSummary$dat$TreeID,
                                                      meshSummary$dat$Frequency)))
    })

    observe({
    if (!is.null(meshSummary$uniqueDat)) {
      output$MeshGraph <- renderPlot({
        #cat("rendering plot...\n")
        
        meshSummary$uniqueDat$Term <- factor(meshSummary$uniqueDat$Term, levels = meshSummary$uniqueDat$Term[order(meshSummary$uniqueDat$Frequency)])
        colors <- rep("darkblue", nrow(meshSummary$uniqueDat))
        print(meshSummary$selectedID)
        
        if (!is.null(meshSummary$hoverID)) {
          m <- match(meshSummary$hoverID, meshSummary$uniqueDat$MeshID)
          term <- meshSummary$uniqueDat$Term[m]
          m <- match(term, levels(meshSummary$uniqueDat$Term))
          colors[m] <- "yellow"
        }
        
        if (!is.null(meshSummary$selectedID)) {
          m <- match(meshSummary$selectedID, meshSummary$uniqueDat$MeshID)
          term <- meshSummary$uniqueDat$Term[m]
          m <- match(term, levels(meshSummary$uniqueDat$Term))
          colors[m] <- "darkred"
        }
        
        ggplot(meshSummary$uniqueDat, aes(Term, Frequency)) + geom_bar(fill = colors, stat = "identity") +
          coord_flip()
      }, height = max(450, nrow(meshSummary$uniqueDat)*26))
    }
    })
    
    
    output$test <- renderText({
        HTML("<h2> how are you? </h2>")
    })
    
  
    #update selected display
    output$x_value <- renderText({
      if (is.null(meshSummary$selectedID)) return(HTML("<h4>Filter by MeSH Term: (none)</h4>"))
      else {
        HTML("<h4>Filter by MeSH Term: <code>", meshSummary$selectedTerm, "</code></h4>")
      }
    })
    
    
})
