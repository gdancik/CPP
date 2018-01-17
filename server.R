# Cancer Publication Portal

library(shiny)
library(DT) # requires development version for single row selection with datatables
library(DBI)
library(RMySQL)
library(ggplot2)

source("functions.R", local = TRUE)

GeneTable <- read.csv("data/human_genes.csv")
GeneTable$SYMBOL <- as.character(GeneTable$SYMBOL)

# some genes have duplicate IDs...we should combine, for now, remove
library(dplyr)
dups <- (GeneTable %>% count(SYMBOL) %>% filter(n > 1))$SYMBOL
GeneTable <- GeneTable %>% filter(!SYMBOL %in% dups)
rownames(GeneTable) <- GeneTable$SYMBOL



shinyServer(function(input, output, session) {

  output$shinyTitle <- renderText("Cancer Publication Portal")
  
  observe( {
    cat("selected = ", input$rbMeshLimits ,"\n")
  })
  
  # clear hover on mouse out of Mesh Graph
#  onevent("mouseout", "MeshGraph", {
#    meshSummary$hoverID <- NULL
#  })
  
  
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
        a("Go to Pubmed", href = src, target = "_blank")
      })
    } else {
      
      output$articleHeader <- renderUI({
      })  
    }
  
  })
  

  
  
  #############################################################
  # Analyze processed files and display wordcloud
  #############################################################

  meshSummary <- reactiveValues(dat = NULL, uniqueDat = NULL, selectedID = NULL, 
                                 selectedTerm = NULL, hoverID = NULL)
  geneSummary <- reactiveValues(dat = NULL, seletectedID = NULL, seletectedTerm = NULL)
  
  pmidList <- reactiveValues(pmids = NULL)
  
  resetReactiveValues <- function() {
    meshSummary$dat = NULL
    meshSummary$uniqueDat = NULL
    meshSummary$selectedID = NULL 
    meshSummary$selectedTerm = NULL
    meshSummary$hoverID = NULL
    pmidList$pmids <- NULL
    geneSummary$dat = NULL
    geneSummary$selectedID <- NULL
    geneSummary$selectedTerm <- NULL
  }
  
  clearSelectedGene <- function() {
    geneSummary$selectedID <- NULL
    geneSummary$selectedTerm <- NULL
  }
  
  clearSelectedMesh <- function() {
    meshSummary$selectedID <- NULL
    meshSummary$selectedTerm <- NULL
  }
  
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
      clearSelectedGene()
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
    clearSelectedGene()
  })
  
  # record click from geneSumary Table 
  observeEvent(input$geneResults_rows_selected, {
    s = input$geneResults_rows_selected
    cat("selected gene: ", s, "\n")
    if (length(s) > 0) {
      print(geneSummary$dat[s,])
    }
    if (s== 1) {
      return()
    }
    gene <- geneSummary$dat[s,1]
    gene <- gsub("\r", "", gene)
    cat("gene = ", gene, "\n")
    m <- match(gene, GeneTable$SYMBOL)
    geneSummary$selectedID <- GeneTable$GeneID[m]
    geneSummary$selectedTerm <- gene
    clearSelectedMesh()
  })
  
  

  # queries mesh terms - currently stores results in 
  # meshSummary$dat and meshSummary$uniqueDat
  retrieveMeshTerms <- function(meshID, limit) {
    con = dbConnect(MySQL(), dbname = "dcast", user = "root", password = "password")
    
    # query MeSH terms
    query = paste("SELECT count(MeshTerms.MeshID), MeshTerms.MeshID, MeshTerms.Term, MeshTerms.TreeID from MeshTerms")
    
    query <- paste(query, "INNER JOIN PubMesh ON MeshTerms.MeshID = PubMesh.MeshID",
                   "INNER JOIN PubGene ON PubMesh.PMID = PubGene.PMID",
                   "WHERE PubGene.GeneID = ", input$geneInput)
    
    if (!is.null(meshID)) {
      query <- paste0(query, " AND MeshTerms.MeshID = ", meshID)
    }
    
    if (limit == "cancer") {
      query <- paste(query, "AND TreeID LIKE 'C04.%'")
    } else if (limit != "none") {
      stop("limit of ", limit, " is not implemented")
    }
    
    query <- paste(query,  "GROUP BY MeshTerms.MeshID, MeshTerms.Term, MeshTerms.TreeID",
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
  
  
  # queries mesh terms - currently stores results in 
  # geneSummary$dat 
  retrieveGenes <- function(meshID, limit) {
    con = dbConnect(MySQL(), dbname = "dcast", user = "root", password = "password")
    
    # query MeSH terms
    query = paste("SELECT count(MeshTerms.MeshID), MeshTerms.MeshID, MeshTerms.Term, MeshTerms.TreeID from MeshTerms")
    
    query <- paste("select g.SYMBOL, count(g.SYMBOL) from Genes g",
    "INNER JOIN PubGene p on g.GeneID = p.GeneID",
    "INNER JOIN PubGene p2 ON p.PMID = p2.PMID",
    "where p2.GeneID = ", input$geneInput, 
    "group by g.SYMBOL",
    "order by count(g.SYMBOL) desc;")
    
    print(query)
    geneSummary$dat <- dbGetQuery(con, query)
    colnames(geneSummary$dat) <- c("Symbol", "Frequency")
    dbDisconnect(con)
    
  }
  
  
  # retreives articles for specific gene and (optional) meshID
  retrieveArticles <- function(meshID = NULL, geneID2 = NULL) {
    con = dbConnect(MySQL(), dbname = "dcast", user = "root", password = "password")
  
    if (!is.null(geneID2)) {
    
      query <- paste("SELECT PMID from PubGene",
                     "where GeneID= ", input$geneInput, " or GeneID = ", geneID2, 
                     "GROUP BY PMID HAVING count(PMID) > 1;"
      )
    } else {
    
      # query PMIDs
      query = paste("SELECT distinct(PubGene.PMID) from PubGene", 
                  "INNER JOIN PubMesh ON PubGene.PMID = PubMesh.PMID",
                  "WHERE PubGene.GeneID = ", input$geneInput)
    
      # add MeSH filter if term is selected
      if (!is.null(meshID)) {
        query <- paste0(query, " AND MeshID = ", meshID)
      }
    
      query <- paste(query, "ORDER BY PubGene.PMID DESC")
    
    }

    print("pmid query: ")
    print(query)
    pmids <- dbGetQuery(con, query)
    dbDisconnect(con)
    rownames(pmids) <- NULL
    pmidList$pmids <- pmids
  
  }
  
    geneIDs = c(1, 2, 3, 9, 153,2261, 178)
    geneIDs <- GeneTable$GeneID
    names(geneIDs) <- GeneTable$SYMBOL
    
    updateSelectizeInput(session, "geneInput", choices = geneIDs, selected = 178, server = TRUE)

    
    # on initial search
    observeEvent(
      {input$btnGeneSearch
      input$rbMeshLimits},{

        if (is.null(input$geneInput) | input$geneInput == "") return()
        
        resetReactiveValues()
        
        shinyjs::html("bar-text", "Retreiving Articles, please wait...")
        retrieveArticles()
        
        shinyjs::html("bar-text", "Retreiving MeSH terms, please wait...")
        retrieveMeshTerms(NULL, input$rbMeshLimits)
        
        shinyjs::html("bar-text", "Retreiving related genes, please wait...")
        retrieveGenes()
        
        
        
        shinyjs::html("bar-text", "Please wait...")
        
    })
    
    # update PMID list when Selected Mesh Term changes
    observe( {
      if (is.null(meshSummary$selectedID) & is.null(geneSummary$selectedID)) {
        return()
      }
      retrieveArticles(paste0("'", meshSummary$selectedID, "'"), geneSummary$selectedID)
    })
    
    
    #update geneSummary
    observe ({
      output$geneResults <- renderDataTable(datatable(geneSummary$dat, rownames = FALSE,
                                            selection = "single",
                                            options = list(paging = FALSE, scrollY = 300)))
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
      if (is.null(meshSummary$selectedID) & is.null(geneSummary$selectedID)) return(HTML("<h4>Filters: (none)</h4>"))
      else if (!is.null(meshSummary$selectedID)) {
        HTML("<h4>Filter by MeSH Term: <span style = \"color:red\">", meshSummary$selectedTerm, "</span></h4>")
      } else if (!is.null(geneSummary$selectedID)) {
        HTML("<h4>Filter by additional gene: <span style = \"color:red\">", geneSummary$selectedTerm, "</span></h4>")
      }
    })
    
})
