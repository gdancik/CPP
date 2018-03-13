# Cancer Publication Portal

library(shiny)
library(DT) # requires development version for single row selection with datatables
library(DBI)
library(RMySQL)
library(ggplot2)

source("functions.R", local = TRUE)
source("sql_functions.R", local = TRUE)
source("setResults.R", local = TRUE)

GeneTable <- read.csv("data/human_genes.csv")
GeneTable$SYMBOL <- as.character(GeneTable$SYMBOL)

# some genes have duplicate IDs...we should combine, for now, remove
library(dplyr)
dups <- (GeneTable %>% count(SYMBOL) %>% filter(n > 1))$SYMBOL
GeneTable <- GeneTable %>% filter(!SYMBOL %in% dups)
rownames(GeneTable) <- GeneTable$SYMBOL



shinyServer(function(input, output, session) {

  #shinyjs::toggleClass("x_value", "shiny-html-output")
  #shinyjs::toggleClass("x_value", "shiny-bound-output")
  
  shinyjs::toggle("tspSummary")
  #shinyjs::toggle("x_value")
  
  output$shinyTitle <- renderText("Cancer Publication Portal")
  
  observe( {
    cat("selected = ", input$rbDiseaseLimits ,"\n")
  })
  

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
  
  
  #############################################################
  # Analyze processed files and display wordcloud
  #############################################################

  diseaseSummary <- reactiveValues(dat = NULL, uniqueDat = NULL, selectedID = NULL, 
                                 selectedTerm = NULL, hoverID = NULL)
  chemSummary <- reactiveValues(dat = NULL, uniqueDat = NULL, selectedID = NULL, 
                                   selectedTerm = NULL, hoverID = NULL)
  geneSummary <- reactiveValues(dat = NULL, selectedID = NULL, selectedTerm = NULL)
  
  pmidList <- reactiveValues(pmids = NULL)
  
  lastTab <- reactiveValues(tab = NULL)
  
  resetReactiveValues <- function() {
    diseaseSummary$dat = NULL
    diseaseSummary$uniqueDat = NULL
    diseaseSummary$selectedID = NULL 
    diseaseSummary$selectedTerm = NULL
    diseaseSummary$hoverID = NULL
    pmidList$pmids <- NULL
    geneSummary$dat <- NULL
    geneSummary$selectedID <- NULL
    geneSummary$selectedTerm <- NULL
    chemSummary$dat <- NULL
    chemSummary$selectedID <- NULL
    chemSummary$selectedTerm <- NULL
    
  }
  
  clearSelectedGene <- function() {
    geneSummary$selectedID <- NULL
    geneSummary$selectedTerm <- NULL
  }
  
  clearSelectedDisease <- function() {
    diseaseSummary$selectedID <- NULL
    diseaseSummary$selectedTerm <- NULL
  }
  
  clearSelectedChem <- function() {
    chemSummary$selectedID <- NULL
    chemSummary$selectedTerm <- NULL
  }
  
  # record click from Disease Graph (store in diseaseSummary reactive)
  observeEvent(input$DiseaseGraph_click$y, {
    # get Disease from selected graph
    if (!is.null(input$DiseaseGraph_click$x)) {
      lvls <- levels(diseaseSummary$uniqueDat$Term)
      name <- lvls[round(input$DiseaseGraph_click$y)]
      cat("You've selected <code>", name, "</code>\n")
      m <- match(name, diseaseSummary$uniqueDat$Term)
      diseaseSummary$selectedID <- append(diseaseSummary$selectedID, diseaseSummary$uniqueDat$MeshID[m])
      diseaseSummary$selectedTerm <- append(diseaseSummary$selectedTerm, as.character(diseaseSummary$uniqueDat$Term)[m])

#      clearSelectedGene()
#      clearSelectedChem()
    }
  })
  
  
  togglePubs <- function() {
    shinyjs::toggle("colSummary", anim = TRUE)
    shinyjs::toggleClass("colPubs", "col-sm-6")
    shinyjs::toggleClass("colPubs", "col-sm-12")
    
    shinyjs::toggleClass("colSummary", "col-sm-6")
    shinyjs::toggleClass("colSummary", "col-sm-12")
    
    
    print("clicked")
    
    
  }
  
  shinyjs::onclick("togglePubs", {
    togglePubs()
    shinyjs::toggle("linkShowSummaries")
  })
  
  observeEvent(input$tspSummary, {
    cat("clicked on: ", input$tspSummary, "\n")
    if (input$tspSummary == "hide") {
      togglePubs()
      shinyjs::toggle("linkShowSummaries")
      updateTabsetPanel(session, "tspSummary", lastTab$tab)
    } else {
      lastTab$tab = input$tspSummary
    }
  })
  
  
  # record click from Disease Table (store in diseaseSummary reactive)
  observeEvent(input$diseaseResults_rows_selected, {
    s = input$diseaseResults_rows_selected
    cat("selected: ", s, "\n")
    if (length(s) > 0) {
      print(diseaseSummary$uniqueDat[s,])
    }
    diseaseSummary$selectedID <- append(diseaseSummary$selectedID, diseaseSummary$uniqueDat$MeshID[s])
    diseaseSummary$selectedTerm <- append(diseaseSummary$selectedTerm, as.character(diseaseSummary$uniqueDat$Term)[s])

    #clearSelectedGene()
    #clearSelectedChem()
  })
  
  # record click from Chem Table (store in chemSummary reactive)
  observeEvent(input$chemResults_rows_selected, {
    s = input$chemResults_rows_selected
    cat("selected: ", s, "\n")
    if (length(s) > 0) {
      print(chemSummary$uniqueDat[s,])
    }
    chemSummary$selectedID <- append(chemSummary$selectedID, chemSummary$uniqueDat$MeshID[s])
    chemSummary$selectedTerm <- append(chemSummary$selectedTerm, s.character(chemSummary$uniqueDat$Term)[s])
    #clearSelectedGene()
    #clearSelectedDisease()
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
    gene <- geneSummary$dat[s,2]
    cat("gene = ", gene, "\n")
    gg <- geneSymbolToID(gene, GeneTable)
    geneSummary$selectedID <- append(geneSummary$selectedID, gg$ID)
    geneSummary$selectedTerm <- append(geneSummary$selectedTerm, gg$Symbol)
    cat("set selected term to: ", gg$Symbol,"\n")
    
    #clearSelectedDisease()
    #clearSelectedChem()
  })

  
  geneSymbolToID <- function(symbols, GeneTable) {
    symbols <- gsub("\r", "", symbols)
    m <- match(symbols, GeneTable$SYMBOL)
    data.frame(Symbol = as.character(symbols), ID = as.character(GeneTable$GeneID)[m],
               stringsAsFactors = FALSE)
  }
  
  
  # queries mesh terms - currently stores results in 
  # diseaseSummary$dat and diseaseSummary$uniqueDat
  retrieveDiseases <- function(meshID, limit) {
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
    diseaseSummary$dat <- dbGetQuery(con, query)
    dbDisconnect(con)
    
    if (!is.null(diseaseSummary$dat)) {
      colnames(diseaseSummary$dat)[1] = "Frequency"
    }
    
    rownames(diseaseSummary$dat) = NULL
    
    setDiseaseResults(session, unique(diseaseSummary$dat[,1:3]), diseaseSummary)
    
  }
  
  retrieveChemicals <- function(meshID) {
    
    cat("\n\nGETTING CHEMICALS!!\n\n")
    con = dbConnect(MySQL(), dbname = "dcast", user = "root", password = "password")
    
    # query MeSH terms
    query = paste("SELECT count(MeshTerms.MeshID), MeshTerms.MeshID, MeshTerms.Term, MeshTerms.TreeID from MeshTerms")
    
    query <- paste(query, "INNER JOIN PubChem ON MeshTerms.MeshID = PubChem.MeshID",
                   "INNER JOIN PubGene ON PubChem.PMID = PubGene.PMID",
                   "WHERE PubGene.GeneID = ", input$geneInput)
    
    if (!is.null(meshID)) {
      query <- paste0(query, " AND MeshTerms.MeshID = ", meshID)
    }
    
    query <- paste(query,  "GROUP BY MeshTerms.MeshID, MeshTerms.Term, MeshTerms.TreeID",
                   "ORDER BY count(MeshTerms.MeshID) desc")
    print(query)
    chemSummary$dat <- dbGetQuery(con, query)
    dbDisconnect(con)
    
    if (!is.null(chemSummary$dat)) {
      colnames(chemSummary$dat)[1] = "Frequency"
    }
    
    rownames(chemSummary$dat) = NULL
    
    setChemResults(session, unique(chemSummary$dat[,1:3]), chemSummary)
    
    
  } # end retrieveChemicals
  
  
  
    
  # queries mesh terms - currently stores results in 
  # geneSummary$dat 
  retrieveGenes <- function() {
    con = dbConnect(MySQL(), dbname = "dcast", user = "root", password = "password")
    
    # query MeSH terms
    query = paste("SELECT count(MeshTerms.MeshID), MeshTerms.MeshID, MeshTerms.Term, MeshTerms.TreeID from MeshTerms")
    
    query <- paste("select count(g.SYMBOL) as Freqency, g.SYMBOL as Symbol from Genes g",
    "INNER JOIN PubGene p on g.GeneID = p.GeneID",
    "INNER JOIN PubGene p2 ON p.PMID = p2.PMID",
    "where p2.GeneID = ", input$geneInput, 
    "group by g.SYMBOL",
    "order by count(g.SYMBOL) desc;")
    
    print(query)
    geneSummary$dat <- dbGetQuery(con, query)
    setGeneResults(session, geneSummary$dat, geneSummary)
    
    dbDisconnect(con)
    
  }
  
  

  # retrieve articles based on diseaseMeshID OR chemMeshID OR geneID2 (only 1 will be applied)
  # if no IDs are specified we do not retreive articles
  retrieveArticles <- function(diseaseMeshID = NULL, chemMeshID = NULL, geneID2 = NULL) {
    
  
    if (is.null(input$geneInput) | input$geneInput=="") {
      return()
    }
    con = dbConnect(MySQL(), dbname = "dcast", user = "root", password = "password")
  
    # gene selection: retrieve articles containing both genes
    if (!is.null(geneID2)) {
    
      query <- paste("SELECT PMID from PubGene",
                     "where GeneID= ", input$geneInput, " or GeneID = ", geneID2, 
                     "GROUP BY PMID HAVING count(PMID) > 1"
      )
    } 
    # if meshID selected, retreive all articles with selected gene and meshID
    else if (!is.null(diseaseMeshID)) {
      
      # query PMIDs
      query = paste("SELECT distinct(PubGene.PMID) from PubGene", 
                  "INNER JOIN PubMesh ON PubGene.PMID = PubMesh.PMID",
                  "WHERE PubGene.GeneID = ", input$geneInput)
      query <- paste0(query, " AND MeshID = ", diseaseMeshID)
    }
    else if (!is.null(chemMeshID)) {

      query = paste("SELECT distinct(PubGene.PMID) from PubGene", 
                    "INNER JOIN PubChem ON PubGene.PMID = PubChem.PMID",
                    "WHERE PubGene.GeneID = ", input$geneInput)
      query <- paste0(query, " AND MeshID = ", chemMeshID)
    } else {
      # return; but we could retrieve all articles with no filters
      dbDisconnect(con)
      return()
    }
    
    query <- paste(query, " ORDER BY PubGene.PMID DESC;")
    

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
      input$rbDiseaseLimits},{

        if (is.null(input$geneInput) | input$geneInput == "") return()
        
        resetReactiveValues()
        
        shinyjs::show("tspSummary")
        
        shinyjs::html("bar-text", "Retreiving Articles, please wait...")
        retrieveArticles(NULL, NULL, NULL)
        
        shinyjs::html("bar-text", "Retreiving Diseases, please wait...")
        retrieveDiseases(NULL, input$rbDiseaseLimits)
        
        shinyjs::html("bar-text", "Retreiving Chemicals, please wait...")
        retrieveChemicals(NULL)
        
        shinyjs::html("bar-text", "Retreiving related genes, please wait...")
        retrieveGenes()
        
        shinyjs::html("bar-text", "Please wait...")
        
    })
    
    
    respondToSelectionRefresh <- function() {
      
      diseaseMeshID <- NULL
      chemMeshID <- NULL
      
      if (!is.null(diseaseSummary$selectedID)) {
        diseaseMeshID <- paste0("'", diseaseSummary$selectedID, "'")
      } else if (!is.null(chemSummary$selectedID)) {
        chemMeshID <- paste0("'", chemSummary$selectedID, "'")
      }
      
      cat("calling retrieveArticles...")
      retrieveArticles(diseaseMeshID, chemMeshID, geneSummary$selectedID)
      cat("done")
      
      
    }

    respondToSelectionDrill <- function() {
      
      num <- 0
      p1 <- list(PMID=NULL); p2 <- list(PMID=NULL); p3 <- list(PMID=NULL)
      
      con = dbConnect(MySQL(), dbname = "dcast", user = "root", password = "password")
      
      # get PMIDS for Mesh Selection
      if (!is.null(diseaseSummary$selectedID)) {
          cat("geting PMIDS for: ", diseaseSummary$selectedID, "\n")
          p1 <- getPMIDs("PubMesh", "MeshID", diseaseSummary$selectedID, con)
          num = num + 1
      }
      
      # get PMIDs for Chem selection
      if (!is.null(chemSummary$selectedID)) {
        cat("geting PMIDS for: ", chemSummary$selectedID, "\n")
        p2 <- getPMIDs("PubChem", "meshID", chemSummary$selectedID, con)
        num = num + 1
      }
      
      # get PMIDs for gene selection - add original geneInput
      genes <- c(input$geneInput, geneSummary$selectedID)
      cat("geting PMIDS for: ", genes, "\n")
      p3 <- getPMIDs("PubGene", "GeneID", genes, con)
      
      
      num = num + 1
      
      cat("all done, num = ", num, "\n")
      
      if (num == 0) {
        dbDisconnect(con)
        return()
      }

      p_all <- table(c(p1$PMID,p2$PMID,p3$PMID))
      pmids <- names(p_all)[p_all>=num]
      cat("final pmids are: ", pmids, "\n")
      
      if (length(pmids) == 0) {
        dbDisconnect(con)
        cat("NO RESULTS -- NEED TO UPDATE!")
        return()
      }
      
      
      # update MeshSummary
      diseaseSummary$dat <- getMeshSummaryByPMIDs(pmids, con)
      setDiseaseResults(session, diseaseSummary$dat, diseaseSummary)
      
      # update ChemSummary
      chemSummary$dat <- getChemSummaryByPMIDs(pmids, con)
      setChemResults(session, chemSummary$dat, chemSummary)
      
      # update geneSummary
      geneSummary$dat <- getGeneSummaryByPMIDs(pmids, con)
      setGeneResults(session, geneSummary$dat, geneSummary)
      
      
      dbDisconnect(con)
      
      #update PMIDs
      pmidList$pmids <- data.frame(PMID = pmids)
      
    }
    
    # TO DO: check for disease or chemical
    # update PMID list when Selection changes
    observe( {
      
      if (is.null(diseaseSummary$selectedID) & is.null(geneSummary$selectedID) & is.null(chemSummary$selectedID)) {
        return()
      }
      
      #respondToSelectionRefresh()
      respondToSelectionDrill()
      

    })
    
    
    
    
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
      output$diseaseResults <- DT::renderDataTable(datatable(diseaseSummary$uniqueDat, rownames = FALSE, 
                                                      selection = "single",
                                                      options = list(paging = FALSE, scrollY = 300)))
    
      output$diseaseHierarchy <- renderUI(HTML(displayMesh(diseaseSummary$dat$TreeID,
                                                      diseaseSummary$dat$Frequency)))
    })

    
    # update chem table
    observe ({
      output$chemResults <- DT::renderDataTable(datatable(chemSummary$uniqueDat, rownames = FALSE, 
                                                             selection = "single",
                                                             options = list(paging = FALSE, scrollY = 300)))
    })
    
    
    observe({
    if (!is.null(diseaseSummary$uniqueDat)) {
      output$DiseaseGraph <- renderPlot({
        #cat("rendering plot...\n")
        
        diseaseSummary$uniqueDat$Term <- factor(diseaseSummary$uniqueDat$Term, levels = diseaseSummary$uniqueDat$Term[order(diseaseSummary$uniqueDat$Frequency)])
        colors <- rep("darkblue", nrow(diseaseSummary$uniqueDat))
        print(diseaseSummary$selectedID)
        
        if (!is.null(diseaseSummary$hoverID)) {
          m <- match(diseaseSummary$hoverID, diseaseSummary$uniqueDat$MeshID)
          term <- diseaseSummary$uniqueDat$Term[m]
          m <- match(term, levels(diseaseSummary$uniqueDat$Term))
          colors[m] <- "yellow"
        }
        
        if (!is.null(diseaseSummary$selectedID)) {
          m <- match(diseaseSummary$selectedID, diseaseSummary$uniqueDat$MeshID)
          term <- diseaseSummary$uniqueDat$Term[m]
          m <- match(term, levels(diseaseSummary$uniqueDat$Term))
          colors[m] <- "darkred"
        }
        
        ggplot(diseaseSummary$uniqueDat, aes(Term, Frequency)) + geom_bar(fill = colors, stat = "identity") +
          coord_flip()
      }, height = max(450, nrow(diseaseSummary$uniqueDat)*26))
    }
    })
    
    
    output$test <- renderText({
        HTML("<h2> how are you? </h2>")
    })

})
