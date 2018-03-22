# Cancer Publication Portal

library(shiny)
library(DT) # requires development version for single row selection with datatables
library(DBI)
library(RMySQL)
library(ggplot2)

source("functions.R", local = TRUE)
source("setResults.R", local = TRUE)


# some genes have duplicate IDs...we should combine, for now, remove
library(dplyr)


shinyServer(function(input, output, session) {

  output$shinyTitle <- renderText("Cancer Publication Portal")
  
  source("server-reactives.R", local = TRUE)
  source("server-articles.R", local = TRUE)
  source("sql_functions.R", local = TRUE)
  source("server-GeneTable.R", local = TRUE)
  source("server-updateTables.R", local = TRUE)
  source("server-tableClicks.R", local = TRUE)
  
  # disable drop downs on startup
  shinyjs::disable("filterDisease")
  shinyjs::disable("filterChem")
  shinyjs::disable("filterGenes")
  
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
  
    # on initial search
    observeEvent(
      {input$btnGeneSearch
      input$rbDiseaseLimits},{

        if (is.null(input$geneInput) | input$geneInput == "") return()
        
        resetReactiveValues()
        
        respondToSelectionDrill()
        
    })
    
  
  ###################################################################################################    
  # This is the main function that drives db queries, and works as follows:
  # 1) if cancer-specific, get list of cancer-specific PMIDS if not already set
  # 2) get PMIDs based on Mesh selection (limited to current PMID list)
  # 3) get PMIDs based on Chem selection (limited to current PMID list)
  # 4) get PMIDs based on Gene selection (limited to current PMID list)
  # 5) get intersection of (2) - (4) producing list of PMIDs matching search criteria
  # 6) get summaries for Mesh terms, Chemicals, and Genes
  ###################################################################################################    
    respondToSelectionDrill <- function() {
      
      num <- 0
      p1 <- list(PMID=NULL); p2 <- list(PMID=NULL); p3 <- list(PMID=NULL)
      
      con = dbConnect(MySQL(), group = "CPP")
      
      # get cancer PMIDs if specified #
      if (is.null(pmidList$pmids_initial) & input$rbDiseaseLimits == "cancer") {
        cat("getting cancer IDs for ", input$geneInput, "\n")
        pmidList$pmids_initial = getCancerPMIDs(con, input$geneInput)
      }
      
      # get PMIDS for Mesh Selection
      if (!is.null(diseaseSummary$selectedID)) {
          shinyjs::html("bar-text", "Retreiving Articles for Selected Diseases, please wait...")
          cat("Disease selection, geting PMIDS for: ", diseaseSummary$selectedID, "\n")
          p1 <- getPMIDs("PubMesh", "MeshID", diseaseSummary$selectedID, con, pmidList$pmids_initial$PMID)
          num = num + 1
      }
      
      # get PMIDs for Chem selection
      if (!is.null(chemSummary$selectedID)) {
        shinyjs::html("bar-text", "Retreiving Articles for Selected Chemicals, please wait...")
        cat("Chem selection, getting PMIDS for: ", chemSummary$selectedID, "\n")
        p2 <- getPMIDs("PubChem", "meshID", chemSummary$selectedID, con, pmidList$pmids_initial$PMID)
        num = num + 1
      }
      
      # get PMIDs for gene selection
      genes <- c(input$geneInput, geneSummary$selectedID)
      cat("Gene selection, getting PMIDS for: ", genes, "\n")
      shinyjs::html("bar-text", "Retreiving Articles for Selected Genes, please wait...")
      
      p3 <- getPMIDs("PubGene", "GeneID", genes, con, pmidList$pmids_initial$PMID)
      
      a <- pmidList$pmids
      
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
      shinyjs::html("bar-text", "Retreiving Related Diseases, please wait...")
      diseaseSummary$dat <- getMeshSummaryByPMIDs(pmids, con)
      setDiseaseResults(session, diseaseSummary$dat, diseaseSummary)
      
      # update ChemSummary
      shinyjs::html("bar-text", "Retreiving Related Chemicals, please wait...")
      chemSummary$dat <- getChemSummaryByPMIDs(pmids, con)
      setChemResults(session, chemSummary$dat, chemSummary)
      
      # update geneSummary
      shinyjs::html("bar-text", "Retreiving Related Genes, please wait...")
      geneSummary$dat <- getGeneSummaryByPMIDs(pmids, con)
      setGeneResults(session, geneSummary$dat, geneSummary)
      
  
      dbDisconnect(con)
      
      #update PMIDs
      pmidList$pmids <- data.frame(PMID = pmids)
      
      if (is.null(pmidList$pmids_initial)) {
        pmidList$pmids_initial <- pmidList$pmids
      }
      
    }
    
    # TO DO: check for disease or chemical
    # update PMID list when Selection changes
    observe( {
      
      if (is.null(diseaseSummary$selectedID) & is.null(geneSummary$selectedID) & is.null(chemSummary$selectedID)) {
        return()
      }
      
      
      respondToSelectionDrill()
      

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
