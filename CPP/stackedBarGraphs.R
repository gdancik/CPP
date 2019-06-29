##################################################################
# update chemical stacked bar graph
##################################################################

source("abbreviate.R", local = TRUE)

#### return data for stacked bar graph, with x1 = disease and
# x2 specified by group
# sql_function: sql function for each disease
# diseases are those in cancerSelectionSummary$selected1 or if no
#    selection, the top 10 diseases
# group.filters: ids for additional filtering
getStackedResults2 <- function(sql_function, group, group.filters) {
  
  if (is.null(diseaseSummary$dat) || nrow(diseaseSummary$dat) <= 0) {
    return(NULL)
  }
  
  con = dbConnect(MariaDB(), group = "CPP")
  

  # select meshIDs and diseases
  if (!is.null(cancerSelectionSummary$selected1)) {
    meshIDs <- c(cancerSelectionSummary$selected1, cancerSelectionSummary$selected2)
    m <- match(meshIDs, cancerSelectionSummary$dat$MeshID)
    diseases <- cancerSelectionSummary$dat$Term[m]
  } else {
    meshIDs <- diseaseSummary$dat$MeshID
    diseases <- diseaseSummary$dat$Term
  }

  # limit to 10
  if (length(meshIDs) > 10) {
    meshIDs <- meshIDs[1:10]
    diseases <- diseaseSummary$dat$Term[1:10]
  }
    
  res <- sql_function(pmidList$pmids$PMID, con, meshIDs, diseases)
  dbDisconnect(con)
  
  if (nrow(res) == 0) {
    return(NULL)
  }
  
  
  # keep results for most frequent diseases
  t <- sort(table(res$Disease), decreasing = TRUE)
  numDiseases <- min(10, length(t))
  res2 <- filter(res, Disease %in% names(t)[1:numDiseases])
  
  res2$Frequency <- as.double(res2$Frequency)
  
  keepThese <- group.filters
  
  if (is.null(keepThese)) {
    # keep results for most frequent groups
    s <- split(res2$Frequency, res2[[group]])
    s <- sapply(s, sum)
    s <- sort(s, decreasing = TRUE)
  
    numGroups <- min(10, length(s))
    keepThese <- names(s)[1:numGroups]
    #res2[[group]][!res2[[group]]%in%keepThese] <- NA
  }
  
  res2 <- filter(res2, res2[[group]] %in% keepThese)
  
  
  
  #re-order bars
  sr <- split(res2$Frequency, res2$Disease)
  st <- sort(sapply(sr, sum))
  res2$Disease <- factor(res2$Disease, levels = names(st)[order(st)])
  
  s <- split(res2$Frequency, res2[[group]])
  n <- names(sort(sapply(s, sum), decreasing = TRUE))
  res2[[group]] <- factor(res2[[group]], levels = n)

  res2
  
}



updateCurrentStackedGraph <- function() {
  if (input$MainPage == "Mutations") {
    plotStackedMutations()
  } else if (input$MainPage == "Drugs") {
    plotStackedChem()
  } else if (input$MainPage == "Cancer Terms") {
    plotStackedCancerTerms()
  } else if (input$MainPage == "Genes") {
    plotStackedGenes()
  }
}


observeEvent(input$MainPage, {
  updateCurrentStackedGraph()
})



plotStackedChem <- reactive({
   cat("in chemical stacked bar observe...\n")
   msg <- "Summarizing chemicals, please wait..."
   showProgress(msg)
   shinyjs::html("bar-text", msg)
 
#    res2 <- getStackedResults(getChemByDiseaseContingency, "Chemical")
  
   res2 <- getStackedResults2(getChemByDiseaseContingency2, "Term", chemSummary$selectedTerm)
   
   if (is.null(res2)) {
     output$chemGraph <- renderPlotly({}) 
     hideProgress()
     return()
   }

   # abbreviate chemical labels (refactor to keep order)
   tmp <- abbreviate(res2$Term, minlength=25, dot = TRUE, strict = TRUE, named = FALSE)
   tmp2 <- abbreviate(levels(res2$Term), minlength=25, dot = TRUE, strict = TRUE, named = FALSE)
   res2$Term <- factor(tmp, levels = tmp2)
   
   output$chemGraph <- renderPlotly({
      stackedBarGraph(res2, "Disease", "Frequency", "Term", "Drug mentions by cancer type")
    })

    hideProgress()
})



plotStackedMutations <- reactive({
  cat("in mut stacked bar observe...\n")
  msg <- "Summarizing mutations, please wait..."
  showProgress(msg)
  shinyjs::html("bar-text", msg)
  
  res2 <- getStackedResults2(getMutByDiseaseContingency2, "Mutation", mutationSummary$selectedID)
  
  if (is.null(res2)) {
    output$mutGraph <- renderPlotly({})
    hideProgress()
    return()
  }
  
  output$mutGraph <- renderPlotly({
    stackedBarGraph(res2, "Disease", "Frequency", "Mutation", "Mutation mentions by cancer type", abbreviate = FALSE)
  })
  hideProgress()
  return()
})



plotStackedCancerTerms <- reactive({
  
  cat("in CancerTerm stacked bar observe...\n")
  msg <- "Summarizing cancer terms, please wait..."
  showProgress(msg)
  shinyjs::html("bar-text", msg)
  
  res2 <- getStackedResults2(getCancerTermsByDiseaseContingency2, "Term", cancerTermSummary$selectedTerm)
  
  if (is.null(res2)) {
    output$cancerTermGraph <- renderPlotly({})
    hideProgress()
    return()
  }
  
  output$cancerTermGraph <- renderPlotly({
    stackedBarGraph(res2, "Disease", "Frequency", "Term", "Cancer term mentions by cancer type", abbreviate = FALSE)
  })
  hideProgress()
  return()
})


plotStackedGenes <- reactive({
  
  cat("in Genes stacked bar observe...\n")
  
  msg <- "Summarizing genes, please wait..."
  showProgress(msg)
  shinyjs::html("bar-text", msg)
  
  res2 <- getStackedResults2(getGenesByDiseaseContingency2, "Gene", geneSummary$selectedTerm)
 
  if (is.null(res2)) {
    output$geneGraph <- renderPlotly({})
    hideProgress()
    return()
  }

  gene <- isolate(input$geneInput)

  if (!is.null(gene)) {
   print(gene)
   print(geneID_to_symbol(gene))

   symbol <- trimws(geneID_to_symbol(gene)) 

   res2 <- dplyr::filter(res2, Gene!=symbol)

  }

  if (nrow(res2) == 0) {
    output$geneGraph <- renderPlotly({})
    hideProgress()
    return()
  }

  output$geneGraph <- renderPlotly({
    stackedBarGraph(res2, "Disease", "Frequency", "Gene", "Additional gene mentions by cancer type", abbreviate = FALSE)
  })
  hideProgress()
  return()
})

# generates a stacked bar graph using ggplotly with specified data frame (res2)
# res2 must be data.frame with columns corresponding to x,fill, and y
stackedBarGraph <- function(res2, xname, yname, fillName, title, ylab = "Number of articles", abbreviate = TRUE) {
  
  str <- paste0("gg <- ggplot(res2, aes(x=", xname, ", y = ", yname, ", fill = ", fillName, "))")
  eval(parse(text = str))
  
  #save(gg, res2, abbreviate2, title, file = "gg.RData")
  #gg <- ggplot(res2, aes(x=x, y = y, fill = fill)) + 
  gg <- gg +  geom_bar(stat = "identity") + coord_flip() +
    ggtitle(title) +
    xlab("") + ylab(ylab) +
    theme_linedraw() + scale_x_discrete(label=function(x) abbreviate2(x, 2)) +
    theme(plot.title = element_text(face = "bold"),  
            #axis.title = element_text(face = "bold"),
            #axis.text = element_text(face = "bold"),
            legend.title = element_blank())
    #scale_y_discrete(label=function(x) abbreviate(x,4, dot = TRUE))
  
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 8)) 
  
  p <- ggplotly(gg, tooltip = c("x", "fill", "y"))
  #p <- ggplotly(gg, tooltip = "all")
  return(p %>% config(displayModeBar = 'hover') %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE)))
}


