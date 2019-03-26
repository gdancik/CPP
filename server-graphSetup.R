##########################################################################################
# Functions to handle graphSetupModalTerm
##########################################################################################
observeEvent(input$graphSetupModalTerm,{
  
  #default settings (15 most frequent terms in 10 most frequent cancers)
  if (is.null(input$ctype)) {
    res2 <- getStackedResults(getCancerTermsByDiseaseContingency, "Term")
  
    # drop downs including all diseases and terms with default selection
    updateSelectInput(session, "ctype", choices = diseaseSummary$dat$Term, selected = res2$Disease)
    updateSelectInput(session, "termType", choices = cancerTermSummary$dat$Term, selected = res2$Term)
  
  } else {
    # previous selection
    updateSelectInput(session, "ctype", choices = diseaseSummary$dat$Term, selected = input$ctype)
    updateSelectInput(session, "termType", choices = cancerTermSummary$dat$Term, selected = input$termType)
  }
  
})

# enable/disable Update Graph button
observe({
  if ((length(input$ctype) > 0) && (length(input$termType) > 0)) {
    shinyjs::enable("btnUpdateGraphTerm")
  } else {
    shinyjs::disable("btnUpdateGraphTerm")
  }
})
  

# restore defaults button
observeEvent(input$defaultCterms,{
  #default settings (15 most frequent terms in 10 most frequent cancers)
    res2 <- getStackedResults(getCancerTermsByDiseaseContingency, "Term")
    updateSelectInput(session, "ctype", choices = diseaseSummary$dat$Term, selected = res2$Disease)
    updateSelectInput(session, "termType", choices = cancerTermSummary$dat$Term, selected = res2$Term)
})


# clear button
observeEvent(input$clearCterms,{
  updateSelectInput(session, "ctype", choices = diseaseSummary$dat$Term, selected = NULL)
  updateSelectInput(session, "termType", choices = cancerTermSummary$dat$Term, selected = NULL)
})


# Update Graph button
observeEvent(input$btnUpdateGraphTerm, {
  
    # try to apply user settings   
    con = dbConnect(MariaDB(), group = "CPP")
    res <- getCancerTermsByDiseaseContingency(pmidList$pmids$PMID, con)
    dbDisconnect(con)
    
    res2 <- filter(res, Disease %in% input$ctype)
    res2$Frequency <- as.double(res2$Frequency)
    res2 <- filter(res2, res2[["Term"]] %in% input$termType)
    
    # if user settings return empty dataframe  
    if (nrow(res2) == 0) {
      shinyjs::alert("Sorry, the terms you chose are not mentioned together for the gene. Please choose different settings or restore defaults.")
      return(NULL)
    }
    
    toggleModal(session, "graphSetupModalTerm")
  
    #re-order bars
    sr <- split(res2$Frequency, res2$Disease)
    st <- sort(sapply(sr, sum))
    res2$Disease <- factor(res2$Disease, levels = names(st)[order(st)])
    
    # render graph with user setting
    output$cancerTermGraph <- renderPlotly({
      stackedBarGraph(res2, "Disease", "Frequency", "Term", "Distribution of Cancer Terms by cancer",
                      "Number of cancer term mentions", abbreviate = FALSE)
    })

})


##########################################################################################
# Functions to handle graphSetupModalChem
##########################################################################################
observeEvent(input$graphSetupModalChem,{
  
  #default settings (15 most frequent terms in 10 most frequent cancers)
  if (is.null(input$ctypeChem)) {
    res2 <- getStackedResults(getChemByDiseaseContingency, "Chemical")
    
    # drop downs including all diseases and terms with default selection
    updateSelectInput(session, "ctypeChem", choices = diseaseSummary$dat$Term, selected = res2$Disease)
    updateSelectInput(session, "chems", choices = chemSummary$dat$Term, selected = res2$Chemical)
    
  } else {
    # previous selection
    updateSelectInput(session, "ctypeChem", choices = diseaseSummary$dat$Term, selected = input$ctypeChem)
    updateSelectInput(session, "chems", choices = chemSummary$dat$Term, selected = input$chems)
  }
  
})

# enable/disable Update Graph button
observe({
  if ((length(input$ctypeChem) > 0) && (length(input$chems) > 0)) {
    shinyjs::enable("btnUpdateGraphChem")
  } else {
    shinyjs::disable("btnUpdateGraphChem")
  }
})


# restore defaults button
observeEvent(input$defaultChems,{
  #default settings (15 most frequent chemicals in 10 most frequent cancers)
  res2 <- getStackedResults(getChemByDiseaseContingency, "Chemical")
  updateSelectInput(session, "ctypeChem", choices = diseaseSummary$dat$Term, selected = res2$Disease)
  updateSelectInput(session, "chems", choices = chemSummary$dat$Term, selected = res2$Chemical)
})


# clear button
observeEvent(input$clearChems,{
  updateSelectInput(session, "ctypeChem", choices = diseaseSummary$dat$Term, selected = NULL)
  updateSelectInput(session, "chems", choices = chemSummary$dat$Term, selected = NULL)
})


# Update Graph button
observeEvent(input$btnUpdateGraphChem, {
  
  # try to apply user settings   
  con = dbConnect(MariaDB(), group = "CPP")
  res <- getChemByDiseaseContingency(pmidList$pmids$PMID, con)
  dbDisconnect(con)
  
  res2 <- filter(res, Disease %in% input$ctypeChem)
  res2$Frequency <- as.double(res2$Frequency)
  res2 <- filter(res2, res2[["Chemical"]] %in% input$chems)
  
  # if user settings return empty dataframe  
  if (nrow(res2) == 0) {
    shinyjs::alert("Sorry, the terms you chose are not mentioned together for the gene. Please choose different settings or restore defaults.")
    return(NULL)
  }
  
  toggleModal(session, "graphSetupModalChem")
  
  #re-order bars
  sr <- split(res2$Frequency, res2$Disease)
  st <- sort(sapply(sr, sum))
  res2$Disease <- factor(res2$Disease, levels = names(st)[order(st)])
  
  # render graph with user setting
  output$chemGraph <- renderPlotly({
    stackedBarGraph(res2, "Disease", "Frequency", "Chemical", "Distribution of drug mentions by cancer",
                    "Number of chemical mentions", abbreviate = FALSE)
  })
  
})


##########################################################################################
# Functions to handle graphSetupModalMut
##########################################################################################
observeEvent(input$graphSetupModalMut,{
  
  #default settings (15 most frequent terms in 10 most frequent cancers)
  if (is.null(input$ctypeMut)) {
    res2 <- getStackedResults(getMutByDiseaseContingency, "Mutation")
    
    # drop downs including all diseases and terms with default selection
    updateSelectInput(session, "ctypeMut", choices = diseaseSummary$dat$Term, selected = res2$Disease)
    updateSelectInput(session, "muts", choices = mutationSummary$dat$MutID, selected = res2$Mutation)
    
  } else {
    # previous selection
    updateSelectInput(session, "ctypeMut", choices = diseaseSummary$dat$Term, selected = input$ctypeMut)
    updateSelectInput(session, "muts", choices = mutationSummary$dat$MutID, selected = input$muts)
  }
  
})

# enable/disable Update Graph button
observe({
  if ((length(input$ctypeMut) > 0) && (length(input$muts) > 0)) {
    shinyjs::enable("btnUpdateGraphMut")
  } else {
    shinyjs::disable("btnUpdateGraphMut")
  }
})


# restore defaults button
observeEvent(input$defaultMuts,{
  #default settings (15 most frequent mutations in 10 most frequent cancers)
  res2 <- getStackedResults(getMutByDiseaseContingency, "Mutation")
  updateSelectInput(session, "ctypeMut", choices = diseaseSummary$dat$Term, selected = res2$Disease)
  updateSelectInput(session, "muts", choices = mutationSummary$dat$MutID, selected = res2$Mutation)
})


# clear button
observeEvent(input$clearMuts,{
  updateSelectInput(session, "ctypeMut", choices = diseaseSummary$dat$Term, selected = NULL)
  updateSelectInput(session, "muts", choices = mutationSummary$dat$MutID, selected = NULL)
})


# Update Graph button
observeEvent(input$btnUpdateGraphMut, {
  
  # try to apply user settings   
  con = dbConnect(MariaDB(), group = "CPP")
  res <- getMutByDiseaseContingency(pmidList$pmids$PMID, con)
  dbDisconnect(con)
  
  res2 <- filter(res, Disease %in% input$ctypeMut)
  res2$Frequency <- as.double(res2$Frequency)
  res2 <- filter(res2, res2[["Mutation"]] %in% input$muts)
  
  # if user settings return empty dataframe  
  if (nrow(res2) == 0) {
    shinyjs::alert("Sorry, the terms you chose are not mentioned together for the gene. Please choose different settings or restore defaults.")
    return(NULL)
  }
  
  toggleModal(session, "graphSetupModalMut")
  
  #re-order bars
  sr <- split(res2$Frequency, res2$Disease)
  st <- sort(sapply(sr, sum))
  res2$Disease <- factor(res2$Disease, levels = names(st)[order(st)])
  
  # render graph with user setting
  output$mutGraph <- renderPlotly({
    stackedBarGraph(res2, "Disease", "Frequency", "Mutation", "Distribution of mutations by cancer (max 10 cancers and 15 mutations)",
                    "Number of mutation mentions", abbreviate = FALSE)
  })
  
})