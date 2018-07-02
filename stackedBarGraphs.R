##################################################################
# update chemical stacked bar graph
##################################################################
observe({
  cat("in chemical stacked bar observe...\n")
  
  if (!is.null(diseaseSummary$dat)) {
    
    con = dbConnect(MySQL(), group = "CPP")
  
    cat("warning: NO FILTERS ARE APPLIED to Chemical stacked bar graph\n")
    res <- getChemByDiseaseContingency(pmidList$pmids$PMID, con)
    dbDisconnect(con)
    
 
    if (nrow(res) == 0) {
      return()
    }
    
    t <- sort(table(res$Disease), decreasing = TRUE)
    numDiseases <- min(5, length(t))
    res2 <- filter(res, Disease %in% names(t)[1:numDiseases])
    s <- split(res2$Frequency, res2$Chemical)
    s <- sapply(s, sum)
    s <- sort(s, decreasing = TRUE)
    
    numChems <- min(5, length(s))
    keepThese <- names(s)[1:numChems]
    res2$Chemical[!res2$Chemical%in%keepThese] <- NA
    
    res2 <- filter(res2, Chemical %in% keepThese)
    
    #re-order bars
    sr <- split(res2$Frequency, res2$Disease)
    st <- sort(sapply(sr, sum))
    res2$Disease <- factor(res2$Disease, levels = names(st)[order(st)])
    
    output$chemGraph <- renderPlot({
      
      ggplot(res2, aes(Disease, Frequency, fill = Chemical)) + 
        geom_bar(stat = "identity") + coord_flip() +
        theme(plot.title = element_text(face = "bold"),  
              axis.title = element_text(face = "bold"),
              axis.text = element_text(face = "bold")) +
        ggtitle("Distribution of chemical terms by disease (top 5)") +
        xlab("") + ylab("Number of chemical mentions") +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 15))
    }, height = 600)
  
  } # end if no diseases
})

