##################################################################
# update chemical stacked bar graph
##################################################################

source("abbreviate.R", local = TRUE)


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
    numDiseases <- min(10, length(t))
    res2 <- filter(res, Disease %in% names(t)[1:numDiseases])
    s <- split(res2$Frequency, res2$Chemical)
    s <- sapply(s, sum)
    s <- sort(s, decreasing = TRUE)
    
    numChems <- min(15, length(s))
    keepThese <- names(s)[1:numChems]
    res2$Chemical[!res2$Chemical%in%keepThese] <- NA
    
    res2 <- filter(res2, Chemical %in% keepThese)
    
    #re-order bars
    sr <- split(res2$Frequency, res2$Disease)
    st <- sort(sapply(sr, sum))
    res2$Disease <- factor(res2$Disease, levels = names(st)[order(st)])
    
    output$chemGraph <- renderPlotly({
      
      gg <- ggplot(res2, aes(Disease, Frequency, fill = Chemical)) + 
        geom_bar(stat = "identity") + coord_flip() +
        theme(plot.title = element_text(face = "bold"),  
              axis.title = element_text(face = "bold"),
              axis.text = element_text(face = "bold")) +
        ggtitle("Distribution of chemical terms by disease (top 5)") +
        xlab("") + ylab("Number of chemical mentions") +
        theme_linedraw() +
        scale_x_discrete(label=function(x) abbreviate2(x, 2))
        #scale_x_discrete(labels = function(x) str_wrap(x, width = 8)) 
      
      
      p <- ggplotly(gg, tooltip = c("x", "fill", "y"))
      return(p %>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE)))
      
      
    })
  
  } # end if no diseases
})

