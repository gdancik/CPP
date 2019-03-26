# server-articles


# PubTator iframe
output$articles <- renderUI({
    src <- "https://www.ncbi.nlm.nih.gov/CBBresearch/Lu/Demo/PubTator/"
    my_test <- tags$html(tags$iframe(id = "iframeid", src=src, style = "width:100%;", height = 800))
#    print(my_test)
    my_test
})

# Add clipboard button
output$clip <- renderUI({
    pmids <- paste0(pmidList$pmids$PMID,collapse = "\n")
    r <- rclipButton("clipbtn", "Copy PMIDs", pmids, icon("clipboard"))
    x <- r[[1]]$attribs
    x$class <- paste(x$class, "blue-button")
    r[[1]]$attribs <- x
    r
})

# display message following clipboard copy
observeEvent(input$clipbtn,{
    msg <- paste(length(pmidList$pmids$PMID), "PMIDs copied to the clipboard") 
    shinyjs::alert(msg)
})
