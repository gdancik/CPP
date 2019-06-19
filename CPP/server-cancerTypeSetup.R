##########################################################################################
# Functions to handle cancerTypeSetupModal
##########################################################################################
observeEvent(input$cancerTypeSetupModal,{
  updateSelectInput(session, "cancerType", choices = diseaseSummary$dat$Term, selected = NULL)
  
})

# Select Cancer Type button
observeEvent(input$btnSelectCancerType, {
  toggleModal(session, "cancerTypeSetupModal",  toggle = "close")
  respondToSelectionDrill()
})

