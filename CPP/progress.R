############################################################################
# Functions for working with progress bars
############################################################################

# generate div used to hold a progress bar, styled for use in large modals
# such as welcomeModal and cancerTypeSetupModal
# these can be toggled by adding/removing the 'hide' class                      
progressDiv <- function(div.id, span.id, span.text = 'Processing, please wait...') {

      HTML(paste0("<div id = '", div.id, "' class=\"progress hide\" style=\"position: fixed;  width: 97%; height:35px; !important\">
              <div class=\"progress-bar progress-bar-striped active\" role=\"progressbar\" aria-valuenow=\"100\" 
                    aria-valuemin=\"0\" aria-valuemax=\"100\" style=\"opacity: 1; width:100%\" !important>
                                  <span id='", span.id, "' style = \"display:inline-block; margin-top: 5px; font: 1.1em\"><b>", span.text, "</b></span>
              </div>
            </div>"))
}


# update both the cancerTypeSetUpModal and main progress bars
setProgressBarText <- function(txt) {

    shinyjs::html("bar-text", txt)
    shinyjs::html("cancerSelection-bar-text", txt)
}


