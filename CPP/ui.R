
lib.loc <- NULL
lib.loc <- "/Users/dancikg/RESEARCH/research_easternct/work/GenePubViewer/DCAST/Github/CPP/lib"

library(shinyBS)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders, lib.loc = lib.loc)
library(plotly)
library(rclipboard)

source("addDeps.R")
source("ui-about.R")
source("modals.R")
source("welcome.R")
source("addTabPanel.R")
source("tabResults.R")

jsCode <- "shinyjs.setReadOnly = function(id) {
  document.getElementById(id).setAttribute('readonly', 'readonly');
}"

#jsCode <- "shinyjs.setAttribute = function(id, attr, value){alert('hi')};"

commonStyles <- list(
  includeCSS('www/ecsu.css'),
  HTML("<link href='https://fonts.googleapis.com/css?family=Courgette' rel='stylesheet' type='text/css'>"),
  HTML("<link rel='stylesheet' href='https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css' integrity='sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u' crossorigin='anonymous'>"),
  tags$style(HTML("
                               
                  /* color table selections */
                  table.dataTable tr.selected td, table.dataTable td.selected {
                  background-color: maroon !important;
                  color: white
                  }
                  
              .blue-button {
                background-image: linear-gradient(#04519b,#044687 60%,#033769);
                color:#FFF;
              }

              .width100 {
                width: 100%;
              }

              .hide {
                //display:none;
                height: 20px;
              }


              .noclick {
                pointer-events: none;
              }

              .noclick tr {
                  color: lightgrey;
              }

                  /* 'hide' tab */
                  #sidebar, .navbar {
                  background-image: linear-gradient(#04519b,#044687 60%,#033769);
                  color:#FFF;
                  }
                  
                  td, td {
                    font-size:80%;
                  }

                 .modal-open {
                      overflow-y: auto;
                      height: 100px;
                 }
 
                  .navbar-header > .navbar-brand {
                  font-family:Courgette;
                  color:#FFF;
                  }
                  
                  .navbar-nav > li > a[data-value='hide'] {color: maroon !important;}
                  
                  /* Top level tabs */
                  .navbar-default .navbar-nav > .active > a, .navbar-default .navbar-nav > .active > a:focus, .navbar-default .navbar-nav > .active > a:hover {
                  color: white;
                  background-color: maroon
                  }
                  
                  
                  
                  .navbar-default .navbar-nav > li > a:hover {
                  color: darkred;
                  }
                  
                  
                  .navbar-default .navbar-nav > li > a {
                  color: white;
                  }
                  
                  
                  /* 2nd level (Disease menu) tabs */
                  .navbar-default .navbar-nav > li > ul > li[class='active'] > a {
                  color: white;
                  background-color: maroon
                  }
                  
                  .navbar-default .navbar-nav > li > ul > li > a {
                  color: darkblue;
                  }
                  
                  
                  .navbar-default .navbar-nav > .active > li > ul > li > a, .navbar-default .navbar-nav > .active > li > ul > li > a:focus, .navbar-default .navbar-nav > .active > li > ul > li > a:hover {
                  color: white;
                  background-color: maroon
                  }
                  
                  .navbar-default .navbar-nav > li >ul > li > a:hover {
                  color: darkred;
                  }
                  
                #summaryRow {
                  position:sticky;
                  position:-webkit-sticky;
                  top:0;
                  //background:white;
                  //z-index:1000;
                }
          
                .shiny-notification {
                    position: fixed;                    
                    top: 15%;
                    left: 65%;
                    font-size: 1.3em;
                    padding-left: 20px;
                    padding-right: 20px;
                    padding-top: 10px; 
                    padding-bottom: 10px;
                    border-style: solid;
                    border-color: black;
                    background-color: #f2dede;
                    opacity: 1;
                    font-weight: bold;
                }

                .graph-button {
                  font:bold 12px verdana;
                  width:60%;
                  position: relative;
                  bottom: 220px;
                  margin: 10px 32% 0 28%;
                  height: 50px;
                  background-image: linear-gradient(#04519b,#044687 60%,#033769);
                  color:#FFF;
                }

                  ")),

              #modal styles  
              tags$head(tags$style("button.close, #welcomeModal button.cancel, #cancerTypeSetupModal button.cancel {display:none}")),
              tags$head(tags$style("#cancerTypeDiv {min-height: 20em; max-height: 30em; overflow-y: auto;}"))
  
)



shinyUI(
  
  navbarPage(title = 'Cancer Publication Portal',
             id = "headerNavBarPage", 
    
    tabPanel('Search', 
             commonStyles,
             tags$head(HTML(
               "<!-- Global site tag (gtag.js) - Google Analytics -->
                <script async src='https://www.googletagmanager.com/gtag/js?id=UA-62826724-4'></script>
               <script>
               window.dataLayer = window.dataLayer || [];
               function gtag(){dataLayer.push(arguments);}
               gtag('js', new Date());
               
               gtag('config', 'UA-62826724-4');
               </script>
               "
             )),
             div(class = "welcome", welcomePage)),
    
      tabResults,
      tabAbout,
    
    # activate shinyJS
    useShinyjs(),
    shinyjs::extendShinyjs(text = jsCode)
    
      #logPanel()
  ) # end navbarPage
) # end shinyUI
               

