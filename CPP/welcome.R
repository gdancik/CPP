welcomePage <- list(

      p(strong("Instructions:"), "Welcome to the Cancer Publication Portal for summarizing and searching cancer-related literature.",
        "To start, select a gene or genes and click the 'Summarize cancer types' button to select the cancer types you are interested in.
        After selecting the cancer types of interest, articles will be summarized based on drugs, cancer terms, mutations, and additional genes
        mentioned in article titles and abstracts. Additional filters can be applied by clicking on any of the tables."),
      p(strong("Resuts:"), "For each term, an enrichment", em("score"), "is calculated that measures how much
      more likely a term appears in the selected articles compared to all articles in the database. For example, a",
        em("score"), "of 4 means that the term is 4x as likely to appear in the title/abstract of selected articles than all cancer-related
        articles in the database. Results are ranked in order of statistical significance."), 
      p(strong("Last updated:"), "Data is up-to-date with PubTator data release from 2/15/2020."),
      p('Please note that the Back button on your browser does not work on this page'),
        
      fluidRow(column(12,
               progressDiv('welcomeModalProgress', 'welcomeModal-bar-text', "Summarizing cancer types, please wait..."))),


      hr(class = "blue-button", style="height: 2px"),
      HTML("<p>Note: <i>CPP</i> uses official gene symbols. Check your genes or lookup invalid symbols from 
             <a href = 'https://www.genenames.org/tools/multi-symbol-checker/' target = '_blank'>genenames.org</a>.</p>"),
      
      fluidRow(
        column(3, style="padding-right:0px",
          selectizeInput("geneInput", label = "Select a gene (begin typing for more)", 
                         #selected = "AGL", choices = "AGL"
                         choices = NULL)
        ),
        column(2,style = "vertical-align:middle; padding-left:0px",
           HTML("<label class = 'control-label' style='visibility:hidden'>Hello</label>"),
           div(
                actionButton("btnGeneSearch", "Summarize cancer types", class = "blue-button")
           )
        )
      ), 
      fluidRow(
        h4('- OR- ', style = 'padding-left:15px')
      ),
      fluidRow(
        column(4, style = "padding-right:0px",
               textAreaInput('multiGeneInput', label = 'Select multiple genes (500 max)',
                              value = "",
                              placeholder = 'Enter multiple genes, separated by spaces or one per line', 
                                rows = 6, resize = "none")
        ),
        column(4, style = "padding-right:0px",
               textAreaInput('invalidGeneOutput', label = HTML("<span style = 'color:red'>Invalid Genes</span>"), 
                                                               rows = 6)
        )
      ),
      
      fluidRow(
        h4('- OR- ', style = 'padding-left:15px')
      ),
      
      fluidRow(
        column(4, style = "padding-left:15px",
          fileInput("multiGeneFile", "Upload genes from file (500 max)",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
          )
        ),
        column(4, style = "padding-right:0px", 
               div(width = "100%",
                   HTML("<label style = 'visibility:hidden'>hi</label>"),
                   actionButton("btnMultiGeneSearch", "Summarize Cancer Types", class = "blue-button",
                                style = "width:100%;", disabled = 'disabled')
               )
        )
      )
)


