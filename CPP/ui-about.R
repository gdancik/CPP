
role <- function(r) {
  span(class = 'label label-primary', r)
}




tabAbout = tabPanel("About",
  div(class = "panel panel-default",
    div(class = 'panel-heading', 
        h3(class='panel-title', style = 'font-weight: bold', 
           "Primary Contributor")
       ),
    div(class = "panel-body",
        HTML("<ul>"),
        HTML("<li>"),
        p(span(style='font-weight:bold', "Garrett M. Dancik, PhD"),
         "is an Associate Professor of Computer Science / ",
         a(href = "https://gdancik.github.io/bioinformatics/", "Bioinformatics"), "at Eastern Connecticut State University (Wilimantic, CT).", 
         role("Maintainer"),
         role("Contributer")
         ),
        HTML("</li>"),
        HTML("</ul>")
    ),

    div(class = 'panel-heading', h3(class='panel-title', style = 'font-weight: bold', "Additional Contributors")),

    div(class = "panel-body",
      HTML("<ul>"),

      HTML("<li>"),
      p(span(style='font-weight:bold', "Kevin Williams"), "is a Computer 
        Science major at Eastern Connecticut State University with an interest in bioinformatics, data processing, and algorithms.", 
        role("Contributor")),           
      HTML("</li>"),
      
      HTML("<li>"),
         p(span(style='font-weight:bold', "Andrew Johnson"), "graduated from Eastern Connecticut State University with a degree in Computer Science. 
           He is currently a  Windows administrator at Eastern and has research interests in bioinformatics.",
           role("Contributor")),           
      HTML("</li>"),
      
      HTML("<li>"),
          p(span(style='font-weight:bold', "Myron Zhang"), "is a Computer Science major at Eastern Connecticut State University and is interested in Big Data.",
          role("Contributor")),
      HTML("</li>"),
      HTML("<li>"),
          p(span(style='font-weight:bold',"Stefanos Stravoravdis"),"is a Biology and Mathematics major at Eastern Connecticut State University",
          "with research interests in bioinformatics and resistance in microorganisms.",
          role("Contributor")),
      HTML("</li>"),
      
      HTML("<li>"),
      p(span(style='font-weight:bold', "Nataliia Romanenko"), "graduated from Eastern Connecticut State University with a degree in Computer Science. 
        She is pursuing a Masters in Computer Science at Georgia Tech. ",
        role("Contributor")),           
      HTML("</li>"),
      
      HTML("</ul>")
    )
    )
)

