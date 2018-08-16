
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
        p(span(style='font-weight:bold', "Garrett M. Dancik, PhD"),
         "is an Associate Professor of Computer Science / ",
         a(href = "https://gdancik.github.io/bioinformatics/", "Bioinformatics"), "at Eastern Connecticut State University (Wilimantic, CT).", 
         role("Maintainer"),
         role("Contributer")
         ) 
    ),

    div(class = 'panel-heading', h3(class='panel-title', style = 'font-weight: bold', "Additional Contributors")),

    div(class = "panel-body",
          p(span(style='font-weight:bold',"Stefanos Stravoravdis"),"is a Biology and Mathematics major at Eastern Connecticut State University",
          "with research interests in bioinformatics and resistance in microorganisms.",
          role("Contributor")),
          p("Person 2", role("Contributor"))
    )
  )
)

