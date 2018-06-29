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
         span(class = 'label label-primary', "Maintainer"),
         span(class = 'label label-primary', "Contributer")
         ) 
    ),

    div(class = 'panel-heading', h3(class='panel-title', style = 'font-weight: bold', "Additional Contributors")),

    div(class = "panel-body",
        p("Person 1"),
        p("Person 2")

    )
  )
)

