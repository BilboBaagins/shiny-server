source("global.R")

shinyUI(navbarPage(title = "Wagusta Tour",
                   theme = "style/style.css",
                   footer = includeHTML("footer.html"),
                   fluid = TRUE, 
                   collapsible = TRUE,
                   
                   # ----------------------------------
                   # tab panel 1 - Home
                   tabPanel("Home",
                            includeHTML("home.html"),
                            tags$script(src = "plugins/scripts.js"),
                            tags$head(
                              tags$link(rel = "stylesheet", 
                                        type = "text/css", 
                                        href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                              tags$link(rel = "icon", 
                                        type = "image/png", 
                                        href = "images/logo_icon.png")
                            )
                   )#,
                   
#                   # ----------------------------------
#                   # tab panel 2 - Neighborhood Browser
#                   tabPanel("Neighborhood Browser",
#                            neighborhoodDescription(),
#                            includeHTML("scrollToTop.html")
#                   ),
#                   
#                   # ----------------------------------
#                   # tab panel 3 - Location Comparison
#                   tabPanel("Location Comparison",
#                            propertyComparison()
#                   ),
#                   
#                   # ----------------------------------
#                   # tab panel 4 - About
#                   tabPanel("About",
#                            includeHTML("about.html"),
#                            shinyjs::useShinyjs(),
#                            tags$head(
#                                tags$link(rel = "stylesheet", 
#                                          type = "text/css", 
#                                          href = "plugins/carousel.css"),
#                                tags$script(src = "plugins/holder.js")
#                            ),
#                            tags$style(type="text/css",
#                                       ".shiny-output-error { visibility: hidden; }",
#                                       ".shiny-output-error:before { visibility: hidden; }"
#                            )
#                   )
                   
))