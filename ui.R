source("global.R")
source("appParts.R")

#shinyUI(navbarPage(title = div(img(src="golf_loading.gif")),#, height = 50, width = 180)),
shinyUI(navbarPage(title = "Putt & Pint Tour",
                   theme = "style/style.css",
                   footer = includeHTML("footer.html"),
                     useShinyjs(),
                    extendShinyjs("www/app-shinyjs.js", functions = c("updateHistory")),
                    useShinyalert(),
                   fluid = TRUE, 
                   collapsible = TRUE,
                   
                   # ----------------------------------
                   # tab panel 1 - Home
                   tabPanel("Home",
                            includeHTML("home.html"),
                            tags$script(src = "plugins/scripts.js"),
                            tags$script(src = "https://kit.fontawesome.com/8f64345e9e.js"),
                            tags$head(
                              tags$link(rel = "icon", 
                                        type = "image/png", 
                                        href = "favicon.png")
                            )
                   ),
                   
                   # ----------------------------------
                   # tab panel 2 - Neighborhood Browser
                   tabPanel("Schedule",
                    fluidRow(
                      div(leafletOutput("schedule_courseMap", width = "100%", height = "500")),
                      div(DT::dataTableOutput("schedule_courseTable"), style = "height:250px")
                    )
                   ),
                   
                   # ----------------------------------
                   # tab panel 3 - FedEx Cup
                   tabPanel("FedEx Cup",
                    div(actionButton("test", "test"))
                   ),
                   
                   # ----------------------------------
                   # tab panel 4 - About
                   tabPanel("Players",
                            includeHTML("about.html"),
                            shinyjs::useShinyjs(),
                            tags$head(
                                tags$link(rel = "stylesheet", 
                                          type = "text/css", 
                                          href = "plugins/carousel.css"),
                                tags$script(src = "plugins/holder.js")
                            ),
                            tags$style(type="text/css",
                                       ".shiny-output-error { visibility: hidden; }",
                                       ".shiny-output-error:before { visibility: hidden; }"
                            )
                   ),
                   # ----------------------------------
                   # tab panel 5 - Stats
                   tabPanel("Stats",
                      fluidRow(
                        div("1. History of Major Winners"),
                        div("2. Top 3 finishes"),
                        div("3. FedEx Cup Standings"),
                        div("4. FedEx Cup Standings")
                      )
                   ),
                   # ----------------------------------
                   # tab panel 6 - Media
                   tabPanel("Media",
                      fluidRow(
                        div("Media section")
                      )
                   ),
                   # ----------------------------------
                   # tab panel 7 - Admin
                   tabPanel("Admin",
                    fluidRow(
                      div(uiOutput("admin_uploadResults"))
                    )
                  )
                  
                   
))
