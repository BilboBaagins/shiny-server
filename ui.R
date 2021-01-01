source("global.R")
source("appParts.R")

shinyUI(
    navbarPage(
      title = "paptour.com",
      id = "navBar",
      theme = "style/style.css",
      footer = includeHTML("footer.html"),
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
      reactableOutput("fedExCup_table", width = "100%")
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
        shinyjs::useShinyjs(),
        tags$head(
            tags$link(rel = "stylesheet", 
                      type = "text/css", 
                      href = "plugins/carousel.css"),
            tags$script(src = "plugins/holder.js")
        ),
        isMobile(),
        fluidPage(
          fluidRow(
            column(6, 
              div(uiOutput("fedExCupTitle"), style="text-align:center; margin-bottom:10px;"),
              div(uiOutput("fedExCup"), style="text-align:center;"),
              div(uiOutput("fedExCupSeeMore"), style="text-align:center;margin-bottom:50px;")
            ),
            column(6, 
              div(uiOutput("majorWinsTitle"), style="text-align:center; margin-bottom:10px;"),
              div(uiOutput("majorWins"), style="text-align:center; margin-bottom:50px;")
            )
          ),
          fluidRow(
            column(6, 
              div(uiOutput("topThreeFinishesTitle"), style="text-align:center; margin-bottom:10px;"),
              div(uiOutput("topThreeFinishes"), style="text-align:center; margin-bottom:50px;")
            ),
            column(6, 
              div(uiOutput("majorAttendanceTitle"), style="text-align:center; margin-bottom:10px;"),
              div(uiOutput("majorAttendance"), style="text-align:center; margin-bottom:50px;")
            )
          ),          
          fluidRow(
            column(6, 
              div(uiOutput("avgScoringTitle"), style="text-align:center; margin-bottom:10px;"),
              div(uiOutput("avgScoring"), style="text-align:center; margin-bottom:50px;")
            ),
            column(6, 
              div(uiOutput("woodenSpoonTitle"), style="text-align:center; margin-bottom:10px;"),
              div(uiOutput("woodenSpoon"), style="text-align:center; margin-bottom:50px;")
            )
          )
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
      useFirebase(), # import dependencies
      useFirebaseUI(), # import UI
      fluidRow(
        div(uiOutput("admin_uploadResults")),
        div(uiOutput("signoutButton"))
      )
    )

))
