source("global.R")
source("appParts.R")

shinyUI(
  div(
    reqSignout(
      div(class="bgimg", style="position:absolute; width:100%;"
      )
    ),
    #div(style="position: relative; top:250px;",
    div(class="mobile_firebase_css",
      useFirebase(), # import dependencies
      useFirebaseUI() # import UI  
    ),
  reqSignin(
    tagList(
    tags$head(tags$style(HTML("
                           .navbar-nav {
                           float: none !important;
                           }
                           .navbar-nav > li:nth-child(9) {
                           float: right;
                           }
                           ")))),
    navbarPage(
      title = "paptour.com",
      selected = "Home",
      position = c("fixed-top"),
      id = "navBar",
      theme = "style/style.css",
      footer = includeHTML("footer.html"),
      fluid = TRUE, 
      collapsible = TRUE,

      # ----------------------------------
      # tab panel 1 - Home
      tabPanel("Home",
        shinyjs::useShinyjs(),
        includeHTML("home.html"),
        tags$style(type="text/css", "padding-top: 70px;"),
        tags$script(src = "plugins/scripts.js"),
        tags$script(src = "https://kit.fontawesome.com/8f64345e9e.js"),

        tags$head(
          tags$link(rel = "icon", 
                    type = "image/png", 
                    href = "favicon.png")
        )
      ),
      
      # ----------------------------------
      # tab panel 2 - Schedule
      tabPanel("Schedule",
        div(class="navbar-buffer"),
        fluidRow(
          div(uiOutput("majorScheduleTableTitle"), style="text-align:center; margin-bottom:10px;"),
          div(uiOutput("majorScheduleTable"), style="text-align:center; margin-bottom:50px;")
        )
      ),
      
      # ----------------------------------
      # tab panel 3 - FedEx Cup
      tabPanel("FedEx Cup",
        div(class="navbar-buffer"),
        fluidRow(
          div(uiOutput("fedExCupMainTableTitle"), style="text-align:center; margin-bottom:10px;"),
          div(uiOutput("fedExCupMainTable"), style="text-align:center; margin-bottom:50px;"),
          div(style='height:60px;')
        )
      ),
            
      # ----------------------------------
      # tab panel 4 - OWGR Rankings
      tabPanel("OWGR",
        div(class="navbar-buffer"),
        fluidRow(
          div(plotlyOutput("owgrTimeseries", width="80%", height="600px")),
          uiOutput('owgrInfogrphic')
        )
      ),
      
      # ----------------------------------
      # tab panel 5 - Players
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
      # tab panel 6 - Stats
      tabPanel("Stats",
        shinyjs::useShinyjs(),
        tags$head(
            tags$link(rel = "stylesheet", 
                      type = "text/css", 
                      href = "plugins/carousel.css"),
            tags$script(src = "plugins/holder.js")
        ),
        isMobile(),
        div(class="navbar-buffer"),
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
      # tab panel 7 - Results
      tabPanel("Results",
        div(class="navbar-buffer"),
        fluidRow(
              div(uiOutput("majorResultsTableTitle"), style="text-align:center; margin-bottom:10px;"),
              div(uiOutput("majorResultsTable"), style="text-align:center; margin-bottom:50px;")
        )
      ),
      # ----------------------------------
      # tab panel 8 - Admin
      tabPanel("Admin",
      shinyjs::useShinyjs(),
      div(class="navbar-buffer"),
      fluidRow(
        div(uiOutput("admin_uploadResults"), style="text-align:center; margin-top:50px;")
      )
    ),
      # ----------------------------------
      # tab panel 9 - SignOut
      tabPanel(
        actionButton(
          #<i class='fa fa-cloud-download' style='font-size: 32px; margin: 5px; color: #00A0AF;'></i>
          "signout",
          HTML("<i class='fas fa-sign-out-alt' style='font-size:16px;'></i> Sign Out"),
          style="padding:0px;border:0px;"
        )
      )
    )
  )
)
)