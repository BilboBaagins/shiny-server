source("global.R")
source("appParts.R")

shinyUI(
  div(
    reqSignout(
      div(class="bgimg", style="position:absolute; width:100%;"
      )
    ),
    #disconnectMessage2(),
    #div(style="position: relative; top:250px;",
    div(class="mobile_firebase_css",
      useFirebase(), # import dependencies
      useFirebaseUI() # import UI  
    ),
  reqSignin(
    tagList(
    tags$head(
      tags$style(
        HTML(".navbar-nav {
              float: none !important;
              }
              .navbar-nav > li:nth-child(9) {
              float: right;
              }
              ")
        )
      )
    ),
    navbarPage(
      id = "navBar",
      title = "paptour.com",
      selected = "Home",
      position = c("fixed-top"),
      theme = "style/style.css",
      footer = includeHTML("footer.html"),
      fluid = TRUE, 
      collapsible = TRUE,

      # ----------------------------------
      # tab panel 1 - Home
      tabPanel("Home",
        shinyjs::useShinyjs(),
        useShinyalert(),  # Set up shinyalert
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
          #,verbatimTextOutput( outputId = "text")
        )
      ),
      
      # ----------------------------------
      # tab panel 3 - FedEx Cup
      tabPanel("FedEx",
        div(class="navbar-buffer"),
        #fluidRow(
        #  div(uiOutput("rankingPlayer")),
        #  div(style='height:60px;')
        #),
        fluidRow(
          div(uiOutput("fedExCupMainTableTitle"), style="text-align:center; margin-bottom:10px;"),
          div(uiOutput("fedExCupMainTable"), style="text-align:center; margin-bottom:50px;"),
          div(style='height:60px;')
        ),
        fluidRow(
          div(uiOutput("fedExCupHistoryTableTitle"), style="text-align:center; margin-bottom:10px;"),
          div(uiOutput("fedExCupHistoryTable"), style="text-align:center; margin-bottom:50px;"),
          div(style='height:60px;')
        ),
        fluidRow(
          uiOutput("fedexInfogrphic")
        )
      ),
            
      # ----------------------------------
      # tab panel 4 - OWGR Rankings
      tabPanel("OWGR",
        div(class="navbar-buffer"),
        fluidRow(style="text-align:center;", 
          div(uiOutput("owgr_owgrHeadlineBox"), style="vertical-align:top; display:inline-block; margin-bottom:50px;")
        ),
        fluidRow(
          div(
            div(uiOutput("owgrTimeseries_conditional"), style = "text-align:center;"),
            div(uiOutput("owgrMainTableTitle"), style="text-align:center; margin-bottom:10px;"),
            div(uiOutput("owgrMainTable"), style = "text-align:center;"),
            uiOutput('owgrInfogrphic')
          )
        )
      ),

      # ----------------------------------
      # tab panel 5 - Handicaps
      tabPanel("Handicaps",
        div(class="navbar-buffer"),
        fluidRow(
          style="text-align:center;", 
          div(uiOutput("handicapsHeadlineBox"), style="vertical-align:top; display:inline-block; margin-bottom:50px;")
        ),
        fluidRow(
          div(uiOutput("handicapsTimeseries_conditional"), style = "text-align:center;"),
        ),
        fluidRow(
              div(uiOutput("handicapsTableTitle"), style="text-align:center; margin-bottom:10px;"),
              div(uiOutput("handicapsTable"), style="text-align:center; margin-bottom:50px;")
        )
      ),
      
      # ----------------------------------
      # tab panel 6 - Players
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
      # tab panel 7 - Stats
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
        div(style="height:20px;"),
        fluidPage(
          fluidRow(style="text-align:center;", 
            column(6,
              column(6, 
                div(uiOutput("owgrHeadlineBox"), style="vertical-align:top; display:inline-block;")
              ),
              column(6,
                div(uiOutput("fedExHeadlineBox"), style="vertical-align:top; display:inline-block;")
              ),
              style="vertical-align:top; display:inline-block;"
            ),
            column(6,
              column(6, 
                div(uiOutput("majorWinsHeadlineBox"), style="vertical-align:top; display:inline-block;")
              ),
              column(6,
                div(uiOutput("lowestHanicapHeadlineBox"), style="vertical-align:top; display:inline-block;")
              ),
              style="vertical-align:top; display:inline-block;"
            )
          ),
          div(style="height:100px;"),
          fluidRow(
            column(6, 
              div(uiOutput("owgrTitle"), style="text-align:center; margin-bottom:10px;"),
              div(uiOutput("owgr"), style="text-align:center;"),
              div(uiOutput("owgrSeeMore"), style="text-align:center;margin-bottom:50px;")
            ),
            column(6, 
              div(uiOutput("fedExCupTitle"), style="text-align:center; margin-bottom:10px;"),
              div(uiOutput("fedExCup"), style="text-align:center;"),
              div(uiOutput("fedExCupSeeMore"), style="text-align:center;margin-bottom:50px;")
            )
          ),
          fluidRow(
            column(6, 
              div(uiOutput("handicapsTitle"), style="text-align:center; margin-bottom:10px;"),
              div(uiOutput("handicaps"), style="text-align:center;"),
              div(uiOutput("handicapsSeeMore"), style="text-align:center;margin-bottom:50px;")
            ),
            column(6, 
              div(uiOutput("majorWinsTitle"), style="text-align:center; margin-bottom:10px;"),
              div(uiOutput("majorWins"), style="text-align:center; margin-bottom:50px;")
            )
          ),          
          fluidRow(
            column(6, 
              div(uiOutput("avgScoringTitle"), style="text-align:center; margin-bottom:10px;"),
              div(uiOutput("avgScoring"), style="text-align:center; margin-bottom:50px;")
            ),
            column(6, 
              div(uiOutput("topThreeFinishesTitle"), style="text-align:center; margin-bottom:10px;"),
              div(uiOutput("topThreeFinishes"), style="text-align:center; margin-bottom:50px;")
            )
          ),          
          fluidRow(
            column(6, 
              div(uiOutput("venueFrequencyTitle"), style="text-align:center; margin-bottom:10px;"),
              div(uiOutput("venueFrequency"), style="text-align:center; margin-bottom:50px;")
            ),
            column(6, 
              div(uiOutput("majorAttendanceTitle"), style="text-align:center; margin-bottom:10px;"),
              div(uiOutput("majorAttendance"), style="text-align:center; margin-bottom:50px;")
            )
          ),          
          fluidRow(
            column(6, 
              div(uiOutput("woodenSpoonTitle"), style="text-align:center; margin-bottom:10px;"),
              div(uiOutput("woodenSpoon"), style="text-align:center; margin-bottom:50px;")
            )
          )
        )
      ),
      # ----------------------------------
      # tab panel 8 - Results
      tabPanel("Results",
        div(class="navbar-buffer"),
        fluidRow(
          div(uiOutput("rotateLandscape"))
        ),
        fluidRow(
              div(uiOutput("majorResultsTableTitle"), style="text-align:center; margin-bottom:10px;"),
              div(uiOutput("majorResultsTable"), style="text-align:center; margin-bottom:50px;")
        )#,
        #fluidRow(
        #  div(uiOutput("admin_uploadResults"), style="text-align:center; margin-top:50px;")
        #)
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