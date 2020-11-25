# ui.R

  #### Table of Contents
  #### Section 1: Home Page
  #### Section 2: Leaderboard
  #### Section 3: Schedule
  #### Section 4: Tee-Times
  #### Section 5: Players
  #### Section 6: FedEx Cup Standings
  #### Section 7: Stats
  #### Section 8: Media
  #### Section 9: Admin
    #### a) Upload Major Results
    #### b) Major Preview Table

# Source global script that houses libraries and defines functions used throughout app.
source("global.R")

header <- dashboardHeader(title = "contact@wagustatour.com", titleWidth = 220)

#### Section 1: Home Page
sidebar <- dashboardSidebar(
  useShinyjs(),
  useShinyalert(),
  tags$head(
    tags$style(HTML(".sidebar {
                      height: 90vh; overflow-y: auto;
                    }")
                    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style/style.css"),
    # Pull JavaSctipt file from www folder that checks screen resolution size in order to detect if on mobile device or not. 
    includeScript('www/detectmobile.js'),
    ),
  width=220,
  sidebarMenu(id = "tabs",
    br(),
    div(img(src="logo.png"), style="margin-top: 10px; margin-left: 13px; margin-right: 15px; margin-bottom: 10px;"),
    br(),
    br(),
    menuItem("Home2", tabName = "home2", icon = tags$i(class = "fal fa-home fa-fw")),
    menuItem("Home", tabName = "home", icon = tags$i(class = "fal fa-home fa-fw")),
    menuItem("Leaderboard", tabName = "leaderboard", icon = tags$i(class = "fal fa-home fa-fw")),
    menuItem("Schedule", tabName = "schedule", icon = tags$i(class = "fal fa-home fa-fw")),
    menuItem("Players", tabName = "players", icon = tags$i(class = "fal fa-home fa-fw")),
    menuItem("Admin", tabName = "admin", icon = tags$i(class = "fal fa-home fa-fw")),
    br(),
    div(conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         img(src="golf_loading.gif", style="height:180px;width:180px;")), style="margin-left: 25px;")
  )
)

body <- dashboardBody(
tags$script(HTML("$('body').addClass('fixed');")),
#includeScript('www/keybindings.js'),
  tabItems( 
    tabItem(
      tabName = "home2",
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
    ),
    tabItem(
      tabName = "home",
      div(span(tags$i(class = "fal fa-home fa-fw"), style = "margin-left: 20px; font-size: 30px; font-weight: 400; color:#3F607C;margin-top: 15px;"), span("Wagusta", 
              style = "margin-left: 5px; font-size: 30px; font-weight: 600; color:#192F44;margin-top: 15px;")),
      div("Golf Tour", style = "margin-left:75px;font-size: 24px; font-weight: 600; color:#192F44;margin-top:0px;"), 
      #div(img(id="homeImage_input", src="lads_2.jpg", style = "width:850px;margin-top:10px;margin-left:20px;margin-right:20px;margin-bottom:20px;box-shadow: 0 1px 2px 0 rgba(0, 0, 0, 0.2), 0 2px 5px 0 rgba(0, 0, 0, 0.19);")),
      div(slickROutput("homeImage")), 
      div(div(tags$i(class = "fal fa-info fa-fw"), "Welcome to the official homepage of the Wagusta Tour.", 
              style = "margin-left: 18px; font-size: 14px; font-weight: 300; color:#192F44; margin-top: 5px;"))
    ),
    #### Section 2: Leaderboard
    tabItem(
      tabName = "leaderboard",
      fluidRow(
        box(width=12,
            title = div(tags$i(class="fal fa-user-cog fa-fw"), span("Leaderboard", style = "margin-left:3px;")), 
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "primary",
            div(
              div(uiOutput("rankingPlayer"), style = "display:inline-block;vertical-align:top;"),
              div(uiOutput("rankingPoints"), style = "display:inline-block;vertical-align:top;"),
            style = "display:inline-block;vertical-align:top;")
        )
      )
    ),
    #### Section 3: Schedule
    tabItem(
      tabName = "schedule",
      fluidRow(
        box(width=12,
            title = div(tags$i(class="fal fa-map-marked-alt"), span("Map", style = "margin-left:3px;")), 
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "primary",
            div(leafletOutput("schedule_courseMap", width = "100%", height = "500"))
        ),
        box(width=12,
            title = div(tags$i(class="fal fa-map-marked-alt"), span("Schedule", style = "margin-left:3px;")), 
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "primary",
            div(DT::dataTableOutput("schedule_courseTable"), style = "height:250px")
        )
      )
    ),
    #### Section 5: Players
    tabItem(
      tabName = "players",
      fluidRow(
        box(width=12,
            title = div(tags$i(class="fal fa-user-cog fa-fw"), span("Players", style = "margin-left:3px;")), 
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "primary"
        )
      )
    ),

    # Tab 9: 
    tabItem(
      tabName = "admin",
      fluidRow(
        box(width=12,
            title = div(tags$i(class="fal fa-user-cog fa-fw"), span("Upload Major Results", style = "margin-left:3px;")), 
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "primary",
            div(uiOutput("admin_uploadResults"))
        ),
      ),
      fluidRow(
        box(width=12,
            title = div(tags$i(class="fal fa-user-cog fa-fw"), span("Major Table Preview", style = "margin-left:3px;")), 
            collapsible = TRUE,
            solidHeader = TRUE,
            status = "primary",
            div("Major Table Preview")
        )
      )
    )
  )
)

dashboardPage(title = "Wagusta", header, sidebar, body, skin = "green")












