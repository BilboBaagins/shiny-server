# server.R

# Source global script that houses libraries and defines functions used throughout app.
source("global.R")


  #### Table of Contents
  #### Section 0: Load Data
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
  
  
  

shinyServer(function(input, output, session) {


  # print list of input events
  output$text <- renderPrint({reactiveValuesToList(input)})

  # Sanity check - print to console whether mobile device or not. 
  observeEvent(input$isMobile, {
    
    if(input$isMobile){
      print("You're on mobile device.")
    } 
    else{
      print("You're not on mobile device.")
    }

  })



 #----- Detect Mobile ------
 #is_mobile_device <- reactive(isTRUE(input$is_mobile_device))


 #----- Firebase Authentication -----
  # Note, this only runs in the Admin tab as the firebase initialise
  #  functions are only called when this tab is live. 
  f <- FirebaseUI$
    new()$ # instantiate
    set_providers( # define providers
      email = TRUE#,
      #google = TRUE,
      #facebook = TRUE
    )$
    launch() # launch



  #----- Browser Check Logic -----
  # Observe what browser is being used to run application.
  # If Firefox or Edge are detected, display shinyalert advising 
  #  user to run application on Google Chrome browser.
  observeEvent(input$check, {
     
    if( input$check$data[1] == TRUE | input$check$data[2] == TRUE ){
 
      shinyalert(title = HTML("<img src='chrome.png' alt='Google Chrome Browser' width='50' height='50'> Chrome browser not detected"),
             text = "For best performance, please launch this application using a Google Chrome browser.",
             type = "",
             closeOnEsc = TRUE,
             closeOnClickOutside = TRUE,
             showCancelButton = FALSE, 
             html = TRUE)
      }
    
  })

  # Collapse the navbar menu after li selection.
  observeEvent(input$navBar, {
    runjs('
      var elem = document.getElementsByClassName("navbar-collapse")[0]
      elem.setAttribute("aria-expanded", "false");
      elem.setAttribute("class", "navbar-collapse collapse");
    ')
  })



  
  #### Section 0: Load Data

  course_data <- reactive({
    data <- data.frame(Course = c("Cradockstown", "Naas Golf Club", "Glenlo Abbey"), 
                       SI = c(72, 72, 71), 
                       Location = c("Naas", "Naas", "Galway"), 
                       lat = c(53.58, 53.63, 52.10), 
                       lon = c(-7.83, -7.32, -6.45)
                       )
    return(data)
  })

  major_history_data <- reactive({
    data <- read.csv("data/history_of_majors.csv", stringsAsFactors=FALSE, check.names=FALSE)
    return(data)
  })

  major_results_data <- reactive({
    data <- read.csv("data/major_results.csv", stringsAsFactors=FALSE, check.names=FALSE)
    return(data)
  })

  # FedEx Cup Standings / OWGR (Official World Golf Ranking)
  owgr_data <- reactive({
    data <- read.csv("data/ogr.csv", stringsAsFactors=FALSE, check.names=FALSE)
    colnames(data) <- gsub("\\.", " ", colnames(data)) %>%
      tolower()
    return(data)
  })

  # Players (lookup table)
  players_data <- reactive({
    data <- data.frame(
      Name = c(
        "Billy Archbold", 
        "Tiarnan O'Brien", 
        "Dan Courtney",
        "Gearoid Comber",
        "Sean Whitson",
        "Luke Smith",
        "David Benn",
        "Aron Morris",
        "Craig Hyland",
        "Darragh Sheehan",
        "David Lynch",
        "Feidhlim Dowling",
        "Mark Donnelly",
        "Niall Devane",
        "Sean McDermott",
        "Oisin Tyrell",
        "Eoin Messitt",
        "Stephen Pigott",
        "Ian Cox",
        "Phil Mahon",
        "Dave McGrath",
        "Will Molloy"
        ),
      Alias = c(
        "billy",
        "t",
        "dinny",
        "g",
        "winnie",
        "smith",
        "davidbenn",
        "aaronmorris",
        "craighyland",
        "sheano",
        "davidlynch",
        "faylo",
        "markdonnelly",
        "nialldevane",
        "seaniemac",
        "oisin",
        "messy",
        "piggy",
        "iancox",
        "philmahon",
        "davemcgrath",
        "willmolloy"
      )
    )
    data$ID <- 1:nrow(data)
    return(data)
  })


  #### Section 1: Home Page

  # Objective of this is to be the one-stop-shop for information.
  #  Should be kind of like the 'Dashboard' page of a Tableau or PowerBI dashboard.
  #  It's built off all the other tabs as small previews.
  #  Would be great if I could implement the navigate to tab based on click. 
  

  # TO DO: Move to the News/Media Tab. 
  output$homeImage <- renderSlickR({

    # Get list of images from public folder for display on carosuel.
    img_path_list <- dir("www")[grep(".jpg", dir("www"))]
    img_path_list <- paste0("www/", img_path_list)

    if(is_mobile_device()){
      width <- "85%"
      height <- 100
    } else{
        width <- "85%"
        height <- 400
    }

    # Create slickR carosuel of images. 
    slick <- slickR(
      img_path_list,
      slideId = "baseDiv",
      slideType = "img",
      objLinks = "www.google.com",
      padding = 1,
      width = width,
      height = height,
      elementId = NULL,
      slickOpts = NULL,
      synchSlides = NULL,
      dotObj = NULL
    )

    # Apply extra customisation settings to slickR object such as, autoplay.
    slick + settings(autoplay = TRUE, 
                    autoplaySpeed = 3000,
                    arrows = FALSE,
                    speed = 600)

  })



  #### Section 2: Leaderboard
  # Because we don't do multi-day events, this might be a bit redundant. 
  # But it can be used in same way as PGA tour, where leaderboard is literally
  #  the previous leaderboard from last tournament. 
  # So it is essentially the previous major results until the next one is played.
  # The leaderboard table should probably be same layout etc as the table that 
  #  extends from the 'Tournament Schedule' tab.

  # ReactJS or datatable here?
  # Prob reactable table as it would work better with schedule and will
  #  need to keep them both consistent. 




  #### Section 3: Schedule

  # Create map object. 
  output$schedule_courseMap <- renderLeaflet({

    # Custom function to add map legend. 
    addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5){
        colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
        labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")

        return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity))
    }

    # Get course data from reactive. 
    course_data <- course_data()

    # Create the leaflet map.
    leaflet() %>%

    setView(lat = 53.10, lng = -7.93, zoom = 4) %>%

    addProviderTiles("CartoDB.Positron", group = "Greyscale") %>% 
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>% 
    addBootstrapDependency() %>%
    addFullscreenControl() %>%

    # Reset map view centre of USA.
    addEasyButton(easyButton(
        id = "reset_map_input",
        icon="fa-globe", 
        title="Reset View",
        onClick=JS("
            function(btn, map) {
                    map.setView([53.10, -7.93], 4);
                  Shiny.onInputChange('reset_schedule_courseMap_input', null);
                  Shiny.onInputChange('reset_schedule_courseMap_input', 'clicked');
                }
            "))) %>%

    addCircleMarkers(
        data = course_data,
        radius = 10,
        color = "#7b9fcf", # Blue
        stroke = FALSE, 
        fillOpacity = 0.5,
        group = "USA", 
        label = course_data$Course,
        popup = paste0("Location <b>", course_data$Location, "</b><br>", 
            "SI <b>", course_data$SI, "</b><br>")
    ) %>% 

    # Layer Control
    addLayersControl(
        baseGroups = c("Greyscale", "Satellite"),
        options = layersControlOptions(collapsed = FALSE)) %>%

    addLegendCustom(colors = c("#7b9fcf"), labels = c("Course"), sizes = c(20))

  })

  # Display parent themes in datatable
  output$schedule_courseTable <- DT::renderDataTable({

    data <- course_data()

    # Create a new ZOOM column in table df. Populate it with HTML for creating a zoom icon.
    data$Zoom <- paste0("<i class='fal fa-dot-circle zoom-table-button'
                                onclick='Shiny.setInputValue(\"schedule_courseMap_zoom\", null); Shiny.setInputValue(\"schedule_courseMap_zoom\", ", as.numeric(rownames(data)), ");'>
                            </i>")

    print(data)

    colnames(data)[6] <- ""

    datatable(data,
        style = "bootstrap", 
        class = "row-border hover compact",
        selection = list(mode = "single",
                        target = "row",
                        selected = 1),
        
        rownames = FALSE,
        escape = FALSE,
        editable = FALSE,
        options = list(sDom = '<"top">f<"bottom">rtli',
                        pageLength = 100,
                        lengthMenu = list(c(100, -1), c("100", "All")),
                        sortClasses = TRUE,
                        ordering = TRUE,
                        scrollX = TRUE,
                        autoWidth = FALSE,
                        scrollY = "200px",
                        searching = TRUE
        )
    )
  })

  # Listen for table click event.
  observeEvent(input$schedule_courseMap_zoom ,{

    data <- course_data()

    print("print(input$schedule_courseMap_zoom)")
    print(input$schedule_courseMap_zoom)

    #data <- data[data$Course == input$schedule_courseMap_zoom,]
    data <- data[input$schedule_courseMap_zoom,]

    leafletProxy("schedule_courseMap", session) %>%
        setView(lng = data$lon, lat = data$lat, zoom = 12) %>%
        addCircleMarkers(
            data = data,
            radius = 15,
            color = "#2dbdb6", # Teal
            stroke = FALSE, 
            fillOpacity = 0.5,
            layerId = "highlighted", 
            label = data$Course,
            popup = paste0("Course <b>", data$Course, "</b><br>", 
                "Location <b>", data$Location, "</b><br>")
        )


  })

  observeEvent(input$reset_schedule_courseMap_input, {

      leafletProxy("schedule_courseMap", session) %>%
      removeLayer(c("highlighted")) 

  })






  #### Section 6: FedEx Cup Standings

  # Points Leader
  output$rankingPlayer <- renderUI({
    outputValue <- "Billy Archbold"
    labelIcon <- tags$i(class = "fad fa-digging fa-fw")
    labelText = "Golfer of the Year"
    div(createInfoBox(outputValue, labelIcon, labelText), class = "combo-box combo-teal")
  })

  # World Ranking Points
  output$rankingPoints <- renderUI({
    outputValue <- 34
    labelIcon <- tags$i(class = "fad fa-digging fa-fw")
    labelText = "OWGR Points"
    div(createInfoBox(outputValue, labelIcon, labelText), class = "combo-box combo-teal")
  })

  # FedEx Cup / OWGR (Offical World Golf Rankings) Table
  output$fedExCup_table <- renderReactable({

    # Load data. 
    # TO DO:
    # Currently being read from CSV> - change to RSQLite DB table on Shiny Server.
    data <- owgr_data()
    data$'average ranking points' <- dplyr::coalesce(data$'average ranking points', 0)
    data$'events played' <- dplyr::coalesce(data$'events played', 0)
    players_data <- players_data()

    data <- data %>%
      select(
        "world ranking position", 
        "name", 
        "average ranking points", 
        "events played", 
        "major wins"
      )

    # Build the data table.
    reactable(
      data,
      filterable = TRUE,
      searchable = TRUE,
      highlight = TRUE,
      columns = list(
        name = colDef(
          html = TRUE,
          minWidth = 200,
          maxWidth = 200,
          width = 200,
          cell = function(value) {
            # Use player_data as lookup table to grab alias for photos. 
            temp <- players_data %>% 
              filter(Name == value)
            paste0('<a href="#" data-toggle="modal" data-target="#player-', temp$Alias, '">' , value, '</a>')
          }
        ),
        'major wins' = colDef(
          minWidth = 200,
          maxWidth = 200,
          width = 200,
          cell = function(value){
            rating_stars(value)
          }
        ),
        'average ranking points' = colDef(
          align = "left", 
          cell = function(value){
            width <- paste0(value / max(data$'average ranking points') * 100, "%")
            bar_chart(format(round(value, 2), nsmall = 2), width = width, background = "#e1e1e1")
          }
        ),
        'events played' = colDef(
          align = "left", 
          cell = function(value){
            width <- paste0(value / max(data$'events played') * 100, "%")
            bar_chart(value, width = width, fill = "#fc5185", background = "#e1e1e1")
          }
        )
      )
    )

  })





  #### Section 7: Stats

  # FedEx Cup -----------
  # FedEx Cup - Title. 
  output$fedExCupTitle <- renderUI({

    div(
      div("FedEx Cup", HTML("<i id='fedExCupTitleID' style='font-size:20px;' class='fas fa-info-circle'></i>"), class="table-title"),
      style = "margin-left:10px;",
      shinyBS::bsPopover("fedExCupTitleID", "FedEx Cup Standings", "FedEx Cup standings for the current season.", placement = "bottom", trigger = "hover"),
      class="align"
    )

  })

  # FedEx Cup - temp.
  output$fedExCup_temp <- renderReactable({

    if(input$isMobile){
      var_width <- 150
      var_width_rank <- 50
    } 
      else{
        var_width <- 200
        var_width_rank <- 120
      }

    # Load data. 
    data <- owgr_data()
    data$'average ranking points' <- dplyr::coalesce(data$'average ranking points', 0)
    data$'events played' <- dplyr::coalesce(data$'events played', 0)
    players_data <- players_data()

    data <- data %>%
      rename(Rank = "world ranking position") %>% 
      rename(Player = name) %>% 
      rename("FedEx Cup Points" = "average ranking points") %>% 
      select(
        Rank, 
        Player, 
        "FedEx Cup Points"
      )

    # Build the data table.
    reactable(
      data,
      filterable = FALSE,
      searchable = FALSE,
      minRows = 5,
      defaultPageSize = 5,
      highlight = TRUE,
      columns = list(
        Rank = colDef(
          minWidth = var_width_rank,
          maxWidth = var_width_rank,
          width = var_width_rank,
        ),
        Player = colDef(
          html = TRUE,
          minWidth = 200,
          maxWidth = 200,
          width = 200,
          cell = function(value) {
            # Use player_data as lookup table to grab alias for photos. 
            temp <- players_data %>% 
              filter(Name == value)
            paste0('<a href="#" data-toggle="modal" data-target="#player-', temp$Alias, '">' , value, '</a>')
          }
        )
      )
    )


  })

  # FedEx Cup. 
  output$fedExCup <- renderUI({
    
   if(input$isMobile){
     var_width <- "width:100%;"
   } 
     else{
       var_width <- "width:500px;"
     }

    div(reactableOutput("fedExCup_temp"), style = var_width, class="reactBox align")

  })

  # See More button for FedExCup tab.
  output$fedExCupSeeMore <- renderUI({

    actionLink("fedExCupSeeMore_input", "See More")

  })

  # Jump user to FedExCup tab. 
  observeEvent(input$fedExCupSeeMore_input, {

    updateTabsetPanel(session, "navBar", selected = "FedEx Cup")

  })


  # Top 3 Finishes -----------
  # Top 3 Finishes - Title. 
  output$topThreeFinishesTitle <- renderUI({

    div(
      div("Top 3 Finishes", HTML("<i id='topThreeFinishesTitleID' style='font-size:20px;' class='fas fa-info-circle'></i>"), class="table-title"),
      style = "margin-left:10px;",
      shinyBS::bsPopover("topThreeFinishesTitleID", "Top 3 Finishes", "Number of career top three finishes.", placement = "bottom", trigger = "hover"),
      class="align"
    )

  })

  # Number of Top 3 Finishes - temp.
  output$topThreeFinishes_temp <- renderReactable({

    # Load data.
    data <- major_results_data()
    players_data <- players_data()

    if(input$isMobile){
      var_width <- 150
      var_width_rank <- 50
    } 
      else{
        var_width <- 200
        var_width_rank <- 120
      }


    # Wrangle major results data to extract top three finishes.
    data <- data %>% 
      # Create temp table of top three scores (incl. ties) per major.
      arrange(Major, -Score) %>%
      group_by(Major) %>% 
      top_n(3, Score) %>% 
      mutate(Position = case_when(
        ( Score == max(Score) & playoff_win == 1 ) ~ 1,
        ( Score == max(Score) & playoff_win == 2 ) ~ 2,
        Score == max(Score) ~ 1,
        ( Score < max(Score) & Score > min(Score) ) ~ 2,
        Score == min(Score) ~ 3)
      ) %>%
      select(-playoff_win) %>%
      data.frame() %>%
      # Create final count & rank of top three finishes. 
      group_by(Player) %>%
      summarise(Top3 = n()) %>%
      arrange(-Top3, Player) %>%
      mutate( Rank = dense_rank(-Top3) ) %>%
      data.frame() %>% 
      rename('No. Top 3 Finishes' = Top3) %>% 
      select(Rank, Player, 'No. Top 3 Finishes')

    # Build the table.
    reactable(
      data,
      filterable = FALSE,
      searchable = FALSE,
      defaultPageSize = 5, 
      minRows = 5,
      highlight = TRUE,
      columns = list(
        Rank = colDef(
          minWidth = var_width_rank,
          maxWidth = var_width_rank,
          width = var_width_rank,
        ),
        Player = colDef(
          html = TRUE,
          minWidth = var_width,
          maxWidth = var_width,
          width = var_width,
          cell = function(value) {
            # Use player_data as lookup table to grab alias for photos. 
            temp <- players_data %>% 
              filter(Name == value)
            paste0('<a href="#" data-toggle="modal" data-target="#player-', temp$Alias, '">' , value, '</a>')
          }
        )
      )
    )

  })

  # Number of Top 3 Finishes.
  output$topThreeFinishes <- renderUI({
    
   if(input$isMobile){
     var_width <- "width:100%;"
   } 
     else{
       var_width <- "width:500px;"
     }

    div(reactableOutput("topThreeFinishes_temp"), style = var_width, class="reactBox align")

  })


  # Major Wins -----------
  # Major Wins - Title. 
  output$majorWinsTitle <- renderUI({

    div(
      div("Major Wins", HTML("<i id='majorWinsTitleID' style='font-size:20px;' class='fas fa-info-circle'></i>"), class="table-title"),
      style = "margin-left:10px;",
      shinyBS::bsPopover("majorWinsTitleID", "Major Wins", "Number of career major wins.", placement = "bottom", trigger = "hover"),
      class="align"
    )

  })

  # Number of Major Wins - temp.
  output$majorWins_temp <- renderReactable({

    # Load data.
    data <- major_results_data()
    players_data <- players_data()

    if(input$isMobile){
      var_width <- 150
      var_width_rank <- 50
    } 
      else{
        var_width <- 200
        var_width_rank <- 120
      }

    # Wrangle major results data to extract top three finishes.
    data <- data %>% 
      # Create temp table of top three scores (incl. ties) per major.
      arrange(Major, -Score) %>%
      group_by(Major) %>% 
      top_n(1, Score) %>% 
      mutate(Position = case_when(
        ( Score == max(Score) & playoff_win == 1 ) ~ 1,
        ( Score == max(Score) & playoff_win == 2 ) ~ 2,
        Score == max(Score) ~ 1,
        ( Score < max(Score) & Score > min(Score) ) ~ 2,
        Score == min(Score) ~ 3)
      ) %>%
      select(-playoff_win) %>%
      data.frame() %>%
      # Create final count & rank of top three finishes. 
      group_by(Player) %>%
      summarise(Top1 = n()) %>%
      arrange(-Top1, Player) %>%
      mutate( Rank = dense_rank(-Top1) ) %>%
      data.frame() %>% 
      rename('Wins' = Top1) %>% 
      select(Rank, Player, 'Wins')

    # Build the table.
    reactable(
      data,
      filterable = FALSE,
      searchable = FALSE,
      minRows = 5,
      defaultPageSize = 5,
      highlight = TRUE,
      columns = list(
        Rank = colDef(
          minWidth = var_width_rank,
          maxWidth = var_width_rank,
          width = var_width_rank,
        ),
        Player = colDef(
          html = TRUE,
          minWidth = var_width,
          maxWidth = var_width,
          width = var_width,
          cell = function(value) {
            # Use player_data as lookup table to grab alias for photos. 
            temp <- players_data %>% 
              filter(Name == value)
            paste0('<a href="#" data-toggle="modal" data-target="#player-', temp$Alias, '">' , value, '</a>')
          }
        )
      )
    )

  })

  # Number of Major Wins.
  output$majorWins <- renderUI({
    
   if(input$isMobile){
     var_width <- "width:100%;"
   } 
     else{
       var_width <- "width:500px;"
     }

   div(reactableOutput("majorWins_temp"), style = var_width, class="reactBox align")

  })


  # Major Attendance -----------
  # Major Attendance - Title.
  output$majorAttendanceTitle <- renderUI({

    div(
      div("Major Attendance", HTML("<i id='majorAttendanceTitleID' style='font-size:20px;' class='fas fa-info-circle'></i>"), class="table-title"),
      style = "margin-left:10px;",
      shinyBS::bsPopover("majorAttendanceTitleID", "Major Attendance", "The number of major tournaments entered by each player.", placement = "bottom", trigger = "hover"),
      class="align"
    )

  })

  # Major Attendance - temp.
  output$majorAttendance_temp <- renderReactable({

    # Load data.
    data <- major_results_data()
    players_data <- players_data()

    if(input$isMobile){
      var_width <- 150
      var_width_rank <- 50
    } 
      else{
        var_width <- 200
        var_width_rank <- 120
      }

    # Wrangle major results data to extract attendance.
    data <- data.frame(table(data$Player))
    colnames(data)[1] <- "Player"
    colnames(data)[2] <- "Majors"
    data <- data[order(data$Majors, decreasing=TRUE),]
    data <- data %>% 
      mutate( Rank = dense_rank(-Majors) ) %>% 
      data.frame()
    data <- data[c('Rank', 'Player', 'Majors')]

    # Build the table.
    reactable(
      data,
      filterable = FALSE,
      searchable = FALSE,
      minRows = 5,
      defaultPageSize = 5,
      highlight = TRUE,
      columns = list(
        Rank = colDef(
          minWidth = var_width_rank,
          maxWidth = var_width_rank,
          width = var_width_rank,
        ),
        Player = colDef(
          html = TRUE,
          minWidth = var_width,
          maxWidth = var_width,
          width = var_width,
          cell = function(value) {
            # Use player_data as lookup table to grab alias for photos. 
            temp <- players_data %>% 
              filter(Name == value)
            paste0('<a href="#" data-toggle="modal" data-target="#player-', temp$Alias, '">' , value, '</a>')
          }
        )
      )
    )

  })

  # Major Attendance.
  output$majorAttendance <- renderUI({
    
   if(input$isMobile){
     var_width <- "width:100%;"
   } 
     else{
       var_width <- "width:500px;"
     }

   div(reactableOutput("majorAttendance_temp"), style = var_width, class="reactBox align")

  })


  # Average Scoring -----------
  # Average Scoring - Title.
  output$avgScoringTitle <- renderUI({

    div(
      div("Average Scoring", HTML("<i id='avgScoringeTitleID' style='font-size:20px;' class='fas fa-info-circle'></i>"), class="table-title"),
      style = "margin-left:10px;",
      shinyBS::bsPopover("avgScoringeTitleID", "Average Scoring", "Stableford scores averaged across all entered tournaments.", placement = "bottom", trigger = "hover"),
      class="align"
    )

  }) 

  # Average Scoring - temp. 
  output$avgScoring_temp <- renderReactable({

    # Load data.
    data <- major_results_data()
    players_data <- players_data()

    if(input$isMobile){
      var_width <- 150
      var_width_rank <- 50
    } 
      else{
        var_width <- 200
        var_width_rank <- 120
      }

    # Wrangle data to extract Average Scoring across attended majors.
    data <- data %>% 
      group_by(Player) %>% 
      mutate(Avg_Score = mean(Score)) %>%
      select(Player, Avg_Score) %>%
      unique() %>%
      arrange(-Avg_Score) %>%
      data.frame() %>% 
      mutate( Rank = dense_rank(-Avg_Score) ) %>% 
      rename('Average Score' = Avg_Score)

    data$'Average Score' <- round(data$'Average Score', 1)
    data <- data[c('Rank', 'Player', 'Average Score')]

    # Build the table.
    reactable(
      data,
      filterable = FALSE,
      searchable = FALSE,
      minRows = 5,
      defaultPageSize = 5,
      highlight = TRUE,
      columns = list(
        Rank = colDef(
          minWidth = var_width_rank,
          maxWidth = var_width_rank,
          width = var_width_rank,
        ),
        Player = colDef(
          html = TRUE,
          minWidth = var_width,
          maxWidth = var_width,
          width = var_width,
          cell = function(value) {
            # Use player_data as lookup table to grab alias for photos. 
            temp <- players_data %>% 
              filter(Name == value)
            paste0('<a href="#" data-toggle="modal" data-target="#player-', temp$Alias, '">' , value, '</a>')
          }
        )
      )
    )

  })

  # Average Scoring.
  output$avgScoring <- renderUI({
    
   if(input$isMobile){
     var_width <- "width:100%;"
   } 
     else{
       var_width <- "width:500px;"
     }

   div(reactableOutput("avgScoring_temp"), style = var_width, class="reactBox align")

  })


  # Wooden Spoon -----------
  # Wooden Spoon - Title. 
  output$woodenSpoonTitle <- renderUI({

    div(
      div("Wooden Spoon", HTML("<i id='woodenSpoonTitleID' style='font-size:20px;' class='fas fa-info-circle'></i>"), class="table-title"),
      style = "margin-left:10px;",
      shinyBS::bsPopover("woodenSpoonTitleID", "Wooden Spoon", "Number of last-place finishes.", placement = "bottom", trigger = "hover"),
      class="align"
    )

  }) 
  
  # Wooden Spoon - temp. 
  output$woodenSpoon_temp <- renderReactable({

    # Load data.
    data <- major_results_data()
    players_data <- players_data()

    if(input$isMobile){
      var_width <- 150
      var_width_rank <- 50
    } 
      else{
        var_width <- 200
        var_width_rank <- 120
      }

    # Wrangle major results data to extract number of last place finishes.
    data <- data %>% 
      arrange(Major, Score) %>%
      group_by(Major) %>% 
      top_n(1, -Score) %>% 
      data.frame()

    data <- data.frame(table(data$Player))

    colnames(data)[1] <- "Player"
    colnames(data)[2] <- "No. Wooden Spoons"
    data <- data[order(data$'No. Wooden Spoons', decreasing=TRUE),]

    data <- data %>% 
      mutate( Rank = dense_rank(-data$'No. Wooden Spoons') )

    data <- data[c('Rank', 'Player', 'No. Wooden Spoons')]

    # Build the table.
    reactable(
      data,
      filterable = FALSE,
      searchable = FALSE,
      minRows = 5,
      defaultPageSize = 5,
      highlight = TRUE,
      columns = list(
        Rank = colDef(
          minWidth = var_width_rank,
          maxWidth = var_width_rank,
          width = var_width_rank,
        ),
        Player = colDef(
          html = TRUE,
          minWidth = var_width,
          maxWidth = var_width,
          width = var_width,
          cell = function(value) {
            # Use player_data as lookup table to grab alias for photos. 
            temp <- players_data %>% 
              filter(Name == value)
            paste0('<a href="#" data-toggle="modal" data-target="#player-', temp$Alias, '">' , value, '</a>')
          }
        )
      )
    )

  })

  # Wooden Spoon. 
  output$woodenSpoon <- renderUI({
    
   if(input$isMobile){
     var_width <- "width:100%;"
   } 
     else{
       var_width <- "width:500px;"
     }

   div(reactableOutput("woodenSpoon_temp"), style = var_width, class="reactBox align")

  })
  
  







  #### Section 9: Admin

  # List of golfers.
  golfers <- reactive({
    df <- data.frame(Name = c(
      "Billy Archbold",
      "Tiarnan O'Brien",
      "Gearoid Comber",
      "Stephen Piggott",
      "Craig Hyland",
      "Oisin Tyrell",
      "Dinny Courtney",
      "Luke Smith",
      "Sean Whitson",
      "Seany Mac",
      "Dave McGrath"
    ))
  })

  # Upload Results.
  output$admin_uploadResults <- renderUI({

    f$req_sign_in() # require sign in

    actionButton(
      inputId = "admin_uploadResults_input",
      label = "Upload Results"
    )
  })

  # Sign-out button.
  output$signoutButton <- renderUI({
    f$req_sign_in()
    
    div(style = "padding:8px;", 
        tags$li(actionButton("signout", "Sign out", class = "btn-danger", style = "color:white;"), class = "dropdown"))
    
  })

  # Sign user out.
  observeEvent(input$signout,{
    f$sign_out()
  })

  # Listener - admin_uploadResults_input.
  observeEvent(input$admin_uploadResults_input, {
    # Launch the results upload modal.
    showModal(uploadResults())
  })

  # Upload Results Modal - Add.
  uploadResults <- function(failed = FALSE) {

    # Create modal UI.
    modalDialog(
      div(
        tags$i(class = "fal fa-file-plus fa-fw"),
        "Add New Major Result",
        class = "title-modal"
      ),
      div(
        dateInput(
          "majorDate",
          "Date of Major",
          value = Sys.Date(), # Default to today's date.
          format = "yyyy-mm-dd",
          width = 350
        ),
        style = "margin-left: 15px;"
      ),
      div(
        selectizeInput(
          "courseName",
          "Course Name",
          width = 350,
          # Might have a list of pre-prepared courses here with ability to add new one.
          choices = c("Select a course" = ""),# sort(pilot_contracts$Project_Name)),
          multiple = FALSE
        ),
        style = "margin-left: 15px;"
      ),
      br(),
      div(
        div( 
          # TO DO: Change to selectize input and have list of names to pull from and 
          #  also ability to add new name.
          # When name is added from list, remove name from list. 
          selectizeInput(
            "golferName",
            "Golfer",
            width = 175,
            multiple = FALSE,
            choices = golfers()
        ), style = "margin-left: 15px;display: inline-block;vertical-align:top;"),
        div( 
          numericInput(
            inputId = "golferHandicap", 
            label = "Handicap", 
            value = NULL, 
            min = 0, 
            max = 36, 
            step = 1, 
            width = 175
        ), 
        style = "margin-left: 15px;display: inline-block;vertical-align:top;"
        ),
        div( 
          numericInput(
            inputId = "golferScore", 
            label = "Score", 
            value = NULL, 
            min = 0, 
            step = 1, 
            width = 175
        ), 
        style = "margin-left: 15px;display: inline-block;vertical-align:top;"
        ),
        div(actionButton("addGolferResults", "Add", width = 79), style = "display: inline-block;margin-top:26px;margin-left:15px;"),
        div(disabled(actionButton("deleteGolferResults", "Delete", width = 79)), style = "margin-left:15px;display:inline-block;vertical-align:middlemargin-top:26px;")
      ),
      div(DT::dataTableOutput('resultsTable'), style = "margin-left:30px;font-size:12px;min-height:50px;margin-right:50px;padding-bottom:40px;width:400px;"),
      br(),
      if (failed)
        # Show the error message
        div(addWarning$message, style = "color: red; margin-left:15px;"),
      
      footer = tagList(
        actionButton("cancelUploadResults", "Cancel"),
        actionButton("uploadResults", "Add Results")
      ), size = 'm'
    )
  }

  # Add golfer result to temp table.
  observeEvent(input$addGolferResults, {

    if( is.null(rv$resultsTable_temp) ){
      data <- results_df()
    }
      else{
        data <- rv$resultsTable_temp
    }

    golfer_name <- input$golferName
    golfer_handicap <- input$golferHandicap
    golfer_score <- input$golferScore

    data_temp <- data.frame(
        Golfer = golfer_name,
        Handicap = golfer_handicap,
        Score = golfer_score
        )

    data_temp <- rbind(data, data_temp)

    rv$resultsTable_temp <- data_temp

  })
  
  # Upload Results Modal - Save.
  observeEvent(input$uploadResults, {
    print("SQL INSERT query.")
  })

  # Upload Results Modal - Cancel. 
  observeEvent(input$cancelUploadResults, {
    # Close the inspector modal.
    removeModal()
  })

  rv <- reactiveValues( 
    resultsTable_temp = NULL
  )

  # Cretae reactive value to store and empty data.frame that will be updated with inspector inputs.
  results_df <- reactive({
    df <- data.frame( Golfer = character(), 
                      Handicap = numeric(),
                      Score = numeric(),
                      
                      stringsAsFactors = FALSE )
    return(df)
  })
  
 # Results Table.
  output$resultsTable <- DT::renderDataTable({
    
    #data <- results_df()
    data <- rv$resultsTable_temp
      
    # Draw the datatable. 
    DT::datatable(data,
                  style = "bootstrap",
                  class = "row-border hover compact",
                  selection = list(mode = "single", target = "row", selected = NULL),
                  # Turn row names on for the row counter. 
                  rownames = TRUE,
                  #filter = "top",
                  options = list(#columnDefs = list(list(visible=FALSE, targets=c(1, 2, 6))),
                                 stateSave = FALSE,
                                 #searchHighlight = TRUE,
                                 server = FALSE, 
                                 ordering = FALSE,
                                 sDom  = '<"row"<"col-6"><"top">t<"bottom">',
                                 #lengthMenu = list(c(15, 25, 50), c('15', '25', '50')),
                                 #pageLength = 15,
                                 autoWidth = FALSE),
                                 # Creates a row index / counter. 
                                 callback = JS("table.on('draw.dt', function(){
                                                var PageInfo = table.page.info();
                                                table.column(0, {page: 'current'}).nodes().each(function(cell,i){
                                                cell.innerHTML = i + 1 + PageInfo.start;
                                                });
                                                })
                                                ")
                  ) %>%
                  # Format the row counter to be greyed out. 
                  formatStyle(0, color = "#C1C1C1")
  
                   
  })





  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
  

# End App.
})