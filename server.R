# server.R

# Source global script that houses libraries and defines functions used throughout app.
source("global.R")
source("mongoDB.R")


  #### Table of Contents
  #### Section 0: Load Data
  #### Section 1: Home
  #### Section 2: Schedule
  #### Section 3: FedEx Cup
  #### Section 4: OWGR
  #### Section 4: Players
  #### Section 5: Stats
  #### Section 6: Results
  #### Section 7: Admin
  
    

shinyServer(function(input, output, session) {


 # print list of input events
  output$text <- renderPrint({reactiveValuesToList(input)})

  is_mobile <- reactiveVal(NULL)

  global <- reactiveValues(response = FALSE)

  # Sanity check - print to console whether mobile device or not. 
  observeEvent(input$isMobile, {
    
    if(input$isMobile){
      print("You're on mobile device.")
      FONT_SIZE <<- 12
      print("print(FONT_ZIZE)")
      print(FONT_SIZE)

      is_mobile(TRUE)

      print("print( is_mobile() )")
      print( is_mobile() )

    } 
    else{
      
      print("You're not on mobile device.")
      FONT_SIZE <<- 14
      print("print(FONT_ZIZE)")
      print(FONT_SIZE)

      is_mobile(FALSE)

      print("print( is_mobile() )")
      print( is_mobile() )

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
      email = TRUE,
      google = TRUE,
      facebook = TRUE,
      twitter = TRUE
    )$
    launch() # launch



#    # Get the logged-in user info.
#    user <- reactive({
#      f$get_signed_in() # get logged in user info
#      print(user)
#    })


  observeEvent( f$get_signed_in(), {
    
    # Get logged in user info.
    user <- f$get_signed_in()$response$displayName

    print(user)

    print( f$get_signed_in()$response$email )

    print(user == "BilboBaagins")

    print(user != "BilboBaagins")

  })


  # Collapse the navbar menu after li selection.
  observeEvent(input$navBar, {

    # Get logged in user info.
    user <- f$get_signed_in()$response$displayName

    print("print(input$navBar)")
    print(input$navBar)

    if( is.null(user) || user == "BilboBaagins" ){

      #shinyjs::enable("admin_uploadResults_input")

      #toggleState("admin_uploadResults")

      print("Billybillybilly")

    
    }

  })



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

  user_access_levels <- reactive({
    data <- data.frame(
      User = c(
        "BilboBaagins",
        "Bilbo Baagins",
        "bbaagins@gmail.com",
        "archbold.billy@gmail.com",
        "tobrien5527@gmail.com"
      )
    )
  })

  # Create Major Schedule Data.
  # NOTE: I will need to figure out how to do this on the fly as it will be relatively dynamic.
  # Not like we will have four majors planned months in advance, although that would be ideal.
  major_schedule_data <- reactive({
    
    data <- data.frame(
      Date = c("2021-01-05", "2021-01-06", "2021-01-07", "2021-01-08"),
      Course = c("Glenlo Abbey", "Cradockstown", "Naas Golf Club", "St. Helens Bay"), 
      Location = c("Galway", "Kildare", "Kildare", "Wexford"), 
      'Defending Champion' = c("---", "Sean Whitson", "---", "Billy Archbold"), 
      'Previous Major' = c("New Event Location", "2020-07-25", "New Event Location", "2019-08-24"), 
      'FedEx Cup Points' = c(500, 600, 500, 600), 
      lat = c(53.30153932321068, 53.20600158084217, 53.2491351564649, 52.232036548098975), 
      lon = c(-9.099569139421591, -6.638881368349008, -6.644518363898139, -6.3249840192094835),
      check.names=FALSE
    )

    return(data)

  })

  # NOTE: can probably delete this as take it and wrangle it from 'major_results_data'.
  major_history_data <- reactive({
    data <- read.csv("data/history_of_majors.csv", stringsAsFactors=FALSE, check.names=FALSE)
    return(data)
  })

  # Main data.
  major_results_data <- reactive({
    
    # Unsuccessful connections may be result of IP address not being whitelisted. 
    # To fix, navigate to [project_name] > Network Access

    # Connect to your MongoDB instance.
    con <- mongo(
      collection = "data",
      db = "major_results",
      url = url,
      verbose = FALSE,
      options = ssl_options()
    )

    # Read data form collection into data.frame. 
    data <- con$find(query = '{}')

    # Disconnect
    rm(con)

    #data <- read.csv("data/major_results.csv", stringsAsFactors=FALSE, check.names=FALSE)
    
    return(data)

  })

  # FedEx Cup Standings / OWGR (Official World Golf Ranking).
  owgr_data <- reactive({
    data <- read.csv("data/ogr.csv", stringsAsFactors=FALSE, check.names=FALSE)
    data$WORLD.RANKING.POSITION <- gsub("[[:alpha:]]", "", data$WORLD.RANKING.POSITION) %>%
      as.numeric()
    colnames(data) <- gsub("\\.", " ", colnames(data)) %>%
      tolower()
    return(data)
  })

  # Players (lookup table).
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
  




  #### Section 2: Schedule

  # Schedule Table - Title. 
  output$majorScheduleTableTitle <- renderUI({

    div(
      div("Major Schedule", HTML("<i id='majorScheduleTitleID' style='font-size:20px;' class='fas fa-info-circle'></i>"), class="table-title"),
      style = "margin-left:10px;",
      shinyBS::bsPopover("majorScheduleTitleID", "Major Schedule", "Upcoming major events. Expand table rows for more detail on upcoming majors.", placement = "bottom", trigger = "hover"),
      class="align"
    )

  }) 
  
  # Schedule Table - temp. 
  output$majorScheduleTable_temp <- renderReactable({

    # Load data.
    data <- major_schedule_data() 
    
    # Build the table with nested table inside.
    reactable(
      data, 
      filterable = FALSE,
      searchable = FALSE,
      highlight = TRUE,
      theme = reactableTheme(
        style = list(fontSize = FONT_SIZE)
      ),
      columns = list(
        lat = colDef(show=FALSE),
        lon = colDef(show=FALSE)
      ),
      details = function(index) {
        
        map_data <- data[index, ]

        htmltools::div(
          htmltools::div(style = "padding-top: 26px; padding-left: 10px; padding-right: 10px;",

            tags$iframe(
              seamless = "seamless",
              src = paste0("https://forecast.io/embed/#lat=", map_data$lat, "&lon=", map_data$lon, "&name=", map_data$Course, ", ", map_data$Location, "&units=ca"),
              width = "100%",
              height = 250,
              style = "border:0px;"
            )

          ),
          htmltools::div(style = "padding-top: 0px; padding-left: 10px; padding-right: 10px; padding-bottom: 100px;",

            leaflet(options = leafletOptions(attributionControl = FALSE)) %>% 
              setView(lat = map_data$lat, lng = map_data$lon, zoom = 16) %>%
              addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetMap") %>% 
              addProviderTiles("Esri.WorldImagery", group = "Satellite") %>% 
              addProviderTiles("CartoDB.Positron", group = "Greyscale") %>% 
              addMarkers(
                lat = map_data$lat, 
                lng = map_data$lon,
                label = map_data$Course,
                popup = paste0(
                  "Course <b>", map_data$Course, "</b><br>", 
                  "Location <b>", map_data$Location, "</b><br>",
                  "Defending Champion <b>", map_data$'Defending Champion', "</b><br>",
                  "Previous Major <b>", map_data$'Previous Major', "</b><br>",
                  "FedEx Cup Points <b>", map_data$'FedEx Cup Points', "</b><br>"
                )
              ) %>% 
              addFullscreenControl() %>% 
              addMeasure(
                position = "bottomleft",
                primaryLengthUnit = "feet",
                secondaryLengthUnit = "meters",
                activeColor = "#3D535D",
                completedColor = "#7D4479"
              ) %>% 
              # Reset map view to original render.
              addEasyButton(easyButton(
                  icon="fas fa-location-arrow",
                  title="Reset View",
                  onClick=JS(paste0("
                      function(btn, map) {
                        map.setView([", map_data$lat,", ", map_data$lon,"], 16);
                      }
                      ")))) %>%
              # Layers control
              addLayersControl(
                baseGroups = c("Satellite", "OpenStreetMap", "Greyscale"),
                #overlayGroups = c("Quakes", "Outline"),
                options = layersControlOptions(collapsed = TRUE)
              )

          )
        )
      }
    )

  })

  # Schedule Table. 
  output$majorScheduleTable <- renderUI({
    
   if(input$isMobile){
     var_width <- "width:90%;"
   } 
     else{
       var_width <- "width:90%;"
     }

   div(reactableOutput("majorScheduleTable_temp"), style = var_width, class="reactBox align")

  })


  # ShinyAlert modal if portait & on mobile. 
  observeEvent(input$navBar, {

    # Get JavaScript to check if the device is in Portrait or Landscape mode.
    shinyjs::runjs("
      if(window.innerHeight < window.innerWidth){
        
        Shiny.setInputValue('landscapeMode_schedule', null);
        Shiny.setInputValue('landscapeMode_schedule', 'yes');
        
      } else{
          
          Shiny.setInputValue('landscapeMode_schedule', null);
          Shiny.setInputValue('landscapeMode_schedule', 'no');

      }
    ")
  })

  observeEvent(input$landscapeMode_schedule, {

    if(input$navBar %in% "Schedule" && is_mobile() && input$landscapeMode_schedule %in% "no"){

      shinyalert(
        inputId = "schedule_shinyalert",
        title = "",
        text = "Switch mobile orientation to view full table",
        size = "s", 
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = TRUE,
        type = "",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "switch_to_landscape.png",
        imageWidth = 150,
        imageHeight = 150,
        animation = TRUE
      )

    }

  })





  #### Section 3: FedEx Cup Standings

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

  # FedEx Cup Table - Title.
  output$fedExCupMainTableTitle <- renderUI({

    div(
      div("FedEx Cup", HTML("<i id='fedExCupMainTitleID' style='font-size:20px;' class='fas fa-info-circle'></i>"), class="table-title"),
      style = "margin-left:10px;",
      shinyBS::bsPopover("fedExCupMainTitleID", "FedEx Cup Standings", "FedEx Cup standings for the current season.", placement = "bottom", trigger = "hover"),
      class="align"
    )

  })

  # FedEx Cup Table - temp.
  output$fedExCupMainTable_temp <- renderReactable({

    if(input$isMobile){
      var_width <- 150
      var_width_rank <- 60
    } 
      else{
        var_width <- 200
        var_width_rank <- 120
      }

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
      ) %>% 
      rename("Rank" = "world ranking position") %>%
      rename(Name = name) %>%
      rename("Average Ranking Points" = "average ranking points") %>%
      rename("Events Played" = "events played") %>%
      rename("Major Wins" = "major wins")  


    # Sticky column CSS.
    sticky_style_one <- list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
    sticky_style_two <- list(position = "sticky", left = var_width_rank, background = "#fff", zIndex = 1, borderRight = "1px solid #eee")

    # Build the data table.
    reactable(
      data,
      filterable = TRUE,
      searchable = FALSE,
      highlight = TRUE,
      pagination = FALSE,
      height = 500,
      theme = reactableTheme(
        style = list(fontSize = FONT_SIZE)
      ),
      columns = list(
        'Rank' = colDef(
          minWidth = var_width_rank,
          maxWidth = var_width_rank,
          width = var_width_rank,
          #style = sticky_style_one,
          #headerStyle = sticky_style_one,
          #class = "sticky left-col-1",
          #headerClass = "sticky left-col-1",
          align = "left"
        ),
        Name = colDef(
          html = TRUE,
          minWidth = 200,
          maxWidth = 200,
          width = 150,
          #style = sticky_style_two,
          #headerStyle = sticky_style_two,
          #class = "sticky left-col-2",
          #headerClass = "sticky left-col-2",
          cell = function(value) {
            # Use player_data as lookup table to grab alias for photos. 
            temp <- players_data %>% 
              filter(Name == value)
            paste0('<a href="#" data-toggle="modal" data-target="#player-', temp$Alias, '">' , value, '</a>')
          }
        ),
        'Major Wins' = colDef(
          minWidth = 200,
          maxWidth = 200,
          width = 200,
          cell = function(value){
            rating_stars(value)
          }
        ),
        'Average Ranking Points' = colDef(
          align = "left", 
          cell = function(value){
            width <- paste0(value / max(data$'Average Ranking Points') * 100, "%")
            bar_chart(format(round(value, 2), nsmall = 2), width = width, background = "#e1e1e1")
          }
        ),
        'Events Played' = colDef(
          align = "left", 
          cell = function(value){
            width <- paste0(value / max(data$'Events Played') * 100, "%")
            bar_chart(value, width = width, fill = "#fc5185", background = "#e1e1e1")
          }
        )
      )
    )

  })

  # FedEx Cup Table.
  output$fedExCupMainTable <- renderUI({

   if(input$isMobile){
     var_width <- "width:90%;"
   } 
     else{
       var_width <- "width:90%;"
     }

    div(reactableOutput("fedExCupMainTable_temp"), style = var_width, class="reactBox align")

  })

  # ShinyAlert modal if portait & on mobile. 
  observeEvent(input$navBar, {

    # Get JavaScript to check if the device is in Portrait or Landscape mode.
    shinyjs::runjs("
      if(window.innerHeight < window.innerWidth){

        Shiny.setInputValue('landscapeMode_fedex', null);
        Shiny.setInputValue('landscapeMode_fedex', 'yes');

      } else{

          Shiny.setInputValue('landscapeMode_fedex', null);
          Shiny.setInputValue('landscapeMode_fedex', 'no');
          
      }
    ")

    print("print(input$landscapeMode_fedex)")
    print(input$landscapeMode_fedex)

  })

  observeEvent(input$landscapeMode_fedex, {

    if(input$navBar %in% "FedEx" && is_mobile() && input$landscapeMode_fedex %in% "no"){

      shinyalert(
        inputId = "fedex_shinyalert",
        title = "",
        text = "Switch mobile orientation to view full table",
        size = "s", 
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = TRUE,
        type = "",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "switch_to_landscape.png",
        imageWidth = 150,
        imageHeight = 150,
        animation = TRUE
      )

    }

  })



  #### Section 4: OWGR
  output$owgrTimeseries <- renderPlotly({

    data <- read.csv("data/major_results.csv", stringsAsFactors=FALSE, check.names=FALSE)
    data$Events <- 1

    # Set up constants.
    N <- 8
    COST <- 0.5
    weights <- (10:2) / 10.5

    temp <- data.frame()

    # For loop to do backfill.
    for(j in N:max(data$Major)){

        RANKING_PERIOD <- j

        # Subset data to the most recent 10 majors. 
        owgr <- data %>% 
            #filter( Major %in% (max(data$Major)-(N-1)):max(data$Major) )
            filter( Major %in% ( (j-(N-1)):j ))

        # Assign points based on finishing position. 
        owgr <- owgr %>% 
            group_by(Major) %>% 
            # Handle playoff.
            mutate(Pos = min_rank(- (Score + coalesce(10 - playoff_win, 0)) )) %>% 
            data.frame()

        # Apply recency weighting to past N majors.
        owgr$Weighted_Score <- NA
        i <- 1
        for(Major in unique(owgr$Major) ){
            owgr[owgr$Major %in% Major,]$Weighted_Score <- owgr[owgr$Major %in% Major,]$Score * weights[i]
            i <- i + 1
        }

        pts_tbl <- data.frame(Pos = 1:6, Pts = c(6.5, 5:1))

        owgr <- left_join(owgr, pts_tbl, by=c("Pos" = "Pos"))

        owgr[is.na(owgr$Pts),]$Pts <- 0

        # Apply recency weighting to past N majors No. Events.
        owgr$Weighted_Pts <- NA
        i <- 1
        for(Major in unique(owgr$Major) ){
            owgr[owgr$Major %in% Major,]$Weighted_Pts <- owgr[owgr$Major %in% Major,]$Pts * weights[i]
            i <- i + 1
        }

        x <- owgr %>% 
            group_by(Player) %>%
            summarise(
                Ranking_Period = RANKING_PERIOD,
                Events = n(),
                Score_sum = sum(Score),
                Weighted_Score_sum = sum(Weighted_Score),
                Pts_sum = sum(Pts), 
                Weighted_Pts_sum = sum(Weighted_Pts)
            ) %>% data.frame() %>% 
            arrange(-Weighted_Pts_sum)

        x$Cost <- x$Events * COST
        x$Add <- x$Weighted_Score_sum/100

        x$OWGR <- x$Weighted_Pts_sum - x$Cost + x$Add
        x$OWGR <- ifelse(x$OWGR < 0, 0, x$OWGR)
        x <- x[order(-x$OWGR),]
        

        temp <- rbind(temp, x)

    }



    major_dates <- data.frame(unique(data[c("Major", "Date")]))

    temp <- left_join(temp, major_dates[c("Major", "Date")], by = c("Ranking_Period" = "Major"))
    temp$Date <- lubridate::dmy(temp$Date)

    # Plot
    p <- temp %>%
      ggplot( aes(x=reorder(paste0("Major ", Ranking_Period, " (", lubridate::year(Date), ")"), Ranking_Period), y=OWGR, group=Player, color=Player)) +
        geom_line() +
        geom_point() +
        #scale_color_viridis(discrete = TRUE) +
        ggtitle("OWGR Timeseries") +
        theme_ipsum() +
        ylab("OWGR") +
        xlab("Major Timeline") +
        theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=1))
    # Turn it interactive with ggplotly
    p <- ggplotly(p)

    p <- plotly_build(p) 

    legend_hide <- data.frame()
    for(i in 1:length(p$x$data)){
        legend_hide <- rbind(legend_hide, tail(p$x$data[[i]]$y, 1))
        colnames(legend_hide)[1] <- "Value"
        p$x$data[[i]]$visible <- "legendonly"
    }

    for(i in 1:length(p$x$data)){
        if( tail(p$x$data[[i]]$y, 1) %in% top_n(legend_hide, 5)$Value ){
            p$x$data[[i]]$visible <- NULL
        }
    }

    return(p) 

  })

  # OWGR Infographic
  output$owgrInfogrphic <- renderUI({

    div(style = "margin:50px;",
      HTML('    
        <div class="row example-centered">
            <div class="col-md-12 example-title">
                <h2>Rankings Explained</h2>
            </div>
            <div class="col-xs-10 col-xs-offset-1 col-sm-8 col-sm-offset-2">
                <ul class="timeline timeline-centered">
                    <li class="timeline-item">
                        <div class="timeline-info">
                            <span>Note 1)</span>
                        </div>
                        <div class="timeline-marker"></div>
                        <div class="timeline-content">
                            <p>Rankings are calculated based on results over the last 8 major championships.</p>
                        </div>
                    </li>
                    <li class="timeline-item">
                        <div class="timeline-info">
                            <span>Note 2)</span>
                        </div>
                        <div class="timeline-marker"></div>
                        <div class="timeline-content">
                            <p>Players cant recieve a negative ranking. The lowest ranking you can have is 0.</p>
                        </div>
                    </li>
                    <li class="timeline-item period">
                        <div class="timeline-info"></div>
                        <div class="timeline-marker"></div>
                        <div class="timeline-content">
                            <h2 class="timeline-title">Calculation</h2>
                        </div>
                    </li>
                    <li class="timeline-item">
                        <div class="timeline-info">
                            <span>Step 1)</span>
                        </div>
                        <div class="timeline-marker"></div>
                        <div class="timeline-content">
                            <p>Players are deducted 0.5 ranking points on entry</p>
                        </div>
                    </li>
                    <li class="timeline-item">
                        <div class="timeline-info">
                            <span>Step 2)</span>
                        </div>
                        <div class="timeline-marker"></div>
                        <div class="timeline-content">
                            <p>Points are awarded to top 6 places as follows; <br>
                            <table style="width:70%">
                              <tr>
                                <th>Place</th>
                                <th>Points</th>
                              </tr>
                              <tr>
                                <td>1st</td>
                                <td>6.5</td>
                              </tr>
                              <tr>
                                <td>2nd</td>
                                <td>5</td>
                              </tr>
                              <tr>
                                <td>3rd</td>
                                <td>4</td>
                              </tr>
                              <tr>
                                <td>4th</td>
                                <td>3</td>
                              </tr>
                              <tr>
                                <td>5th</td>
                                <td>2</td>
                              </tr>
                              <tr>
                                <td>6th</td>
                                <td>1</td>
                              </tr>
                            </table>
                            </p>
                        </div>
                    </li>
                    <li class="timeline-item">
                        <div class="timeline-info">
                            <span>Step 3)</span>
                        </div>
                        <div class="timeline-marker"></div>
                        <div class="timeline-content">
                            <p>Previouly awarded points from past majors are weighted based on 
                            recency. 
                            Points awarded for the most recent major are worth full value while 
                            points awarded for each preceding major reduce by 10% until fall out 
                            of the rolling 8 major calculation.
                            </p>
                        </div>
                    </li>
                    <li class="timeline-item">
                        <div class="timeline-info">
                            <span>Step 4)</span>
                        </div>
                        <div class="timeline-marker"></div>
                        <div class="timeline-content">
                            <p>
                            </p>
                        </div>
                    </li>
                    <li class="timeline-item">
                        <div class="timeline-info">
                            <span>Step 5)</span>
                        </div>
                        <div class="timeline-marker"></div>
                        <div class="timeline-content">
                            <p>Finally, the players recency weighted stableford score over the 
                              rolling 8 major window is summed and divided by 100, then added
                              to the players OWGR score. This is intended to reward high-scoring 
                              esults.
                          </p>
                        </div>
                    </li>
                </ul>
            </div>
        </div>'
    ))

  })







  #### Section 4: Stats

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
      var_width_rank <- 60
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
      theme = reactableTheme(
        style = list(fontSize = FONT_SIZE)
      ),
      columns = list(
        Rank = colDef(
          minWidth = var_width_rank,
          maxWidth = var_width_rank,
          width = var_width_rank,
          align = "left"
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
      var_width_rank <- 60
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
      theme = reactableTheme(
        style = list(fontSize = FONT_SIZE)
      ),
      columns = list(
        Rank = colDef(
          minWidth = var_width_rank,
          maxWidth = var_width_rank,
          width = var_width_rank,
          align = "left"
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
      var_width_rank <- 60
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
      theme = reactableTheme(
        style = list(fontSize = FONT_SIZE)
      ),
      columns = list(
        Rank = colDef(
          minWidth = var_width_rank,
          maxWidth = var_width_rank,
          width = var_width_rank,
          align = "left"
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
      var_width_rank <- 60
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
      theme = reactableTheme(
        style = list(fontSize = FONT_SIZE)
      ),
      columns = list(
        Rank = colDef(
          minWidth = var_width_rank,
          maxWidth = var_width_rank,
          width = var_width_rank,
          align = "left"
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
      var_width_rank <- 60
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
      theme = reactableTheme(
        style = list(fontSize = FONT_SIZE)
      ),
      columns = list(
        Rank = colDef(
          minWidth = var_width_rank,
          maxWidth = var_width_rank,
          width = var_width_rank,
          align = "left"
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
      var_width_rank <- 60
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
      theme = reactableTheme(
        style = list(fontSize = FONT_SIZE)
      ),
      columns = list(
        Rank = colDef(
          minWidth = var_width_rank,
          maxWidth = var_width_rank,
          width = var_width_rank,
          align = "left"
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
  
  







  #### Section 4: Players
  # There is no server logic for the Players tab- it's all contained in the about.html file. 








  #### Section 6: Results

  # Results Table - Title. 
  output$majorResultsTableTitle <- renderUI({

    div(
      div("Major Results", HTML("<i id='majorResultsTitleID' style='font-size:20px;' class='fas fa-info-circle'></i>"), class="table-title"),
      style = "margin-left:10px;",
      shinyBS::bsPopover("majorResultsTitleID", "Major Results", "Historic major results. Expand table rows for more information on particular event.", placement = "bottom", trigger = "hover"),
      class="align"
    )

  }) 
  
  # Results Table - temp. 
  output$majorResultsTable_temp <- renderReactable({

    # Load data.
    data <- major_results_data()

    # Create high-level major results data.
    major_results_data_temp1 <- data %>% 
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
      # Remove any playoff non-winners.
      filter(Position == min(Position)) %>% 
      select(-playoff_win, -Position) %>%
      data.frame()

    # Supplementary high-level major stats,
    major_results_data_temp2 <- data %>%       
      group_by(Major) %>% 
      mutate(Field = n()) %>% 
      mutate(Mean_Score = round(mean(Score), 1)) %>% 
      mutate(Median_Score = round(median(Score), 1)) %>% 
      mutate(Mean_Handicap = round(mean(Handicap), 1)) %>% 
      mutate(Median_Handicap = round(median(Handicap), 1)) %>% 
      select(Major, Field, Mean_Score, Median_Score, Mean_Handicap, Median_Handicap) %>%
      unique() %>% 
      data.frame() 

    # Join into one view and rename some columns. 
    major_results_data <- left_join(major_results_data_temp1, major_results_data_temp2) %>% 
      rename(Winner = Player) %>%
      rename('Mean Handicap' = Mean_Handicap) %>%
      rename("Median Handicap" = Median_Handicap) %>%
      rename("Mean Score" = Mean_Score) %>%
      rename("Median Score" = Median_Score) %>%
      #select(Major, Date, Venue, Field, Winner, Score, 'Mean Score', 'Median Score', Handicap, 'Mean Handicap', 'Median Handicap')
      select(Major, Date, Venue, Field, Winner, Score, Handicap)

    # Convert to date field.
    major_results_data$Date <- lubridate::dmy(major_results_data$Date)


    if(input$isMobile){
      var_width <- 150
      var_width_rank <- 70
    } 
      else{
        var_width <- 200
        var_width_rank <- 120
      }

    # Sticky column CSS.
    sticky_style_one <- list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
    sticky_style_two <- list(position = "sticky", left = var_width_rank, background = "#fff", zIndex = 1, borderRight = "1px solid #eee")

    # Build the table with nested table inside.
    reactable(
      major_results_data, 
      filterable = TRUE,
      searchable = FALSE,
      highlight = TRUE,
      pagination = FALSE,
      height = 500,
      theme = reactableTheme(
        style = list(fontSize = FONT_SIZE)
      ),
      columns = list(
        Major = colDef(
          minWidth = var_width_rank,
          maxWidth = var_width_rank,
          width = var_width_rank,
          #style = sticky_style_one,
          #headerStyle = sticky_style_one,
          align = "center"
        ),
        Field = colDef(
          minWidth = var_width_rank,
          maxWidth = var_width_rank,
          width = var_width_rank,
          align = "center"
        ),
        Score = colDef(
          minWidth = var_width_rank,
          maxWidth = var_width_rank,
          width = var_width_rank,
          align = "center"
        ),
        Handicap = colDef(
          minWidth = var_width_rank,
          maxWidth = var_width_rank,
          width = var_width_rank,
          align = "center"
        ),
        Venue = colDef(
          minWidth = 120,
          maxWidth = 120,
          width = 120,
          align = "left"
        )
      ),
      details = function(index) {
        event_data <- data[data$Major == major_results_data$Major[index], ]
        htmltools::div(style = "padding: 16px",
          reactable(
            event_data[
              c("Major",
                "Date",
                "Player",
                "Handicap",
                "Score",
                "Venue")
            ], 
            outlined = TRUE,
            highlight = TRUE
          )
        )
      }
    )

  })

  # Results Table. 
  output$majorResultsTable <- renderUI({
    
   if(input$isMobile){
     var_width <- "width:90%;"
   } 
     else{
       var_width <- "width:90%;"
     }

   div(reactableOutput("majorResultsTable_temp"), style = var_width, class="reactBox align")

  })

  # ShinyAlert modal if portait & on mobile. 
  observeEvent(input$navBar, {

    # Get JavaScript to check if the device is in Portrait or Landscape mode.
    shinyjs::runjs("
      if(window.innerHeight < window.innerWidth){

        Shiny.setInputValue('landscapeMode_results', null);
        Shiny.setInputValue('landscapeMode_results', 'yes');

      } else{

          Shiny.setInputValue('landscapeMode_results', null);
          Shiny.setInputValue('landscapeMode_results', 'no');

      }
    ")

    print("print(input$landscapeMode_results)")
    print(input$landscapeMode_results)

  })


  observeEvent(input$landscapeMode_results, {
    if(input$navBar %in% "Results" && is_mobile() && input$landscapeMode_results %in% "no"){

      shinyalert(
        inputId = "results_shinyalert",
        title = "",
        text = "Switch mobile orientation to view full table",
        size = "s", 
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = TRUE,
        type = "",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "switch_to_landscape.png",
        imageWidth = 150,
        imageHeight = 150,
        animation = TRUE
      )

    }

  })

  #observeEvent(input$results_shinyalert, {
  #  # Get JavaScript to check if the device is in Portrait or Landscape mode.
  #  shinyjs::runjs("
  #    if(window.innerHeight < window.innerWidth){
  #      Shiny.setInputValue('landscapeMode', null);
  #      Shiny.setInputValue('landscapeMode', 'yes');
  #    } else{
  #        Shiny.setInputValue('landscapeMode', null);
  #        Shiny.setInputValue('landscapeMode', 'no');
  #    }
  #  ")
  #})










  #### Section 9: Admin

  # Upload Results.
  output$admin_uploadResults <- renderUI({

    # Get logged in user info.
    user <- f$get_signed_in()$response$displayName
    email <- f$get_signed_in()$response$email

    user_access_levels <- user_access_levels()

    # print("user")
    # print(user)
    # print("email")
    # print(email)

    if( is.null(user) || user %in% user_access_levels$User || email %in% user_access_levels$User ){

      actionButton(
        inputId = "admin_uploadResults_input",
        label = "Upload Results"
      )
    
    }else{
      shinyjs::disabled(
        actionButton(
          inputId = "admin_uploadResults_input",
          label = "Upload Results"
        )
      )
    }
  })

  ## Sign-out button.
  #output$signoutButton <- renderUI({
  #  f$req_sign_in()
  #  
  #  div(style = "padding:8px;", 
  #      tags$li(actionButton("signout", "Sign out", class = "btn-danger", style = "color:white;"), class = "dropdown"))
  #  
  #})

  # Sign user out.
  observeEvent(input$signout,{
    
    updateTabsetPanel(session, "navBar", selected = "Home")

    f$sign_out()

  })

  # Listener - admin_uploadResults_input.
  observeEvent(input$admin_uploadResults_input, {
    # Launch the results upload modal.
    showModal(uploadResults())
  })

  # Upload Results Modal - Add.
  uploadResults <- function(failed = FALSE) {

    # Get golfers. 
    golfers <- players_data()

    # Create modal UI.
    modalDialog(
      div(
        tags$i(class = "fas fa-cloud-upload-alt"),
        "Add New Major Result",
        class = "title-modal",
        style="font-size:20px;margin-bottom:40px;"
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
            choices = golfers
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
      ), size = 'm', easyClose = TRUE
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
















  

# End App.
})
