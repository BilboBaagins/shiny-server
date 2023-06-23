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




  # TO DO: (2022-06-11)
  # 1. Add in FedEX Cup Champion Stat
  # 2. CRUD admin functionality - upload results
  # 3. Update the schedule
  
    

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

      window.scrollTo(0, 0);
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
    
    # data <- data.frame(
    #   Date = c("2021-06-26", "2021-08-07", "2021-10-02"),
    #   Course = c("Cradockstown Golf Club", "Tulfaris Golf Club", "Millicent Golf Club"), 
    #   Location = c("Kildare", "Wicklow", "Kildare"), 
    #   'Defending Champion' = c("Sean Whitson", "Darragh Sheehan", "---"), 
    #   'Previous Major' = c("2020-07-25", "2018-05-06", "New Event Location"), 
    #   'FedEx Cup Points' = c(650, 650, 650), 
    #   lat = c(53.205828972592265, 53.12509004778715, 53.28098556258424), 
    #   lon = c(-6.639070763009378, -6.559711595660492, -6.688538741988058),
    #   check.names=FALSE
    # )

    data <- data.frame(
      Date = c(
        "2023-06-24", 
        "TBC",
        "TBC",
        "TBC"
        ),
      Course = c(
        "Druids Heath", 
        "TBC",
        "TBC",
        "TBC"
        ), 
      Location = c(
        "Co. Wicklow", 
        "TBC",
        "TBC",
        "TBC"
        ), 
      'Defending Champion' = c(
        "N/A - New Major Venue", 
        "TBC",
        "TBC",
        "N/A"
        ), 
      'Previous Major' = c(
        "N/A - New Major Venue", 
        "TBC",
        "TBC",
        "N/A"
        ), 
      'FedEx Cup Points' = c(
        650, 
        650,
        650,
        650
        ), 
      lat = c(
        53.09048505464224, 
        NA,
        NA,
        NA
        ), 
      lon = c(
        -6.079314444722932, 
        NA,
        NA,
        NA
        ),
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
        collection = "major_results",
        db = "paptour_db",
        url = url,
        verbose = FALSE,
        options = ssl_options()
    )

    # Read data form collection into data.frame. 
    data <- con$find(query = '{}')

    # Make sure it's in the correct order. 
    data <- data[order(-data$Major, -data$Score),]

    # Disconnect
    rm(con)

    #data <- read.csv("data/major_results.csv", stringsAsFactors=FALSE, check.names=FALSE)
    
    return(data)

  })

  # Handicap data. 
  handicaps_data <- reactive({
    
    # Unsuccessful connections may be result of IP address not being whitelisted. 
    # To fix, navigate to [project_name] > Network Access

    # Connect to your MongoDB instance.
    con <- mongo(
      collection = "handicaps",
      db = "paptour_db",
      url = url,
      verbose = FALSE,
      options = ssl_options()
    )

    # Read data form collection into data.frame. 
    data <- con$find(query = '{}')

    # Make sure it's in the correct order. 
    data <- data[order(data$Handicap, data$Handicap),]

    # Disconnect
    rm(con)

    #data <- read.csv("data/major_results.csv", stringsAsFactors=FALSE, check.names=FALSE)
    
    return(data)

  })

  # Load handicaps data (not in reactive), for removal from table (for screengrabs)
  con <- mongo(
    collection = "handicaps",
    db = "paptour_db",
    url = url,
    verbose = FALSE,
    options = ssl_options()
  )

  # Read data form collection into data.frame. 
  handicap_data_2 <- con$find(query = '{}')

  # Make sure it's in the correct order. 
  handicap_data_2 <- handicap_data_2[order(handicap_data_2$Handicap, handicap_data_2$Handicap),]

  # Disconnect
  rm(con)

  # FedEx Cup Standings / OWGR (Official World Golf Ranking).
  fedex_data <- reactive({
    
    #data <- read.csv("data/ogr.csv", stringsAsFactors=FALSE, check.names=FALSE)
    #data$WORLD.RANKING.POSITION <- gsub("[[:alpha:]]", "", data$WORLD.RANKING.POSITION) %>%
    #  as.numeric()
    #colnames(data) <- gsub("\\.", " ", colnames(data)) %>%
    #  tolower()

    # Connect to your MongoDB instance.
    con <- mongo(
      collection = "major_results",
      db = "paptour_db",
      url = url,
      verbose = FALSE,
      options = ssl_options()
  )

    # Read data form collection into data.frame. 
    data <- con$find(query = '{}')

    # Make sure it's in the correct order. 
    data <- data[order(-data$Major, -data$Score),]

    # Disconnect
    rm(con)

    data$Year <- lubridate::year(lubridate::dmy(data$Date))

    temp <- data.frame()

    # For loop to do backfill.
    for(year in sort(unique(data$Year))){

        # Subset data to the most recent year. 
        #data <- data[data$Year %in% max(data$Year), ]

        fedex <- data[data$Year %in% year, ]

        # Assign points based on finishing position. 
        fedex <- fedex %>% 
            group_by(Major) %>% 
            # Handle playoff.
            mutate(Pos = min_rank(- (Score + coalesce(10 - playoff_win, 0)) )) %>% 
            data.frame()

        pts_tbl <- data.frame(Pos = 1:6, Pts = c(650, 500, 400, 300, 200, 100))

        fedex <- left_join(fedex, pts_tbl, by=c("Pos" = "Pos"))

        fedex[is.na(fedex$Pts),]$Pts <- 0

        fedex <- fedex %>% 
            group_by(Player) %>%
            summarise(
                Year = year,
                Events = n(),
                Score = sum(Score),
                #Weighted_Score_sum = sum(Weighted_Score),
                FedEx_Points = sum(Pts), 
                #Weighted_Pts_sum = sum(Weighted_Pts)
            ) %>% data.frame() %>% 
            arrange(-FedEx_Points)

         #Assign a rank based on FedEx Cup Points
        fedex <- fedex %>% 
            group_by(Year) %>% 
            mutate(Pos = min_rank(-FedEx_Points)) %>% 
            data.frame()

        temp <- rbind(temp, fedex)

    }

    data <- temp
    
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
        "Seanie McDermott",
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

  # OWGR Timeseries Data. 
  owgr_tseries <- reactive({

    # This should contain the same info as is being read from the MongoDB connection below. 
    #data <- read.csv("data/major_results.csv", stringsAsFactors=FALSE, check.names=FALSE)
    
    # Connect to your MongoDB instance.
    con <- mongo(
      collection = "major_results",
      db = "paptour_db",
      url = url,
      verbose = FALSE,
      options = ssl_options()
  )

    # Read data form collection into data.frame. 
    data <- con$find(query = '{}')

    # Make sure it's in the correct order. 
    data <- data[order(-data$Major, -data$Score),]

    # Disconnect
    rm(con)
    
    data$Events <- 1

    # Set up constants.
    N <- 8
    COST <- 0.5
    #weights <- (10:2) / 10.5
    weights <- (10:2) / 10

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

    data <- temp
    return(data)

  })

  owgr_players_results <- reactive({

    # Get the major results data for the expandable table.
    # Connect to your MongoDB instance.
    con <- mongo(
      collection = "major_results",
      db = "paptour_db",
      url = url,
      verbose = FALSE,
      options = ssl_options()
  )

    # Read data form collection into data.frame. 
    data <- con$find(query = '{}')

    # Make sure it's in the correct order. 
    data <- data[order(-data$Major, -data$Score),]

    # Disconnect
    rm(con)

    # Set up constants.
    weights <- (10:2) / 10

    last_eight_majors <- head(order(unique(data$Major)), 8)
    last_eight_majors_df <- data[data$Major %in% last_eight_majors, ]
    last_eight_majors_df <- unique(last_eight_majors_df[c("Major", "Date", "Venue") ])

    data <- data[data$Major %in% last_eight_majors, ]

    # major_results_data[major_results_data$Player %in% "Billy Archbold", ]



    # Assign points based on finishing position. 
    data <- data %>% 
        group_by(Major) %>% 
        # Handle playoff.
        mutate(Pos = min_rank(- (Score + coalesce(10 - playoff_win, 0)) )) %>% 
        data.frame()

    # Apply recency weighting to past N majors.
    data$Weighted_Score <- NA
    i <- 1
    for(Major in unique(data$Major) ){
        data[data$Major %in% Major,]$Weighted_Score <- data[data$Major %in% Major,]$Score * weights[i]
        i <- i + 1
    }

    pts_tbl <- data.frame(Pos = 1:6, Pts = c(6.5, 5:1))

    data <- left_join(data, pts_tbl, by=c("Pos" = "Pos"))

    data[is.na(data$Pts),]$Pts <- 0

    # Apply recency weighting to past N majors No. Events.
    data$Weighted_Pts <- NA
    i <- 1
    for(Major in unique(data$Major) ){
        data[data$Major %in% Major,]$Weighted_Pts <- data[data$Major %in% Major,]$Pts * weights[i]
        i <- i + 1
    }

    return(data)

  })

  # Data frame of the previous eight majors.
  last_eight_majors_df <- reactive({

    # Get the major results data for the expandable table.
    # Connect to your MongoDB instance.
    con <- mongo(
        collection = "major_results",
        db = "paptour_db",
        url = url,
        verbose = FALSE,
        options = ssl_options()
    )

    # Read data form collection into data.frame. 
    data <- con$find(query = '{}')

    # Make sure it's in the correct order. 
    data <- data[order(-data$Major, -data$Score),]

    # Disconnect
    rm(con)

    # Set up constants.
    weights <- (10:2) / 10

    last_eight_majors <- head(order(unique(data$Major)), 8)
    last_eight_majors_df <- data[data$Major %in% last_eight_majors, ]
    last_eight_majors_df <- unique(last_eight_majors_df[c("Major", "Date", "Venue") ])

    return(last_eight_majors_df)

  })

  # Handicap Timeseries Data. 
  handicap_timeseries <- reactive({

    # Read data from mongoDB
    #-----------------------------
    # Connect to your MongoDB instance.
    con <- mongo(
        collection = "major_results",
        db = "paptour_db",
        url = url,
        verbose = FALSE,
        options = ssl_options()
    )

    # Read data form collection into data.frame. 
    data <- con$find(query = '{}')

    data$Year <- lubridate::year(lubridate::dmy(data$Date))

    return(data)

  })

  #### Section 1: Home Page

  # Objective of this is to be the one-stop-shop for information.
  #  Should be kind of like the 'Dashboard' page of a Tableau or PowerBI dashboard.
  #  It's built off all the other tabs as small previews.
  #  Would be great if I could implement the navigate to tab based on click. 
  

  output$home <- renderUI({

    f$req_sign_in()

    HTML('
      <!-- Example of media query for landscape prompt on specific tab.  
      <div id="info">
        <p>Integer velit nulla, condimentum vitae risus ut, rhoncus vulputate quam. Fusce lacus elit, accumsan eu dolor vel, scelerisque pretium turpis. Vivamus ac lectus vitae enim lacinia fringilla vel id tellus. Curabitur pharetra tortor eget risus ornare scelerisque. Morbi tempus et felis vitae venenatis. Suspendisse vitae ultrices est, nec sagittis arcu.</p>
      </div> -->

      <!-- Carousel -->
      ================================================== 
      <div id="" style ="margin-right:-15px; margin-left:-15px;" class="carousel slide" data-ride="carousel">
        <div>
          <div class="item active">
            <img style="height: 100%; width: 100%; object-fit: cover" src="images/sthelensbay.jpeg" alt="1">
            <div class="container">
            </div>
          </div>
        </div>
      </div>
      <!-- /.carousel -->


      <div class="container marketing">

       <!-- <div class="row app-links">-->
        <div class="col-xs-6 col-sm-6 col-md-4">
          <!--<img src="images/calendar.svg" width="50%" alt="novice">-->
          <a onclick="$(`li:eq(1) a`).tab(`show`);">
            <i class="far fa-calendar-check"></i>
          </a>
          <div class="nav-icons">Schedule</div>
        </div><!-- /.col-md-4 -->
        <div class="col-xs-6 col-sm-6 col-md-4">
          <a onclick="$(`li:eq(2) a`).tab(`show`);">
            <i class="fas fa-trophy"></i>
          </a>
          <div class="nav-icons">FedEx</div>
        </div><!-- /.col-md-4 -->
        <div class="col-xs-6 col-sm-6 col-md-4">
          <a onclick="$(`li:eq(3) a`).tab(`show`);">
            <i class="fas fa-list-ol"></i>
          </a>
          <div class="nav-icons">OWGR</div>
        </div><!-- /.col-md-4 -->
       <!-- </div><!-- /.row -->
      <!--<br>-->
       <!--<div class="row app-links">-->
        <div class="col-xs-6 col-sm-6 col-md-4">
          <!--<img src="images/calendar.svg" width="50%" alt="novice">-->
          <a onclick="$(`li:eq(5) a`).tab(`show`);">
            <i class="fas fa-users"></i>
          </a>
          <div class="nav-icons">Players</div>
        </div><!-- /.col-md-4 -->
        <div class="col-xs-6 col-sm-6 col-md-4">
          <a onclick="$(`li:eq(6) a`).tab(`show`);">
            <i class="fas fa-chart-line"></i>
          </a>
          <div class="nav-icons">Stats</div>
        </div><!-- /.col-md-4 -->
        <div class="col-xs-6 col-sm-6 col-md-4">
          <a onclick="$(`li:eq(7) a`).tab(`show`);">
          <!--<i class="fas fa-users-cog"></i>-->
          <i class="fas fa-archive"></i>
          </a>
          <div class="nav-icons">Results</div>
        </div><!-- /.col-md-4 -->
       <!--</div><!-- /.row -->
      </div>

       <br>
       <br>

        <!-- START THE FEATURETTES -->

        <hr class="featurette-divider" > <!--style="border-top: 1px solid #504848 !important;">-->


      <div class="container marketing">
        <div class="row featurette center-on-xs">
        <div class="col-md-5 col-md-push-7">
          <img class="featurette-image img-responsive" src="images/pretty in pink.jpeg" alt = "">
          <!--<iframe width="80%" height = "550" src="https://www.instagram.com/p/CDGoNSrAV9J/embed/" frameborder="0"></iframe>-->
        </div>
        <div class="col-md-7 col-md-pull-5">
          <h2 class="featurette-heading">Pretty in Pink<br><span class="text-muted">Gearldine Comber</span></h2>
          <p class="lead">Some people will do anything for a 36 handicap. <a data-toggle="collapse" data-parent="#accordionOne" href="#collapseOne">Read more&hellip;</a></p>
           <div class="panel-group transparent-panel-group" id="accordionOne">
              <div class="panel panel-default">
                <div id="collapseOne" class="panel-collapse collapse">
                  <div class="panel-body text-left">
                    <p><img href="#" data-toggle="modal" data-target="#player-oisin" src="images/oisin.png" src="images/oisin.png" alt="Oisin Tyrell PNG" class="img-circle player_profile" style="width: 10%; height: 10%"><a href="#" data-toggle="modal" data-target="#player-oisin"> Oisin Tyrell,</a> Editor-in-chief</p>
                    <blockquote class="blockquote">
                      <p>
                        Geraldine (as he wishes to go by these days) is determined to convince his competitors that he should both be allowed to play off the red tees 
                         while being allowed brandish an outrageous handicap of 36.  It appears this three time major winner is willing to do whatever it takes to win.  While
                         this "killer" take-no-prisoners attitude is always bound to rub some people up the wrong way - his sheer cavalier nature has to be admired.
                      </p>
                      <br>
                      <p>
                        Do not let his "pretty-in-pink" style, or his superficial name change fool you.  Gearldine is single-minded in his (or her) mission to 
                        claim a fourth major title.
                      </p>
                      <br>
                      <p>
                        This reporter has recenty been speaking with the PAP Tour player formerly known as Gearoid. It seems the chips are stacked against him 
                        once again (certainly in his own mind), with the predictable flow of excuses wafting in; "I broke my driver last month"; "I am going for beers tongiht
                        so I will be off it in the morning"; "You chauvinist pigs wont even let me tee it up off the reds" etc...
                      </p>
                      <br>
                      <p>
                        Could this all be an elaborate ruse?  Smoke and mirrors and fifty shades of pink?  Only one person knows for sure.  
                      </p>
                      <br>
                      <p>
                        When the dust settles at 12:00 on Saturday morning, 24th June 2023 - we will all find out.  One things for sure - Geraldine will be teeing it
                        up off 20, with the rest of the boys on the white tee box. 
                      </p>
                    </blockquote>
                  </div>
                </div>
              </div>
            </div>
        </div>
        </div>

        <div style="height: 200px;"></div>

        <hr class="featurette-divider" > <!--style="border-top: 1px solid #504848 !important;">-->

        <div class="row featurette center-on-xs">
        <div class="col-md-5">
          <img class="featurette-image img-responsive" src="images/dr shanks.jpeg" alt = "">
        </div>
        <div class="col-md-7">
          <h2 class="featurette-heading">Dr. Shanks<br></h2>
          <p class="lead">Has the Doctor finally found a cure for his awful case of the shanks? <a data-toggle="collapse" data-parent="#accordionTwo" href="#collapseTwo">Read more&hellip;</a></p>
           <div class="panel-group transparent-panel-group" id="accordionTwo">
              <div class="panel panel-default">
                <div id="collapseTwo" class="panel-collapse collapse">
                  <div class="panel-body text-left">
                    <p><img href="#" data-toggle="modal" data-target="#player-oisin" src="images/oisin.png" src="images/oisin.png" alt="Oisin Tyrell PNG" class="img-circle player_profile" style="width: 10%; height: 10%"><a href="#" data-toggle="modal" data-target="#player-oisin"> Oisin Tyrell,</a> Editor-in-chief</p>
                    <blockquote class="blockquote">
                    <p>
                      You might wonder, why am I writing an article about myself?  Is this some kind of auto-biorgaphy?  No.  The simple fact is, I am a busy doctor 
                      who has outsourced his duties as editor-in-chielf.  
                    </p>
                    <br>
                    <p>
                      This doctor is never off-duty.  When he is not saving lives or taking selfies with his stethoscope in the canteen, he can be sometimes found hiding 
                      away in empty theatres, with a pen, a pad and a pitching wedge, trying to unravel the mystery (and misery) of why he so frequently shanks the golf ball.
                    </p>
                    <br>
                    <p>
                      His erratic swing and his love for the sciences is what earned Dr. Oisin Tyrell his nickname "Dr. Shanks" in the early days of his PAP Tour career.  Funnily 
                      enough, the career path he ended up choosing was to become a doctor.  A man known for not taking his medicine out on the course, trying all too frequently 
                      to pull off the hero shot, is now practicing what he dare not preach. 
                    </p>
                    <br>
                    <p>
                      In one of Dr. Shanks most recent (if not last) major starts - he arrived onto the tee box at Rathcore, hot off a grueling 24 hour shift.  Full 
                      of coffee and hope, he quickly learned that sleep is one of the key ingridents to a successful major campaign.  The doctor well and truly shanked 
                      his way around the course that day.  He can certainly be afforded some sympathy, given his noble profession and dedication to both his craft and 
                      his tee time.
                    </p>
                    <br>
                    <p>
                      We are all hoping Dr. Shanks does not repeat this mis-step and books time off to sleep before his upcoming major start on 
                      Saturday 24th June, 2023 - lest his nickname may swiftly transition to Dr. Stinky Stanks. 
                    </p>
                    </blockquote>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>

      </div>
      <!-- /END THE FEATURETTES -->

    ')

  })


  #### Section 2: Schedule

  # Schedule Table - Title. 
  output$majorScheduleTableTitle <- renderUI({

    f$req_sign_in()

    div(
      div("Major Schedule", HTML("<i id='majorScheduleTitleID' style='font-size:20px;' class='fas fa-info-circle'></i>"), class="table-title"),
      style = "margin-left:10px;",
      shinyBS::bsPopover("majorScheduleTitleID", "Major Schedule", "Upcoming major events. Expand table rows for more detail on upcoming majors.", placement = "bottom", trigger = "hover"),
      class="align"
    )

  }) 
  
  # Schedule Table - temp. 
  output$majorScheduleTable_temp <- renderReactable({

    f$req_sign_in()

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

        #if (index %in% c(1, 2, 3)) {
        if (index %in% c(1)) {
        
          map_data <- data[index, ]
   
          htmltools::div(
            ##htmltools::div(style = "padding-top: 26px; padding-left: 10px; padding-right: 10px;",
            ##
            ##  tags$iframe(
            ##    seamless = "seamless",
            ##    src = paste0("https://forecast.io/embed/#lat=", map_data$lat, "&lon=", map_data$lon, "&name=", map_data$Course, ", ", map_data$Location, "&units=ca"),
            ##    width = "100%",
            ##    height = 250,
            ##    style = "border:0px;"
            ##  )
            ##
            ##),
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
                #addFullscreenControl() %>% 
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
      }
    )

  })

  # Schedule Table. 
  output$majorScheduleTable <- renderUI({
    
    f$req_sign_in()

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

  # fedExHeadlineBox
  output$fedEx_fedExHeadlineBox <- renderUI({

    f$req_sign_in()

    # Load data. 
    # TO DO:
    # Currently being read from CSV> - change to RSQLite DB table on Shiny Server.
    data <- fedex_data()
    data$Score <- dplyr::coalesce(data$Score, 0)
    data$Events <- dplyr::coalesce(data$Events, 0)
    players_data <- players_data()


    # Load major_results_data.
    major_results_data <- major_results_data()

    # Create high-level major results data.
    major_results_data_temp1 <- major_results_data %>% 
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
    major_results_data_temp2 <- major_results_data %>%       
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

    major_winners <- table(major_results_data$Winner) %>% data.frame()
    major_winners <- major_winners[order(-major_winners$Freq), ]

    data <- dplyr::left_join(data, major_winners, by = c("Player" = "Var1"))
    data <- data %>% 
      rename('Major Wins' = Freq, "FedEx Points" = FedEx_Points)


    data <- data %>%
      select(
        "Year", 
        "Pos", 
        "Player", 
        "FedEx Points", 
        "Events", 
        "Major Wins"
      ) %>% 
        rename(
          "Rank" = "Pos",
          "Events Played" = "Events"
          )

    # Replace NAs with 0 in Major wins column. 
    data$'Major Wins' <- dplyr::coalesce(data$'Major Wins', 0)

    data <- data %>% select(-'Major Wins') 

    # Subset data for current season/year. 
    data <- data[data$Year %in% max(data$Year), ]

    # Subset data for top ranked golfer(s).
    data <- data[data$'FedEx Points' == max(data$'FedEx Points'), ]

    if(nrow(data) > 1){
      text <- " (Multiple  Golfers)"

      labelIcon <- tags$i(class = "fab fa-fedex", style = "color:white;")

      div(createInfoBox2("+ ", text, labelIcon, "FedEx Cup Leader"), class = "combo-box combo-grey")

    } else{

      # Get first inital + surname to fit in info box. 
      text <- paste0(
        " (",
        substr(data$Player, 1, 1),
        ". ", 
        sub(".* ", "", data$Player),
        ")"
      )

      labelIcon <- tags$i(class = "fab fa-fedex", style = "color:white;")

      div(createInfoBox(text, labelIcon, "FedEx Cup Leader"), class = "combo-box combo-grey")

    }



  })

  # Points Leader
  output$rankingPlayer <- renderUI({

    f$req_sign_in()

    outputValue <- "Billy Archbold"
    labelIcon <- tags$i(class = "fad fa-digging fa-fw")
    labelText = "Golfer of the Year"
    div(createInfoBox(outputValue, labelIcon, labelText), class = "combo-box combo-teal")
  })

  # World Ranking Points
  output$rankingPoints <- renderUI({

    f$req_sign_in()

    outputValue <- 34
    labelIcon <- tags$i(class = "fad fa-digging fa-fw")
    labelText = "OWGR Points"
    div(createInfoBox(outputValue, labelIcon, labelText), class = "combo-box combo-teal")
  })

  # FedEx Cup Table - Title.
  output$fedExCupMainTableTitle <- renderUI({

    f$req_sign_in()

    data <- fedex_data()

    current_year <- lubridate::year(Sys.Date())

    div(
      div(paste0("FedEx Cup Standings (", current_year, ")"), HTML("<i id='fedExCupMainTitleID' style='font-size:20px;' class='fas fa-info-circle'></i>"), class="table-title"),
      style = "margin-left:10px;",
      shinyBS::bsPopover("fedExCupMainTitleID", paste0("FedEx Cup Standings (", current_year, ")"), "FedEx Cup standings for the current season.", placement = "bottom", trigger = "hover"),
      class="align"
    )

  })

  # FedEx Cup Table - temp.
  output$fedExCupMainTable_temp <- renderReactable({

    f$req_sign_in()

    if(input$isMobile){
      var_width <- 150
      var_width_rank <- 60
    } 
      else{
        var_width <- 200
        var_width_rank <- 120
      }

    # Load data. 
    current_year <- lubridate::year(Sys.Date())
    # TO DO:
    # Currently being read from CSV> - change to RSQLite DB table on Shiny Server.
    data <- fedex_data()
    data$Score <- dplyr::coalesce(data$Score, 0)
    data$Events <- dplyr::coalesce(data$Events, 0)
    players_data <- players_data()


    # Load major_results_data.
    major_results_data <- major_results_data()

    # Create high-level major results data.
    major_results_data_temp1 <- major_results_data %>% 
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
    major_results_data_temp2 <- major_results_data %>%       
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

    major_winners <- table(major_results_data$Winner) %>% data.frame()
    major_winners <- major_winners[order(-major_winners$Freq), ]

    data <- dplyr::left_join(data, major_winners, by = c("Player" = "Var1"))
    data <- data %>% 
      rename('Major Wins' = Freq, "FedEx Points" = FedEx_Points)


    data <- data %>%
      select(
        "Year", 
        "Pos", 
        "Player", 
        "FedEx Points", 
        "Events", 
        "Major Wins"
      ) %>% 
        rename(
          "Rank" = "Pos",
          "Events Played" = "Events"
          )

    # Replace NAs with 0 in Major wins column. 
    data$'Major Wins' <- dplyr::coalesce(data$'Major Wins', 0)


    # Sticky column CSS.
    sticky_style_one <- list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
    sticky_style_two <- list(position = "sticky", left = var_width_rank, background = "#fff", zIndex = 1, borderRight = "1px solid #eee")

    data <- data %>% select(-'Major Wins') 

    # Subset data for current season/year. 
    data <- data[data$Year %in% max(data$Year), ]

    # Catch for start of new season.  If current_year (from Sys.Date()) is greater than the max(data$Year),  
    #  then don't display any of the previous years data BUT retain the players info (i.e. rows and cols.
    if(current_year > max(data$Year)){
      prev_year <- data$Year
      data$Year <- current_year
      data$Rank <- 1
      data$'FedEx Points' <- 0
      data$'Events Played' <- 0
    }

    # Build the data table.
    reactable(
      data,
      filterable = TRUE,
      searchable = FALSE,
      highlight = TRUE,
      #pagination = FALSE,
      #height = 500,
      minRows = 10,
      defaultPageSize = 10,
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
        Player = colDef(
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
        'Year' = colDef(
          minWidth = var_width_rank,
          maxWidth = var_width_rank,
          width = var_width_rank,
          #style = sticky_style_one,
          #headerStyle = sticky_style_one,
          #class = "sticky left-col-1",
          #headerClass = "sticky left-col-1",
          align = "left"
        ),
        'FedEx Points' = colDef(
          align = "left", 
          cell = function(value){

            if(current_year == max(data$Year)){
              
              width <- paste0(round(value / max(data$'FedEx Points') * 100), "%")
              bar_chart(format(round(value, 0), nsmall = 0), width = width, fill = "#67c9c5", background = "#e1e1e1")
            
            }

          } #57636b
        ),
        'Events Played' = colDef(
          minWidth = var_width_rank,
          maxWidth = var_width_rank,
          width = var_width_rank,
          #style = sticky_style_one,
          #headerStyle = sticky_style_one,
          #class = "sticky left-col-1",
          #headerClass = "sticky left-col-1",
          align = "left"
        )
      )
    )

  })

  # FedEx Cup Table.
  output$fedExCupMainTable <- renderUI({

    f$req_sign_in()

   if(input$isMobile){
     var_width <- "width:90%;"
   } 
     else{
       var_width <- "width:90%;"
     }

    div(reactableOutput("fedExCupMainTable_temp"), style = var_width, class="reactBox align")

  })

  # FedEx Cup History Table - Title.
  output$fedExCupHistoryTableTitle <- renderUI({

    f$req_sign_in()

    data <- fedex_data()

    div(
      div("Past FedEx Cup Champions", HTML("<i id='fedExCupHistoryTitleID' style='font-size:20px;' class='fas fa-info-circle'></i>"), class="table-title"),
      style = "margin-left:10px;",
      shinyBS::bsPopover("fedExCupHistoryTitleID", "Past FedEx Cup Seasons", "FedEx Cup results for previous seasons.", placement = "bottom", trigger = "hover"),
      class="align"
    )

  })

  # FedEx Cup History Table - temp.
  output$fedExCupHistoryTable_temp <- renderReactable({

    f$req_sign_in()

    if(input$isMobile){
      var_width <- 150
      var_width_rank <- 60
    } 
      else{
        var_width <- 200
        var_width_rank <- 120
      }

    # Load data. 
    current_year <- lubridate::year(Sys.Date())
    # TO DO:
    # Currently being read from CSV> - change to RSQLite DB table on Shiny Server.
    data <- fedex_data()
    data$Score <- dplyr::coalesce(data$Score, 0)
    data$Events <- dplyr::coalesce(data$Events, 0)
    players_data <- players_data()


    # Load major_results_data.
    major_results_data <- major_results_data()

    # Create high-level major results data.
    major_results_data_temp1 <- major_results_data %>% 
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
    major_results_data_temp2 <- major_results_data %>%       
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

    major_winners <- table(major_results_data$Winner) %>% data.frame()
    major_winners <- major_winners[order(-major_winners$Freq), ]

    data <- dplyr::left_join(data, major_winners, by = c("Player" = "Var1"))
    data <- data %>% 
      rename('Major Wins' = Freq, "FedEx Points" = FedEx_Points)


    data <- data %>%
      select(
        "Year",
        "Pos", 
        "Player", 
        "FedEx Points", 
        "Events", 
        "Major Wins"
      ) %>% 
        rename(
          "Rank" = "Pos",
          "Events Played" = "Events"
          )

    # Replace NAs with 0 in Major wins column. 
    data$'Major Wins' <- dplyr::coalesce(data$'Major Wins', 0)

    data_checkpoint <- data

    # Create high-level summary of FedEx Cup winners for each season. 
    data <- data[data$Rank %in% min(data$Rank), ]

    # Remove current year.
    data <- data[data$Year != current_year, ]

    data <- data %>% rename('All Time Major Wins' = 'Major Wins')


    # Sticky column CSS.
    sticky_style_one <- list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
    sticky_style_two <- list(position = "sticky", left = var_width_rank, background = "#fff", zIndex = 1, borderRight = "1px solid #eee")

    data <- data[order(data$Year, decreasing=TRUE), ]

    # Build the data table.
    reactable(
      data,
      filterable = TRUE,
      searchable = FALSE,
      highlight = TRUE,
      #pagination = FALSE,
      #height = 500,
      minRows = 10,
      defaultPageSize = 10,
      theme = reactableTheme(
        style = list(fontSize = FONT_SIZE)
      ),
      columns = list(
        'Year' = colDef(
          minWidth = var_width_rank,
          maxWidth = var_width_rank,
          width = var_width_rank,
          #style = sticky_style_one,
          #headerStyle = sticky_style_one,
          #class = "sticky left-col-1",
          #headerClass = "sticky left-col-1",
          align = "left"
        ),
        'Player' = colDef(
          minWidth = var_width_rank,
          maxWidth = var_width_rank,
          width = var_width_rank,
          #style = sticky_style_one,
          #headerStyle = sticky_style_one,
          #class = "sticky left-col-1",
          #headerClass = "sticky left-col-1",
          align = "left"
        ),
        'FedEx Points' = colDef(
          minWidth = var_width_rank,
          maxWidth = var_width_rank,
          width = var_width_rank,
          #style = sticky_style_one,
          #headerStyle = sticky_style_one,
          #class = "sticky left-col-1",
          #headerClass = "sticky left-col-1",
          align = "left"
        ),
        'All Time Major Wins' = colDef(
          minWidth = 200,
          maxWidth = 200,
          width = 200,
          cell = function(value){
            rating_stars(value)
          }
        )
      ),
      details = function(index) {
        year_data <- data_checkpoint[data_checkpoint$Year == data$Year[index], ]
        htmltools::div(style = "padding: 16px",
          reactable(
            year_data[
              c("Rank",
                "Player",
                "FedEx Points",
                "Events Played") # TO DO: would be useful here to show no. majors won that particular year/season...
            ], 
            outlined = TRUE,
            highlight = TRUE
          )
        )
      }
    )

  })

  # FedEx Cup History Table.
  output$fedExCupHistoryTable <- renderUI({

    f$req_sign_in()

   if(input$isMobile){
     var_width <- "width:90%;"
   } 
     else{
       var_width <- "width:90%;"
     }

    div(reactableOutput("fedExCupHistoryTable_temp"), style = var_width, class="reactBox align")

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

  # FedEx Infographic
  output$fedexInfogrphic <- renderUI({

    f$req_sign_in()

    div(style = "margin:50px;",
      HTML('    
        <div class="row example-centered">
            <div class="col-md-12 example-title">
                <h2>FedEx Cup Explained</h2>
            </div>
            <div class="col-xs-10 col-xs-offset-1 col-sm-8 col-sm-offset-2">
                <ul class="timeline timeline-centered">
                    <li class="timeline-item">
                        <div class="timeline-info">
                            <span>Note 1)</span>
                        </div>
                        <div class="timeline-marker"></div>
                        <div class="timeline-content">
                            <p>FedEx Cup points are awarded based on results for the current season.</p>
                        </div>
                    </li>
                    <li class="timeline-item">
                        <div class="timeline-info">
                            <span>Note 2)</span>
                        </div>
                        <div class="timeline-marker"></div>
                        <div class="timeline-content">
                            <p>Points are awarded to the top 6 places as follows; <br>
                            <div style = "float:right;">
                            <table style="width:70%;">
                              <tr>
                                <th>Place</th>
                                <th>Points</th>
                              </tr>
                              <tr>
                                <td>1st</td>
                                <td>650</td>
                              </tr>
                              <tr>
                                <td>2nd</td>
                                <td>500</td>
                              </tr>
                              <tr>
                                <td>3rd</td>
                                <td>400</td>
                              </tr>
                              <tr>
                                <td>4th</td>
                                <td>300</td>
                              </tr>
                              <tr>
                                <td>5th</td>
                                <td>200</td>
                              </tr>
                              <tr>
                                <td>6th</td>
                                <td>100</td>
                              </tr>
                            </table>
                            </div>
                            </p>
                        </div>
                    </li>
                </ul>
            </div>
        </div>'
    ))

  })















  #### Section 4: OWGR

  # owgrHeadlineBox
  output$owgr_owgrHeadlineBox <- renderUI({

    f$req_sign_in()

    # Load data. 
    data <- owgr_tseries()
    players_data <- players_data()

    # Most recent handicaps from latest major attended.   
    data <- data %>% 
        group_by(Player) %>%
        top_n(1, abs(Ranking_Period)) %>% 
        data.frame()

    data <- data[order(data$OWGR, decreasing=TRUE),]

    data <- data %>%
      rename('Ranking Period' = Ranking_Period) %>% 
      rename('No. Events Entered' = Events) %>% 
      rename('Score' = Score_sum) %>% 
      rename('Weighted Score' = Weighted_Score_sum) %>% 
      rename('Pts' = Pts_sum) %>% 
      rename('Weighted Pts' = Weighted_Pts_sum) 
      
      data <- data %>% 
        mutate( Rank = dense_rank(-data$OWGR) )

      data <- data %>% select(
        Rank, 
        Player, 
        OWGR
      )

      data$OWGR <- round(data$OWGR, 2)

      # Get the current top ranked golfer. 
      top_ranked_golfer <- head(data, 1)
      # Get first inital + surname to fit in info box. 
      top_ranked_golfer <- paste0(
        substr(top_ranked_golfer$Player, 1, 1),
        ". ", 
        sub(".* ", "", top_ranked_golfer$Player)
      )

      labelIcon <- tags$i(class = "fas fa-list-ol", style = "color:white;")

      div(createInfoBox(top_ranked_golfer, labelIcon, "No. 1 Ranked Player"), class = "combo-box combo-dark")

  })

  # OWGR Timeseries Chart.
  output$owgrTimeseries <- renderPlotly({

    f$req_sign_in()

    # Load OWGR Timeseries data. 
    data <- owgr_tseries()

    Event <- reorder(paste0("Major ", data$Ranking_Period, " (", lubridate::year(data$Date), ")"), data$Ranking_Period)

    # Plot
    p <- data %>%
      ggplot( aes(x=Event, y=OWGR, group=Player, color=Player)) +
        geom_line() +
        geom_point() +
        #scale_color_viridis(discrete = TRUE) +
        ggtitle("OWGR Ranking Timeseries") +
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

  # OWGR Timeseries Chart (conditional UI for mobile/desktop)
  output$owgrTimeseries_conditional <- renderUI({

    f$req_sign_in()

    if(input$isMobile){
      div(NULL)
    }else{
        div(
          shinycssloaders::withSpinner(
            plotlyOutput("owgrTimeseries", width="1300px", height="600px"),
            type = 8, 
            color = "#233845"
          ), style = 'display: inline-block;'
        )
    }

  })

  # OWGR Table - Title. 
  output$owgrMainTableTitle <- renderUI({

    f$req_sign_in()

    div(
      div("OWGR Rankings", HTML("<i id='owgrMainTableTitleID' style='font-size:20px;' class='fas fa-info-circle'></i>"), class="table-title"),
      style = "margin-left:10px;",
      shinyBS::bsPopover("owgrMainTableTitleID", "OWGR Standings", "Official World Golf Rankings for the PAP Pro Tour.", placement = "bottom", trigger = "hover"),
      class="align"
    )

  })

  # OWGR Table - temp.
  output$owgrMainTable_temp <- renderReactable({

    f$req_sign_in()

    if(input$isMobile){
      var_width <- 120
      var_width_rank <- 60
    } 
      else{
        var_width <- 200
        var_width_rank <- 120
      }

    # Load data. 
    data <- owgr_tseries()
    players_data <- players_data()
    owgr_players_results <- owgr_players_results()
    last_eight_majors_df <- last_eight_majors_df()




    # Most recent handicaps from latest major attended.   
    data <- data %>% 
        group_by(Player) %>%
        top_n(1, abs(Ranking_Period)) %>% 
        data.frame()

    data <- data[order(data$OWGR, decreasing=TRUE),]

    data <- data %>%
      rename('Ranking Period' = Ranking_Period) %>% 
      rename('No. Events Entered' = Events) %>% 
      rename('Score' = Score_sum) %>% 
      rename('Weighted Score' = Weighted_Score_sum) %>% 
      rename('Pts' = Pts_sum) %>% 
      rename('Weighted Pts' = Weighted_Pts_sum) 
      
      data <- data %>% 
        mutate( Rank = dense_rank(-data$OWGR) )

      data <- data %>% select(
        Rank, 
        'Ranking Period',
        Player, 
        OWGR,
        'No. Events Entered',
        Score,
        'Weighted Score', 
        Pts, 
        'Weighted Pts'
      )

      data$OWGR <- round(data$OWGR, 2)
      data$Score <- round(data$Score, 2)
      data$'Weighted Score' <- round(data$'Weighted Score', 2)
      data$Pts <- round(data$Pts, 2)
      data$'Weighted Pts' <- round(data$'Weighted Pts', 2)

    # Build the data table.
    reactable(
      data,
      filterable = TRUE,
      searchable = FALSE,
      highlight = TRUE,
      #pagination = FALSE,
      #height = 500,
      minRows = 10,
      defaultPageSize = 10,
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
      ),
      details = function(index) {

        owgr_players_results <- owgr_players_results[
          owgr_players_results$Player == data$Player[index],
        ]

        owgr_players_results <- owgr_players_results[
              c(
                "Major",
                "Date",
                "Venue",
                "Player",
                "Handicap",
                "Pos",
                "Score",
                "Weighted_Score",
                "Pts",
                "Weighted_Pts"
              )
            ]
        
        # Rename fields
        owgr_players_results <- owgr_players_results %>% 
          rename(
            Position = Pos,
            'Weighted Score' = Weighted_Score,
            'Weighted Points' = Weighted_Pts,
          )


        htmltools::div(style = "padding: 16px",
          reactable(
            owgr_players_results, 
            outlined = TRUE,
            highlight = TRUE,
            columns = list(
              Major = colDef(
                footer = "Total"
              ),
              Venue = colDef(
                minWidth = var_width,
                maxWidth = var_width,
                width = var_width,
                align = "left"
              ),
              Player = colDef(
                minWidth = var_width,
                maxWidth = var_width,
                width = var_width,
                align = "left"
              ),
              Score = colDef(
                footer = JS("function(colInfo) {
                    var total = 0
                    colInfo.data.forEach(function(row) {
                      total += row[colInfo.column.id]
                    })
                    return total.toFixed(2)
                  }")
              ),
              'Weighted Score' = colDef(
                minWidth = var_width,
                maxWidth = var_width,
                width = var_width,
                align = "left",
                footer = JS("function(colInfo) {
                    var total = 0
                    colInfo.data.forEach(function(row) {
                      total += row[colInfo.column.id]
                    })
                    return total.toFixed(2)
                  }"
                )
              ),
              Pts = colDef(
                minWidth = var_width,
                maxWidth = var_width,
                width = var_width,
                align = "left",
                footer = JS("function(colInfo) {
                    var total = 0
                    colInfo.data.forEach(function(row) {
                      total += row[colInfo.column.id]
                    })
                    return total.toFixed(2)
                  }")
              ),
              'Weighted Points' = colDef(
                minWidth = var_width,
                maxWidth = var_width,
                width = var_width,
                align = "left",
                footer = JS("function(colInfo) {
                    var total = 0
                    colInfo.data.forEach(function(row) {
                      total += row[colInfo.column.id]
                    })
                    return total.toFixed(2)
                  }")
              )
            ),
            defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
          )
        )
      }
    )


  })

  # OWGR Table. 
  output$owgrMainTable <- renderUI({

    f$req_sign_in()
    
   if(input$isMobile){
     var_width <- "width:90%;"
   } 
     else{
       var_width <- "width:90%;"
     }

    div(
      reactableOutput("owgrMainTable_temp"), 
      style = var_width, 
      class="reactBox align"
    )

  })

  # OWGR Infographic
  output$owgrInfogrphic <- renderUI({

    f$req_sign_in()

    div(style = "margin:50px;",
      HTML('    
        <div class="row example-centered">
            <div class="col-md-12 example-title">
                <h2>OWGR Rankings Explained</h2>
            </div>
            <div class="col-xs-10 col-xs-offset-1 col-sm-8 col-sm-offset-2">
                <ul class="timeline timeline-centered">
                    <li class="timeline-item">
                        <div class="timeline-info">
                            <span>Note 1)</span>
                        </div>
                        <div class="timeline-marker"></div>
                        <div class="timeline-content">
                            <p>Rankings are calculated based on results over the last 8 major championships.</p><br><p>Recency plays key role in determining OWGR rankings (i.e. recent majors hold more weight on your ranking).</p>
                        </div>
                    </li>
                    <li class="timeline-item">
                        <div class="timeline-info">
                            <span>Note 2)</span>
                        </div>
                        <div class="timeline-marker"></div>
                        <div class="timeline-content">
                            <p>Players will not recieve a negative ranking. The lowest ranking you can have is 0.</p>
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
                            <p>Each player is deducted 0.5 ranking points on entry to a major.</p>
                        </div>
                    </li>
                    <li class="timeline-item">
                        <div class="timeline-info">
                            <span>Step 2)</span>
                        </div>
                        <div class="timeline-marker"></div>
                        <div class="timeline-content">
                            <p>Points are awarded to the top 6 places as follows; <br>
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
                            <p>Points awarded for the most recent major are worth full value.</p>
                            <br>
                            <p>Points awarded for each preceding major reduce by 10% in value until they fall out 
                            of the calculation (i.e. they were awarded more than 8 majors ago and are therefore not taken into consideration).
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
                              Your total weighted points are calculated for the previous 8 majors.  
                            </p>
                        </div>
                    </li>
                    <li class="timeline-item">
                        <div class="timeline-info">
                            <span>Step 5)</span>
                        </div>
                        <div class="timeline-marker"></div>
                        <div class="timeline-content">
                            <p>
                              Finally, your stableford scores over the previous 8 majors are summed and weighted in the same way as the points (described above). 
                            </p>
                              <br>
                            <p>
                              This weighted stableford score is then divided by 100 and added to your weighted points total. This is intended to reward high-scores posted in majors.
                            </p>
                            <br>
                            <p>
                              <table style="width:100%">
                              <tr>
                                <th>OWGR = Weighted Points - Cost of Entry + (Weighted Stableford Score / 100)</th>
                              </tr>
                            </table>
                            </p>
                        </div>
                    </li>
                </ul>
            </div>
        </div>'
    ))

  })

  # ShinyAlert modal if portait & on mobile. 
  observeEvent(input$navBar, {

    # Get JavaScript to check if the device is in Portrait or Landscape mode.
    shinyjs::runjs("
      if(window.innerHeight < window.innerWidth){
        
        Shiny.setInputValue('landscapeMode_owgr', null);
        Shiny.setInputValue('landscapeMode_owgr', 'yes');
        
      } else{
          
          Shiny.setInputValue('landscapeMode_owgr', null);
          Shiny.setInputValue('landscapeMode_owgr', 'no');

      }
    ")
  })

  observeEvent(input$landscapeMode_owgr, {

    if(input$navBar %in% "OWGR" && is_mobile() && input$landscapeMode_owgr %in% "no"){

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





  #### Section 5: Stats

  #owgrHeadlineBox
  output$owgrHeadlineBox <- renderUI({

    f$req_sign_in()

    # Load data. 
    data <- owgr_tseries()
    players_data <- players_data()

    # Most recent handicaps from latest major attended.   
    data <- data %>% 
        group_by(Player) %>%
        top_n(1, abs(Ranking_Period)) %>% 
        data.frame()

    data <- data[order(data$OWGR, decreasing=TRUE),]

    data <- data %>%
      rename('Ranking Period' = Ranking_Period) %>% 
      rename('No. Events Entered' = Events) %>% 
      rename('Score' = Score_sum) %>% 
      rename('Weighted Score' = Weighted_Score_sum) %>% 
      rename('Pts' = Pts_sum) %>% 
      rename('Weighted Pts' = Weighted_Pts_sum) 
      
      data <- data %>% 
        mutate( Rank = dense_rank(-data$OWGR) )

      data <- data %>% select(
        Rank, 
        Player, 
        OWGR
      )

      data$OWGR <- round(data$OWGR, 2)

      # Get the current top ranked golfer. 
      top_ranked_golfer <- head(data, 1)
      # Get first inital + surname to fit in info box. 
      top_ranked_golfer <- paste0(
        substr(top_ranked_golfer$Player, 1, 1),
        ". ", 
        sub(".* ", "", top_ranked_golfer$Player)
      )

      labelIcon <- tags$i(class = "fas fa-list-ol", style = "color:white;")

      div(createInfoBox(top_ranked_golfer, labelIcon, "No. 1 Ranked Player"), class = "combo-box combo-dark")

  })

  #fedExHeadlineBox
  output$fedExHeadlineBox <- renderUI({

    f$req_sign_in()

    # Load data. 
    # TO DO:
    # Currently being read from CSV> - change to RSQLite DB table on Shiny Server.
    data <- fedex_data()
    data$Score <- dplyr::coalesce(data$Score, 0)
    data$Events <- dplyr::coalesce(data$Events, 0)
    players_data <- players_data()


    # Load major_results_data.
    major_results_data <- major_results_data()

    # Create high-level major results data.
    major_results_data_temp1 <- major_results_data %>% 
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
    major_results_data_temp2 <- major_results_data %>%       
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

    major_winners <- table(major_results_data$Winner) %>% data.frame()
    major_winners <- major_winners[order(-major_winners$Freq), ]

    data <- dplyr::left_join(data, major_winners, by = c("Player" = "Var1"))
    data <- data %>% 
      rename('Major Wins' = Freq, "FedEx Points" = FedEx_Points)


    data <- data %>%
      select(
        "Year", 
        "Pos", 
        "Player", 
        "FedEx Points", 
        "Events", 
        "Major Wins"
      ) %>% 
        rename(
          "Rank" = "Pos",
          "Events Played" = "Events"
          )

    # Replace NAs with 0 in Major wins column. 
    data$'Major Wins' <- dplyr::coalesce(data$'Major Wins', 0)

    data <- data %>% select(-'Major Wins') 

    # Subset data for current season/year. 
    data <- data[data$Year %in% max(data$Year), ]

    # Subset data for top ranked golfer(s).
    data <- data[data$'FedEx Points' == max(data$'FedEx Points'), ]

    if(nrow(data) > 1){
      text <- " (Multiple  Golfers)"

      labelIcon <- tags$i(class = "fab fa-fedex", style = "color:white;")

      div(createInfoBox2("+ ", text, labelIcon, "FedEx Cup Leader"), class = "combo-box combo-grey")

    } else{

      # Get first inital + surname to fit in info box. 
      text <- paste0(
        " (",
        substr(data$Player, 1, 1),
        ". ", 
        sub(".* ", "", data$Player),
        ")"
      )

      labelIcon <- tags$i(class = "fab fa-fedex", style = "color:white;")

      div(createInfoBox(text, labelIcon, "FedEx Cup Leader"), class = "combo-box combo-grey")

    }



  })

  #majorWinsHeadlineBox
  output$majorWinsHeadlineBox <- renderUI({

    f$req_sign_in()

    # Load data. 
    data <- major_results_data()
    players_data <- players_data()

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
      labelIcon <- tags$i(class = "fas fa-list-ol", style = "color:white;")

      data <- data[data$Wins %in% max(data$Wins), ]

      if(nrow(data) > 1){
        text <- " (Multiple Golfers)"
      } else{

        # Get first inital + surname to fit in info box. 
        text <- paste0(
          " (",
          substr(data$Player, 1, 1),
          ". ", 
          sub(".* ", "", data$Player),
          ")"
        )

      }


      labelIcon <- tags$i(class = "fas fa-trophy", style = "color:white;")

      div(createInfoBox2(max(data$Wins), text, labelIcon, "Most Major Wins"), class = "combo-box combo-teal")

  })

  #lowestHanicapHeadlineBox
  output$lowestHanicapHeadlineBox <- renderUI({

    f$req_sign_in()

    # Load data.
    data <- major_results_data()
    players_data <- players_data()

    # Most recent handicaps from latest major attended.   
    data <- data %>% 
        group_by(Player) %>%
        top_n(1, abs(Major)) %>% 
        data.frame()

    data <- data[order(data$Handicap, decreasing=FALSE),]

    data <- data %>% 
      mutate( Rank = dense_rank(data$Handicap) )

    # Subset data to lowest handicap. 
    data <- data[data$Handicap == min(data$Handicap), ]

    if(nrow(data) > 1){
        text <- " (Multiple Golfers)"
      } else{
        
        # Get first inital + surname to fit in info box. 
        text <- paste0(
          " (",
          substr(data$Player, 1, 1),
          ". ", 
          sub(".* ", "", data$Player),
          ")"
        )

      }

    labelIcon <- tags$i(class = "fas fa-angle-down", style = "color:white;")

    div(createInfoBox2(min(data$Handicap), text, labelIcon, "Lowest Handicap"), class = "combo-box combo-blue")

  })


  # OWGR -----------
  # OWGR - Title. 
  output$owgrTitle <- renderUI({

    f$req_sign_in()

    div(
      div("OWGR Rankings", HTML("<i id='owgrTitleID' style='font-size:20px;' class='fas fa-info-circle'></i>"), class="table-title"),
      style = "margin-left:10px;",
      shinyBS::bsPopover("owgrTitleID", "OWGR Standings", "Official World Golf Rankings for the PAP Pro Tour.", placement = "bottom", trigger = "hover"),
      class="align"
    )

  })

  # OWGR - temp.
  output$owgr_temp <- renderReactable({

    f$req_sign_in()

    if(input$isMobile){
      var_width <- 150
      var_width_rank <- 60
    } 
      else{
        var_width <- 200
        var_width_rank <- 120
      }

    # Load data. 
    data <- owgr_tseries()
    players_data <- players_data()

    # Most recent handicaps from latest major attended.   
    data <- data %>% 
        group_by(Player) %>%
        top_n(1, abs(Ranking_Period)) %>% 
        data.frame()

    data <- data[order(data$OWGR, decreasing=TRUE),]

    data <- data %>%
      rename('Ranking Period' = Ranking_Period) %>% 
      rename('No. Events Entered' = Events) %>% 
      rename('Score' = Score_sum) %>% 
      rename('Weighted Score' = Weighted_Score_sum) %>% 
      rename('Pts' = Pts_sum) %>% 
      rename('Weighted Pts' = Weighted_Pts_sum) 
      
      data <- data %>% 
        mutate( Rank = dense_rank(-data$OWGR) )

      data <- data %>% select(
        Rank, 
        Player, 
        OWGR
      )

      data$OWGR <- round(data$OWGR, 2)

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

  # OWGR Cup. 
  output$owgr <- renderUI({

    f$req_sign_in()
    
   if(input$isMobile){
     var_width <- "width:100%;"
   } 
     else{
       var_width <- "width:500px;"
     }

    div(reactableOutput("owgr_temp"), style = var_width, class="reactBox align")

  })

  # See More button for OWGR tab.
  output$owgrSeeMore <- renderUI({

    f$req_sign_in()

    actionLink("owgrSeeMore_input", "See More")

  })

  # Jump user to OWGR tab. 
  observeEvent(input$owgrSeeMore_input, {

    updateTabsetPanel(session, "navBar", selected = "OWGR")

  })

  # FedEx Cup -----------
  # FedEx Cup - Title. 
  output$fedExCupTitle <- renderUI({

    f$req_sign_in()

    data <- fedex_data()

    div(
      div(paste0("FedEx Cup Standings (", max(data$Year), ")"), HTML("<i id='fedExCupTitleID' style='font-size:20px;' class='fas fa-info-circle'></i>"), class="table-title"),
      style = "margin-left:10px;",
      shinyBS::bsPopover("fedExCupTitleID", "FedEx Cup Standings", "FedEx Cup standings for the current season.", placement = "bottom", trigger = "hover"),
      class="align"
    )

  })

  # FedEx Cup - temp.
  output$fedExCup_temp <- renderReactable({

    f$req_sign_in()

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
    data <- fedex_data()
    data$Score <- dplyr::coalesce(data$Score, 0)
    data$Events <- dplyr::coalesce(data$Events, 0)
    players_data <- players_data()


    # Load major_results_data.
    major_results_data <- major_results_data()

    # Create high-level major results data.
    major_results_data_temp1 <- major_results_data %>% 
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
    major_results_data_temp2 <- major_results_data %>%       
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

    major_winners <- table(major_results_data$Winner) %>% data.frame()
    major_winners <- major_winners[order(-major_winners$Freq), ]

    data <- dplyr::left_join(data, major_winners, by = c("Player" = "Var1"))
    data <- data %>% 
      rename('Major Wins' = Freq, "FedEx Points" = FedEx_Points)


    data <- data %>%
      select(
        "Year", 
        "Pos", 
        "Player", 
        "FedEx Points", 
        "Events", 
        "Major Wins"
      ) %>% 
        rename(
          "Rank" = "Pos",
          "Events Played" = "Events"
          )

    # Replace NAs with 0 in Major wins column. 
    data$'Major Wins' <- dplyr::coalesce(data$'Major Wins', 0)

    data <- data %>% select(-'Major Wins') 

    # Subset data for current season/year. 
    data <- data[data$Year %in% max(data$Year), ]

    data <- data %>% select("Rank", "Player", "FedEx Points")

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

    f$req_sign_in()
    
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

    f$req_sign_in()

    actionLink("fedExCupSeeMore_input", "See More")

  })

  # Jump user to FedExCup tab. 
  observeEvent(input$fedExCupSeeMore_input, {

    updateTabsetPanel(session, "navBar", selected = "FedEx")

  })


  # Handicaps -----------
  # Handicaps - Title. 
  output$handicapsTitle <- renderUI({

    f$req_sign_in()

    div(
      div("Handicaps", HTML("<i id='handicapsTitleID' style='font-size:20px;' class='fas fa-info-circle'></i>"), class="table-title"),
      style = "margin-left:10px;",
      shinyBS::bsPopover("handicapsTitleID", "Handicaps", "Current player handicaps.", placement = "bottom", trigger = "hover"),
      class="align"
    )

  }) 
  
  # Handicaps - temp. 
  output$handicaps_temp <- renderReactable({

    f$req_sign_in()

    # Load data.
    data <- handicaps_data()
    players_data <- players_data()

    if(input$isMobile){
      var_width <- 150
      var_width_rank <- 60
    } 
      else{
        var_width <- 200
        var_width_rank <- 120
      }

    data <- data[order(data$Handicap, decreasing=FALSE),]

    data <- data %>% 
      mutate( Rank = dense_rank(data$Handicap) )

    data <- data[c('Rank', 'Player', 'Handicap')]

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

  # Handicaps. 
  output$handicaps <- renderUI({

    f$req_sign_in()
    
   if(input$isMobile){
     var_width <- "width:100%;"
   } 
     else{
       var_width <- "width:500px;"
     }

   div(reactableOutput("handicaps_temp"), style = var_width, class="reactBox align")

  })
  
  # See More button for Handicaps tab.
  output$handicapsSeeMore <- renderUI({

    f$req_sign_in()

    actionLink("handicapsSeeMore_input", "See More")

  })

  # Jump user to Handicaps tab. 
  observeEvent(input$handicapsSeeMore_input, {

    updateTabsetPanel(session, "navBar", selected = "Handicaps")

  })
  

  # Major Wins -----------
  # Major Wins - Title. 
  output$majorWinsTitle <- renderUI({

    f$req_sign_in()

    div(
      div("Major Wins", HTML("<i id='majorWinsTitleID' style='font-size:20px;' class='fas fa-info-circle'></i>"), class="table-title"),
      style = "margin-left:10px;",
      shinyBS::bsPopover("majorWinsTitleID", "Major Wins", "Number of career major wins.", placement = "bottom", trigger = "hover"),
      class="align"
    )

  })

  # Number of Major Wins - temp.
  output$majorWins_temp <- renderReactable({

    f$req_sign_in()

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
      data.frame() 
      
      # Remove playoff loser. 
      data <- data[data$Position == 1, ]
      
      data <- data %>%
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

    f$req_sign_in()
    
   if(input$isMobile){
     var_width <- "width:100%;"
   } 
     else{
       var_width <- "width:500px;"
     }

   div(reactableOutput("majorWins_temp"), style = var_width, class="reactBox align")

  })


    # Top 3 Finishes -----------
  # Top 3 Finishes - Title. 
  output$topThreeFinishesTitle <- renderUI({

    f$req_sign_in()

    div(
      div("Top 3 Finishes", HTML("<i id='topThreeFinishesTitleID' style='font-size:20px;' class='fas fa-info-circle'></i>"), class="table-title"),
      style = "margin-left:10px;",
      shinyBS::bsPopover("topThreeFinishesTitleID", "Top 3 Finishes", "Number of career top three finishes.", placement = "bottom", trigger = "hover"),
      class="align"
    )

  })

  # Number of Top 3 Finishes - temp.
  output$topThreeFinishes_temp <- renderReactable({

    f$req_sign_in()

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

    f$req_sign_in()
    
   if(input$isMobile){
     var_width <- "width:100%;"
   } 
     else{
       var_width <- "width:500px;"
     }

    div(reactableOutput("topThreeFinishes_temp"), style = var_width, class="reactBox align")

  })



  # Major Attendance -----------
  # Major Attendance - Title.
  output$majorAttendanceTitle <- renderUI({

    f$req_sign_in()

    div(
      div("Major Attendance", HTML("<i id='majorAttendanceTitleID' style='font-size:20px;' class='fas fa-info-circle'></i>"), class="table-title"),
      style = "margin-left:10px;",
      shinyBS::bsPopover("majorAttendanceTitleID", "Major Attendance", "The number of major tournaments entered by each player.", placement = "bottom", trigger = "hover"),
      class="align"
    )

  })

  # Major Attendance - temp.
  output$majorAttendance_temp <- renderReactable({

    f$req_sign_in()

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

    f$req_sign_in()
    
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

    f$req_sign_in()

    div(
      div("Average Scoring", HTML("<i id='avgScoringeTitleID' style='font-size:20px;' class='fas fa-info-circle'></i>"), class="table-title"),
      style = "margin-left:10px;",
      shinyBS::bsPopover("avgScoringeTitleID", "Average Scoring", "Stableford scores averaged across all entered tournaments.", placement = "bottom", trigger = "hover"),
      class="align"
    )

  }) 

  # Average Scoring - temp. 
  output$avgScoring_temp <- renderReactable({

    f$req_sign_in()

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

    f$req_sign_in()
    
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

    f$req_sign_in()

    div(
      div("Wooden Spoon", HTML("<i id='woodenSpoonTitleID' style='font-size:20px;' class='fas fa-info-circle'></i>"), class="table-title"),
      style = "margin-left:10px;",
      shinyBS::bsPopover("woodenSpoonTitleID", "Wooden Spoon", "Number of last-place finishes.", placement = "bottom", trigger = "hover"),
      class="align"
    )

  }) 
  
  # Wooden Spoon - temp. 
  output$woodenSpoon_temp <- renderReactable({

    f$req_sign_in()

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

    f$req_sign_in()
    
   if(input$isMobile){
     var_width <- "width:100%;"
   } 
     else{
       var_width <- "width:500px;"
     }

   div(reactableOutput("woodenSpoon_temp"), style = var_width, class="reactBox align")

  })
  
  # Venue Frequency -----------
  # Venue Frequency - Title. 
  output$venueFrequencyTitle <- renderUI({


    f$req_sign_in()

    div(
      div("Venue Frequency", HTML("<i id='venueFrequencyTitleID' style='font-size:20px;' class='fas fa-info-circle'></i>"), class="table-title"),
      style = "margin-left:10px;",
      shinyBS::bsPopover("venueFrequencyTitleID", "Venue Frequency", "Most popular major venues.", placement = "bottom", trigger = "hover"),
      class="align"
    )

  }) 
  
  # Venue Frequency - temp. 
  output$venueFrequency_temp <- renderReactable({

    f$req_sign_in()

    # Load data.
    data <- major_results_data()

    if(input$isMobile){
      var_width <- 150
      var_width_rank <- 60
    } 
      else{
        var_width <- 200
        var_width_rank <- 120
      }

    # Wrangle major results data to extract number of last place finishes.
    data <- data[c("Major", "Venue")] %>% unique
    data <- data.frame(sort(table(data$Venue), decreasing = TRUE))
    colnames(data)[1] <- "Venue"
    colnames(data)[2] <- "No. Majors"

    data <- data %>% 
      mutate( Rank = dense_rank(-data$'No. Majors') )

    data <- data[c('Rank', 'Venue', 'No. Majors')]

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
        )
      )
    )

  })

  # Venue Frequency. 
  output$venueFrequency <- renderUI({

    f$req_sign_in()
    
   if(input$isMobile){
     var_width <- "width:100%;"
   } 
     else{
       var_width <- "width:500px;"
     }

   div(reactableOutput("venueFrequency_temp"), style = var_width, class="reactBox align")

  })
  





  #### Section 6: Handicaps
  # handicapsHeadlineBox
  output$handicapsHeadlineBox <- renderUI({

    f$req_sign_in()

    # Load data. 
    data <- handicaps_data()

    lowest_handicap_player <- data %>%
      filter(Handicap == min(data$Handicap))
    
    labelIcon <- tags$i(class = "fas fa-list-ol", style = "color:white;")

    div(createInfoBox2(lowest_handicap_player$Handicap, lowest_handicap_player$Player, labelIcon, "Lowest Handicap"), class = "combo-box combo-blue")

  })

  # Handicap Timeseries Chart.
  output$handicapsTimeseries <- renderPlotly({

    f$req_sign_in()

    # Load Handicap Timeseries Data. 
    data <- handicap_timeseries()

    Event <- reorder(paste0("Major ", data$Major, " (", data$Year, ")"), data$Major)

    # Plot
    p <- data %>%
        ggplot( aes(x=Event, y=Handicap, group=Player, color=Player)) +
        geom_line() +
        geom_point() +
        #scale_color_viridis(discrete = TRUE) +
        ggtitle("Handicap Timeseries") +
        theme_ipsum() +
        ylab("Handicaps") +
        xlab("Major Timeline") +
        theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=1))
    # Turn it interactive with ggplotly
    p <- ggplotly(p)


    legend_hide <- data.frame()
        for(i in 1:length(p$x$data)){
            legend_hide <- rbind(legend_hide, tail(p$x$data[[i]]$y, 1))
            colnames(legend_hide)[1] <- "Value"
            p$x$data[[i]]$visible <- "legendonly"
        }
        
        for(i in 1:length(p$x$data)){
            if( tail(p$x$data[[i]]$y, 1) %in% top_n(legend_hide, -9)$Value ){
                p$x$data[[i]]$visible <- NULL
            }
        }

    return(p) 

  })

  # Handicap Timeseries Chart (conditional UI for mobile/desktop)
  output$handicapsTimeseries_conditional <- renderUI({

    f$req_sign_in()

    if(input$isMobile){
      div(NULL)
    }else{
        div(
          shinycssloaders::withSpinner(
            plotlyOutput("handicapsTimeseries", width="1300px", height="600px"),
            type = 8, 
            color = "#233845"
          ), style = 'display: inline-block;'
        )
    }

  })

  # Handicap Table - Title. 
  output$handicapsTableTitle <- renderUI({

    f$req_sign_in()

    div(
      div(
        div(
          "Current Handicaps", 
          HTML("<i id='handicapsTitleID' style='font-size:20px;' class='fas fa-info-circle'></i>"), 
          class="table-title"
        ),
        style = "margin-left:10px;",
        shinyBS::bsPopover(
          "handicapsTitleID", 
          "Handicaps", 
          "Current player handicaps.", 
          placement = "bottom", 
          trigger = "hover"
        ),
        class="align"
      ),
      div(
        div(
          HTML("<i id='remove_golfers' style='font-size:20px;' class='fas fa-minus'></i>"), 
          class="table-title"
        ),
        style = "margin-left:10px;",
        shinyBS::bsPopover(
          "remove_golfers", 
          "Remove Golfers", 
          "Remove selected golfers form handicaps table.", 
          placement = "bottom", 
          trigger = "hover"
        ),
        class="align"
      ),
      div(
        div(
          HTML("<i id='reset_golfers' style='font-size:20px;' class='fas fa-redo'></i>"), 
          class="table-title"
        ),
        style = "margin-left:10px;",
        shinyBS::bsPopover(
          "reset_golfers", 
          "Reset Golfers", 
          "Reset removed golfers to original handicaps table.", 
          placement = "bottom", 
          trigger = "hover"
        ),
        class="align"
      )
    )

  }) 

  # Set up the button click event for removing players from handicap table (for easy screen capture). 
  shinyjs::onevent(
    "click", 
    "remove_golfers",
    shinyjs::runjs("
      Shiny.setInputValue('remove_golfers', null);
      Shiny.setInputValue('remove_golfers', 'clicked');
    ")
  )

  # Set up the button click event for resetting handicaps table. 
  shinyjs::onevent(
    "click", 
    "reset_golfers",
    shinyjs::runjs("
      Shiny.setInputValue('reset_golfers', null);
      Shiny.setInputValue('reset_golfers', 'clicked');
    ")
  )
  
  # Reactive value to store data in handicaps table (for removing functionality)
  rv_handicap_data <- reactiveValues(data = handicap_data_2)

  # Handicap Table - temp. 
  output$handicapsTable_temp <- renderReactable({

    f$req_sign_in()

    # Load data.
    #data <- handicaps_data()
    data <- rv_handicap_data$data
    players_data <- players_data()

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

    data <- data %>% 
          mutate( Rank = dense_rank(data$Handicap) )

    data <- data[
      c(
        "Rank",
        "Major",
        "Player",
        "Handicap",
        "Date",
        "Year",
        "Venue"
        )]

    colnames(data[5]) <- "Date last played"
    colnames(data[6]) <- "Year last played"
    colnames(data[7]) <- "Venue last played"

    # Assign the data to rv_handicap_data reactive values. 
    rv_handicap_data$data <- data

    # Build the table with nested table inside.
    reactable(
      rv_handicap_data$data, 
      style = "display:inline-block;",
      selection = "multiple", 
      onClick = "select",
      filterable = TRUE,
      searchable = FALSE,
      highlight = TRUE,
      pagination = FALSE,
      #height = 500,
      minRows = 10,
      #defaultPageSize = 10,
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
        Major = colDef(
          minWidth = var_width_rank,
          maxWidth = var_width_rank,
          width = var_width_rank,
          #style = sticky_style_one,
          #headerStyle = sticky_style_one,
          align = "center"
        ),
        Date = colDef(
          name = "Date last played",
          minWidth = var_width,
          maxWidth = var_width,
          width = var_width,
          #style = sticky_style_one,
          #headerStyle = sticky_style_one,
          align = "center"
        ),
        Player = colDef(
          html = TRUE,
          minWidth = var_width,
          maxWidth = var_width,
          width = var_width,
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
        Handicap = colDef(
          minWidth = var_width_rank,
          maxWidth = var_width_rank,
          width = var_width,
          align = "center"
        ),
        Venue = colDef(
          name = "Venue last played",
          minWidth = var_width,
          maxWidth = var_width,
          width = var_width
        ),
        Year = colDef(
          name = "Year last played",
          minWidth = var_width,
          maxWidth = var_width,
          width = var_width
        )
      )
    )

  })

  # Remove golfers from table for easy screen grab of player handicaps.
  observeEvent(input$remove_golfers, {

    rows <- getReactableState("handicapsTable_temp", "selected")

    print("print(rows)")
    print(rows)

    req(rows)

    print("print(rv_handicap_data$data) before")
    print(rv_handicap_data$data)

    rv_handicap_data$data <- rv_handicap_data$data[-rows, ]

    print("print(rv_handicap_data$data)")
    print(rv_handicap_data$data)

  })

  # Listener - reset handicap table (after removing players for screenshot)
  observeEvent(input$reset_golfers ,{

    rv_handicap_data$data <- handicap_data_2

  })
  

  # Handicap Table. 
  output$handicapsTable <- renderUI({

    f$req_sign_in()
    
   if(input$isMobile){
     var_width <- "width:90%;"
   } 
     else{
       var_width <- "width:90%; display:inline-block;"
     }

   div(reactableOutput("handicapsTable_temp", width = "auto", inline=TRUE), style = var_width, class="reactBox align")

  })

  # ShinyAlert modal if portait & on mobile. 
  observeEvent(input$navBar, {

    # Get JavaScript to check if the device is in Portrait or Landscape mode.
    shinyjs::runjs("
      if(window.innerHeight < window.innerWidth){
        
        Shiny.setInputValue('landscapeMode_handicaps', null);
        Shiny.setInputValue('landscapeMode_handicaps', 'yes');
        
      } else{
          
          Shiny.setInputValue('landscapeMode_handicaps', null);
          Shiny.setInputValue('landscapeMode_handicaps', 'no');

      }
    ")
  })

  observeEvent(input$landscapeMode_handicaps, {

    if(input$navBar %in% "Handicaps" && is_mobile() && input$landscapeMode_handicaps %in% "no"){

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







  #### Section 6: Players
  # There is no server logic for the Players tab- it's all contained in the about.html file. 
  output$about <- renderUI({

    f$req_sign_in()

    HTML('
      <div style="height:20px;"></div>
        <div class="container">
          <div class="row">
            <div class="col-md-12">
              <h1 style="font-weight:bold !important; font-size:48px !important; color:#416983;">pap pro tour</h1>
            </div>
          </div>
          <div class="marketing">
            <div class="row">
              <div class="col-md-4">
                <!--<a href="#" data-toggle="modal" data-target="#modal-credits">Credits &amp; Disclaimer</a>-->
                <img href="#" data-toggle="modal" data-target="#player-billy" class="img-circle player_profile" src="images/billy.png" alt="">
                <h2>Billy Archbold</h2>
                <p>Itchy and Scratchy</p>
              </div>
              <div class="col-md-4">
                <img href="#" data-toggle="modal" data-target="#player-t" class="img-circle player_profile" src="images/t.png" alt="">
                <h2>Tiarnan OBrien</h2>
                <p>Dhinininininino</p>
              </div>
              <div class="col-md-4">
                <img href="#" data-toggle="modal" data-target="#player-dinny" class="img-circle player_profile" src="images/dinny.png" alt="">
                <h2>Dan Courtney</h2>
                <p>The Bionic Man</p>
              </div>
            </div>
            <div class="row">
              <div class="col-md-4">
                <img href="#" data-toggle="modal" data-target="#player-g" class="img-circle player_profile" src="images/g.png" alt="">
                <h2>Gearoid Comber</h2>
                <p>Garry Slice</p>
              </div>
              <div class="col-md-4">
                <img href="#" data-toggle="modal" data-target="#player-winnie" class="img-circle player_profile" src="images/winnie.png" alt="">
                <h2>Sean Whitson</h2>
                <p>Winnie-the-Pooh</p>
                <!--<a href="https://www.linkedin.com/in/leon-shim-9925a8b/">LinkedIn</a>-->
              </div>
              <div class="col-md-4">
                <img href="#" data-toggle="modal" data-target="#player-smith" class="img-circle player_profile" src="images/smith.png" alt="">
                <h2>Luke Smith</h2>
                <p>Crocodile Dundee</p>
              </div>
            </div>
            <div class="row">
              <div class="col-md-4">
                <img href="#" data-toggle="modal" data-target="#player-davidbenn" class="img-circle player_profile" src="images/davidbenn.png" alt="">
                <h2>David Benn</h2>
                <p>Big Benn</p>
              </div>
              <div class="col-md-4">
                <img href="#" data-toggle="modal" data-target="#player-aronmorris" class="img-circle player_profile" src="images/aronmorris.png" alt="">
                <h2>Aron Morris</h2>
                <p>International Man of Mystery</p>
              </div>
              <div class="col-md-4">
                <img href="#" data-toggle="modal" data-target="#player-craighyland" class="img-circle player_profile" src="images/craighyland.png" alt="">
                <h2>Craig Hyland</h2>
                <p>Hylos Box</p>
              </div>
            </div>
            <div class="row">
              <div class="col-md-4">
                <img href="#" data-toggle="modal" data-target="#player-sheano" class="img-circle player_profile" src="images/sheano.png" alt="">
                <h2>Darragh Sheehan</h2>
                <p>Deano, Smelly Peter</p>
              </div>
              <div class="col-md-4">
                <img href="#" data-toggle="modal" data-target="#player-davidlynch" class="img-circle player_profile" src="images/davidlynch.png" alt="">
                <h2>David Lynch</h2>
                <p>Billy Walsh</p>
              </div>
              <div class="col-md-4">
                <img href="#" data-toggle="modal" data-target="#player-faylo" class="img-circle player_profile" src="images/faylo.png" alt="">
                <h2>Feidhlim Dowling</h2>
                <p>Forrest Gump, Lightning Bolt</p>
              </div>
            </div>
            <div class="row">
              <div class="col-md-4">
                <img href="#" data-toggle="modal" data-target="#player-markdonnelly" class="img-circle player_profile" src="images/markdonnelly.png" alt="">
                <h2>Mark Donnelly</h2>
                <p>Yankee Doodle</p>
              </div>
              <div class="col-md-4">
                <img href="#" data-toggle="modal" data-target="#player-nialldevane" class="img-circle player_profile" src="images/nialldevane.png" alt="">
                <h2>Niall Devane</h2>
                <p>The Cow Whisperer</p>
              </div>
              <div class="col-md-4">
                <img href="#" data-toggle="modal" data-target="#player-seaniemac" class="img-circle player_profile" src="images/seaniemac.png" alt="">
                <h2>Seany Mac</h2>
                <p>Return of the Mac</p>
              </div>
            </div>
            <div class="row">
              <div class="col-md-4">
                <img href="#" data-toggle="modal" data-target="#player-oisin" class="img-circle player_profile" src="images/oisin.png" alt="">
                <h2>Dr. Oisin Tyrell</h2>
                <p>Dr. Shanks</p>
              </div>
              <div class="col-md-4">
                <img href="#" data-toggle="modal" data-target="#player-messy" class="img-circle player_profile" src="images/messy.png" alt="">
                <h2>Eoin Messitt</h2>
                <p>The Golf Ghost</p>
              </div>
              <div class="col-md-4">
                <img href="#" data-toggle="modal" data-target="#player-piggy" class="img-circle player_profile" src="images/piggy.png" alt="">
                <h2>Stephen Pigott</h2>
                <p>Piggie Wiggie</p>
              </div> 
            </div>
            <div class="row">
              <div class="col-md-4">
                <img href="#" data-toggle="modal" data-target="#player-philmahon" class="img-circle player_profile" src="images/philmahon.png" alt="">
                <h2>Phil Mahon</h2>
                <p>Phil the Thrill</p>
              </div>  
            <div class="col-md-4">
              <img href="#" data-toggle="modal" data-target="#player-iancox" class="img-circle player_profile" src="images/iancox.png" alt="">
              <h2>Ian Cox</h2>
              <p>The Jack Hammer</p>
            </div>
              <div class="col-md-4">
                <img href="#" data-toggle="modal" data-target="#player-davemcgrath" class="img-circle player_profile" src="images/davemcgrath.png" alt="">
                <h2>Dave McGrath</h2>
                <p>Ravey Davey</p>
              </div>
            </div>
            <div class="row">    
              <div class="col-md-4">
                <!--<a href="#" data-toggle="modal" data-target="#modal-credits">Credits &amp; Disclaimer</a>-->
                <img href="#" data-toggle="modal" data-target="#player-willmolloy" class="img-circle player_profile" src="images/willmolloy.png" alt="">
                <h2>Will Molloy</h2>
                <p>DJ Skwillex</p>
              </div>
            </div>
          </div>
        </div>
      ')

  })







  #### Section 7: Results

  # majorWinsHeadlineBox
  output$results_majorWinsHeadlineBox <- renderUI({

    f$req_sign_in()

    # Load data. 
    data <- major_results_data()
    players_data <- players_data()

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
      labelIcon <- tags$i(class = "fas fa-list-ol", style = "color:white;")

      data <- data[data$Wins %in% max(data$Wins), ]

      if(nrow(data) > 1){
        text <- " (Multiple Golfers)"
      } else{

        # Get first inital + surname to fit in info box. 
        text <- paste0(
          " (",
          substr(data$Player, 1, 1),
          ". ", 
          sub(".* ", "", data$Player),
          ")"
        )

      }


      labelIcon <- tags$i(class = "fas fa-trophy", style = "color:white;")

      div(createInfoBox2(max(data$Wins), text, labelIcon, "Most Major Wins"), class = "combo-box combo-teal")

  })

  # Results Table - Title. 
  output$majorResultsTableTitle <- renderUI({

    f$req_sign_in()

    # Get logged in user info.
    user <- f$get_signed_in()$response$displayName
    email <- f$get_signed_in()$response$email

    user_access_levels <- user_access_levels()

    # print("user")
    # print(user)
    # print("email")
    # print(email)

    # onclick='Shiny.setInputValue(\"\", null); Shiny.setInputValue(\"\", 'clicked');

    if( is.null(user) || user %in% user_access_levels$User || email %in% user_access_levels$User ){

      div(
      div(
        div("Major Results", 
          HTML("<i id='majorResultsTitleID' style='font-size:20px;' class='fas fa-info-circle'></i>"), class="table-title"
        ),
        style = "margin-left:10px;",
        shinyBS::bsPopover(
          "majorResultsTitleID", 
          "Major Results", 
          "Historic major results. Expand table rows for more information on particular event.", 
          placement = "bottom", 
          trigger = "hover"
          ),
        class="align"
      ),
      div(
        div(
          HTML("<i id='admin_uploadResults_input' style='font-size:20px;' class='fas fa-plus'></i>"), 
          class="table-title"
        ),
        style = "margin-left:10px;",
        shinyBS::bsPopover(
          "admin_uploadResults_input", 
          "Upload Results", 
          "Add recent major results.", 
          placement = "bottom", 
          trigger = "hover"
          ),
        class="align"
      ),
      div(
        div(
          HTML("<i id='admin_editResults_input' style='font-size:20px;' class='fas fa-edit'></i>"), class="table-title"
        ),
        style = "margin-left:10px;",
        shinyBS::bsPopover(
          "admin_editResults_input", 
          "Edit Results", 
          "Edit historic major results.", 
          placement = "bottom", 
          trigger = "hover"
          ),
        class="align"
      )
    )
    
    } else{ # don't show admin functionality

      div(
        div("Major Results", 
          HTML("<i id='majorResultsTitleID' style='font-size:20px;' class='fas fa-info-circle'></i>"), class="table-title"
        ),
        style = "margin-left:10px;",
        shinyBS::bsPopover(
          "majorResultsTitleID", 
          "Major Results", 
          "Historic major results. Expand table rows for more information on particular event.", 
          placement = "bottom", 
          trigger = "hover"
          ),
        class="align"
      )
    
    }

  }) 

  # Set up the button click event for uploading major results. 
  shinyjs::onevent(
    "click", 
    "admin_uploadResults_input",
    shinyjs::runjs("
      Shiny.setInputValue('admin_uploadResults_input', null);
      Shiny.setInputValue('admin_uploadResults_input', 'clicked');
    ")
  )

  # Set up the button click event for editng major results. 
  shinyjs::onevent(
    "click", 
    "admin_editResults_input",
    shinyjs::runjs("
      Shiny.setInputValue('admin_editResults_input', null);
      Shiny.setInputValue('admin_editResults_input', 'clicked');
    ")
  )

  # Results Table - temp. 
  output$majorResultsTable_temp <- renderReactable({

    f$req_sign_in()

    # Load data.
    data <- major_results_data()
    players_data <- players_data()

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

    # Re-order. 
    major_results_data <- major_results_data[order(-major_results_data$Major), ]


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
      #pagination = FALSE,
      #height = 500,
      minRows = 10,
      defaultPageSize = 10,
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
        Winner = colDef(
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
          width = var_width,
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

    f$req_sign_in()
    
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












  #### Section 9: Admin

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

    f$req_sign_in()

    # Get golfers & current handicaps for writing major results data to DB. 
    data <- handicaps_data()

    # re-order data.
    data <- data[order(-data$Major), ]

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
          format = "dd/mm/yyyy",
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
          multiple = FALSE,
          options = list(create = TRUE)
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
            choices = data$Player
        ), style = "margin-left: 15px;display: inline-block;vertical-align:top;"),
        div( 
          numericInput(
            inputId = "golferHandicap", 
            label = "Handicap", 
            # Note: this value is updated when a golfer is selected from input list above.  
            #   Observe Event action below. 
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
        # Add a golfer + their score to mini table.
        div(
          actionButton(
            "addGolferResults", 
            "Add", 
            width = 79
          ), 
          style = "display: inline-block;margin-top:26px;margin-left:15px;"
        ),
        # Remove golfer + their score from mini table (if entered incorrectly).
        div(
          disabled(
            actionButton(
              "deleteGolferResults", 
              "Delete", 
              width = 79
            )
          ), 
          style = "margin-left:15px;display:inline-block;vertical-align:middlemargin-top:26px;"
        )
      ),
      div(
        DT::dataTableOutput('resultsTable'), 
        style = "margin-left:30px;font-size:12px;min-height:50px;margin-right:50px;padding-bottom:40px;width:400px;"
      ),
      br(),
      if (failed)
        # Show the error message
        div(addWarning$message, style = "color: red; margin-left:15px;"),
      
      footer = htmltools::tagList(
        actionButton("uploadResults", "Upload Results"),
        actionButton("cancelUploadResults", "Cancel")
      ), size = 'm', easyClose = TRUE
    )
  }


  # Listener - admin_editResults_input.
  observeEvent(input$admin_editResults_input, {

    # Launch the edit results modal.
    showModal(editResults())

  })

  # Edit Results Modal - Add.
  editResults <- function(failed = FALSE) {

    f$req_sign_in()

    # Create modal UI.
    modalDialog(
      div(
        div(
          DT::dataTableOutput('resultsTable_edit'), 
          style = "margin-left:30px;font-size:12px;min-height:50px;margin-right:50px;padding-bottom:40px;width:400px;"
        )
      ),
      br(),
      if (failed)
        # Show the error message
        div(addWarning$message, style = "color: red; margin-left:15px;"),
      
      footer = htmltools::tagList(
        actionButton("saveEditedResults", "Save Edits"),
        actionButton("cancelEditedResults", "Cancel")
      ), size = 'l', easyClose = TRUE
    )

  }

  # Edit Results Modal - Save. 
  observeEvent(input$saveEditedResults, {

    print("SQL ALTER query here...")

  })

  # Edit Results Modal - Cancel. 
  observeEvent(input$cancelEditedResults, {

    # Close the inspector modal.
    removeModal()
  
  })

  # Results Table - Edit.
  output$resultsTable_edit <- DT::renderDataTable({

    f$req_sign_in()
    
    #data <- rv$resultsTable_temp
    data <- major_results_data()
    data <- data %>% 
      arrange(Major, -Score)

    # Draw the datatable. 
    DT::datatable(
      data,
      style = "bootstrap",
      class = "row-border hover compact",
      selection = list(mode = "single", target = "row", selected = NULL),
      # Turn row names on for the row counter. 
      rownames = TRUE,
      filter = "top",
      options = list(
        #columnDefs = list(list(visible=FALSE, targets=c(1, 2, 6))),
        stateSave = FALSE,
        searchHighlight = TRUE,
        server = FALSE, 
        ordering = FALSE,
        sDom  = '<"row"<"col-6"><"top">t<"bottom">',
        #lengthMenu = list(c(15, 25, 50), c('15', '25', '50')),
        pageLength = nrow(data),
        autoWidth = TRUE),
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



  # Update handicap numericInput with corresponding selected player.  
  observeEvent(input$golferName, {

    # Get golfers & current handicaps for writing major results data to DB. 
    data <- handicaps_data()

    updateNumericInput(
      session, 
      inputId = "golferHandicap",
      value = as.numeric(data[data$Player %in% input$golferName, ]$Handicap)
    )

  })

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

    print("...Printing the players score table...")
    print("print(rv$resultsTable_temp)")
    print(rv$resultsTable_temp)

  })

  # Track row selection of mini golfer results table. 
  selectedRowResultsTable <- reactiveVal(NULL)

  # Determine if a row is de-sleected form the mini golfer results table. 
  de_selectedRowResultsTable <- reactive({ !is.null(input$resultsTable_rows_selected) })

  # Click event on mini golfer results table when row selected.
  observeEvent(input$resultsTable_rows_selected, {

    # Enable the "delete" button in modal for mini reults table. 
    shinyjs::enable("deleteGolferResults")

    # Get the selected row/golfer from the table. 
    selected_golfer <- rv$resultsTable_temp$Golfer[input$resultsTable_rows_selected]
    print(selected_golfer)

    # Update the reactive value stores the row index selected form table.
    selectedRowResultsTable(input$resultsTable_rows_selected)

  })

  # Click event when row is de-selected on mini golfer results table.
  observeEvent(de_selectedRowResultsTable(), {

    # If row is de-selected
    if( !de_selectedRowResultsTable() ){
      
      print("Row has been successfully de-selected...")

      # Disable the "delete" button again. 
      shinyjs::disable("deleteGolferResults")

    }
    
  })

  # Listener for "delete" button in mini golfer results table.
  observeEvent(input$deleteGolferResults, {

    # Get data from the mini golfer results table, as it's being built
    data <- rv$resultsTable_temp

    print("Removing player from batch upload table...")
    print(selectedRowResultsTable())

    # I need to update the data that is being piped into the resultsTable UI. 
    #  Need to remove the selected player from this dataset. 
    # Get golfer
    selected_row <- selectedRowResultsTable()
    # Remove golfer from data
    data <- data[-selected_row, ]
    # Update the reactive Values being piped into the resultsTable UI.
    rv$resultsTable_temp <- data

  })

  # Upload Results Modal - Save. 
  observeEvent(input$uploadResults, {

    # The purpose of this section is to add the Major results data (once happy with mini table) to MongoDB. 

    # Then, get the last major number (i.e. Major 24)
    # Connect to your MongoDB instance.
    con <- mongo(
      collection = "major_results",
      db = "paptour_db",
      url = url,
      verbose = FALSE,
      options = ssl_options()
    )

    # Firstly, get the mini golfer results table.
    data <- rv$resultsTable_temp
    # Rename col
    colnames(data)[1] <- "Player"

    # Read data form collection into data.frame. 
    prev_major_data <- con$find(query = '{}')
    prev_major <- max(prev_major_data$Major)

    # Next, create data structured to upload straight into MongoDB.
    data$Major <- prev_major + 1 # increment by one
    data$Date <- input$majorDate
    data$Venue <- input$courseName
    # This needs to be worked on in future. 
    data$playoff_win <- NA

    # Re-order data in correct format for uplaod to MongoDB. 
    data <- data[c("Major", "Date", "Player", "Handicap", "Score", "Venue", "playoff_win")]

    # Then upload the data into MongoDB. 
    # Append new data to MongoDB.
    con$insert(data)

    # Disconnect from MongoDB.
    rm(con)

    # Close the inspector modal.
    removeModal()

  })

  # Upload Results Modal - Cancel. 
  observeEvent(input$cancelUploadResults, {

    # Reset the data that has been entered into the mini golfer results table. 
    #  I.e. delete all data from the reactive values data set.
    rv$resultsTable_temp <- data.frame(
      Golfer = character(), 
      Handicap = numeric(),
      Score = numeric(),
      stringsAsFactors = FALSE
    )

    # Close the inspector modal.
    removeModal()
  
  })

  # Set up the reactiveValues dataset to track the mini golfer results table. 
  rv <- reactiveValues( 
    resultsTable_temp = NULL
  )

  # Cretae reactive value to store and empty data.frame that will be updated with inspector inputs.
  results_df <- reactive({

    df <- data.frame(
      Golfer = character(), 
      Handicap = numeric(),
      Score = numeric(),
      stringsAsFactors = FALSE
    )
    return(df)

  })
  
  # Results Table.
  output$resultsTable <- DT::renderDataTable({

    f$req_sign_in()
    
    #data <- results_df()
    data <- rv$resultsTable_temp
      
    # Draw the datatable. 
    DT::datatable(
      data,
      style = "bootstrap",
      class = "row-border hover compact",
      selection = list(mode = "single", target = "row", selected = NULL),
      # Turn row names on for the row counter. 
      rownames = TRUE,
      #filter = "top",
      options = list(
        #columnDefs = list(list(visible=FALSE, targets=c(1, 2, 6))),
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




  output$footer <- renderUI({

    f$req_sign_in()

    HTML('


      <div class="container">
        <img src="logo.png" alt="logo" class="center-block img-responsive" width="300" style="margin-bottom: 80px;" />

        <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">
        <style>
        .fa {
          padding-bottom: 20px;
          font-size: 30px;
          width: 30px;
          text-align: center;
          text-decoration: none;
          margin: 5px 2px;
          border-radius: 50%;
          color: #202020;
        }
        
        .fa:hover {
            opacity: 0.7;
            text-decoration: none;
            color: #202020;
        }
        
        .fa:active  {
            text-decoration: none;
            color: #202020;
        }
        
        .fa:visited  {
            text-decoration: none;
            color: #202020;
        }
        
        </style>
        
        <!-- Add font awesome icons -->
        <div style = "margin-left:-5px;">
        <!--<a href="#" class="fa fa-facebook"></a>-->
        <a href="#" class="fa fa-twitter"></a>
        <a href="#" class="fa fa-youtube"></a>
        <a href="#" class="fa fa-instagram"></a>
        <!--<a href="#" class="fa fa-vimeo"></a>-->
        </div>

        <!-- FOOTER -->
        <footer>
        <p class="pull-right"><a href="#">Back to top</a></p>
        <p>Putt & Pint Pro Tour&mdash;2022</p>
        <p><a href="#" data-toggle="modal" data-target="#modal-credits">Credits &amp; Disclaimer</a></p>
        </footer>

      </div>

      <!-- Modal - Credits -->
      <div class="modal fade" id="modal-credits" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
      <div class="modal-dialog">
      <div class="modal-content">
        <div class="modal-header">
        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
        <h4 class="modal-title" id="myModalLabel">Credits</h4>
        </div>
        <div class="modal-body">
          <p>Brought to you by Billy Archbold, Tiarnán OBrien & Oisin Tyrell, 2022.</p>
        </div>
        <div class="modal-footer">
        <button type="button" class="btn btn-primary" data-dismiss="modal">Close</button>
        </div>
      </div><!-- /.modal-content -->
      </div><!-- /.modal-dialog -->
      </div><!-- /.modal -->



      <!-- Player Profiles -->

      <!-- PAP PRO TOUR -->

      <div class="modal fade" id="player-billy" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
      <div class="modal-dialog">
      <div class="modal-content">
        <div class="modal-header">
        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
        <h4 class="modal-title" id="myModalLabel">Billy Archbold</h4>
        </div>
        <div class="modal-body">
          <img class="img-circle player_profile img-container" src="images/billy.png" alt="">
          <br><br>
          <p><b>Strengths: </b> Effective, slow tempo fish face swing. Length off the tee.</p>
          <p><b>Weaknesses: </b> Once you’re in his head its GAME OVER. Very easy to wind up. Can chunk it BAD.</p>
          <p></p>
        </div>
        <div class="modal-footer">
        <button type="button" class="btn btn-primary" data-dismiss="modal">Close</button>
        </div>
      </div><!-- /.modal-content -->
      </div><!-- /.modal-dialog -->
      </div><!-- /.modal -->

      <div class="modal fade" id="player-t" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
      <div class="modal-dialog">
      <div class="modal-content">
        <div class="modal-header">
        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
        <h4 class="modal-title" id="myModalLabel">Tiarnan OBrien</h4>
        </div>
        <div class="modal-body">
          <img class="img-circle player_profile img-container" src="images/t.png" alt="">
          <br><br>
          <p><b>Strengths: </b> Calculating and tactical with a consistent iron swing.</p>
          <p><b>Weaknesses: </b>Length off the tee.</p>
          <p></p>
        </div>
        <div class="modal-footer">
        <button type="button" class="btn btn-primary" data-dismiss="modal">Close</button>
        </div>
      </div><!-- /.modal-content -->
      </div><!-- /.modal-dialog -->
      </div><!-- /.modal -->

      <div class="modal fade" id="player-dinny" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
      <div class="modal-dialog">
      <div class="modal-content">
        <div class="modal-header">
        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
        <h4 class="modal-title" id="myModalLabel">Dan Courtney</h4>
        </div>
        <div class="modal-body">
          <img class="img-circle player_profile img-container" src="images/dinny.png" alt="">
          <br><br>
          <p><b>Strengths: </b> Self-belief and resilience. Has the ability to shape shots when needed.</p>
          <p><b>Weaknesses: </b>Hips. Playoffs. Stamina on the back nine. Mind games.</p>
          <p></p>
        </div>
        <div class="modal-footer">
        <button type="button" class="btn btn-primary" data-dismiss="modal">Close</button>
        </div>
      </div><!-- /.modal-content -->
      </div><!-- /.modal-dialog -->
      </div><!-- /.modal -->

      <div class="modal fade" id="player-g" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
      <div class="modal-dialog">
      <div class="modal-content">
        <div class="modal-header">
        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
        <h4 class="modal-title" id="myModalLabel">Gearoid Comber</h4>
        </div>
        <div class="modal-body">
          <img class="img-circle player_profile img-container" src="images/g.png" alt="">
          <br><br>
          <p><b>Strengths: </b> Can hit it right to left.</p>
          <p><b>Weaknesses: </b> Can ONLY hit it right to left.</p>
          <p></p>
        </div>
        <div class="modal-footer">
        <button type="button" class="btn btn-primary" data-dismiss="modal">Close</button>
        </div>
      </div><!-- /.modal-content -->
      </div><!-- /.modal-dialog -->
      </div><!-- /.modal -->

      <div class="modal fade" id="player-winnie" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
      <div class="modal-dialog">
      <div class="modal-content">
        <div class="modal-header">
        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
        <h4 class="modal-title" id="myModalLabel">Sean Whitson</h4>
        </div>
        <div class="modal-body">
          <img class="img-circle player_profile img-container" src="images/winnie.png" alt="">
          <br><br>
          <p><b>Strengths: </b> Incredible distance off the tee.</p>
          <p><b>Weaknesses: </b> Can let the pressure get to him.</p>
          <p></p>
        </div>
        <div class="modal-footer">
        <button type="button" class="btn btn-primary" data-dismiss="modal">Close</button>
        </div>
      </div><!-- /.modal-content -->
      </div><!-- /.modal-dialog -->
      </div><!-- /.modal -->

      <div class="modal fade" id="player-smith" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
      <div class="modal-dialog">
      <div class="modal-content">
        <div class="modal-header">
        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
        <h4 class="modal-title" id="myModalLabel">Luke Smith</h4>
        </div>
        <div class="modal-body">
          <img class="img-circle player_profile img-container" src="images/smith.png" alt="">
          <br><br>
          <p><b>Strengths: </b> Deadly accurate from wthin 100 yards. Super temperament.</p>
          <p><b>Weaknesses: </b> Can be VERY erratic off the tee.</p>
          <p></p>
        </div>
        <div class="modal-footer">
        <button type="button" class="btn btn-primary" data-dismiss="modal">Close</button>
        </div>
      </div><!-- /.modal-content -->
      </div><!-- /.modal-dialog -->
      </div><!-- /.modal -->

      <div class="modal fade" id="player-davidbenn" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
      <div class="modal-dialog">
      <div class="modal-content">
        <div class="modal-header">
        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
        <h4 class="modal-title" id="myModalLabel">David Benn</h4>
        </div>
        <div class="modal-body">
          <img class="img-circle player_profile img-container" src="images/davidbenn.png" alt="">
          <br><br>
          <p><b>Strengths: </b> Usually very accurate with the rescue off the tee.</p>
          <p><b>Weaknesses: </b> So many, but to name a few.... Biggest man with the weakest brain. Self belief is non existent.</p>
          <p></p>
        </div>
        <div class="modal-footer">
        <button type="button" class="btn btn-primary" data-dismiss="modal">Close</button>
        </div>
      </div><!-- /.modal-content -->
      </div><!-- /.modal-dialog -->
      </div><!-- /.modal -->

      <div class="modal fade" id="player-aronmorris" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
      <div class="modal-dialog">
      <div class="modal-content">
        <div class="modal-header">
        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
        <h4 class="modal-title" id="myModalLabel">Aron Morris</h4>
        </div>
        <div class="modal-body">
          <img class="img-circle player_profile img-container" src="images/aronmorris.png" alt="">
          <br><br>
          <p><b>Strengths: </b> Has learned the ancient Chinese ways golf Zen. </p>
          <p><b>Weaknesses: </b> Has yet to convert these learnings into results on the course.</p>
          <p></p>
        </div>
        <div class="modal-footer">
        <button type="button" class="btn btn-primary" data-dismiss="modal">Close</button>
        </div>
      </div><!-- /.modal-content -->
      </div><!-- /.modal-dialog -->
      </div><!-- /.modal -->

      <div class="modal fade" id="player-craighyland" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
      <div class="modal-dialog">
      <div class="modal-content">
        <div class="modal-header">
        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
        <h4 class="modal-title" id="myModalLabel">Craig Hyland</h4>
        </div>
        <div class="modal-body">
          <img class="img-circle player_profile img-container" src="images/craighyland.png" alt="">
          <br><br>
          <p><b>Strengths: </b> Has trialled roughly 60-70% of all golf clubs in Ireland. Rapid improvement since joining the tour, almost winning a major on multiple occasions, future major winner.</p>
          <p><b>Weaknesses: </b> Too busy putting the fire out instead of igniting it.</p>
          <p></p>
        </div>
        <div class="modal-footer">
        <button type="button" class="btn btn-primary" data-dismiss="modal">Close</button>
        </div>
      </div><!-- /.modal-content -->
      </div><!-- /.modal-dialog -->
      </div><!-- /.modal -->

      <div class="modal fade" id="player-sheano" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
      <div class="modal-dialog">
      <div class="modal-content">
        <div class="modal-header">
        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
        <h4 class="modal-title" id="myModalLabel">Darragh Sheehan</h4>
        </div>
        <div class="modal-body">
          <img class="img-circle player_profile img-container" src="images/sheano.png" alt="">
          <br><br>
          <p><b>Strengths: </b> Vast array of nicknames.</p>
          <p><b>Weaknesses: </b> Quite a bad smelly you know what.</p>
          <p></p>
        </div>
        <div class="modal-footer">
        <button type="button" class="btn btn-primary" data-dismiss="modal">Close</button>
        </div>
      </div><!-- /.modal-content -->
      </div><!-- /.modal-dialog -->
      </div><!-- /.modal -->

      <div class="modal fade" id="player-davidlynch" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
      <div class="modal-dialog">
      <div class="modal-content">
        <div class="modal-header">
        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
        <h4 class="modal-title" id="myModalLabel">David Lynch</h4>
        </div>
        <div class="modal-body">
          <img class="img-circle player_profile img-container" src="images/davidlynch.png" alt="">
          <br><br>
          <p><b>Strengths: </b> Has every shot, knows all the angles, has some of the best equipment.</p>
          <p><b>Weaknesses: </b> Unfortunately for Lynch, Im not talking about his golf game. </p>
          <p></p>
        </div>
        <div class="modal-footer">
        <button type="button" class="btn btn-primary" data-dismiss="modal">Close</button>
        </div>
      </div><!-- /.modal-content -->
      </div><!-- /.modal-dialog -->
      </div><!-- /.modal -->

      <div class="modal fade" id="player-faylo" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
      <div class="modal-dialog">
      <div class="modal-content">
        <div class="modal-header">
        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
        <h4 class="modal-title" id="myModalLabel">Feidhlim Dowling</h4>
        </div>
        <div class="modal-body">
          <img class="img-circle player_profile img-container" src="images/faylo.png" alt="">
          <br><br>
          <p><b>Strengths: </b> Fastest man on tour, super stamina. </p>
          <p><b>Weaknesses: </b> Very fond of a wooden spoon.</p>
          <p></p>
        </div>
        <div class="modal-footer">
        <button type="button" class="btn btn-primary" data-dismiss="modal">Close</button>
        </div>
      </div><!-- /.modal-content -->
      </div><!-- /.modal-dialog -->
      </div><!-- /.modal -->

      <div class="modal fade" id="player-markdonnelly" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
      <div class="modal-dialog">
      <div class="modal-content">
        <div class="modal-header">
        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
        <h4 class="modal-title" id="myModalLabel">Mark Donnelly</h4>
        </div>
        <div class="modal-body">
          <img class="img-circle player_profile img-container" src="images/markdonnelly.png" alt="">
          <br><br>
          <p><b>Strengths: </b> Big drive. Grew up playing golf at elite level among some of the tours finest in Naas Golf Club. </p>
          <p><b>Weaknesses: </b> Lack of major experience.  Likes to travel light, sometimes forgetting to pack any clothes at all - let alone golf gear. </p>
          <p></p>
        </div>
        <div class="modal-footer">
        <button type="button" class="btn btn-primary" data-dismiss="modal">Close</button>
        </div>
      </div><!-- /.modal-content -->
      </div><!-- /.modal-dialog -->
      </div><!-- /.modal -->

      <div class="modal fade" id="player-nialldevane" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
      <div class="modal-dialog">
      <div class="modal-content">
        <div class="modal-header">
        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
        <h4 class="modal-title" id="myModalLabel">Niall Devane</h4>
        </div>
        <div class="modal-body">
          <img class="img-circle player_profile img-container" src="images/nialldevane.png" alt="">
          <br><br>
          <p><b>Strengths: </b> Smooth, natual swing and great ball-striking. No doubt a natural convert from hurling.</p>
          <p><b>Weaknesses: </b> Easily distractable. Mind always on the farm.</p>
          <p></p>
        </div>
        <div class="modal-footer">
        <button type="button" class="btn btn-primary" data-dismiss="modal">Close</button>
        </div>
      </div><!-- /.modal-content -->
      </div><!-- /.modal-dialog -->
      </div><!-- /.modal -->

      <div class="modal fade" id="player-seaniemac" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
      <div class="modal-dialog">
      <div class="modal-content">
        <div class="modal-header">
        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
        <h4 class="modal-title" id="myModalLabel">Seanie Mac</h4>
        </div>
        <div class="modal-body">
          <img class="img-circle player_profile img-container" src="images/seaniemac.png" alt="">
          <br><br>
          <p><b>Strengths: </b> Very athletic, doesnt feel the pressure. </p>
          <p><b>Weaknesses: </b> Has lost that competitive edge ever since his inaugural major win. Once offered to caddie for Dan instead of play the major himself. </p>
          <p></p>
        </div>
        <div class="modal-footer">
        <button type="button" class="btn btn-primary" data-dismiss="modal">Close</button>
        </div>
      </div><!-- /.modal-content -->
      </div><!-- /.modal-dialog -->
      </div><!-- /.modal -->

      <div class="modal fade" id="player-oisin" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
      <div class="modal-dialog">
      <div class="modal-content">
        <div class="modal-header">
        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
        <h4 class="modal-title" id="myModalLabel">Dr. Oisin Tyrell</h4>
        </div>
        <div class="modal-body">
          <img class="img-circle player_profile img-container" src="images/oisin.png" alt="">
          <br><br>
          <p><b>Strengths: </b> Best LOOKING swing on tour.</p>
          <p><b>Weaknesses: </b> Water. Hazards. Sand. Every club he owns seems to be customised with extra loft.</p>
          <p></p>
        </div>
        <div class="modal-footer">
        <button type="button" class="btn btn-primary" data-dismiss="modal">Close</button>
        </div>
      </div><!-- /.modal-content -->
      </div><!-- /.modal-dialog -->
      </div><!-- /.modal -->


      <div class="modal fade" id="player-messy" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
        <div class="modal-dialog">
        <div class="modal-content">
          <div class="modal-header">
          <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
          <h4 class="modal-title" id="myModalLabel">Eoin Messitt</h4>
          </div>
          <div class="modal-body">
            <img class="img-circle player_profile img-container" src="images/messy.png" alt="">
            <br><br>
            <p><b>Strengths: </b> Unknown.</p>
          <p><b>Weaknesses: </b> Unknown.</p>
          <p></p>
          </div>
          <div class="modal-footer">
          <button type="button" class="btn btn-primary" data-dismiss="modal">Close</button>
          </div>
        </div><!-- /.modal-content -->
        </div><!-- /.modal-dialog -->
        </div><!-- /.modal -->
        
      <div class="modal fade" id="player-iancox" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
        <div class="modal-dialog">
        <div class="modal-content">
          <div class="modal-header">
          <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
          <h4 class="modal-title" id="myModalLabel">Ian Cox</h4>
          </div>
          <div class="modal-body">
            <img class="img-circle player_profile img-container" src="images/iancox.png" alt="">
            <br><br>
            <p><b>Strengths: </b> First out in first group tee shots in front of the whole tour allegedly dont get to him. Purest 3 iron on tour, half swing perfection, silky smooth with the wedges! </p>
          <p><b>Weaknesses: </b> First out in first group tee shots in front of the whole tour probably will get to him.</p>
          <p></p>
          </div>
          <div class="modal-footer">
          <button type="button" class="btn btn-primary" data-dismiss="modal">Close</button>
          </div>
        </div><!-- /.modal-content -->
        </div><!-- /.modal-dialog -->
        </div><!-- /.modal -->
          
      <div class="modal fade" id="player-piggy" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
        <div class="modal-dialog">
        <div class="modal-content">
          <div class="modal-header">
          <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
          <h4 class="modal-title" id="myModalLabel">Stephen Pigott</h4>
          </div>
          <div class="modal-body">
            <img class="img-circle player_profile img-container" src="images/piggy.png" alt="">
            <br><br>
            <p><b>Strengths: </b> A natural ball-striker, no doubt easily transitioned from his hurling prowess.</p>
          <p><b>Weaknesses: </b> Sometimes too heavy-handed around the greens. Could potentially leave mid-round to go surfing if a strong off-shore wind floats in. </p>
          <p></p>
          </div>
          <div class="modal-footer">
          <button type="button" class="btn btn-primary" data-dismiss="modal">Close</button>
          </div>
        </div><!-- /.modal-content -->
        </div><!-- /.modal-dialog -->
        </div><!-- /.modal -->
        
      <div class="modal fade" id="player-philmahon" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
        <div class="modal-dialog">
        <div class="modal-content">
          <div class="modal-header">
          <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
          <h4 class="modal-title" id="myModalLabel">Phil Mahon</h4>
          </div>
          <div class="modal-body">
            <img class="img-circle player_profile img-container" src="images/philmahon.png" alt="">
            <br><br>
            <p><b>Strengths: </b> Has a cannon off the tee, past holder of long-drive competition in a major.</p>
          <p><b>Weaknesses: </b> Drive for show, putt for dough. If Phil the Thrill can improve in the areas so well versed by his nick-namesake, he will be a real contender in the future. </p>
          <p></p>
          </div>
          <div class="modal-footer">
          <button type="button" class="btn btn-primary" data-dismiss="modal">Close</button>
          </div>
        </div><!-- /.modal-content -->
        </div><!-- /.modal-dialog -->
        </div><!-- /.modal -->

        <div class="modal fade" id="player-davemcgrath" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
          <div class="modal-dialog">
          <div class="modal-content">
            <div class="modal-header">
            <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
            <h4 class="modal-title" id="myModalLabel">Dave McGrath</h4>
            </div>
            <div class="modal-body">
              <img class="img-circle player_profile img-container" src="images/davemcgrath.png" alt="">
              <br><br>
              <p><b>Strengths: </b> Has a very positive attitude when the putts are rolling in. </p>
          <p><b>Weaknesses: </b> The putts... they dont roll in that often.</p>
          <p></p>
            </div>
            <div class="modal-footer">
            <button type="button" class="btn btn-primary" data-dismiss="modal">Close</button>
            </div>
          </div><!-- /.modal-content -->
          </div><!-- /.modal-dialog -->
          </div><!-- /.modal -->

          <div class="modal fade" id="player-willmolloy" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
            <div class="modal-dialog">
            <div class="modal-content">
              <div class="modal-header">
              <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
              <h4 class="modal-title" id="myModalLabel">Will Molloy</h4>
              </div>
              <div class="modal-body">
                <img class="img-circle player_profile img-container" src="images/willmolloy.png" alt="">
                <br><br>
                <p><b>Strengths: </b> Good 5-aside player.</p>
          <p><b>Weaknesses: </b> Looks out of place in an 11-aside game.</p>
          <p></p>
              </div>
              <div class="modal-footer">
              <button type="button" class="btn btn-primary" data-dismiss="modal">Close</button>
              </div>
            </div><!-- /.modal-content -->
            </div><!-- /.modal-dialog -->
            </div><!-- /.modal -->
                  



      <script src="plugins/fittext_1.2/jquery.fittext.js"></script>
      <script src="plugins/fittext.js"></script>

      <script type="text/javascript">
        jQuery(".fit-h1").fitText(1.2, { minFontSize: "32px", maxFontSize: "38px" });
        jQuery(".fit-h2").fitText(1.2, { minFontSize: "18px", maxFontSize: "26px" });
        jQuery(".fit-text").fitText(0.8, { minFontSize: "18px", maxFontSize: "24px" });
      </script>


    ')

  })











  

  # End App.

})
