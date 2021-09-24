# OWGR Ranking System

source("global.R")
source("mongoDB.R")


# This is the same source as what's being read in from MongoDB instance. 
# data <- read.csv("data/major_results.csv", stringsAsFactors=FALSE, check.names=FALSE)

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

    # Make sure it's in the correct order. 
    data <- data[order(-data$Major, -data$Score),]

    # Disconnect
    rm(con)
    
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

    data <- temp