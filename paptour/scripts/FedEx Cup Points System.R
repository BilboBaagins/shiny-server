# FedEx Cup Points System

# The FedEx Cup is a much more stripped back version of the OWGR Rankigs. 
# It makes for an interesting dynamic when it comes to the last major however 
#  as multiple players can be in contention (much like the real FedEx Cup),
#  while some players will be mathematiclly ruled out. 

# OLD
#data <- read.csv("data/ogr.csv", stringsAsFactors=FALSE, check.names=FALSE)
#    data$WORLD.RANKING.POSITION <- gsub("[[:alpha:]]", "", data$WORLD.RANKING.POSITION) %>%
#      as.numeric()
#    colnames(data) <- gsub("\\.", " ", colnames(data)) %>%
#      tolower()

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
            Score_sum = sum(Score),
            #Weighted_Score_sum = sum(Weighted_Score),
            Pts_sum = sum(Pts), 
            #Weighted_Pts_sum = sum(Weighted_Pts)
        ) %>% data.frame() %>% 
        arrange(-Pts_sum)

    temp <- rbind(temp, fedex)

}