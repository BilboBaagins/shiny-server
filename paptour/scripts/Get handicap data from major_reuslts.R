# Get handicap data from major results. 
#  When new major results are entered, this should update the handicap data. 
#  Basically, handicaps data is just the major results data with calculations done 
#   for handicap movement depending on 1st, 2nd and last place finishes. 

# mongoDB CRUD
# https://docs.mongodb.com/manual/crud/

# Load library.
library(mongolite)

# Credentials.
user <- "billy"
pwd <- "JliMaQpXzTKYFCGe"

# Construct your connection URL.
url <- paste0("mongodb+srv://", user, ":", pwd, "@cluster0.4yzge.mongodb.net/?retryWrites=true&w=majority")

# For a successful connection, need to ensure IP is whitelisted.
# Find it in: [project_name] > Network Access


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





















# Get the handicaps from major just entered. 
# Calculate the new handicaps, based of finishing positions. 
# Also work out previous handicap change, whenever that was. 

billy <- data[data$Player %in% "Billy Archbold", ]
billy <- billy[order(billy$Major), ]

# Possibly use RLE (Run Length Encoding)

billy$Handicap

rle(billy$Handicap)$lengths



g <- data[data$Player %in% "Gearoid Comber", ]
g <- g[order(g$Major), ]

g$Handicap

rle(g$Handicap)$lengths

rle(g$Handicap)$values

# So to identify what G's handicap was previously (i.e. before his current one), we use the
#  last two values of rle()$values
g_current_handicap <- tail(rle(g$Handicap)$values, 1)
g_prev_handicap <- tail(rle(g$Handicap)$values, 2)[1]
# Handicap Movement
g_handicap_movement <-  g_prev_handicap - g_current_handicap

g_current_handicap
g_prev_handicap
ifelse(g_handicap_movement > 0, paste0("+", g_handicap_movement), g_handicap_movement)

# Identify when & what last major was that affected his handicap, using rle()$lengths

 g[sum(head(rle(g$Handicap)$lengths, length(rle(g$Handicap)$lengths) -1)), ]$Major
 g[sum(head(rle(g$Handicap)$lengths, length(rle(g$Handicap)$lengths) -1)), ]$Venue
 g[sum(head(rle(g$Handicap)$lengths, length(rle(g$Handicap)$lengths) -1)), ]$Handicap
 g[sum(head(rle(g$Handicap)$lengths, length(rle(g$Handicap)$lengths) -1)), ]$Date
 




# First I need to calculate the changes in handicaps.  -2 winner, -1 second place, +1 last place.




# Now I need to calculate this for every player in the group. 

# Probably do this in a for loop. 
handicap_df <- data.frame()
for(player in unique(data$Player)){

    print(player)

    data_temp <- data[data$Player %in% player, ]
    data_temp <- data_temp[order(data_temp$Major), ]

    # Get previous handicaps (i.e. before his current one).  We use the
    #  last two values of rle()$values
    current_handicap <- tail(rle(data_temp$Handicap)$values, 1)
    prev_handicap <- tail(rle(data_temp$Handicap)$values, 2)[1]

    # Handicap Movement
    handicap_movement <-  current_handicap - prev_handicap
    handicap_movement <- ifelse(handicap_movement > 0, paste0("+", handicap_movement), handicap_movement)

    # Identify when & what last major was that affected his handicap, using rle()$lengths
    major <- data_temp[sum(head(rle(data_temp$Handicap)$lengths, length(rle(data_temp$Handicap)$lengths) -1)), ]$Major
    venue <- data_temp[sum(head(rle(data_temp$Handicap)$lengths, length(rle(data_temp$Handicap)$lengths) -1)), ]$Venue
    handicap <- data_temp[sum(head(rle(data_temp$Handicap)$lengths, length(rle(data_temp$Handicap)$lengths) -1)), ]$Handicap
    date <- data_temp[sum(head(rle(data_temp$Handicap)$lengths, length(rle(data_temp$Handicap)$lengths) -1)), ]$Date

        
    (nrow(data_temp[sum(head(rle(data_temp$Handicap)$lengths, length(rle(data_temp$Handicap)$lengths) -1)), ]), )

    # Throw it all into a df and rbind each row together for each player. 
    handicap_df_temp <- data.frame(
        Rank = NA,
        Player = player,
        'Current Handicap' = current_handicap, 
        'Previous Handicap' = prev_handicap, 
        'Handicap Movement' = handicap_movement,
        'Date last played' = date,
        'Venue last played' = venue,
        Major = major
    )

    handicap_df <- rbind(handicap_df, handicap_df_temp)

}

print("print(handicap_df)")
print(handicap_df)







