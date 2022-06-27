# AUTOMATE HANDICAPS

# mongoDB CRUD
# https://docs.mongodb.com/manual/crud/

# Load library.
library(mongolite)
library(dplyr)

# Credentials.
user <- "billy"
pwd <- "JliMaQpXzTKYFCGe"

# Construct your connection URL.
url <- paste0("mongodb+srv://", user, ":", pwd, "@cluster0.4yzge.mongodb.net/?retryWrites=true&w=majority")

# For a successful connection, need to ensure IP is whitelisted.
# Find it in: [project_name] > Network Access




# Read data from mongoDB
#-----------------------------
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


# Subset data to retrieve last major all payers entered
hcap <- data %>%
    group_by(Player) %>%
    filter(Major == max(Major)) %>%
    ungroup() %>% 
    arrange(-Major) %>%
    data.frame()

# Update the handicaps based off the most recent major
hcap_updates <- hcap %>% 
    filter(Major == max(Major)) %>% 
    arrange(-Score)

# Rules:
#  1. 1st place gets deducted 2 shots
#  2. 2nd place gets deducted 1 shot
#  3. Last place gets 1 shot back

first_place <- hcap_updates %>% 
    # Get first place
    filter(Score == max(Score)) %>% 
    mutate(Handicap = Handicap - 2)

second_place <- hcap_updates %>% 
    # Remove first place
    filter(Score != max(Score)) %>% 
    # Get second place
    filter(Score == max(Score)) %>% 
    mutate(Handicap = Handicap - 1)

last_place <- hcap_updates %>%
    # Get last place
    filter(Score == min(Score)) %>% 
    mutate(Handicap = Handicap + 1)

first_place
second_place
last_place

hcap

hcap[hcap$Player == first_place$Player, ]$Handicap <- first_place$Handicap
hcap[hcap$Player == second_place$Player, ]$Handicap <- second_place$Handicap
hcap[hcap$Player == last_place$Player, ]$Handicap <- last_place$Handicap

hcap

# Needed to update from last time
hcap[hcap$Player %in% "Dan Courtney", ]$Handicap <- 19




# Create connection to new DB & collection.
handicaps <- mongo(collection = "data", # Creating collection
               db = "handicaps", # Creating DataBase
               url = url, 
               verbose = TRUE)

# Insert data to mongoDB.
handicaps$insert(hcap)


# Connect to your MongoDB instance.
con <- mongo(
    collection = "data",
    db = "handicaps",
    url = url,
    verbose = FALSE,
    options = ssl_options()
)

# Read data form collection into data.frame. 
data <- con$find(query = '{}')



# Remove connection.
# rm(major_results)



# Disconnect
rm(con)








