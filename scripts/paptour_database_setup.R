# paptour database setup

# Load library.
library(mongolite)

# Credentials.
user <- "billy"
pwd <- "JliMaQpXzTKYFCGe"

# Construct your connection URL.
url <- paste0("mongodb+srv://", user, ":", pwd, "@cluster0.4yzge.mongodb.net/?retryWrites=true&w=majority")

# For a successful connection, need to ensure IP is whitelisted.
# Find it in: [project_name] > Network Access




# 1) MAJOR RESULTS
#-----------------------------
data <- read.csv("./data/major_results_2022_06_04.csv")

# Create connection to new DB & collection.
major_results <- mongo(collection = "major_results", # Creating collection
               db = "paptour_db", # Creating DataBase
               url = url, 
               verbose = TRUE)

# Insert data to mongoDB.
major_results$insert(data)

# Remove connection.
rm(major_results)



# 2) HANDICAPS
#-----------------------------
# Subset data to retrieve last major all payers entered
hcap <- data %>%
    group_by(Player) %>%
    filter(Major == max(Major)) %>%
    ungroup() %>% 
    arrange(-Major) %>%
    data.frame()

# Create connection to new DB & collection.
handicaps <- mongo(collection = "handicaps", # Creating collection
               db = "paptour_db", # Creating DataBase
               url = url, 
               verbose = TRUE)

# Insert data to mongoDB.
handicaps$insert(hcap)

# Remove connection.
rm(handicaps)

