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





# Upload data to mongoDB
#-----------------------------
# Read CSV from local directory.
data <- read.csv("data/major_results.csv", stringsAsFactors=FALSE, check.names=FALSE)

# Create connection to new DB & collection.
major_results <- mongo(collection = "data", # Creating collection
               db = "major_results", # Creating DataBase
               url = url, 
               verbose = TRUE)

# Insert data to mongoDB.
major_results$insert(data)

# Remove connection.
rm(major_results)




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
data <- con$find(query = '{"Player":"Billy Archbold"}')
data <- con$find(query = '{"Major":18}')
data <- con$find(query = '{"Score":23}')


# Read data form collection into data.frame. 
data <- con$find(query = '{}')

# Disconnect
rm(con)







# Append data to mongoDB
#-----------------------------

# MANUALLY ENTER
data <- data.frame(
    Major = 25,
    Date = "25/06/2022",
    Player = c(
        "Billy Archbold", 
        "Gearoid Comber",
        "Tiarnan O'Brien",
        "Darragh Sheehan",
        "Will Molloy",
        "Craig Hyland",
        "Dave McGrath",
        "Phil Mahon"
    ),
    Handicap = c(
        14,
        20,
        22,
        22,
        30,
        23,
        32,
        32
    ),
    Score = c(
        27,
        27,
        30,
        31,
        27,
        28,
        27,
        19
    ), 
    Venue = "Newbridge"
)

# Append new data to MongoDB.
con$insert(data)



# FROM CSV
#data <- read.csv("../../Downloads/Major Results - Major Results.csv")
data <- read.csv("./Downloads/Major Results - Major Results.csv")
#data_new <- data[data$Major %in% c(20, 21, 22), ]
data_new <- data[data$Major %in% c(23), ]


# Append new data to MongoDB.
con$insert(data_new)

# Disconnect
rm(con)





# Delete data from mongoDB
#-----------------------------

# Delete
con$remove(query = '{"Major":26}')

con$remove(query = '{"Major":21}')
con$remove(query = '{"Major":20}')

# Read in data to check if it has been successfully deleted.
data <- con$find(query = '{}')





# Edit/update data in mongoDB
#-----------------------------
con$update(query = '{"Player":"Faylo"}', update = '{"$set":{"Player":"Feidhlim Dowling"}}', upsert = FALSE, multiple = TRUE) 









con$remove(query = '{"Major":25}')


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







con$remove(query = '{"Major":27}')

# Append new data to MongoDB.
con$insert(major27)

# Disconnect
rm(con)
