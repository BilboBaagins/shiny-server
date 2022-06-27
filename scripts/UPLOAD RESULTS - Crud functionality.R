# UPLOAD RESULTS - Crud functionality

# mongoDB CRUD
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
    collection = "major_results",
    db = "paptour_db",
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
    Major = 24,
    Date = "04/06/2022",
    Player = c(
        "Niall Devane", 
        "Billy Archbold", 
        "Gearoid Comber",
        "Tiarnan O'Brien",
        "Darragh Sheehan",
        "Ian Cox",
        "Will Molloy",
        "Craig Hyland",
        "David Benn",
        "Oisin Tyrell"
    ),
    Handicap = c(
        20,
        15,
        20,
        22,
        22,
        32,
        30,
        23,
        21,
        19
    ),
    Score = c(
        34,
        32,
        30,
        29,
        28,
        27,
        23,
        20,
        19,
        12
    ), 
    Venue = "Rathsallagh GC"
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
con$remove(query = '{"Major":22}')
con$remove(query = '{"Major":21}')
con$remove(query = '{"Major":20}')

# Read in data to check if it has been successfully deleted.
data <- con$find(query = '{}')





# Edit/update data in mongoDB
#-----------------------------
con$update(query = '{"Player":"Faylo"}', update = '{"$set":{"Player":"Feidhlim Dowling"}}', upsert = FALSE, multiple = TRUE) 
