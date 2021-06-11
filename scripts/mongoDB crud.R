# mongoDB CRUD
# https://docs.mongodb.com/manual/crud/

# Load library.
library(mongolite)

# Credentials.
user <- "billy"
pwd <- "JliMaQpXzTKYFCGe"

# Construct your connection URL.
url <- paste0("mongodb+srv://", user, ":", pwd, "@cluster0.4yzge.mongodb.net/")

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







# Append/Edit data to mongoDB
#-----------------------------








# Delete data from mongoDB
#-----------------------------
