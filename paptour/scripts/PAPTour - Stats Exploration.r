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








# Disconnect
rm(con)
