# mongoDB connections

# Load library.
library(mongolite)

# Credentials.
user <- "billy"
pwd <- "JliMaQpXzTKYFCGe"

# Construct your connection URL.
url <- paste0("mongodb+srv://", user, ":", pwd, "@cluster0.4yzge.mongodb.net/")

# For a successful connection, need to ensure IP is whitelisted.
# Find it in: [project_name] > Network Access

