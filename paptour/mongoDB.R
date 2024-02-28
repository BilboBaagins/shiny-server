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

# If getting the following type of error: Error in mongo_collection_command_simple(col, "{\"ping\":1}") : ignoring SIGPIPE signal
#  Your IP is not whitelisted. 
# Check for "Your current IP address included" when looking at whitelisted IP addresses. 
# Not a very descriptive error. 