# Investigating RSQLite for persistent data storage. 

library(DBI)
# Create an ephemeral in-memory RSQLite database
con <- dbConnect(RSQLite::SQLite(), ":memory:")

dbListTables(con)







# Load the RSQLite Library
library(RSQLite)
# Load the mtcars as an R data frame put the row names as a column, and print the header.
data("mtcars")
mtcars$car_names <- rownames(mtcars)

dbListTables(con)
  
head(mtcars)
# Create a connection to our new database, CarsDB.db
# you can check that the .db file has been created on your working directory
conn <- dbConnect(RSQLite::SQLite(), "data/CarsDB.db")

# Write the mtcars dataset into a table names mtcars_data
dbWriteTable(conn, "cars_data", mtcars)
# List all the tables available in the database
dbListTables(conn)

# Read in data from the table.
data <-  dbGetQuery(conn, "SELECT * FROM CARS_DATA")

# Close the database connection to CarsDB
dbDisconnect(conn)


