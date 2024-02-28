# HISORIC HANDICAPS

# mongoDB CRUD
# https://docs.mongodb.com/manual/crud/

# Load library.
library(mongolite)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(viridis)

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
    collection = "major_results",
    db = "paptour_db",
    url = url,
    verbose = FALSE,
    options = ssl_options()
)

# Read data form collection into data.frame. 
data <- con$find(query = '{}')

data$Year <- lubridate::year(lubridate::dmy(data$Date))

Event <- reorder(paste0("Major ", data$Major, " (", data$Year, ")"), data$Major)


# Plot
p <- data %>%
    ggplot( aes(x=Event, y=Handicap, group=Player, color=Player)) +
    geom_line() +
    geom_point() +
    #scale_color_viridis(discrete = TRUE) +
    ggtitle("Handicap Timeseries") +
    theme_ipsum() +
    ylab("Handicaps") +
    xlab("Major Timeline") +
    theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=1))
# Turn it interactive with ggplotly
p <- ggplotly(p)


legend_hide <- data.frame()
    for(i in 1:length(p$x$data)){
        legend_hide <- rbind(legend_hide, tail(p$x$data[[i]]$y, 1))
        colnames(legend_hide)[1] <- "Value"
        p$x$data[[i]]$visible <- "legendonly"
    }
    
    for(i in 1:length(p$x$data)){
        if( tail(p$x$data[[i]]$y, 1) %in% top_n(legend_hide, -9)$Value ){
            p$x$data[[i]]$visible <- NULL
        }
    }

p

