# parsing_script.R

# Set working directory for wagusta project. 
setwd("/Users/billyarchbold/Documents/wagusta/")

# Leave history.csv and ogr.csv seperated.
# Coallate all the results csvs together into master file & add some extra columns for tagging Year, Event No. etc.

# official golf rankings
ogr <- read.csv('ogr.csv', stringsAsFactors=F)

# 2015
m_2015_01 <- read.csv('2015_01_Grange_Castle.csv', stringsAsFactors=F)
m_2015_02 <- read.csv('2015_02_Corballis_Links.csv', stringsAsFactors=F)
m_2015_03 <- read.csv('2015_03_St_Helens_Bay.csv', stringsAsFactors=F)
m_2015_04 <- read.csv('2015_04_Craddockstown.csv', stringsAsFactors=F)

# 2016
m_2016_01 <- read.csv('2016_01_Killeen.csv', stringsAsFactors=F)
m_2016_02 <- read.csv('2016_02_Rathcore.csv', stringsAsFactors=F)
m_2016_03 <- read.csv('2016_03_Newbridge.csv', stringsAsFactors=F)
m_2016_04 <- read.csv('2016_04_Rosslare.csv', stringsAsFactors=F)

# 2017
m_2017_01 <- read.csv('2017_01_Grange_Castle.csv', stringsAsFactors=F)
m_2017_02 <- read.csv('2017_02_City_West.csv', stringsAsFactors=F)

# 2018
m_2018_01 <- read.csv('2018_01_Tulfaris.csv', stringsAsFactors=F)
m_2018_02 <- read.csv('2018_02_St_Helens_Bay.csv', stringsAsFactors=F)
m_2018_03 <- read.csv('2018_03_Cradockstown.csv', stringsAsFactors=F)

history <- read.csv('history_of_majors.csv', stringsAsFactors=F)



# Combine reuslts together. 
# Add Year to data.
m_2015_01$Year <- 2015
m_2015_02$Year <- 2015
m_2015_03$Year <- 2015
m_2015_04$Year <- 2015
m_2016_01$Year <- 2016
m_2016_02$Year <- 2016
m_2016_03$Year <- 2016
m_2016_04$Year <- 2016
m_2017_01$Year <- 2017
m_2017_02$Year <- 2017
m_2018_01$Year <- 2018
m_2018_02$Year <- 2018
m_2018_03$Year <- 2018

# Add event to data
m_2015_01$Event <- "Grange Castle"
m_2015_02$Event <- "Corballis Links"
m_2015_03$Event <- "St Helens Bay"
m_2015_04$Event <- "Craddockstown"
m_2016_01$Event <- "Killeen"
m_2016_02$Event <- "Rathcore"
m_2016_03$Event <- "Newbridge"
m_2016_04$Event <- "Rosslare Links"
m_2017_01$Event <- "Grange Castle"
m_2017_02$Event <- "Citywest"
m_2018_01$Event <- "Tulfaris"
m_2018_02$Event <- "St Helens Bay"
m_2018_03$Event <- "Craddockstown"

# Combine all majors together into master file. 
master  <- rbind(
    m_2015_01,
    m_2015_02,
    m_2015_03,
    m_2015_04,
    m_2016_01,
    m_2016_02,
    m_2016_03,
    m_2016_04,
    m_2017_01,
    m_2017_02,
    m_2018_01,
    m_2018_02,
    m_2018_03
)

# Save the CSVs in data folder. 
write.csv(master, "major_results.csv", row.names=FALSE)
write.csv(ogr, "ogr.csv", row.names=FALSE)