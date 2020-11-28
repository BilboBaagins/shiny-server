library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)
library(shinyWidgets)
library(shinyalert)
library(rjson)
library(stringr)
library(magick)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(pdftools)
library(quanteda)
library(readtext)
library(lubridate)
library(stringr)
library(shinyWidgets)
library(kableExtra)
library(digest)
library(dplyr)
# Looking into using slickR for carosuel of images. 
# Potentially, use them to hyperlink to 'news' articles. 
# Almost clickbait-y like "Look at what John Rahm did"
library(slickR)
library(reactable)

# Create a custom value / info box
createInfoBox <- function(value, iconName, text) {
  return(
    div(div(div(value, class = "label-big"), span(iconName, class = "label-icon"), class = "label-box"), div(text, class = "label-box-explainer"))
  )
}