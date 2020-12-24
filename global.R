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
library(htmltools)
library(slickR)
library(reactable)
library(firebase)
library(dplyr)

# Create a custom value / info box
createInfoBox <- function(value, iconName, text) {
  return(
    div(div(div(value, class = "label-big"), span(iconName, class = "label-icon"), class = "label-box"), div(text, class = "label-box-explainer"))
  )
}

# Create a rating stars function.
rating_stars <- function(rating) {
  star_icon <- function(empty = FALSE) {
    tagAppendAttributes(shiny::icon("star"),
      style = paste("font-size: 16px; padding: 0px; color: ", if (empty) "#edf0f2" else "orange"),
      "aria-hidden" = "true"
    )
  }
  rounded_rating <- floor(rating + 0.5)  # always round up
  stars <- lapply(seq_len(rounded_rating), function(i) {
    star_icon()
  })
  label <- sprintf("%s stars", rating)
  div(title = label, "aria-label" = label, role = "img", stars)
}

# Render a bar chart with a label on the left.
bar_chart <- function(label, width = "100%", height = "16px", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "8px", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}