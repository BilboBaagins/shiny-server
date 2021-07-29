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
library(ggplot2)
library(hrbrthemes)
library(viridis)
#library(shinydisconnect) # customise the disconnect from server message.
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

#' inserts JavaScript to detect if user is using Mobile Device
#' 
#' @details 
#' use \code{input$isMobile} to receive if user is using Mobile Device. the value
#' is either \code{TRUE} or \code{FALSE}.
#' 
#' @examples
#' if (interactive()){
#' shinyApp(
#'     ui <- fluidPage(
#'         isMobile()
#'     ),
#'     server <- function(input, output, session){
#'         isMobile <- eventReactive(input$isMobile,input$isMobile)
#'         observe(print(res()))
#'     }   
#'     )
#' }
#' @export
isMobile <- function(){
    shiny::tagList(
        shiny::singleton(
            # detect is user are using following devices
            tags$script("
            $(document).on('shiny:sessioninitialized', function(e){
                var isMobile = /((iPhone)|(iPad)|(Android)|(BlackBerry))/.test(navigator.userAgent);
                Shiny.setInputValue('isMobile',isMobile)
            })
          ")
        )
    )
}