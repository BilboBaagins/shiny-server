library(rsconnect)

rsconnect::deployApp(appDir = ".",
                     appFileManifest = "MANIFEST.txt",
                     account = "woodgroupanalytics",
                     server = "shinyapps.io",
                     appName = "DigitalMasterplan",
                     appTitle = "DigitalMasterplan",
                     launch.browser = function(url) {message("Deployment completed: ", url)},
                     lint = FALSE,
                     metadata = list(asMultiple = FALSE,
                                     asStatic = FALSE),
                     logLevel = "verbose")