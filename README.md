# PAP Tour. 

Official website of the Putt & Pint Tour.  

This is an R Shiny web applicaiton with create, read, update & delete (CRUD) functionality to track and manage stats from Major tournaments.   
The application will utilise an SQLite database for persistent data storage.  

The application is hosted on sevrver space rented from Digital Ocean (DO).  
Server is linux ubuntu distribution 20.04 (latest).  
R, RStudio Server, Shiny Server and Git are downloaded onto the linux server hosted at DO.  
The application is developed locally on a MacBook Pro (2010) and updates are committed to this repository under the name 'shiny-server'.  
To push these changes to the website, you ssh (remote) onto the linux server and navigate to the corresponding 'shiny-server' folder and run sudo git pull to get the latest updates developed locally.   
Any shiny application stored in this 'shiny-server' directory will be automatcally picked up by the Shiny Server hosted on the linux machine and will be run as a shiny application.  








Useful resources:

1. Persistent Data Storage Options
https://shiny.rstudio.com/articles/persistent-data-storage.html#sqlite

2. Shiny Dashboard
https://rstudio.github.io/shinydashboard/get_started.html

3. Enterprise Ready Dashboards
https://db.rstudio.com/best-practices/dashboards/
