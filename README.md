# ShinyApp_Lottery

### Overview

This project *Play Your Mega Millions* is created for Duke STA-523 Fall 2022 Final project. We use the lottery data from Texas Mega Millions and design a shiny app for users to retrieve past drawings and explores statistics via tables and plots. Based on the statistics, users are also allowed to generate new potential winning numbers. 

### Reproducibility
To run the Shiny App, we provide the following files:  

* `get_lottery.R`: scrapes the lottery data

* `lottery.rds`: stores the lottery data

* `shinyapp.R`: main code for the shiny app

* `project.Rmd`: all the write-up for the project

Run the `get_lottery.R` to obtain the latest lottery data and the shiny app will be presented by running the `shinyapp.R`.
