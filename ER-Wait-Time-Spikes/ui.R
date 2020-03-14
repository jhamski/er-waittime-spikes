#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("global.R")
# Define UI for application that draws a histogram
shinyUI(
    
    dashboardPage(skin = "black",
        dashboardHeader(title = "ER Wait Time Tracker"),
        dashboardSidebar(collapsed = TRUE, disable = TRUE),
        dashboardBody(
            fluidPage(
                "This app uses web scraping to observe stated Emergency Room (ER) wait times for facilities in the United States. It is informational purposes only and should not be used for any decision making. If you are having a medical emergency, call 911.",
                " Given the current coronavirus (COVID-19) it is possible that this app will show stress on emergency care facilities when aggregated across locations as indicated by increasing wait times.",
                " However, note I have no insight into how these wait times are updated and there is not indication that they will be accurate during a time of crisis.",
                headerPanel(""),
                fluidRow(
                    column(6, 
                           infoBox(title = "ER Facilities Tracked", value = er_facility_count, 
                                   #subtitle = 
                                   icon = icon("plus", lib = "glyphicon"),
                                   fill = TRUE, color = "yellow", width = NULL)
                           ),
                    column(6, infoBox(title = "States Tracked", value = unique(er_wait_times$State) %>% length(), 
                                      #subtitle = 
                                      icon = icon("map-marker", lib = "glyphicon"),
                                      fill = TRUE, color = "yellow", width = NULL)
                    )
                    
                    ),
                headerPanel(""),
            fluidRow(plotOutput("all_observations_scatterplot"))
                ),
            headerPanel(""),
            fluidRow(DTOutput("facilities_tracked"))
            )
    )

        # Show a plot of the generated distribution
 
            

)


