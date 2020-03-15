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
                "This app uses web scraping to observe stated Emergency Room (ER) wait times for several facilities in the United States. It is informational purposes only and should not be used for any decision making. If you are having a medical emergency, call 911.",
                " Given the current coronavirus (COVID-19) pandemic it is possible that this data will show stress on emergency care facilities when aggregated across locations as indicated by increasing wait times.",
                " However, note I have no insight into how these wait times are updated and there is no indication that they will be accurate during a time of crisis.",
                " ER wait time webpages are queried once per hour to minimally stress their hosts.", 
                " Next steps are to (1) search for more wait times to scrape, (2) see if there are proxies that would allow the complex seasonality and variance to be estimated despite the limited number of observations, and (3) improve visualization.",
                " See the data and code here: https://github.com/jhamski/er-waittime-spikes.",
                headerPanel(""),
                fluidRow(
                    column(4, 
                           infoBox(title = "ER Facilities Tracked", value = er_facility_count, 
                                   #subtitle = 
                                   icon = icon("plus", lib = "glyphicon"),
                                   fill = TRUE, color = "yellow", width = NULL)
                           ),
                    column(4, infoBox(title = "States Tracked", value = unique(er_wait_times$State) %>% length(), 
                                      #subtitle = 
                                      icon = icon("map-marker", lib = "glyphicon"),
                                      fill = TRUE, color = "yellow", width = NULL)
                    ),
                    column(4, infoBox(title = "Max Days in Record", 
                           value = round(max(er_wait_times$read_timestamp) - min(er_wait_times$read_timestamp), 1),
                           subtitle = "Not complete for all facilities.",
                           icon = icon("time", lib = "glyphicon"),
                           fill = TRUE, color = "yellow", width = NULL))
                    
                    ),
                headerPanel(""),
            fluidRow(
                column(8, plotOutput("all_observations_scatterplot")),
                column(4, plotOutput("wait_boxplots"))
                ),
            headerPanel(""),
            fluidRow(DTOutput("facilities_tracked")),
            tags$head(tags$style(HTML('
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #ffffff;
                                }

                                ')))
            )
    )

        # Show a plot of the generated distribution
 
            
)
)


