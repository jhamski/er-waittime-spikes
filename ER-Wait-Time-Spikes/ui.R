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
                fluidRow(includeMarkdown("intro-text.md")),
                headerPanel(""),
                fluidRow(
                    column(4, downloadButton("downloadData", "Download Wait Time Data"))
                ),
                headerPanel(""),
                fluidRow(
                    column(4, 
                           infoBox(title = "ER Facilities Tracked", value = er_facility_count, 
                                   #subtitle = 
                                   icon = icon("plus", lib = "glyphicon"),
                                   fill = TRUE, color = "yellow", width = NULL)
                           ),
                    column(4, infoBox(title = "States Represented", value = unique(er_wait_times$State) %>% length(), 
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
                column(8, checkboxGroupInput("states_selected", "States", 
                                             choices = unique(er_wait_times$State),
                                             selected = unique(er_wait_times$State),
                                             inline = TRUE),
                       plotOutput("all_observations_scatterplot")),
                column(4, plotOutput("wait_boxplots"))
                ),
            fluidRow(
                column(8, plotOutput("state_data_timerange", height = 125)),
                column(4, NULL)
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


