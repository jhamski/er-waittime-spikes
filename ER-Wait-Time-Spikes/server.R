
library(shiny)
source("global.R")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$all_observations_scatterplot <- renderPlot({

        er_wait_times %>% 
            ggplot(aes(x = read_timestamp, y = total_minutes)) + 
            geom_point() +
            #geom_line(aes(color = hospital_webpage)) + 
            theme_minimal() +
            geom_smooth() + 
            theme(legend.position = "none") + 
        xlab("Timestamp (UTC)") +
        ylab("Stated Public Wait Time (Minutes)")
        

    })
    
    output$facilities_tracked <- renderDT({
      er_wait_times %>% 
        select(hospital_webpage, site_labels, total_minutes, read_timestamp, State, URL)
    })

})
