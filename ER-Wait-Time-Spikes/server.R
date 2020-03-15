
library(shiny)
source("global.R")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$all_observations_scatterplot <- renderPlot({

        er_wait_times %>% 
          ggplot(aes(x = read_timestamp, y = total_minutes)) + 
          geom_point(aes(color = State)) +
          #geom_line(aes(color = hospital_webpage)) + 
          theme_minimal() +
          geom_smooth() +
          xlab("Timestamp (UTC)") +
          ylab("Stated Public Wait Time (Minutes)")
        

    })
    
    output$wait_boxplots <- renderPlot({
      er_wait_times %>%
        filter(!is.na(State)) %>% 
        ggplot(aes(x = hospital_webpage, y = total_minutes)) +
        geom_boxplot() + 
        #stat_summary(fun.data = give.n, geom = "text", fun.y = median,
        #position = position_dodge(width = 0.75)) +
        facet_wrap(~State, scales = "free_x") +
        theme_minimal() +
        ylab("Stated Wait Time (Minutes)") +
        xlab("")
    })
    
    output$facilities_tracked <- renderDT({
      er_wait_times %>% 
        select(hospital_webpage, site_labels, total_minutes, read_timestamp, State, URL)
    })
    
    
    
})
