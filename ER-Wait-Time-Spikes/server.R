
library(shiny)
source("global.R")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$all_observations_scatterplot <- renderPlot({

        er_wait_times %>%
          filter(State %in% input$states_selected) %>% 
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
        filter(State %in% input$states_selected) %>%
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
    
    output$state_data_timerange <- renderPlot({
      state_range <- er_wait_times %>%
        filter(State %in% input$states_selected) %>%
        filter(!is.na(State)) %>% 
        group_by(State) %>% 
        summarize(max_timestamp = max(read_timestamp),
                  min_timestamp = min(read_timestamp))
      
      ggplot(state_range, aes(x =  fct_rev(State))) +
        geom_linerange(aes(ymin = min_timestamp, ymax = max_timestamp, color = State), size = 7) + 
        scale_y_datetime(limits = c(min(er_wait_times$read_timestamp), max(er_wait_times$read_timestamp))) +
        coord_flip() + 
        theme_minimal() +
        xlab("")
    }, height = 125)
    
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("scraped_er_wait_times_",max(er_wait_times$read_timestamp), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(er_wait_times, file, row.names = FALSE)
      }
    )
    
    
})
