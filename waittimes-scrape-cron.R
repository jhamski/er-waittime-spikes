library(cronR)

er_scraper_path = file.path("/Users/Jim/Documents/R/er-waitime-spikes", "er-scraper.R")

cron_add(er_scraper_path, frequency = 'hourly', 
         id = 'er-scraper', 
         description = 'Scrape waittimes every hour')

cron_ls()


read_csv("waittimes_record.csv") %>% 
  View("waittimes csv")
