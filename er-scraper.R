

library(tidyverse)
library(rvest)    
library(stringr)
library(gsheet)
library(lubridate)


realtime_hospital_list <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1MCiyFx-kC2Et88taXHt49x4W0PTJboJ93A-SmZ9Ed4Q/edit?usp=sharing")

waittimes_record_existing <- read_csv("waittimes_record.csv")
scrape_waittimes <- function(){
  
  # Mary Washington
  mary_washington_url <- "https://www.marywashingtonhealthcare.com/ER-Wait-Times.aspx"
  
  mary_washington_content <- read_html(mary_washington_url)
  mary_washington_read_timestamp = Sys.time()
  
  mary_washington_labels <- mary_washington_content %>% 
    rvest::html_nodes('body') %>% 
    xml2::xml_find_all("//div[contains(@class, 'item-text')]") %>% 
    rvest::html_text(trim = TRUE) %>% 
    str_replace_all(regex("\\n"), " ") %>% 
    str_replace_all(regex("\\t"), "") %>% 
    str_extract(regex(".+?(?=More)"))
  
  mary_washington_wait <- mary_washington_content %>% 
    rvest::html_nodes('body') %>% 
    xml2::xml_find_all("//div[contains(@class, 'item-time-inner')]") %>% 
    rvest::html_text() %>% 
    str_match_all("[0-9]+") %>% 
    unlist() %>% 
    as.numeric() %>% 
    tibble::enframe(name = NULL, value = "scraped_waittimes") %>% 
    mutate(hospital_webpage = "Mary Washington",
           total_minutes = as.numeric(scraped_waittimes),
           read_timestamp = mary_washington_read_timestamp, 
           site_labels = mary_washington_labels)
  
  atrium_url <- "https://atriumhealth.org/locations/emergency-care-locations"
  atrium_content <- read_html(atrium_url)
  atrium_read_timestamp = Sys.time()
  
  atrium_site_labels <- atrium_content %>% 
    rvest::html_nodes('body') %>% 
    xml2::xml_find_all("//h3[contains(@class, 'panel-title')]") %>% 
    rvest::html_text() %>% 
    str_replace_all(regex("\\n"), "") %>% 
    str_replace_all(regex("\\r"), "") %>% 
    trimws()
  
  atrium_content_wait <- atrium_content %>% 
    rvest::html_nodes('body') %>% 
    xml2::xml_find_all("//div[contains(@class, 'alert alert-danger')]") %>% 
    rvest::html_text() %>% 
    as.character() %>% 
    tibble::enframe(name = NULL, value = "scraped_waittimes")
  
  atrium_waittimes <- atrium_content_wait %>% 
    mutate(hospital_webpage = "Atrium",
           is_less_than = str_detect(scraped_waittimes, pattern = regex("less than", ignore_case = T)),
           displayed_minutes = str_extract(scraped_waittimes, pattern = regex("\\d*(?= Minutes)", ignore_case = T)) %>% 
             as.numeric(),
           hours = str_extract(scraped_waittimes, pattern = regex("\\d*(?= Hours)", ignore_case = T)) %>% 
             as.numeric() %>% 
             replace_na(0),
           total_minutes = displayed_minutes + (hours * 60),
           read_timestamp = atrium_read_timestamp, 
           site_labels = atrium_site_labels)
  
  waittimes_record = atrium_waittimes %>% 
    select(scraped_waittimes, hospital_webpage, total_minutes, read_timestamp, site_labels) %>% 
    bind_rows(mary_washington_wait %>% mutate(scraped_waittimes = as.character(scraped_waittimes)))
  
  return(waittimes_record)
}

waittimes_record <- scrape_waittimes() 
waittimes_record_updated <- bind_rows(waittimes_record_existing, waittimes_record)

write_csv(waittimes_record_updated, path = "waittimes_record.csv")

print(paste0("Scraped ER wait times at: ", Sys.time()))