library(tidyverse)
library(rvest)    
library(stringr)
library(gsheet)
library(lubridate)

setwd("/Users/Jim/Documents/R/er-waitime-spikes")

waittimes_record_existing <- read_csv("waittimes_record.csv")

scrape_mary_washington <- function(){
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
           site_labels = mary_washington_labels,
           scraped_waittimes = as.character(scraped_waittimes))
  
  return(mary_washington_wait)
}
  
scrape_atrium <- function(){
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
           site_labels = atrium_site_labels,
           scraped_waittimes = as.character(scraped_waittimes))
  
  return(atrium_waittimes)
}
  
scrape_kaleida <- function(){
  kaleida_content <- "https://m.kaleidahealth.org/er/"
  kaleida_content <- read_html(kaleida_content)
  kaleida_read_timestamp = Sys.time()
  
  kaleida_labels <- kaleida_content %>% 
    rvest::html_nodes('body') %>% 
    xml2::xml_find_all("//div[contains(@class, 'er-wait-hospital-listview')]") %>% 
    rvest::html_text(trim = TRUE)
  
  kaleida_wait <- kaleida_content %>% 
    rvest::html_nodes('body') %>% 
    xml2::xml_find_all("//div[contains(@class, 'er-wait-listview')]") %>% 
    rvest::html_text() %>% 
    str_match_all("[0-9]+") %>% 
    unlist() %>% 
    as.numeric() %>% 
    tibble::enframe(name = NULL, value = "scraped_waittimes") %>% 
    mutate(hospital_webpage = "Kaleida",
           total_minutes = as.numeric(scraped_waittimes),
           read_timestamp = kaleida_read_timestamp, 
           site_labels = kaleida_labels,
           scraped_waittimes = as.character(scraped_waittimes))
  return(kaleida_wait)
}  
  
scrape_nyu <- function(){
  nyu_langone_url <- "https://nyulangone.org/locations/emergency-care"
  
  nyu_langone_content <- read_html(nyu_langone_url)
  nyu_langone_timestamp = Sys.time()
  
  nyu_langone_wait <- nyu_langone_content %>% 
    rvest::html_nodes('body') %>%
    xml2::xml_find_all("//*[contains(@class, 'our-locations__wait-time-display')]") %>% 
    rvest::html_text() %>% 
    tibble::enframe(name = NULL, value = "scraped_waittimes") %>% 
    distinct(scraped_waittimes) %>% 
    mutate(hospital_webpage = "NYU Langone",
           total_minutes =  str_extract(scraped_waittimes, regex("[0-9]+")) %>% as.numeric(),
           read_timestamp = nyu_langone_timestamp, 
           site_labels = c("NYU Langone Hospital—Brooklyn", "NYU Langone Health—Cobble Hill"),
           scraped_waittimes = as.character(scraped_waittimes))
  
  return(nyu_langone_wait)
}  
  
scrape_longview <- function(){
  longview_url <- "https://www.longviewregional.com/er-wait-time"
  longview_content <- read_html(longview_url)
  longview_timestamp = Sys.time()
  
  longview_wait <- longview_content %>% 
    rvest::html_nodes('body') %>%
    xml2::xml_find_all("//*[contains(@id, 'spForTable')]") %>% 
    rvest::html_text() %>% 
    tibble::enframe(name = NULL, value = "scraped_waittimes") %>% 
    distinct(scraped_waittimes) %>% 
    mutate(hospital_webpage = "Longview TX",
           total_minutes =  str_extract(scraped_waittimes, regex("[0-9]+")) %>% as.numeric(),
           read_timestamp = longview_timestamp, 
           site_labels = "Longview Regional Medical Center, TX",
           scraped_waittimes = as.character(scraped_waittimes))
  return(longview_wait)
}
  

scrape_tristar <- function(){
  tristar_url <- "https://tristarportland.com/"
  tristar_content <- read_html(tristar_url)
  tristar_timestamp = Sys.time()
  
  tristar_wait <- tristar_content %>% 
    rvest::html_nodes('body') %>%
    xml2::xml_find_all("//*[contains(class, 'time')]") %>% 
    rvest::html_text() %>% 
    tibble::enframe(name = NULL, value = "scraped_waittimes") %>% 
    distinct(scraped_waittimes) %>% 
    mutate(hospital_webpage = "Tristar Portland",
           total_minutes =  str_extract(scraped_waittimes, regex("[0-9]+")) %>% as.numeric(),
           read_timestamp = tristar_timestamp, 
           site_labels = "Tristar Portland",
           scraped_waittimes = as.character(scraped_waittimes))
  return(tristar_wait)
}
  
scrape_south_texas <- function(){
  south_texas_url <- "https://www.southtexashealthsystem.com/our-facilities"
  south_texas_content <- read_html(south_texas_url)
  south_texas_timestamp = Sys.time()
  
  south_texas_labels <- c("South Texas Health System Children's", 
                          "South Texas Health System Edinburg",
                          "South Texas Health System ER Alamo",
                          "South Texas Health System ER McColl",
                          "South Texas Health System ER Mission",
                          "South Texas Health System ER Monte Cristo",
                          "South Texas Health System ER Ware Road",
                          "South Texas Health System ER Weslaco",
                          "South Texas Health System Heart")
  
  south_texas_wait <- south_texas_content %>% 
    rvest::html_nodes('body') %>%
    xml2::xml_find_all("//*[contains(@class, 'display_minutes_time')]") %>% 
    rvest::html_text() %>% 
    tibble::enframe(name = NULL, value = "scraped_waittimes") %>% 
    mutate(hospital_webpage = "South Texas Health System",
           total_minutes =  str_extract(scraped_waittimes, regex("[0-9]+")) %>% as.numeric(),
           read_timestamp = south_texas_timestamp,
           site_labels = south_texas_labels,
           scraped_waittimes = as.character(scraped_waittimes))
  return(south_texas_wait)
}

piedmont_scraper <- function(){
  piedmont_url <- "https://www.piedmont.org/emergency-room-wait-times/emergency-room-wait-times"
  piedmont_content <- read_html(piedmont_url)
  piedmont_timestamp = Sys.time()
  
  piedmont_labels <- c("Piedmont Athens Regional",
                       "Piedmont Atlanta",
                       "Piedmont Columbus Midtown",
                       "Piedmont Columbus Midtown Pediatrics",
                       "Piedmont Columbus Northside",
                       "Piedmont Fayette",
                       "Piedmont Henry",
                       "Piedmont Mountainside",
                       "Piedmont Mountainside - Ellijay",
                       "Piedmont Newnan",
                       "Piedmont Newton",
                       "Piedmont Rockdale Hospital",
                       "Piedmont Walton Hospital")
  
  piedmont_wait <- piedmont_content %>% 
    rvest::html_nodes('body') %>%
    xml2::xml_find_all("//*[contains(@class, 'timeBox')]") %>% 
    rvest::html_text() %>% 
    tibble::enframe(name = NULL, value = "scraped_waittimes") %>% 
    mutate(hospital_webpage = "Piedmont",
           total_minutes =  str_extract(scraped_waittimes, regex("[0-9]+")) %>% as.numeric(),
           read_timestamp = piedmont_timestamp, 
           site_labels = piedmont_labels,
           scraped_waittimes = as.character(scraped_waittimes))
  return(piedmont_wait)
}

hunt_scraper <- function(){
  hunt_regional_url <- "https://www.huntregional.org/our_services/emergency_services/er_wait_times.aspx"
  hunt_regional_content <- read_html(hunt_regional_url)
  hunt_regional_timestamp = Sys.time()
  
  hunt_regional_labels <- hunt_regional_content %>% 
    rvest::html_nodes('body') %>%
    xml2::xml_find_all("//*[contains(@class, 'hospital-location')]") %>% 
    rvest::html_text() %>% 
    str_extract(regex("(\\w+)"))
  
  hunt_regional_wait <- hunt_regional_content %>% 
    rvest::html_nodes('body') %>%
    xml2::xml_find_all("//*[contains(@class, 'times')]") %>% 
    rvest::html_text() %>% 
    tibble::enframe(name = NULL, value = "scraped_waittimes") %>% 
    mutate(hospital_webpage = "Hunt Regional",
           total_minutes =  str_extract(scraped_waittimes, regex("[0-9]+")) %>% as.numeric(),
           read_timestamp = hunt_regional_timestamp, 
           site_labels = hunt_regional_labels,
           scraped_waittimes = as.character(scraped_waittimes))
  return(hunt_regional_wait)
}

scrape_northwell <- function(){
  northwell_ny_url <- "https://www.northwell.edu/wait-times"
  northwell_ny_content <- read_html(northwell_ny_url)
  northwell_ny_timestamp = Sys.time()
  
  northwell_ny_labels <- northwell_ny_content %>% 
    rvest::html_nodes('body') %>%
    xml2::xml_find_all("//div[contains(@class, 'wait-time-header__location row')]") %>% 
    rvest::html_text() %>% 
    tibble::enframe(name = NULL, value = "labels") %>% 
    mutate(labels = trimws(labels, which = "both"))
  
  northwell_ny_wait <- northwell_ny_content %>% 
    rvest::html_nodes('body') %>%
    xml2::xml_find_all("//*[contains(@class, 'wait-time__numeric')]") %>% 
    rvest::html_text() %>% 
    tibble::enframe(name = NULL, value = "scraped_waittimes") %>% 
    mutate(hospital_webpage = "Northwell NY",
           total_minutes =  str_extract(scraped_waittimes, regex("[0-9]+")) %>% as.numeric(),
           read_timestamp = northwell_ny_timestamp, 
           site_labels = northwell_ny_labels$labels,
           scraped_waittimes = as.character(scraped_waittimes))
  
  return(northwell_ny_wait)
}

scrape_waittimes <- function(){
    
  waittimes_record <- bind_rows(
    tryCatch(expr = scrape_mary_washington(), error = function(cond){return(NULL)}),
    tryCatch(expr = scrape_atrium(), error = function(cond){return(NULL)}),
    tryCatch(expr = scrape_kaleida(), error = function(cond){return(NULL)}),
    tryCatch(expr = scrape_longview(), error = function(cond){return(NULL)}),
    tryCatch(expr = scrape_nyu(), error = function(cond){return(NULL)}),
    tryCatch(expr = scrape_south_texas(), error = function(cond){return(NULL)}),
    tryCatch(expr = piedmont_scraper(), error = function(cond){return(NULL)}),
    tryCatch(expr = hunt_scraper(), error = function(cond){return(NULL)}),
    tryCatch(expr = scrape_northwell(), error = function(cond){return(NULL)})
    
  )
  
  
  return(waittimes_record)
}



waittimes_record <- scrape_waittimes() %>% 
  select(-is_less_than, -displayed_minutes, -hours)

waittimes_record_updated <- bind_rows(waittimes_record_existing, waittimes_record) %>% 
  mutate(hospital_webpage = if_else(str_detect(site_labels, "Piedmont"), "Piedmont", hospital_webpage))


write_csv(waittimes_record_updated, path = "waittimes_record.csv")
write_csv(waittimes_record_updated, path = "ER-Wait-Time-Spikes/waittimes_record.csv")

print(paste0("Scraped ER wait times at: ", Sys.time()))





