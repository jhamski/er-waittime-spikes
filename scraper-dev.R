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
         site_labels = hunt_regional_labels)



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
  mutate(hospital_webpage = "South Texas Health System",
         total_minutes =  str_extract(scraped_waittimes, regex("[0-9]+")) %>% as.numeric(),
         read_timestamp = piedmont_timestamp, 
         site_labels = piedmont_labels)



south_texas_url <- "https://www.southtexashealthsystem.com/our-facilities"
south_texas_content <- read_html(south_texas_url)
south_texas_timestamp = Sys.time()

south_texas_labels <- c("South Texas Health System Edinburg",
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
         site_labels = south_texas_labels)


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
         site_labels = "Longview Regional Medical Center, TX")

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
         site_labels = c("NYU Langone Hospital—Brooklyn", "NYU Langone Health—Cobble Hill"))


# uses JS to load time
regional_san_jose_url <- "https://regionalmedicalsanjose.com/about/er-wait-times.dot"

regional_san_jose_content <- read_html(regional_san_jose_url)
regional_san_jose_timestamp = Sys.time()

regional_san_jose_wait <- regional_san_jose_content %>% 
  rvest::html_nodes('body') %>%
  xml2::xml_find_all("//*[contains(@class, 'time')]") %>% 
  rvest::html_text() %>% 
  tibble::enframe(name = NULL, value = "scraped_waittimes") %>% 
  filter(str_detect(scraped_waittimes, regex('min')))
mutate(hospital_webpage = "San Jose Regional",
       total_minutes = as.numeric(scraped_waittimes),
       read_timestamp = regional_san_jose_timestamp, 
       site_labels = san_jose_labels)



# might use JS
adventis_url <- "https://www.adventisthealthcare.com/services/emergency/"
adventis_content <- read_html(adventis_url)
adventis_read_timestamp = Sys.time()

adventis_wait <- adventis_content %>% 
  rvest::html_nodes('body') %>%
  xml2::xml_find_all("//*[contains(@class, 'Time')]") %>% 
  rvest::html_text() %>% 
  tibble::enframe(name = NULL, value = "scraped_waittimes") %>% 
  filter(str_detect(scraped_waittimes, regex('min')))
  mutate(hospital_webpage = "Adventis",
         total_minutes = str_extract(regex("[0-9]+")),
         read_timestamp = adventis_read_timestamp, 
         site_labels = adventis_labels)


# uses JS to load time...
fairview_park_url <- "https://fairviewparkhospital.com/"
fairview_park_content <- read_html(fairview_park_url)
fairview_park_read_timestamp = Sys.time()

fairview_park_wait <- fairview_park_content %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//*[contains(@class, 'row')]") %>% 
  rvest::html_text() %>%
  tibble::enframe(name = NULL, value = "scraped_waittimes") %>% 
  filter(str_detect(scraped_waittimes, regex('min'))) %>% 
  mutate(scraped_waittimes = str_replace_all(scraped_waittimes, "\t", ""))
  unlist() %>% 
  as.numeric() %>% 
  tibble::enframe(name = NULL, value = "scraped_waittimes") %>% 
  mutate(hospital_webpage = "Fairview Park",
         total_minutes = as.numeric(scraped_waittimes),
         read_timestamp = fairview_park_read_timestamp, 
         site_labels = fairview_park_labels)




