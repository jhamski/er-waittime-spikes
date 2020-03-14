
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
       total_minutes =  str_extract(scraped_waittimes, regex("[0-9]+")),
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




