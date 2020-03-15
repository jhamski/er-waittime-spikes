# global.R

library(tidyverse)
library(shinydashboard)
library(DT)
library(gsheet)


er_wait_times <- read_csv("waittimes_record.csv")

er_facility_count <- unique(er_wait_times$site_labels) %>% length()

realtime_hospital_list <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1MCiyFx-kC2Et88taXHt49x4W0PTJboJ93A-SmZ9Ed4Q/edit?usp=sharing")

er_wait_times <- er_wait_times %>% 
  left_join(realtime_hospital_list, by = "site_labels") %>% 
  filter(!is.na(State))


