library(drc)
library(quantreg)
library(mcr)
library(dplyr)
library(broom)
library(lubridate)
library(tidyverse)

ipam_convert <- function(data) {
  data %>% select_if(~ !any(is.na(.))) %>%
    pivot_longer(cols = starts_with("f") | starts_with("y")) %>%
    separate(name, into = c("var", "aoi"), sep = "(?<=[A-Za-z_])(?=[0-9])")
}

pamfiles <- list.files(path = "PAM_data/csv", pattern = "*.csv", recursive = TRUE, full.names = TRUE)


# Import data from each file
pam1 <- pamfiles %>%
  map_dfr(read_delim, delim = ";", .id = "file_id") %>%
  janitor::clean_names() %>%
  mutate(file_id = basename(pamfiles[as.numeric(file_id)]),
         date = as_date(date, format = "%d.%m.%y"))
# # For files that have multiple sat pulses -- keep the last one only
pam1 <- pam1 %>%
  group_by(file_id, date) %>%
  filter(no == max(no)) %>%
  ungroup()
# For each source file, convert to long form data with F, FM, and YII for each AOI
pam1 <- pam1 %>%
  nest(-file_id, -date) %>%
  mutate(data2 = map(data, ipam_convert)) %>%
  unnest(data2) %>%
  group_by(file_id, date) %>%
  select(file_id, date, time, aoi, var, value)

rm(test)

Blue<-pam1%>%
  group_by(file_id)%>%
  filter(var == "y_ii_")%>%
  filter(str_detect(file_id, "Blue"), ignore.case = TRUE)%>%
  mutate(avg = mean(value))

CtrlTreatment<-pam1%>%
  group_by(file_id)%>%
  filter(var == "y_ii_")%>%
  filter(str_detect(file_id, "red"), ignore.case = TRUE)%>%
  mutate(avg = mean(value))

Pink<-pam1%>%
  group_by(file_id)%>%
  filter(var == "y_ii_")%>%
  filter(str_detect(file_id, "Pink"), ignore.case = TRUE)%>%
  mutate(avg = mean(value))


Yellow<-pam1%>%
  group_by(file_id)%>%
  filter(var == "y_ii_")%>%
  filter(str_detect(file_id, "Yellow"), ignore.case = TRUE)%>%
  mutate(avg = mean(value))

Purple<-pam1%>%
  group_by(file_id)%>%
  filter(var == "y_ii_")%>%
  filter(str_detect(file_id, "Purple"), ignore.case = TRUE)%>%
  mutate(avg = mean(value))

Green<-pam1%>%
  group_by(file_id)%>%
  filter(var == "y_ii_")%>%
  filter(str_detect(file_id, "Green"), ignore.case = TRUE)%>%
  mutate(avg = mean(value))
