library(drc)
library(quantreg)
library(mcr)
library(dplyr)
library(broom)
library(lubridate)
library(tidyverse)
library(ggplot2)

ipam_convert <- function(data) {
  data %>% select_if(~ !any(is.na(.))) %>%
    pivot_longer(cols = starts_with("f") | starts_with("y")) %>%
    separate(name, into = c("var", "aoi"), sep = "(?<=[A-Za-z_])(?=[0-9])")
}

pamfiles <- list.files(path = "PAM_data/csv", pattern = "*.csv", recursive = TRUE, full.names = TRUE)
meta1<-read.csv(file ="fluc_bryops_metadata.csv")

meta1<-meta1%>%
  #mutate(date = as_date(date, format = "%d.%m.%y"))%>%# This line makes the date column turn NA
  mutate(aoi = as.character(aoi))

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
  
pam1<-pam1 %>%
  nest(-file_id, -date)%>%
  mutate(data2 = map(data, ipam_convert)) %>%
  unnest(data2) %>%
  group_by(file_id, date) %>%
  select(file_id, date, time, aoi, var, value)

pam_met<-pam1%>%
  full_join(meta1)

f1<-ggplot(pam_met, aes(x = date , y = value, color = conc))+
  geom_point()
f1
#pam_met<-(pam1, meta1, )

#caseWhen (date = 1017, aoi < 3, equals whatever color is necessary)

