
library(readxl)
library(sqldf)
library(dplyr)
library(tidyverse)
library(data.table)

data_january <- read_csv("C:/Users/Vardas/Desktop/Magistrinis/Data/january.csv")
data_february <- read_csv("C:/Users/Vardas/Desktop/Magistrinis/Data/february.csv")
data_march <- read_csv("C:/Users/Vardas/Desktop/Magistrinis/Data/march.csv")
data_april <- read_csv("C:/Users/Vardas/Desktop/Magistrinis/Data/april.csv")
data_may <- read_csv("C:/Users/Vardas/Desktop/Magistrinis/Data/may.csv")
data_june <- read_csv("C:/Users/Vardas/Desktop/Magistrinis/Data/june.csv")
data_july <- read_csv("C:/Users/Vardas/Desktop/Magistrinis/Data/july.csv")
data_august <- read_csv("C:/Users/Vardas/Desktop/Magistrinis/Data/august.csv")
data_september <- read_csv("C:/Users/Vardas/Desktop/Magistrinis/Data/september.csv")
data_october <- read_csv("C:/Users/Vardas/Desktop/Magistrinis/Data/october.csv")
data_november <- read_csv("C:/Users/Vardas/Desktop/Magistrinis/Data/november.csv")
data_december <- read_csv("C:/Users/Vardas/Desktop/Magistrinis/Data/december.csv")

offers <- read_delim("C:/Users/Vardas/Desktop/Magistrinis/Data/cdp_postpaid_nbo_offer_bank_prep.csv", 
                                               delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(offers)
names(data_january) <- gsub("data_prep_testing_kornelija2.", "", names(data_january), fixed = TRUE)
names(data_february) <- gsub("data_prep_testing_kornelija2.", "", names(data_february), fixed = TRUE)
names(data_march) <- gsub("data_prep_testing_kornelija2.", "", names(data_march), fixed = TRUE)
names(data_april) <- gsub("data_prep_testing_kornelija2.", "", names(data_april), fixed = TRUE)
names(data_may) <- gsub("data_prep_testing_kornelija2.", "", names(data_may), fixed = TRUE)
names(data_june) <- gsub("data_prep_testing_kornelija2.", "", names(data_june), fixed = TRUE)
names(data_july) <- gsub("data_prep_testing_kornelija2.", "", names(data_july), fixed = TRUE)
names(data_august) <- gsub("data_prep_testing_kornelija2.", "", names(data_august), fixed = TRUE)
names(data_september) <- gsub("data_prep_testing_kornelija2.", "", names(data_september), fixed = TRUE)
names(data_october) <- gsub("data_prep_testing_kornelija2.", "", names(data_october), fixed = TRUE)
names(data_november) <- gsub("data_prep_testing_kornelija2.", "", names(data_november), fixed = TRUE)
names(data_december) <- gsub("data_prep_testing_kornelija2.", "", names(data_december), fixed = TRUE)

data_january <- data_january %>% filter(year < 2023)
data_february <- data_february %>% filter(year < 2023)
data_march <- data_march %>% filter(year < 2023)
data_april <- data_april %>% filter(year < 2023)
data_may <- data_may %>% filter(year < 2023)

data <- data_january %>% union(data_february) %>%
  union(data_march) %>% union(data_april) %>% union(data_may) %>%
  union(data_june) %>% union(data_july) %>% union(data_august) %>% 
  union(data_september) %>% union(data_october) %>% union(data_november) %>%
  union(data_december)

# assign device type for each user
data <- data %>% group_by(user_id) %>% 
  mutate(SP = sum(device_type == "SMARTPHONE")) %>%
  mutate(FP = sum(device_type == "FEATURE PHONE")) %>%
  select(-device_type) %>%
  mutate(device_type = ifelse(FP >= SP, "FEATURE PHONE", "SMARTPHONE"))

# users that have information for all 36 months
users_base <- data %>% group_by(user_id) %>% summarise(n=n()) %>% filter(n == 36)

data <- data %>% inner_join(users_base, "user_id")

# assign device type for each user
data <- data %>% group_by(user_id) %>% 
  mutate(SP = sum(device_type == "SMARTPHONE")) %>%
  mutate(FP = sum(device_type == "FEATURE PHONE")) %>%
  select(-device_type) %>%
  mutate(device_type = ifelse(FP >= SP, "FEATURE PHONE", "SMARTPHONE"))
  
data %>% group_by(device_type) %>% summarise(n=n())

# enumerate months
data_2020 <- data %>% filter(year == 2020)
data_2021 <- data %>% filter(year == 2021) %>%
  mutate(month = ifelse(month==1,13,
                        ifelse(month==2,14,
                               ifelse(month==3,15,
                                      ifelse(month==4,16,
                                             ifelse(month==5,17,
                                                    ifelse(month==6,18,
                                                           ifelse(month==7,19,
                                                                  ifelse(month==8,20,
                                                                         ifelse(month==9,21,
                                                                                ifelse(month==10,22,
                                                                                       ifelse(month==11,23,24))))))))))))
data_2022 <- data %>% filter(year == 2022) %>%
  mutate(month = ifelse(month==1,25,
                        ifelse(month==2,26,
                               ifelse(month==3,27,
                                      ifelse(month==4,28,
                                             ifelse(month==5,29,
                                                    ifelse(month==6,30,
                                                           ifelse(month==7,31,
                                                                  ifelse(month==8,32,
                                                                         ifelse(month==9,33,
                                                                                ifelse(month==10,34,
                                                                                       ifelse(month==11,35,36))))))))))))
monthly_data <- data_2020 %>% union(data_2021) %>% union(data_2022)

monthly_data$data_gb_used <- monthly_data$data_mb_used / 1000

# calculate min gb used per user
min_gb_per_user <- monthly_data %>% group_by(user_id) %>% summarise(min_gb = min(data_gb_used))
# calculate min voice per user
min_voice_per_user <- monthly_data %>% group_by(user_id) %>%
  summarise(min_voice = min(voice_outgoing_duration_in_minutes))


# how many with at least 1gb per month?
nrow(min_gb_per_user %>% filter(min_gb >= 1)) #1232
# how many with at least 100 min per month?
nrow(min_voice_per_user %>% filter(min_voice >= 100)) #1231

# #################### USERS WITH MIN 1GB AND MIN 100 MINUTES ####################

at_least_1gb_per_month <- min_gb_per_user %>% filter(min_gb >= 1)
at_least_1gb_per_month_df <- monthly_data %>%
  inner_join(at_least_1gb_per_month, by = "user_id")
at_least_100_voice_per_month <- min_voice_per_user %>% filter(min_voice >= 100)
non_0_usage_per_month_df <- monthly_data %>%
  inner_join(at_least_1gb_per_month, by = "user_id") %>%
  inner_join(at_least_100_voice_per_month, by = "user_id")

# OFFERS PREP
offers_prep <- offers %>% subset(select = c("rateplan_id", "voice", "data")) %>%
  mutate(data = ifelse(data == 1000, 999999, data)) %>%
  filter(data >= 1) %>% distinct() %>% filter(rateplan_id != "790" & rateplan_id != "375" & rateplan_id != "407")


library(readr)
voice_offers_prep <- read_delim("C:/Users/Vardas/Desktop/Magistrinis/Data/voice_offers_prep.csv",
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
data_offers_prep <- read_delim("C:/Users/Vardas/Desktop/Magistrinis/Data/data_offers_prep.csv",
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

voice_offers_prep_wo_unlim <- read_delim("C:/Users/Vardas/Desktop/Magistrinis/Data/voice_offers_prep_wo_unlimited.csv",
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
data_offers_prep_wo_unlim <- read_delim("C:/Users/Vardas/Desktop/Magistrinis/Data/data_offers_prep_wo_unlimited.csv",
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)

# fiktyvus mano sudaryti oferiai nuo 100 iki 2000 min by 100
fic_voice_offers_prep_wo_unlimited <- read_delim("C:/Users/Vardas/Desktop/Magistrinis/Data/fic_voice_offers_prep_wo_unlimited.csv",
                                         delim = ";", escape_double = FALSE, trim_ws = TRUE)




