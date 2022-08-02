library(tidyr)
library(tidyverse)
library(janitor)
library(readxl)
candy_15 <- read_excel("raw_data/candy_ranking_data/boing-boing-candy-2015.xlsx")
candy_16 <- read_excel("raw_data/candy_ranking_data/boing-boing-candy-2016.xlsx")
candy_17 <- read_excel("raw_data/candy_ranking_data/boing-boing-candy-2017.xlsx")
#I tried to check their cols here and they are too messy. 
#I am going to tidy them one at a time before join them together 

names(candy_15)
candy_15 <- candy_15 %>% 
  pivot_longer(cols = starts_with("["),
             names_to = "sweets",
             values_to = "emotion") %>% 
  mutate(sweets = str_sub(sweets, 2, -2))
#it's still very messy here but I am not sure if we will look into 
#any of those questions yet, so i will leave it here and go back 
#for more cleaning if needed 
names(candy_16)
candy_16 <- candy_16 %>% 
  pivot_longer(cols = starts_with("["),
               names_to = "sweets",
               values_to = "emotion") %>% 
  mutate(sweets = str_sub(sweets, 2, -2))

names(candy_17)
candy_17 <- candy_17 %>% 
  pivot_longer(cols = starts_with("Q6"),
               names_to = "sweets",
               values_to = "emotion")

candy_17 <- candy_17 %>%
  mutate(sweets = str_sub(sweets, start = 6))

#It looks very messy. and a lot of NA, so I don't know what each col is

candy_17 <- candy_17 %>% 
  pivot_longer(cols = starts_with("Q12"),
               names_to = "media",
               values_to = "ans_of_media")%>%
  mutate(media = str_sub(media, start = 6))

#too many NA here, I don't know what goes in here,



#From here, I will start to do some deep cleaning and getting 
#ready for joining and analysis 
names(candy_15)
candy_15 <- candy_15 %>% 
  rename(`age` = `How old are you?`,
         `going_out` = `Are you going actually going trick or treating yourself?`)
candy_15 <- candy_15 %>% 
  select(cols = -c(4:29))


names(candy_16)
candy_16 <- candy_16 %>% 
  rename(`age` = `How old are you?`,
         `going_out` = `Are you going actually going trick or treating yourself?`,
         `gender` = `Your gender:`,
         `country` = `Which country do you live in?`,
         `s_p_c` = `Which state, province, county do you live in?`)

candy_16 <- candy_16 %>% 
 select(cols = -c(7:22))

candy_16 %>% 
  distinct(country) %>% 
  pull()
#very messy and I will put all the too crazy ones to NA such as 45.0 ...
list_us <- c("Unites States" ,'United Staes', "Unhinged States",
             "North Carolina" ,"Unied States" , "U S"  ,"United State" ,
             "New York" ,"Trumpistan"  ,"United Sates"  ,"California"  ,
             "United Stated"  ,"Ahem....Amerca" ,"New Jersey" ,"United Ststes",
             "United Statss" ,"Alaska"  ,"United Statea" ,"United Ststes",
             "Unite States","Pittsburgh","Murica","Murrika","'Merica",
             "The Yoo Ess Of Aaayyyyyy","Sub-Canadian North America... 'Merica",
             "Merica","United Stetes","Units States")
list_uk <- c("U.K." ,"Scotland","Endland", "U.k.","United Kindom")
list_can <-c("Can", "Canae", "Soviet Canuckistan")
list_NA <- c("Europe","Earth","35","46", "A",  "Insanity Lately","32","1",
             "I Don't Know Anymore","Fear And Loathing", "45", "Narnia",
             "Atlantis","Cascadia","Ud","47.0", "54.0","51.0","God's Country",  
             "One Of The Best Ones","Denial","See Above","30.0",
             "A Tropical Island South Of The Equator","Neverland","This One", 
             "There Isn't One For Old Men","Somewhere","45.0","30.0",
             "The Republic Of Cascadia","Not The Usa Or Canada", "44.0", "Eua")

candy_16 <- candy_16 %>% 
  mutate(country = str_to_title(country)) %>% 
  mutate(country = case_when(str_detect(country, '(?i)usa') ~ "US",
                             str_detect(country, '(?i)america') ~ "US",
                             str_detect(country, '(?i)us') ~ "US",
                             str_detect(country, '[Uu ].[Ss].') ~ "US",
                             str_detect(country, '(?i)united states') ~ "US",
                             str_detect(country, '(?i)uk') ~"UK",
                             str_detect(country, '(?i)united kingdom') ~ "UK",
                             str_detect(country, '(?i)england') ~ "UK",
                             str_detect(country, '(?i)canada') ~ "Canada",
                             country %in% list_uk ~ "UK",
                             country %in% list_us ~"US",
                             country %in% list_can ~"Canada",
                             country %in% list_NA ~ NA_character_,
                             country =="Uae" ~"UAE",
                             TRUE ~ country))

candy_16 %>% 
  distinct(country) %>% 
  pull()
#tidying candy_17
names(candy_17)
candy_17 <- candy_17 %>% 
  rename(`age` = `Q3: AGE`,
         `going_out` = `Q1: GOING OUT?`,
         `gender` = `Q2: GENDER`,
         `country` = `Q4: COUNTRY`,
         `s_p_c` = `Q5: STATE, PROVINCE, COUNTY, ETC`)

candy_17 <- candy_17 %>% 
  select(cols = -c(7:13))
               


candy_17 <- candy_17 %>%
  mutate(country = str_to_title(country)) %>% 
  mutate(country = case_when(str_detect(country, '(?i)usa') ~ "US",
                             str_detect(country, '(?i)america') ~ "US",
                             str_detect(country, '(?i)us') ~ "US",
                             str_detect(country, '[Uu ].[Ss].') ~ "US",
                             str_detect(country, '(?i)united states') ~ "US",
                             str_detect(country, '(?i)uk') ~"UK",
                             str_detect(country, '(?i)united kingdom') ~ "UK",
                             str_detect(country, '(?i)england') ~ "UK",
                             str_detect(country, '(?i)canada') ~ "Canada",
                             country %in% list_uk ~ "UK",
                             country %in% list_us ~"US",
                             country %in% list_can ~"Canada",
                             country %in% list_NA ~ NA_character_,
                             country =="Uae" ~"UAE",
                             TRUE ~ country))
 
#I only kept the col that are useful as the data frame is too big 
#I am only keeping the years in time stamp 
candy_15_s <- candy_15 %>% 
  mutate(Timestamp = "2015") %>% 
  mutate(age = as.integer(age))
candy_16_s <- candy_16 %>% 
  mutate(Timestamp = "2016") %>% 
  mutate(age = as.integer(age))
candy_17_s <- candy_17 %>% 
  mutate(Timestamp = "2017") %>% 
  mutate(age = as.integer(age))

candy_all <- candy_15_s %>% 
  full_join(candy_16_s) %>% 
  full_join(candy_17_s) %>% janitor::clean_names()


candy_all <- candy_all %>%
 mutate(age = case_when(age >= 120 ~ NA_integer_,
                        age <= 0 ~ NA_integer_,
                        TRUE ~ age)) %>% 
  select(1:7)
  
  
#I went back from analysis as my age was messy. 
#I will treat age over 120 and 0 as NA.
candy_all %>% 
  distinct(age) %>% 
  pull()

write_csv(candy_all, "clean_data/clean_candy")
