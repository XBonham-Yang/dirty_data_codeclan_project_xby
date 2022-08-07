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

#candy_17 <- candy_17 %>% 
  #pivot_longer(cols = starts_with("Q12"),
              # names_to = "media",
               #values_to = "ans_of_media")%>%
  #mutate(media = str_sub(media, start = 6))

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
  mutate(country = str_to_title(country)) %>% 
  distinct(country) %>% 
  pull()
#very messy and I will put all the too crazy ones to NA such as 45.0 ...
list_us <- c("Usa", "Us" ,"United States Of America","United States" ,
             "Unites States" ,"Ussa","U.s.a." ,
             'United Staes', "Unhinged States",
             "North Carolina" ,"Unied States" , "U S"  ,"United State" ,
             "New York" ,"Trumpistan"  ,"United Sates"  ,"California"  ,
             "United Stated"  ,"Ahem....Amerca" ,"New Jersey" ,"United Ststes",
             "United Statss" ,"Alaska"  ,"United Statea" ,"United Ststes",
             "Unite States","Pittsburgh","Murica","Murrika","'Merica",
             "The Yoo Ess Of Aaayyyyyy","Sub-Canadian North America... 'Merica",
             "Merica","United Stetes","Units States","Usa!",
             "Usa (I Think But It's An Election Year So Who Can Really Tell)",
             "U.s.","America","Usa Usa Usa","Usa!!!!!!","Usa! Usa!",
             "Usa Usa Usa Usa","United  States Of America","United State",
             "Usa! Usa! Usa!","The Best One - Usa","Usa Usa Usa!!!!","Usausausa",
             "Us Of A","The United States","The United States Of America",
             "Usa? Hard To Tell Anymore..","Usas",
             "I Pretend To Be From Canada, But I Am Really From The United States.",
             "Usaa", "U S A","Usa Usa Usa!!!!")
list_uk <- c("Uk" ,"U.K." ,"Scotland","Endland", "U.k.","United Kindom","England",
             "United Kingdom","United Kingdom","Uk")
list_can <-c("Can", "Canae", "Soviet Canuckistan","Canada")
list_NA <- c("Europe","Earth","35","46", "A",  "Insanity Lately","32","1",
             "I Don't Know Anymore","Fear And Loathing", "45", "Narnia",
             "Atlantis","Cascadia","Ud","47.0", "54.0","51.0","God's Country",  
             "One Of The Best Ones","Denial","See Above","30.0",
             "A Tropical Island South Of The Equator","Neverland","This One", 
             "There Isn't One For Old Men","Somewhere","45.0","30.0",
             "The Republic Of Cascadia","Not The Usa Or Canada", "44.0", "Eua",
             "N. America","Subscribe To Dm4uz3 On Youtube" )
list_sk <- c("Korea","South Korea")
list_nl <- c("The Netherlands","Netherlands")
list_sp <-c("España", "Spain")

candy_16 <-candy_16 %>% 
  mutate(country = str_to_title(country)) %>% 
  mutate(country = case_when(country %in% list_uk ~ "UK",
                             country %in% list_us ~ "US",
                             country %in% list_can ~"Canada",
                             country %in% list_NA ~ NA_character_,
                             country %in% list_nl ~ 'Netherlands',
                             country %in% list_sk ~ "South Korea",
                             country %in% list_sp ~ "Spain",
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
  mutate(country = case_when(country %in% list_uk ~ "UK",
                             country %in% list_us ~"US",
                             country %in% list_can ~"Canada",
                             country %in% list_NA ~ NA_character_,
                             country %in% list_nl ~ 'Netherlands',
                             country %in% list_sk ~ "South Korea",
                             country %in% list_sp ~ "Spain",
                             country =="Uae" ~"UAE",
                             TRUE ~ country))
candy_17 %>% 
  distinct(country) %>% 
  pull()
#I only kept the col that are useful as the data frame is too big 
#I am only keeping the years in time stamp 
candy_15_s <- candy_15 %>% 
  mutate(Timestamp = "2015") %>% 
  mutate(age = as.integer(age))
candy_16_s <- candy_16 %>% 
  mutate(Timestamp = "2016") %>% 
  mutate(age = as.integer(age)) %>% 
  select(-6)
candy_17_s <- candy_17 %>% 
  mutate(Timestamp = "2017") %>% 
  mutate(age = as.integer(age)) %>% 
  select(c(2,3,4,5,11,12,13))

#checks on each columns--age
candy_15_s <- candy_15_s %>% 
  mutate(age = case_when(age >= 120 ~ NA_integer_,
                         age <= 0 ~ NA_integer_,
                         TRUE ~ age))


candy_16_s <- candy_16_s %>% 
  mutate(age = case_when(age >= 120 ~ NA_integer_,
                         age <= 0 ~ NA_integer_,
                         TRUE ~ age))

candy_17_s <- candy_17_s %>% 
  mutate(age = case_when(age >= 120 ~ NA_integer_,
                         age <= 0 ~ NA_integer_,
                         TRUE ~ age))

candy_17_s %>% 
  select(age) %>% 
  distinct() %>% 
  pull()
#check on going_out
candy_17_s %>% 
  select(going_out) %>% 
  distinct() %>% 
  pull()
#check on emotions 
candy_17_s %>% 
  select(emotion) %>% 
  distinct() %>% 
  pull()
#check on gender
candy_17_s %>% 
  select(gender) %>% 
  distinct() %>% 
  pull()
#other and I'd rather not say also allowed 
candy_17_s %>% 
  select(sweets) %>% 
  distinct() %>% 
  pull()
#2015_95 sweets
#2016-101 sweets
#2017_103 sweets 
#didn't realise that this is dirty too.
#I am not very familiar with candy names, this might not be
#100% correct
candy_na <- c("Cash, or other forms of legal tender","Generic Brand Acetaminophen",
              "Glow sticks", "Broken glow stick",
              "Creepy Religious comics/Chick Tracts",                                       
              "Healthy Fruit","Hugs (actual physical hugs)", "Kale smoothie",
              "Minibags of chips", "JoyJoy (Mit Iodine)", "JoyJoy (Mit Iodine!)",
              "Pencils", 
              "Peterson Brand Sidewalk Chalk",  "Vicodin", "White Bread", 
              "Whole Wheat anything",  
              "Person of Interest Season 3 DVD Box Set (not including Disc 4 with hilarious outtakes)",
              "Vials of pure high fructose corn syrup, for main-lining into your vein",
              "York Peppermint Patties] Ignor","Chardonnay", "Generic Brand Acetaminophen",
              "Abstained from M&M'ing.","Real Housewives of Orange County Season 9 Blue-Ray",
              "Spotted Dick", "Bonkers (the board game)" ,"Sea-salt flavored stuff, probably chocolate, since this is the \"it\" flavor of the year"
              )
candy_mm <- c("Regular M&Ms","Blue M&M's", "Red M&M's", "Green Party M&M's",
              "Independent M&M's" )


candy_mj <- c("Mary Janes","Anonymous brown globs that come in black and orange wrappers",
              "Anonymous brown globs that come in black and orange wrappers\t(a.k.a. Mary Janes)")

candy_l <- c("Licorice", "Licorice (yes black)")
candy_b <- c("Bonkers", "Bonkers (the candy)" )
candy_st <- c("Sweetums (a friend to diabetes)","Sweetums" )
candy_br <- c("Box'o'Raisins","Box’o’ Raisins")
candy_15_s <- candy_15_s %>%
  mutate(sweets = case_when(sweets %in% candy_na ~ NA_character_,
                             sweets %in% candy_mm ~"Regular M&M",
                             sweets %in% candy_mj ~ "Mary Janes",
                            sweets %in% candy_l ~ "Licorice",
                            sweets %in% candy_b ~ "Bonkers",
                            sweets %in% candy_st ~ 'Sweetums',
                            sweets %in% candy_br ~ "Box'o'Raisins",
                             TRUE ~ sweets)) %>% 
  arrange(sweets)

candy_16_s <- candy_16_s %>%
  mutate(sweets = case_when(sweets %in% candy_na ~ NA_character_,
                            sweets %in% candy_mm ~"Regular M&M",
                            sweets %in% candy_mj ~ "Mary Janes",
                            sweets %in% candy_l ~ "Licorice",
                            sweets %in% candy_b ~ "Bonkers",
                            sweets %in% candy_st ~ 'Sweetums',
                            sweets %in% candy_br ~ "Box'o'Raisins",
                            TRUE ~ sweets)) 


candy_17_s <- candy_17_s %>%
  mutate(sweets = case_when(sweets %in% candy_na ~ NA_character_,
                            sweets %in% candy_mm ~"Regular M&M",
                            sweets %in% candy_mj ~ "Mary Janes",
                            sweets %in% candy_l ~ "Licorice",
                            sweets %in% candy_b ~ "Bonkers",
                            sweets %in% candy_st ~ 'Sweetums',
                            sweets %in% candy_br ~ "Box'o'Raisins",
                            TRUE ~ sweets)) 
candy_15_s %>% 
  select(sweets) %>% 
  distinct() %>% 
  pull()


candy_all <- candy_15_s %>% 
  full_join(candy_16_s) %>% 
  full_join(candy_17_s) %>%
  arrange(sweets) %>% 
  janitor::clean_names()

  
candy_all %>% 
  select(sweets) %>% 
  distinct() %>% 
  pull()


#write_csv(candy_all, "clean_data/clean_candy")

