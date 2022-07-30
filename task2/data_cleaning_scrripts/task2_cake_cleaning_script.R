library(tidyverse)
library(tidyr)
library(janitor)

cake <- read_csv("raw_data/cake/cake-ingredients-1961.csv")
ingredient <- read_csv("raw_data/cake/cake_ingredient_code.csv")

cake <- cake %>% 
  pivot_longer(cols = c(2:35), 
               names_to = "ing_code",
               values_to = "amount")
#put all the ingredient code in one col, getting ready to 
#two tables 

cake <- cake %>% 
  left_join(ingredient, by = c("ing_code" = "code")) %>% 
  relocate(amount, .after = measure)
#I found relocate function  online 
#It now has a col of cake names, ingredient code, ingredient, 
#measure and amount. 
#I didn't remove the code,  just in case I  want to use it later
#I  am not very happy with the  fact that each cakes
#are taking so many rows.  But make  them into column will be more messy

cake <- cake %>% 
  clean_names() %>% 
  mutate(across(where(is.character), ~str_to_lower(.x)))
#final touch, all names are now follow snake style and all 
#character observations has lower case 

write_csv(cake, "clean_data/clean_cake")

  