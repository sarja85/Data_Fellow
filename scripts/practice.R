library(usethis)
library(gitcreds)
gitcreds_set()
usethis::create_github_token()
gitcreds_set()
use_github()
gh::gh_whoami()


## Clean versus Messy Data 
# Install packages

install.packages("tidyverse")
#Load package

library(tidyverse)

malaria_dat <- data.frame(
  health.facility.id = rep(c("Hnd7FjkO9","1Yud8rt9s", "PLd3j2ncr","LJn8gzrq2", "G5KNp94mL"), 
                           each = 5),
  fac_name = rep(c("Dembé", "Diawar", "Gueye-Padalal", "Bassine", "Fété  niébé"), each = 5),
  health.facility.type = rep(c("Helath center", "Health center", "Health post", "Health Center","NA"), each = 5),
  date = rep(as.character(
    seq.Date(from = as.Date("2010-01-01"), 
             to = as.Date("2010-05-01"), 
             by = "month")), 5),
  cases = c("59","66","NA","55","54","44","NA","65","70",
            "54","49","57","39","34","45",NA,"74","55",
            "NA","73","50","57","70","44",NA),
  hosp = c("0","0","1","1","0","0","NA","0","0","1","1",
           "NA","1","2","2","3","2","NA","2","1","0","1",
           NA,"0","NA"))

# To have overview of the data, run the summary
summary(malaria_dat)

#  Look at the old column names 
names(malaria_dat)

# Rename column names
malaria_dat <- malaria_dat %>% 
  rename(fac_id = health.facility.id, 
         fac_type = health.facility.type)

#  Look at the new column names 
names(malaria_dat)

table(malaria_dat$date, useNA = "always") #Review format of dates
# Convert date from character to Date

malaria_dat <- malaria_dat %>%
  mutate(date = as.Date(date))

# Confirm the class of malaria_dat$date

class(malaria_dat$date)
#3 values are character strings that read NA, while two are true NAs, shown as <NA>
table(malaria_dat$cases, useNA = "always") 
table(malaria_dat$hosp, useNA = "always")
malaria_dat <- malaria_dat %>%
  #This function takes the specified columns and converts them to the specified class.
  mutate(across(c("cases", "hosp"), as.numeric)) 
#Summary statistics now show for cases and hosp, indicating they're numeric.
summary(malaria_dat) 

malaria_dat <- malaria_dat %>%
  #' This line of code will replace all instances of "NA" with NA (missing) for 
  #' the variable fac_type. Note here "NA" is in quotes indicating that it's a 
  #' character, as opposed to NA (missing data)
  mutate(fac_type2 = na_if(fac_type, "NA")) 

#' Compare the missing values in fac_type2 to those in the original. 
#' dnn will let you label your table so it's easier to interpret.
table(malaria_dat$fac_type, malaria_dat$fac_type2, useNA = "always", 
      dnn = c("fac_type", "fac_type2")) 
#' Review the different spellings of "Health center" to identify those that need 
#' to be corrected.
table(malaria_dat$fac_type2, useNA = "always") 

malaria_dat <- malaria_dat %>%
  #' Use str_detect to identify fac_type2 values that are health centers. 
  #' Use if_else to reset them to "Health center." 
  #' Otherwise, keep fac_type2 as is. 
  #' Note: the quotation marks in str_detect are NOT present next to the pipe.
  mutate(fac_type3 = if_else(str_detect(fac_type2, c("Center|center")), "Health center", fac_type2)) 

table(malaria_dat$fac_type2, malaria_dat$fac_type3, useNA = "always", 
      dnn = c("fac_type2", "fac_type3"))
# Review the spelling of the facility names.
table(malaria_dat$fac_name, useNA = "always") 

#' Specify the characters you'd like to replace, the characters you'd like to 
#' replace them with, and the targeted column. Here we've included multiple 
#' characters, most of which aren't in the strings but theoretically might have been.
malaria_dat <- malaria_dat %>%
  mutate(new_fac_name = chartr("áéèàôî", "aeeaoi", fac_name)) 

table(malaria_dat$new_fac_name)

# Review the case of the facility names.
table(malaria_dat$new_fac_name, useNA = "always") 

# Update new_fac_name case to title.
malaria_dat <- malaria_dat %>%
  mutate(new_fac_name = str_to_title(new_fac_name)) 

# Review the change.
table(malaria_dat$new_fac_name, useNA = "always")

# Exercise 1. Rename fac_name in malaria_dat to name.
malaria_dat <- malaria_dat %>% 
rename(name = fac_name)

table(malaria_dat$fac_type3, useNA = "always")
head(malaria_dat)

#  Exercise 2. Now, for practice, change the final three columns to be variables 
# of the character class using mutate() with across().

malaria_dat <- malaria_dat %>%
  mutate(across(c( "fac_type2", "fac_type3", "new_fac_name"), as.character))
# To verify the changes 
class(malaria_dat)
str(malaria_dat)

# Exercise 3. In this demo, we started with a data set that had instances of “NA” written as text in several columns. 
# Ideally, we would like to change these to missing without having to use 
# the na_if() on each column. Rerun the first chunk in the demo so 
# that you’ve restored the messy data set. Now, try using mutate() and across() to change 
# “NA” text to missing values for all columns.

malaria_dat <- malaria_dat %>% 
 mutate(across(c ("fac_type") , na_if("NA")))

 # Exercise 4. Change fac_type to title case.Next, change it to sentence case. 
# Hint: Type stringr::str_to to see to different case functions in the stringr package.         
   
malaria_dat <- malaria_dat %>% 
  mutate(fac_type =str_to_title(fac_type))
table(malaria_dat$fac_type)

malaria_dat <- malaria_dat %>% 
  mutate(fac_type =str_to_sentence(fac_type))
table(malaria_dat$fac_type)

# Exercise 5. Write one line of code to determine if your health facilities are 
# either “Health centers” or “Health posts.”

table(malaria_dat$fac_type)

# Exercise 6. Use str_detect() to replace the double spaces 
# in the fac_name column with single spaces.

malaria_dat <- malaria_dat %>% 
  str_detect (malaria_dat$new_fac_name, "single space" = negate(FALSE)

             