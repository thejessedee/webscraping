#Needed Libraries
library(tidyverse) #tidyverse  
library(rtweet) #twitter API
library(rvest) #web scraping 

#First we need to go out and get the twitter handles for all US representatives 
url_data <- "https://pressgallery.house.gov/member-data/members-official-twitter-handles" 
url_data %>% 
  read_html() 
css_selector <- "#block-system-main" 
data <- url_data %>%
  read_html() %>%
  html_element(css = css_selector) %>%
  html_table() 

#Creating a data frame of the handles and other info 
newdata <- data[2:443, 1:5]  

#Rename variables 
names(newdata) <- c("first.name", "last.name", "handle", "district", "party")

#Let's remove the '@' symbol from the twitter handles so we can do more 
newdata <- newdata %>%
  mutate(handle = gsub("@", "", handle)) 



