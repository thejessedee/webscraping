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
data <- data[2:443, 1:5]  

#Rename variables 
names(data) <- c("first.name", "last.name", "handle", "district", "party")

#Creating a variable for matching dataframes later (twitterhandle) 
data <- data %>%
  mutate(screen_name = gsub("@", "", handle)) 

tweet_holder <- as.data.frame(NULL) 

#This is the code to run through this. However, you will need to figure out 
#which accounts are not valid... and the program will stop. Thus, 
#you will need to start and stop this a couple of times to figure out
#which ones are good and then break around those. 


for(i in 1:180){
  timeline <- get_timeline(data$screen_name[i], n=10) 
  bound <- cbind(timeline, users_data(timeline)[, c("id", "id_str", "name", "screen_name")]) 
  tweet_holder <- rbind(tweet_holder, bound) 
  print(i) 
}

for(i in 181:360){
  timeline <- get_timeline(data$screen_name[i], n=10) 
  bound <- cbind(timeline, users_data(timeline)[, c("id", "id_str", "name", "screen_name")]) 
  tweet_holder <- rbind(tweet_holder, bound) 
  print(i) 
}

for(i in 361:442){
  timeline <- get_timeline(data$screen_name[i], n=10) 
  bound <- cbind(timeline, users_data(timeline)[, c("id", "id_str", "name", "screen_name")]) 
  tweet_holder <- rbind(tweet_holder, bound) 
  print(i) 
}



