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

#Let's remove the '@' symbol from the twitter handles so we can do more 
data <- data %>%
  mutate(handle = gsub("@", "", handle)) 

#Creating a new dataframe that we will use in the next step 
tweet_holder <- as.data.frame(NULL) 

#Running all 442 twitter handles through our program and getting 20 tweets for each 
#Presently you have to run this through three times in order to obtain all the tweets. 
#However, you have to do this at one hour intervals. Because while you can get a tweets fast, 
#Twitter only allows you to get 180 pulls per hour (iirc). So, you can only do 1-180, 
#but you may be able to pull more tweets. Check the rules as they change every now and then. 

#1-180
for(i in 1:180){ 
  handles <- twitter_url_remover(data$handle[i]) 
  tweets <- get_timeline(handles, n=100) 
  tweet_holder <- rbind(tweet_holder, tweets) 
  print(i)
}

#181-360
for(i in 181:360){ 
  handles <- twitter_url_remover(data$handle[i]) 
  tweets <- get_timeline(handles, n=100) 
  tweet_holder <- rbind(tweet_holder, tweets) 
  print(i)
}

#361-442
for(i in 361:442){ 
  handles <- twitter_url_remover(data$handle[i]) 
  tweets <- get_timeline(handles, n=100) 
  tweet_holder <- rbind(tweet_holder, tweets) 
  print(i)
}


