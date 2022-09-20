#Needed Libraries
library(tidyverse) #tidyverse  
library(rtweet) #twitter API
library(rvest) #web scraping 
library(wordcloud) #word picture 
library(quanteda) #for text analysis 
library(quanteda.corpora) #for corpora asignments and analysis 
library(quanteda.textplots) #for textplots 
library(quanteda.textstats) #for textstats 
library(quanteda.sentiment) #for sentiment... but I don't think we actually use this yet 
library(igraph)

#First we need to go out and get the twitter handles for all US representatives 
url_data <- "https://pressgallery.house.gov/member-data/members-official-twitter-handles" 
url_data %>% 
  read_html() 
css_selector <- "#block-system-main" 
data <- url_data %>%
  read_html() %>%
  html_element(css = css_selector) %>%
  html_table() 

#Check your data... as of 9/20 they had included a row indicating they changed the website. 
#Please, to saveheadaches, check your data,... 

#Creating a data frame of the handles and other info 
data <- data[3:443, 1:5]  
data <- data[-c(92), ]

#Rename variables 
names(data) <- c("first.name", "last.name", "handle", "district", "party")

#Creating a variable for matching dataframes later (twitterhandle) 
data <- data %>%
  mutate(screen_name = gsub("@", "", handle)) 

#Change Seth Moulton's screen name to appropriate one 
data$screen_name[data$screen_name == 'teammoulton'] <- 'sethmoulton' 

tweet_holder <- as.data.frame(NULL) 

#This is the code to run through this. However, you will need to figure out 
#which accounts are not valid... and the program will stop. Thus, 
#you will need to start and stop this a couple of times to figure out
#which ones are good and then break around those. 

#Right now 92 (CharlieCrist) does not work... working on a fix for that 

for(i in 1:440){
  timeline <- get_timeline(data$screen_name[i], n=100) 
  bound <- cbind(timeline, users_data(timeline)[, c("id", "id_str", "name", "screen_name")]) 
  tweet_holder <- rbind(tweet_holder, bound) 
  print(i) 
}


#Because this will take manyhours to do (tweet limits and such), you'll want to 
#Keep your computer on and eventually save the file if you want to play with it later.
#I usually save as a .csv file for ease of use later 

save_as_csv(mtweet_holder, "/XXXXXXXXXX/tweets.csv", prepend_ids=TRUE, na = "", fileEncoding="UTF-8") 

#Then when you come back to it, you can just load it up here and begin coding. 
#Just be cognizant of the tweets you have (date, etc.) as they will change
#as people tweet more and more... 

useme <- read_csv("/users/jessededeyne/desktop/tweeties.csv") 

#I am going to do this analysis with a limited set of tweets, so be sure to change your
#dataframe name as needed 


#First, we need to merge the data and the tweet_holder information together to get party assignments (for later) 
tweet_holder <- tweet_holder[-c(44:45)] 

merged_data <- left_join(tweet_holder, data, by= "screen_name") 

#Creating a Corpus of tweets 
useme <- merged_data 

tweet_words <- corpus(useme)

tokens <-tokens(tweet_words) 

clean_tokens <- tokens(
  tweet_words,
  remove_numbers = TRUE,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_twitter = TRUE,
  remove_url = TRUE,
  remove_hyphens = TRUE,
  include_docvars = TRUE
)

# Create word frequency matrix
data_freq_matrix <- dfm(clean_tokens,
                        tolower=TRUE,
                        stem=TRUE,
                        remove=stopwords('english')) %>%
  dfm_remove(c('rt', 'https', 'amp')) 

# Less sparse / get rid of unique words (ex. powergrid or supernova or vicissitude) 
data_freq_matrix <- dfm_trim(data_freq_matrix,
                             min_docfreq = 0.075,
                             max_docfreq = 0.90,
                             docfreq_type = "prop"
) 


# Visualize frequency
# Let's go back and create a DFM from this subset

features <- textstat_frequency(data_freq_matrix, n=30)

features$feature <- with(features, reorder(feature, -frequency))

ggplot(features, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#Now let's see how the different parties are talking 
rep_words <- subset(useme, party == "R")

rep_tweet_words <- corpus(rep_words)

rep_tokens <-tokens(rep_tweet_words) 

rep_clean_tokens <- tokens(
  rep_tweet_words,
  remove_numbers = TRUE,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_twitter = TRUE,
  remove_url = TRUE,
  remove_hyphens = TRUE,
  include_docvars = TRUE
)

# Create word frequency matrix
rep_data_freq_matrix <- dfm(rep_clean_tokens,
                        tolower=TRUE,
                        stem=TRUE,
                        remove=stopwords('english')) %>%
  dfm_remove(c('rt', 'https', 'amp')) 

# Less sparse / get rid of unique words (ex. powergrid or supernova or vicissitude) 
rep_data_freq_matrix <- dfm_trim(rep_data_freq_matrix,
                             min_docfreq = 0.075,
                             max_docfreq = 0.90,
                             docfreq_type = "prop"
) 


# Visualize frequency
# Let's go back and create a DFM from this subset

rep_features <- textstat_frequency(rep_data_freq_matrix, n=30)

rep_features$feature <- with(rep_features, reorder(feature, -frequency))

ggplot(rep_features, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#And now the democrats 
dem_words <- subset(useme, party == "D") 

dem_tweet_words <- corpus(dem_words)

dem_tokens <-tokens(dem_tweet_words) 

dem_clean_tokens <- tokens(
  dem_tweet_words,
  remove_numbers = TRUE,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_twitter = TRUE,
  remove_url = TRUE,
  remove_hyphens = TRUE,
  include_docvars = TRUE
)

# Create word frequency matrix
dem_data_freq_matrix <- dfm(dem_clean_tokens,
                            tolower=TRUE,
                            stem=TRUE,
                            remove=stopwords('english')) %>%
  dfm_remove(c('rt', 'https', 'amp')) 

# Less sparse / get rid of unique words (ex. powergrid or supernova or vicissitude) 
dem_data_freq_matrix <- dfm_trim(dem_data_freq_matrix,
                                 min_docfreq = 0.075,
                                 max_docfreq = 0.90,
                                 docfreq_type = "prop"
) 


# Visualize frequency
# Let's go back and create a DFM from this subset

dem_features <- textstat_frequency(dem_data_freq_matrix, n=30)

dem_features$feature <- with(dem_features, reorder(feature, -frequency))

ggplot(dem_features, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


