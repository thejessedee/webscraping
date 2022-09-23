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
library(stm) 

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

#save_as_csv(mtweet_holder, "/XXXXXXXXXX/tweets.csv", prepend_ids=TRUE, na = "", fileEncoding="UTF-8") 

#Then when you come back to it, you can just load it up here and begin coding. 
#Just be cognizant of the tweets you have (date, etc.) as they will change
#as people tweet more and more... 

#useme <- read_csv("/users/jessededeyne/desktop/tweeties.csv") 

#I am going to do this analysis with a limited set of tweets, so be sure to change your
#dataframe name as needed 


#First, we need to merge the data and the tweet_holder information together to get party assignments (for later) 
tweet_holder <- tweet_holder[-c(44:45)] 

merged_data <- left_join(tweet_holder, data, by= "screen_name") 


#####Creating a Corpus of tweets for all tweets##### 
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


#####Now let's see how the different parties are talking##### 
#Republicans
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


#And now the Democrats 
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



#####Let's do some topic modeling now... #####
#Let's start fresh with our data set 


#We need to remove columns that have all the same answer. 
#Keep those columns you need. 
merged_data1 <- merged_data[-c(5:43)]
merged_data1 <- subset(merged_data1, party=='D' | party=='R')

#####Evang Sample stm#####
#Cleaning the Data for stm 
processed_merged_data1 <- textProcessor(merged_data1$full_text, metadata=merged_data1)

out_merged_data1 <- prepDocuments(processed_merged_data1$documents, processed_merged_data1$vocab, processed_merged_data1$meta) 

docs_merged_data1 <- out_merged_data1$documents

vocab_merged_data1 <- out_merged_data1$vocab

meta_merged_data1 <- out_merged_data1$meta

#Let's find the optimal K, this will take a long time, be prepared
tweet_merged_data1 <- searchK(out_merged_data1$documents, out_merged_data1$vocab, K = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                                  data=out_merged_data1$meta)

tweet_merged_data1 #looking for highest exclus and highest (lowest -) semcoh 

#The best fit is 100 topics 
numberoftopics_merged_data1 <- stm(out_merged_data1$documents, out_merged_data1$vocab, 
                                 K=20, 
                                 prevalence = ~ party, 
                                 max.em.its=40, 
                                 data = out_merged_data1$meta, 
                                 seed=09222022) 

#Let's focus on FREX
#words used in topics
labelTopics(numberoftopics_merged_data1, c(1:20), frexweight = 0.5, n = 15) 

numberoftopics_labels_merged_data1 <- labelTopics(numberoftopics_merged_data1, n = 10)
numberoftopics_topwords_merged_data1 <- data.frame("features" = t(numberoftopics_labels_merged_data1$frex))
colnames(numberoftopics_topwords_merged_data1) <- paste("Topics", c(1:20))

#Prepare for Comparisons 
out_merged_data1$meta$party <- as.factor(out_merged_data1$meta$party) 

numberoftopics_merged_data2 <- stm(out_merged_data1$documents, out_merged_data1$vocab, 
                                   K=20, 
                                   prevalence = ~ party,
                                   content = ~party, 
                                   max.em.its=75, 
                                   data = out_merged_data1$meta, 
                                   seed=09222022) 

prep_merged_data2 <- estimateEffect(formula = 1:20 ~ party, stmobj = numberoftopics_merged_data2, meta = out_merged_data1$meta, uncertainty = "Global") 

#Compare Liberal and Middle of the Road 
plot.estimateEffect(prep_merged_data2, covariate = "party", topics = c(1:20), 
                    model = numberoftopics_merged_data2, method = "difference", 
                    cov.value1 = "D", cov.value2 = "R", 
                    xlab = "Democrat ... Republican",
                    main = "What are we tweeting about?",
                    xlim = c(-.1, .1), labeltype = "custom")
#Difference between Liberal and Middle of the Road at Topics: 30, 
plot.estimateEffect(prep_merged_data2, covariate = "party", topics = c(2, 3, 4, 5, 6, 7, 8, 9, 13, 14, 15, 17, 18, 19, 20), 
                    model = numberoftopics_merged_data2, method = "difference", 
                    cov.value1 = "D", cov.value2 = "R", 
                    xlab = "Democrat ... Republican",
                    main = "What are we tweeting about?",
                    xlim = c(-.06, .06), labeltype = "custom",
                    custom.labels = c('Topic 30'))



summary(prep_merged_data2)

#Insert text examples
merged_data1.fulltext <- merged_data1$meta$full_text

#Topics: 2, 3, 4, 5, 6, 7, 8, 9, 13, 14, 15, 17, 18, 19, 20
numberoftopics_topwords_merged_data1[2] #Vietnam / POW and MIAs 
numberoftopics_topwords_merged_data1[3] #
numberoftopics_topwords_merged_data1[4]
numberoftopics_topwords_merged_data1[5]
numberoftopics_topwords_merged_data1[6]
numberoftopics_topwords_merged_data1[7]
numberoftopics_topwords_merged_data1[8]
numberoftopics_topwords_merged_data1[9] 
numberoftopics_topwords_merged_data1[13]
numberoftopics_topwords_merged_data1[14]
numberoftopics_topwords_merged_data1[15]
numberoftopics_topwords_merged_data1[17]
numberoftopics_topwords_merged_data1[18]
numberoftopics_topwords_merged_data1[19]
numberoftopics_topwords_merged_data1[20]

#wordcloud
cloud(numberoftopics_merged_data1, topic = 2, min.freq=5) #
cloud(numberoftopics_merged_data1, topic = 3, min.freq=5) #
cloud(numberoftopics_merged_data1, topic = 4, min.freq=5) #
cloud(numberoftopics_merged_data1, topic = 5, min.freq=5) #
cloud(numberoftopics_merged_data1, topic = 6, min.freq=5) #
cloud(numberoftopics_merged_data1, topic = 7, min.freq=5) #
cloud(numberoftopics_merged_data1, topic = 8, min.freq=5) #
cloud(numberoftopics_merged_data1, topic = 9, min.freq=5) #
cloud(numberoftopics_merged_data1, topic = 13, min.freq=5) #
cloud(numberoftopics_merged_data1, topic = 14, min.freq=5) #
cloud(numberoftopics_merged_data1, topic = 15, min.freq=5) #
cloud(numberoftopics_merged_data1, topic = 17, min.freq=5) #
cloud(numberoftopics_merged_data1, topic = 18, min.freq=5) #
cloud(numberoftopics_merged_data1, topic = 19, min.freq=5) #
cloud(numberoftopics_merged_data1, topic = 20, min.freq=5) #


#Choosing the Quotes to use
merged_data1_firstdocs.2 <- findThoughts(numberoftopics_merged_data1, texts = out_merged_data1$meta$fulltext, n = 3)$docs[[2]]
merged_data1_firstdocs.3 <- findThoughts(numberoftopics_merged_data1, texts = out_merged_data1$meta$fulltext, n = 3)$docs[[3]]
merged_data1_firstdocs.4 <- findThoughts(numberoftopics_merged_data1, texts = out_merged_data1$meta$fulltext, n = 3)$docs[[4]]
merged_data1_firstdocs.5 <- findThoughts(numberoftopics_merged_data1, texts = out_merged_data1$meta$fulltext, n = 3)$docs[[5]]
merged_data1_firstdocs.6 <- findThoughts(numberoftopics_merged_data1, texts = out_merged_data1$meta$fulltext, n = 3)$docs[[6]]
merged_data1_firstdocs.7 <- findThoughts(numberoftopics_merged_data1, texts = out_merged_data1$meta$fulltext, n = 3)$docs[[7]]
merged_data1_firstdocs.8 <- findThoughts(numberoftopics_merged_data1, texts = out_merged_data1$meta$fulltext, n = 3)$docs[[8]]
merged_data1_firstdocs.9 <- findThoughts(numberoftopics_merged_data1, texts = out_merged_data1$meta$fulltext, n = 3)$docs[[9]]
merged_data1_firstdocs.13 <- findThoughts(numberoftopics_merged_data1, texts = out_merged_data1$meta$fulltext, n = 3)$docs[[13]]
merged_data1_firstdocs.14 <- findThoughts(numberoftopics_merged_data1, texts = out_merged_data1$meta$fulltext, n = 3)$docs[[14]]
merged_data1_firstdocs.15 <- findThoughts(numberoftopics_merged_data1, texts = out_merged_data1$meta$fulltext, n = 3)$docs[[15]]
merged_data1_firstdocs.17 <- findThoughts(numberoftopics_merged_data1, texts = out_merged_data1$meta$fulltext, n = 3)$docs[[17]]
merged_data1_firstdocs.18 <- findThoughts(numberoftopics_merged_data1, texts = out_merged_data1$meta$fulltext, n = 3)$docs[[18]]
merged_data1_firstdocs.19 <- findThoughts(numberoftopics_merged_data1, texts = out_merged_data1$meta$fulltext, n = 3)$docs[[19]]
merged_data1_firstdocs.20 <- findThoughts(numberoftopics_merged_data1, texts = out_merged_data1$meta$fulltext, n = 3)$docs[[20]]

#Displaying the Quotations
plotQuote(merged_data1_firstdocs.2, main="Top Documents")
plotQuote(merged_data1_firstdocs.3, main="Top Documents")
plotQuote(merged_data1_firstdocs.4, main="Top Documents")
plotQuote(merged_data1_firstdocs.5, main="Top Documents") 
plotQuote(merged_data1_firstdocs.6, main="Top Documents") 
plotQuote(merged_data1_firstdocs.7, main="Top Documents") 
plotQuote(merged_data1_firstdocs.8, main="Top Documents")
plotQuote(merged_data1_firstdocs.9, main="Top Documents") 
plotQuote(merged_data1_firstdocs.13, main="Top Documents") 
plotQuote(merged_data1_firstdocs.14, main="Top Documents") 
plotQuote(merged_data1_firstdocs.15, main="Top Documents") 
plotQuote(merged_data1_firstdocs.17, main="Top Documents") 
plotQuote(merged_data1_firstdocs.18, main="Top Documents") 
plotQuote(merged_data1_firstdocs.19, main="Top Documents") 
plotQuote(merged_data1_firstdocs.20, main="Top Documents") 




