#Used libraries 
library(tidyverse) 
library(rtweet) 
library(igraph) 
library(stm) 
library(rvest) 
library(wordcloud) 
library(quanteda) 
library(quanteda.corpora) #for corpora asignments and analysis 
library(quanteda.textplots) #for textplots 
library(quanteda.textstats) #for textstats 
library(quanteda.sentiment) #for sentiment... but I don't think we actually use this yet 
library(readr) 

#Bring in the Twitter handles 
senators <- read_csv("~/desktop/senate - twitter - Aug 2022.csv") 
names(senators) <- c("name", "link", "state", "party")
senators <- senators[-c(1), ]

#Create Twitter Handle Variable 
senators <- senators %>% 
  mutate(screen_name = gsub("https://twitter.com/", "", link)) 

#Change bad links - Won't find this out unti you run later code 
senators$screen_name[senators$screen_name == 'SenatorMarshall'] <- 'RogerMarshallMD' 
senators$screen_name[senators$screen_name == 'senatemajldr'] <- 'LeaderMcConnell' 

#Create data frame to hold tweets
tweet_holder_senate <- as.data.frame(NULL) 

#Go to Twitter
for(i in 1:100){
  timeline <- get_timeline(senators$screen_name[i], n=100) 
  bound <- cbind(timeline, users_data(timeline)[, c("id", "id_str", "name", "screen_name")]) 
  tweet_holder_senate <- rbind(tweet_holder_senate, bound) 
  print(i) 
}

#Remove columns (Check on this later) 
tweet_holder_senate <- tweet_holder_senate[-c(44:45)] 

#Merge with sentors data frame to get Party Affiliation and State 
merged_data_senate <- left_join(tweet_holder_senate, senators, by= "screen_name") 

#Subset by party, putting Independents with Democrats 
republican_senators <- subset(merged_data_senate, party == "R") 
democratic_senators <- subset(merged_data_senate, party == "D" | party == "I") 

#Creating a Republican Corpus 
republican_tweets <- republican_senators  

republican_tweet_words <- corpus(republican_tweets)

republican_tokens <-tokens(republican_tweet_words) 

republican_clean_tokens <- tokens(
  republican_tweet_words,
  remove_numbers = TRUE,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_twitter = TRUE,
  remove_url = TRUE,
  remove_hyphens = TRUE,
  include_docvars = TRUE
)

# Create word frequency matrix
republican_data_freq_matrix <- dfm(republican_clean_tokens,
                        tolower=TRUE,
                        stem=TRUE,
                        remove=stopwords('english')) %>%
  dfm_remove(c('rt', 'https', 'amp')) 

# Less sparse / get rid of unique words (ex. powergrid or supernova or vicissitude) 
republican_data_freq_matrix <- dfm_trim(republican_data_freq_matrix,
                             min_docfreq = 0.075,
                             max_docfreq = 0.90,
                             docfreq_type = "prop"
) 

# Visualize frequency
republican_features <- textstat_frequency(republican_data_freq_matrix, n=30)

republican_features$feature <- with(republican_features, reorder(feature, -frequency))

ggplot(republican_features, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Cleaning the Data for stm 
republican_tweets_1 <- republican_tweets[-c(5:43)]

processed_republican_tweets <- textProcessor(republican_tweets_1$full_text, metadata=republican_tweets_1)

out_republican_tweets <- prepDocuments(processed_republican_tweets$document, processed_republican_tweets$vocab, processed_republican_tweets$meta) 

docs_republican_tweets <- out_republican_tweets$documents 

vocab_republican_tweets <- out_republican_tweets$vocab

meta_republican_tweets <- out_republican_tweets$meta 

#Let's find the optimal K, this will take a long time, be prepared
republican_tweets_k <- searchK(out_republican_tweets$documents, out_republican_tweets$vocab, K = c(2, 3, 4, 5, 6, 7, 8, 9, 10),
                                  data=out_republican_tweets$meta)

republican_tweets_k #looking for highest exclus and highest (lowest -) semcoh 

#The best fit is 8 topics
numberoftopics_republican <- stm(out_republican_tweets$documents, out_republican_tweets$vocab, 
                                   K=8, 
                                   max.em.its=30, 
                                   data = out_republican_tweets$meta, 
                                   seed=09262022) 

#Let's focus on FREX
#words used in topics
labelTopics(numberoftopics_republican, c(1:8), frexweight = 0.5, n = 15) 

numberoftopics_labels_republican <- labelTopics(numberoftopics_republican, n = 10)
numberoftopics_topwords_republican <- data.frame("features" = t(numberoftopics_labels_republican$frex))
colnames(numberoftopics_topwords_republican) <- paste("Topics", c(1:8))

#Prepare for Comparisons 
#out_republican_tweets$meta$party <- as.factor(out_republican_tweets$meta$party) 

republican_tweets_1.fulltext <- republican_tweets_1$full_text

numberoftopics_topwords_republican[1] #
numberoftopics_topwords_republican[2] #
numberoftopics_topwords_republican[3] #
numberoftopics_topwords_republican[4] #
numberoftopics_topwords_republican[5] #
numberoftopics_topwords_republican[6] #
numberoftopics_topwords_republican[7] #
numberoftopics_topwords_republican[8] #

cloud(numberoftopics_republican, topic = 1, min.freq=5) #
cloud(numberoftopics_republican, topic = 2, min.freq=5)
cloud(numberoftopics_republican, topic = 3, min.freq=5)
cloud(numberoftopics_republican, topic = 4, min.freq=5)
cloud(numberoftopics_republican, topic = 5, min.freq=5)
cloud(numberoftopics_republican, topic = 6, min.freq=5)
cloud(numberoftopics_republican, topic = 7, min.freq=5)
cloud(numberoftopics_republican, topic = 8, min.freq=5)

republican_firstdocs.1 <- findThoughts(numberoftopics_republican, texts = out_republican_tweets$meta$full_text, n = 3)$docs[[1]]
republican_firstdocs.2 <- findThoughts(numberoftopics_republican, texts = out_republican_tweets$meta$full_text, n = 3)$docs[[2]]
republican_firstdocs.3 <- findThoughts(numberoftopics_republican, texts = out_republican_tweets$meta$full_text, n = 3)$docs[[3]]
republican_firstdocs.4 <- findThoughts(numberoftopics_republican, texts = out_republican_tweets$meta$full_text, n = 3)$docs[[4]]
republican_firstdocs.5 <- findThoughts(numberoftopics_republican, texts = out_republican_tweets$meta$full_text, n = 3)$docs[[5]]
republican_firstdocs.6 <- findThoughts(numberoftopics_republican, texts = out_republican_tweets$meta$full_text, n = 3)$docs[[6]]
republican_firstdocs.7 <- findThoughts(numberoftopics_republican, texts = out_republican_tweets$meta$full_text, n = 3)$docs[[7]]
republican_firstdocs.8 <- findThoughts(numberoftopics_republican, texts = out_republican_tweets$meta$full_text, n = 3)$docs[[8]]

plotQuote(republican_firstdocs.1, main="Top Documents")
plotQuote(republican_firstdocs.2, main="Top Documents")
plotQuote(republican_firstdocs.3, main="Top Documents")
plotQuote(republican_firstdocs.4, main="Top Documents")
plotQuote(republican_firstdocs.5, main="Top Documents")
plotQuote(republican_firstdocs.6, main="Top Documents")
plotQuote(republican_firstdocs.7, main="Top Documents")
plotQuote(republican_firstdocs.8, main="Top Documents")
