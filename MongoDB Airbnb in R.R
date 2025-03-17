######################################################
############Downloading necessary packages############
######################################################


#install.packages("mongolite")  # Enables working with MongoDB databases in R
#install.packages("tidytext")   # Provides tools for text analysis using the tidy data framework
#install.packages("janeaustenr") # Contains Jane Austen's books as datasets (for text analysis)
#install.packages("dplyr")      # Essential for data wrangling and manipulation
#install.packages("stringr")    # Provides functions for handling and manipulating strings
#install.packages("tidyr")      # Helps reshape and clean data into a tidy format
#install.packages("tidytuesdayR") # Provides weekly datasets for visualization and analysis
#install.packages("ggplot2")    # Install packages for data visualization
#install.packages("scales")     # Helps in formatting scales (e.g., percentages, currency)
#install.packages("widyr")      # Calculates word correlations in text datasets
#install.packages("ggraph")     # Visualizes network graphs using ggplot2
#install.packages("igraph")     # For creating and analyzing network graphs
#install.packages("tidygraph")  # A tidyverse-friendly way to work with graph data
#install.packages("readr")      # Install package for reading CSV and other tabular data


#Load required libraries

library(mongolite)    # For working with MongoDB databases
library(tidytext)     # For text mining and natural language processing
library(janeaustenr)  # Provides Jane Austen's books as datasets (useful for text analysis)
library(dplyr)        # Essential for data manipulation (part of the tidyverse)
library(stringr)      # String manipulation functions (pattern matching, modifications)
library(tidyr)        # For handling tidy data (reshaping, separating, uniting columns, etc.)
library(tidytuesdayR) # Provides weekly datasets for data visualization challenges
library(ggplot2)      # For creating visualizations
library(scales)       # Helps format scales in ggplot (percentages, currency, etc.)
library(widyr)        # For calculating word correlations in tidy datasets
library(ggraph)       # For visualizing network graphs
library(igraph)       # For creating and analyzing network graphs
library(tidygraph)    # A tidyverse-friendly way to work with graph data
library(readr)        # For downloading preprocessed data into csv


######################################################
############connecting with MongoDB server############
######################################################

#replace the <<user>> with your Mongo user name and <<password>> with the mongo password
#lastly, replace the <<server_name>> with your MongoDB server name

connection_string <- 'mongodb+srv://<<user>>:<<password>>@<<server_name>>'
airbnb_collection <- mongo(collection="listingsAndReviews", db="sample_airbnb", url=connection_string)

airbnb <- airbnb_collection$find()  #Downloading data

#if you don't have MongoDB download the data set on excel from the repository using following lines
#install.packages("readxl")
#library(readxl)
#airbnb <- read_excel("Add file path here")

colnames(airbnb)[5] <- "text"       #Changing description column into "text"

airbnb_token <- airbnb %>%
  unnest_tokens(word, text)

########################################################
#######  Looking at frequency of each word    ##########
########################################################
airbnb_top <- airbnb_token %>%
              count(word) %>%
              anti_join(stop_words, by = "word") %>%  # Removing stop words
              arrange(desc(n)) #Sorting in descending order
airbnb_top

#write_csv(airbnb_top, "word_freq.csv")  #for Tableau viz

airbnb_bot <- airbnb_token %>%
              count(word) %>%
              anti_join(stop_words, by = "word") %>%  # Removing stop words
              arrange(n)  # Sorting in ascending order
airbnb_bot

##########################################################################
#######  General sentimental analysis on description words    ############
##########################################################################

# Perform Sentiment Analysis using the AFINN Lexicon
afinn <- airbnb_token %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN")

# Perform Sentiment Analysis using Bing and NRC Lexicons
bing_and_nrc <- bind_rows(
  airbnb_token %>%
    inner_join(get_sentiments("bing"), by = "word") %>%
    mutate(method = "Bing et al."),
  
  airbnb_token %>%
    inner_join(
      get_sentiments("nrc") %>%
        filter(sentiment %in% c("positive", "negative")),  # Keep only positive and negative words
      by = "word"
    ) %>%
    mutate(method = "NRC")
) %>%
  count(method, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# Combine Results & Visualize
bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y") +
  labs(title = "Sentiment Analysis on Overall Description",
       x = "Method",
       y = "Sentiment Score")

# Combine Results
#sentiment_results <- bind_rows(afinn, bing_and_nrc)   # Save as CSV for Tableau
#write_csv(sentiment_results, "sentiment_analysis.csv") # Save as CSV for Tableau

########################################################
#########  Analyzing top 3 property type    ############
########################################################

sorted_accomodation <- head(sort(table(airbnb$property_type), decreasing = TRUE), 10)  #Top 10 accommodation type.
print(sorted_accomodation)

nrc_selected <- get_sentiments("nrc") %>%
  filter(sentiment %in% c("joy", "trust", "anticipation", "fear"))

############ Apartment ############
apartment_sentiment  <- airbnb_token %>%
                        filter(property_type == "Apartment") %>%
                        inner_join(nrc_selected) %>% #inner joining the apartment and selected sentiments
                        count(word, sort=T)
print(apartment_sentiment)

############comparing sentiments on apartment ############

apartment_token <- airbnb_token %>%
  filter(property_type == "Apartment")

afinn <- apartment_token %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  apartment_token%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  apartment_token %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")+
  labs(title = "Sentiment for Apartments")


#apartment_results <- bind_rows(afinn, bing_and_nrc) # Combine results


#write_csv(apartment_results, "apartment_sentiment.csv") # Save as CSV for Tableau

############ House ############
house_sentiment <- airbnb_token %>%
                   filter(property_type == "House") %>%
                   inner_join(nrc_selected, by = "word", relationship = "many-to-many") %>% #inner joining the House and selected sentiments
                   count(word, sort = TRUE)
print(house_sentiment)

############comparing sentiments on apartment ############
house_token <- airbnb_token %>%
               filter(property_type == "House")

afinn <- house_token %>%
         inner_join(get_sentiments("afinn"))%>%
         summarise(sentiment=sum(value)) %>%
         mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  house_token%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  house_token %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")+
  labs(title = "Sentiment for House")


#house_results <- bind_rows(afinn, bing_and_nrc) # Combine results
#write_csv(house_results, "house_sentiment.csv") # Save as CSV for Tableau

############ Condominium ############

condominium_sentiment <- airbnb_token %>%
                         filter(property_type == "Condominium") %>%  # Fixed extra space
                         inner_join(nrc_selected, by = "word", relationship = "many-to-many") %>%
                         count(word, sort = TRUE)
print(condominium_sentiment)

############comparing sentiments on Condominium ############

condominium_token <- airbnb_token %>%
                     filter(property_type == "Condominium")

afinn <- condominium_token %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  condominium_token%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  condominium_token %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")+
  labs(title = "Sentiment for Condominium")

#condominium_results <- bind_rows(afinn, bing_and_nrc) # Combine results
#write_csv(condominium_results, "condominium_sentiment.csv")# Save as CSV for Tableau

########################################################################################
###### identifying common and unique description words from top 3 properties  ##########
########################################################################################

############ creating a tidy format for apartment############
apartment <- airbnb %>%
             filter(property_type== "Apartment")

tidy_apartment <- apartment %>%
                  select(text) %>%  # Keep only the text column
                  unnest_tokens(word, text) %>%
                  anti_join(stop_words)
print(tidy_apartment)

############creating a tidy format for house ############
house <- airbnb %>%
         filter(property_type== "House")

tidy_house <- house %>%
              select(text) %>%  # Keep only the text column
              unnest_tokens(word, text) %>%
              anti_join(stop_words)
print(tidy_house)

############creating a tidy format for condominium ############
condominium <- airbnb %>%
               filter(property_type== "Condominium")

tidy_condominium <- condominium %>%
                    select(text) %>%  # Keep only the text column
                    unnest_tokens(word, text) %>%
                    anti_join(stop_words)
print(tidy_condominium)

############ combine all the datasets and do frequencies ############

frequency <- bind_rows(mutate(tidy_apartment, property_type   ="Apartment"),
                       mutate(tidy_house, property_type       ="House"),
                       mutate(tidy_condominium, property_type ="Condominium")
)%>% #closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(property_type, word) %>%
  group_by(property_type) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(property_type, proportion) %>%
  gather(property_type, proportion, `House`, `Condominium`)

#############plotting the correlograms accross house, apartment, and condominium ############

ggplot(frequency, aes(x=proportion, y=`Apartment`, 
                      color = abs(`Apartment`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~property_type, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Apartment", x=NULL)

##############################################################
################ Pairwise Correlation ########################
##############################################################

############for Apartment ############

apartment_tidy <- airbnb %>%
                    filter(property_type == "Apartment") %>%
                    unnest_tokens(word, text) %>%
                    filter(!word %in% stop_words$word)

#taking out the least common words
apt_cors <- apartment_tidy %>%
  group_by(word) %>%
  filter(n() >= 10) %>%
  pairwise_cor(word, summary, sort=TRUE)

####### creating barcharts for Apartment correlations ############

apt_cors %>%
  filter(item1 %in% c("apartment", "city", "cafes")) %>%
  group_by(item1) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity")+
  facet_wrap(~item1, scales = "free")+
  coord_flip()+
  labs(
    title = "Top 5 Highly Correlated Words with Apartment, City, and Cafes",
    subtitle = "Showing the strongest word correlations for selected terms",
    x = "Correlated Words",
    y = "Correlation Score")

####### creating a correlation network for Apartment #################

apt_cors %>%
  filter(item1 %in% c("apartment", "city", "cafes" )) %>%
  filter(correlation >.20) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
  geom_node_point(color = "lightgreen", size=6)+
  geom_node_text(aes(label=name), repel=T, max_overlaps=20)+
  theme_void()
  
############for Condominium ############

condo_tidy <- airbnb %>%
  filter(property_type == "Condominium") %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

#taking out the least common words
condo_cors <- condo_tidy %>%
  group_by(word) %>%
  filter(n() >= 10) %>%
  pairwise_cor(word, summary, sort=TRUE)

####### creating barcharts for Condominium correlations ############

condo_cors %>%
  filter(item1 %in% c("tropical", "lanai", "golf", "waikiki", "pool", "kihei")) %>%
  group_by(item1) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity")+
  facet_wrap(~item1, scales = "free")+
  coord_flip()+
  labs(
    title = "Top Highly Correlated Words with Unique words associated with Condominium",
    subtitle = "Showing the strongest word correlations for selected terms",
    x = "Correlated Words",
    y = "Correlation Score")

####### creating a correlation network for Condominium #################

condo_cors %>%
  filter(item1 %in% c("tropical", "lanai", "golf", "waikiki", "pool", "kihei")) %>%
  filter(correlation >.25) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
  geom_node_point(color = "lightgreen", size=6)+
  geom_node_text(aes(label=name), repel=T, max_overlaps=50)+
  theme_void()

############for House ############

house_tidy <- airbnb %>%
  filter(property_type == "House") %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

#taking out the least common words
house_cors <- house_tidy %>%
  group_by(word) %>%
  filter(n() >= 10) %>%
  pairwise_cor(word, summary, sort=TRUE)

####### creating barcharts for House correlations ############


house_cors %>%
  filter(item1 %in% c("home", "family", "maison", "deck", "cottage")) %>%
  group_by(item1) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity")+
  facet_wrap(~item1, scales = "free")+
  coord_flip()+
  labs(
    title = "Top Highly Correlated Words with Unique words associated with House",
    subtitle = "Showing the strongest word correlations for selected terms",
    x = "Correlated Words",
    y = "Correlation Score")

####### creating a correlation for House network #################

house_cors %>%
  filter(item1 %in% c("home", "family", "maison", "deck", "cottage")) %>%
  filter(correlation >.25) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
  geom_node_point(color = "lightgreen", size=6)+
  geom_node_text(aes(label=name), repel=T, max_overlaps=50)+
  theme_void()
##############################################################
########################### NGram ############################ 
##############################################################

###################### Bigram Apartment ###################### 
apt_bigrams <- airbnb %>%
               filter(property_type == "Apartment") %>%
               unnest_tokens(bigram, text, token = "ngrams", n=2)

apt_bigrams_separated <- apt_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

apt_bigrams_filtered <- apt_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
apt_bigram_counts <- apt_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

# bigrams result for apartment
apt_bigram_counts

###################### Trigram Apartment ###################### 
apt_trigrams <- airbnb %>%
  filter(property_type == "Apartment") %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3)

#to remove stop words from the bigram data, we need to use the separate function:

apt_trigrams_separated <- apt_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

apt_trigrams_filtered <- apt_trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
apt_trigram_counts <- apt_trigrams_filtered %>%
  count(word1, word2, word3, sort = TRUE)
#see the new bigrams
apt_trigram_counts


###################### Bigram Condominium ###################### 
condo_bigrams <- airbnb %>%
  filter(property_type == "Condominium") %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

#to remove stop words from the bigram data, we need to use the separate function:

condo_bigrams_separated <- condo_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

condo_bigrams_filtered <- condo_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
condo_bigram_counts <- condo_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
#see the new bigrams
condo_bigram_counts

###################### Trigram Condominium ######################
condo_trigrams <- airbnb %>%
  filter(property_type == "Condominium") %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3)

#to remove stop words from the bigram data, we need to use the separate function:

condo_trigrams_separated <- condo_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

condo_trigrams_filtered <- condo_trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
condo_trigram_counts <- condo_trigrams_filtered %>%
  count(word1, word2, word3, sort = TRUE)
#see the new bigrams
condo_trigram_counts

###################### Bigram House ######################
house_bigrams <- airbnb %>%
  filter(property_type == "House") %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

#to remove stop words from the bigram data, we need to use the separate function:

house_bigrams_separated <- house_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

house_bigrams_filtered <- house_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
house_bigram_counts <- house_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
# see the new bigrams
house_bigram_counts

###################### Trigram House ######################
house_trigrams <- airbnb %>%
  filter(property_type == "House") %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3)

#to remove stop words from the trigram data, we need to use the separate function:

house_trigrams_separated <- house_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

house_trigrams_filtered <- house_trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)

#creating the new trigram, "no-stop-words":
house_trigram_counts <- house_trigrams_filtered %>%
  count(word1, word2, word3, sort = TRUE)
#see the new bigrams
house_trigram_counts

##############################################################
########################### TFIDF ############################ 
##############################################################



#we're grouping by the country this time
airbnb_token <- airbnb %>%
  unnest_tokens(word, text) %>%
  count(property_type, word, sort=TRUE) %>%
  ungroup()

total_words <- airbnb_token %>%
  group_by(property_type) %>%
  summarize(total=sum(n))

airbnb_words <- left_join(airbnb_token, total_words)%>%
  filter(property_type %in% c("Apartment", "House", "Condominium"))

print(airbnb_words)

######################################
########## ZIPF's law ################
######################################

freq_by_rank <- airbnb_words %>%
  group_by(property_type) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank

#plot ZIPF's Law
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=property_type))+
  #let's add a tangent line , the first derivative, and see what the slop is
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

###################################################
################# TF_IDF ##########################
###################################################

property_words <- airbnb_words %>%
  bind_tf_idf(word, property_type, n)

property_words 

property_words %>%
  arrange(desc(tf_idf))


#############
#graphical approach:
property_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(property_type) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=property_type))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~property_type, ncol=2, scales="free")+
  coord_flip()+
  labs(
    title = "Top ranking words associated with each property type",
    x = "Words",
    y = "Rank")


#write_csv(condo_cors, "condominium_word_correlation.csv") #for tableau
#write_csv(property_words, "tf_idf_cleaned.csv") #for tableau
