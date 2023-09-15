# database of happy moments. each worker enters 3 instances where they felt happy either in the last 24 hours or 3 months (specified)
# processed file contains pertinent words from each sentence under text column
# use any methods (ideally text mining) to tell a story out of the data
# make a subset of the data and work on that
# ideas to mess with: pets, what does x and y have in common?, topics?? look for ins[o online

###loading packages####
library(tidyverse)
library(haven)
require(scales)
library(broom)
options(digits=3)
library(gmodels)
library(kableExtra)
library(janitor)
library(htmlTable)
library(gmodels)
library(wordcloud)
library(tidytext)
library(tm)
library(topicmodels)
library(reshape2)
library(pals)
library(lda)
library(ldatuning)
library(DT)
library(SnowballC)
library(flextable)

####importing data####
hm_data<-read_csv("C:\\Users\\91886\\OneDrive\\Documents\\GitHub\\ads-fall2023-project1-harinisund\\output\\processed_moments.csv")
demo_data<-read_csv('https://raw.githubusercontent.com/rit-public/HappyDB/master/happydb/data/demographic.csv')
sense_data<-read_csv('https://raw.githubusercontent.com/rit-public/HappyDB/master/happydb/data/senselabel.csv')

####EDA####
head(hm_data)
glimpse(demo_data)
head(demo_data)


####joining with demographic info####
hm_data <- hm_data %>%
  inner_join(demo_data, by = "wid") %>%
  select(wid,
         original_hm,
         cleaned_hm,
         num_sentence,
         gender, 
         marital, 
         parenthood,
         reflection_period,
         age, 
         country, 
         ground_truth_category, 
         predicted_category,
         text)
#%>%
# mutate(count = sapply(hm_data$text, wordcount)) %>%
# filter(gender %in% c("m", "f")) %>%
# filter(marital %in% c("single", "married","divorced","separated","widowed")) %>%
# filter(parenthood %in% c("n", "y")) %>%
# filter(reflection_period %in% c("24h", "3m")) %>%
# mutate(reflection_period = fct_recode(reflection_period, 
#                                       months_3 = "3m", hours_24 = "24h"))

####tokenizing and visualisation####
custom_stop_words<-c("happy")
stop_words2<-stop_words%>%bind_rows(mutate(tibble(custom_stop_words),lexicon='updated'))

hm_data%>%select(text,cleaned_hm)
hm_data_token<-hm_data%>%mutate(id=row_number())%>%unnest_tokens(word,text)%>%anti_join(stop_words2)
hm_data_token#%>%count(word)%>%arrange(desc(n))

hm_data_token%>%count(word,predicted_category)%>%group_by(predicted_category)%>%slice_max(n,n=5)%>%ungroup()%>%mutate(word2=fct_reorder(word,n))%>%ggplot(aes(word2,n))+geom_col()+coord_flip()+facet_wrap(~predicted_category,scales="free_y")+labs(x="word",y="count",title="Frequency of words")+theme(axis.text.y=element_text(angle=15, hjust=1), legend.position="none")

hm_counts<-hm_data_token%>%count(word)

####wordcloud####
wordcloud(words=hm_counts$word,freq=hm_counts$n,max.words = 20,scale=c(2, .5))

####sentiment analysis####
get_sentiments("bing") # positive & negative
get_sentiments("afinn") # -5 to 5
get_sentiments("loughran") # [CITATION NEEDED] negative, positive, litigious, uncertainty, superfluous, constraining
get_sentiments("nrc") # [CITATION NEEDED] negative, positive, fear, anger, trust, sadness, disgust, anticipation, joy, surprise

hm_data_token%>%select(cleaned_hm,word)%>%inner_join(get_sentiments("bing"))%>%count(word,sentiment)%>%pivot_wider(names_from=sentiment,values_from=n)%>%mutate(positive=ifelse(is.na(positive),0,positive),negative=ifelse(is.na(negative),0,negative),overall_sentiment=positive-negative)%>%filter(overall_sentiment<0)
hm_data_token%>%filter(word=='absurd')%>%select(cleaned_hm)

# finding or building a sentiment dictionary that is context-specific would be ideal
# we only want identifying words that have positive sentiment in our word column

###topic modelling- latent Dirichlet allocation (LDA)####
# LDA is  standard topic model
# a corpus is a collection of documents (phrases)
# bag of words treats every word in a doc separately
# topic models find patterns of words appearing together in documents and across corpus
#finds topics by creating a bag for each document, dumping words out to find patterns in which ones appear together - across all bags in corpus
# searching for patternrs rather than predicting - unsupervised learning
# topics - list of all words in corpus w/ p of words appearing within each topic
# similar to clustering (cont vs discrete)
# each topic is a collection of word probabilities for all of the unique words used in the corpus - prob of every word occuring in each topic

#plot document term matrix
hm_dtm<-hm_data_token%>%count(word,id)%>%cast_dtm(id,word,n) #document, term, word counts cols - casting tidy data into dtm
hm_dtm_matrix<-hm_dtm[1:4,2000:2004]%>%as.matrix() #sparse matrix

#running topic model
hm_lda<-LDA(hm_dtm,k=2,method='Gibbs',control=list(seed=42)) # dtm input, number of topics model should produce, estimation method, simulation seed (consistency of topics on repeat model runs)
hm_topics<-hm_lda%>%tidy(matrix='beta') # name and structure to tidy
hm_topics%>%group_by(topic)%>%arrange(desc(beta)) # casting in and out of tidy format

word_probs <- hm_topics %>%
  # Keep the top 10 highest word probabilities by topic
  group_by(topic) %>% 
  slice_max(beta,n=10) %>% 
  ungroup() %>%
  # Create term2, a factor ordered by word probability
  mutate(term2 = fct_reorder(term,beta))
# Plot term2 and the word probabilities
word_probs%>%ggplot(aes(term2,beta,fill=as.factor(topic))) +
  geom_col(show.legend=FALSE) +
  # Facet the bar plot by topic
  facet_wrap(~topic, scales = "free") +
  coord_flip()

#keep adding topics until topics appear to be duplicated
#name topics based on combo of high prob words


####ref####
# hmid (int): Happy moment ID
# wid (int): Worker ID
# reflection_period (str): Reflection period used in the instructions provided to the worker (3m or 24h)
# original_hm (str): Original happy moment
# cleaned_hm (str): Cleaned happy moment
# modified (bool): If True, original_hm is "cleaned up" to generate cleaned_hm (True or False)
# predicted_category (str): Happiness category label predicted by our classifier (7 categories. Please see the reference for details)
# ground_truth_category (str): Ground truth category label. The value is NaN if the ground truth label is missing for the happy moment
# num_sentence (int): Number of sentences in the happy moment

# wid (int): Worker ID
# age (float): Age
# country (str): Country of residence (follows the ISO 3166 Country Code)
# gender (str): {Male (m), Female (f), Other (o)}
# marital (str): Marital status {single, married, divorced, separated, or widowed}
# parenthood (str): Parenthood status {yes (y) or no (n)}

# hmid (int): Happy moment ID
# tokenOffset (int): Position index of a token
# word (str): Token in the original form
# lowercaseLemma (str): Lemmatized token in lowercase
# POS (str): Part-of-Speech tag
# MWE (str): Multi-word expression (MWE) tag in the extended IOB style (See [REF] for further information)
# offsetParent (int): The beginning position of a multi-word expression
# supersenseLabel (str): Supersense classes defined in the WordNet (19 verb and 25 noun classes. See [REF] for further information.)