# database of happy moments. each worker enters 3 instances where they felt happy either in the last 24 hours or 3 months (specified)
# processed file contains pertinent words from each sentence under text column
# use any methods (ideally text mining) to tell a story out of the data
# make a subset of the data and work on that

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
hm_data<-read_csv(paste0(getwd(),'/output/processed_moments.csv'))
#demo_data<-read_csv('https://raw.githubusercontent.com/rit-public/HappyDB/master/happydb/data/demographic.csv')
#sense_data<-read_csv('https://raw.githubusercontent.com/rit-public/HappyDB/master/happydb/data/senselabel.csv')

####keeping relevant rows, tokenizing and visualisation####
hm_data_token <- hm_data %>%
  select(wid, cleaned_hm, text) %>%
  mutate(id=row_number()) %>%
  unnest_tokens(word,text)

#hm_data_token%>%filter(country=='USA'|country=='IND')%>%count(country,word,sort=T)%>%pivot_wider(names_from=country,values_from=n)%>%mutate(prop1=USA/sum(USA,na.rm=T)*100,prop2=IND/sum(IND,na.rm=T)*100)%>%arrange(desc(prop1))%>%select(-USA,-IND)#%>%pivot_longer(!word,names_to="prop",values_to="country")#%>%group_by(prop)%>%slice_max(country,n=10)

#these words occur at a very high frequency and will distort any comparison towards these words
hm_data_token %>% 
  count(word,sort=T) %>%
  slice_max(n,n=10) %>%
  select(-n)

#combine with identifying words for dogs/cats and remove from our data to show in wordclouds
common_words <- hm_data_token %>% 
  count(word,sort=T) %>%
  slice_max(n,n=10) %>%
  select(-n) %>% 
  bind_rows(tribble(~word,"cat","cats","kitten","kittens","dog","dogs","puppies","puppy"))

hm_animal <- hm_data_token %>% 
  group_by(wid) %>%
  mutate(animal=ifelse(word %in% c('cat','cats','kitten','kittens'),"cat",
                       ifelse(word %in% c('dog','dogs','puppy','puppies'),"dog","other")),
         animal_lvr=ifelse((sum(ifelse(animal!='other',1,0)))>0,"y","n")) %>% 
  ungroup()

hm_ani_cl <- hm_animal %>%
  anti_join(common_words)

hm_ani_cl %>%
  select(wid,animal_lvr,animal,word,cleaned_hm) %>% 
  count(animal_lvr,word) %>% 
  group_by(animal_lvr) %>% 
  slice_max(n,n=10) %>% 
  ggplot(aes(reorder(word,-n),n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~animal_lvr,scales="free")

# hm_data_token%>%filter(country=='IND')%>%count(word,sort=T)%>%mutate(prop=n/n(),sum=prop)
# hm_data_token%>%filter((country=='USA'|country=='IND'))%>%count(word,country,marital)%>%group_by(country,marital)%>%slice_max(n,n=5)%>%ungroup()%>%mutate(word2=fct_reorder(word,n))%>%ggplot(aes((reorder(word, -n)),n))+geom_col()+coord_flip()+facet_wrap(~country~marital,scales="free")+labs(x="word",y="count",title="Frequency of words")+theme(axis.text.x=element_text(angle=15, hjust=1), legend.position="none")

####wordcloud####

hm_ani_cl %>%
  filter(animal_lvr=='y') %>%
  count(word) %>%
  with(wordcloud(word, n,
                 scale=c(2,0.5),
                 max.words=40,
                 min.freq=1,
                 random.order=FALSE,
                 rot.per=0.2,
                 use.r.layout=T,
                 random.color=F,
                 colors=brewer.pal(9,"Greens")))

hm_ani_cl %>%
  filter(animal_lvr=='n') %>%
  count(word) %>%
  with(wordcloud(word, n,
                 max.words = 40,
                 scale=c(1.25, 0.5),
                 rot.per=0.2,
                 colors=brewer.pal(9,"Reds")))

hm_ani_cl %>%
  count(word, animal_lvr, sort = TRUE) %>%
  acast(word ~ animal_lvr, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red3", "green4"),
                   max.words = 100, scale=c(1.25, .5))

hm_data_token%>%filter(word=="dog"|word=="kitten")%>%select(cleaned_hm)%>%head(n=5)
#of mentions of cuddling, % about animals
#put all % animals in one stacked chart??
hm_ani_cl%>%filter(animal_lvr=="y"&(word=="cuddled"))%>%select(cleaned_hm)%>%print(n=10)
hm_ani_cl%>%filter(animal_lvr=="y"&(word=="cuddled"))%>%select(id,cleaned_hm)%>%inner_join((hm_animal%>%select(id,word,Animal=animal)),relationship='many-to-many')%>%count(Animal)%>%mutate(Proportion=n/sum(n))%>%select(-n)
#maybe animal lovers are just morning people and like the outdoors- weather, rain, walk, morning activities
hm_ani_cl%>%filter(animal_lvr=="y"&(word=="morning"))%>%select(cleaned_hm)%>%print(n=10)
hm_ani_cl%>%filter(animal_lvr=="y"&(word=="morning"))%>%select(id,cleaned_hm)%>%inner_join((hm_animal%>%select(id,word,Animal=animal)),relationship='many-to-many')%>%count(Animal)%>%mutate(Proportion=n/sum(n))%>%select(-n)
hm_ani_cl%>%filter(animal_lvr=="y"&(word=="walk"))%>%select(cleaned_hm)%>%print(n=10)
hm_ani_cl%>%filter(animal_lvr=="y"&(word=="walk"))%>%select(id,cleaned_hm)%>%inner_join((hm_animal%>%select(id,word,Animal=animal)),relationship='many-to-many')%>%count(Animal)%>%mutate(Proportion=n/sum(n))%>%select(-n)
hm_ani_cl%>%filter(animal_lvr=="y"&(word=="weather"))%>%select(cleaned_hm)%>%print(n=10)
hm_ani_cl%>%filter(animal_lvr=="y"&(word=="weather"))%>%select(id,cleaned_hm)%>%inner_join((hm_animal%>%select(id,word,Animal=animal)),relationship='many-to-many')%>%count(Animal)%>%mutate(Proportion=n/sum(n))%>%select(-n)
hm_ani_cl%>%filter(animal_lvr=="y"&(word=="rain"))%>%select(cleaned_hm)%>%print(n=10)

#adopted, pet, shelter, vet
#kids, marriage, college, exams, relatives, functions, events, office etc

#cats vs dogs

##filter sentences that mention cat/dog
ani_ids <- hm_animal %>%
  group_by(id) %>%
  select(id, animal) %>%
  filter(animal!="other") %>%
  distinct()

hm_animal %>%
  group_by(id) %>%
  select(id, animal) %>%
  filter(animal!="other") %>%
  distinct() %>%
  ungroup() %>%
  count(animal) %>%
  mutate(n/sum(n)) %>% select(-n)


##we want info on whether a sentence mentions cat/dog at a sentence level
hm_catdog <- hm_ani_cl %>%
  inner_join(ani_ids, by="id",relationship='many-to-many') 

#type of words used in happy moments
##comparison cloud

hm_catdog %>%
  count(word, animal.y, sort = TRUE) %>%
  acast(word ~ animal.y, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("orange3", "brown4"),
                   max.words = 100,scale=c(1.25, .5))

####sentiment analysis####
# get_sentiments("bing") # positive & negative
# get_sentiments("afinn") # -5 to 5
# get_sentiments("loughran") # [CITATION NEEDED] negative, positive, litigious, uncertainty, superfluous, constraining
# get_sentiments("nrc") # [CITATION NEEDED] negative, positive, fear, anger, trust, sadness, disgust, anticipation, joy, surprise

# we only want words thar contribute a positive sentiment to the sentence, since we're concerned with happiness.
#we remove common words as previously defined so we filter out overall happy indicating words, including animal indicators as puppy and kitten have sentiments attached
nrc_pos <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy"|sentiment == "anticipation"|sentiment == "surprise"|sentiment == "trust")%>%
  anti_join(common_words)

afinn_pos<-get_sentiments("afinn") %>% 
  filter(value>=0)%>%
  anti_join(common_words)

#sentences where dog/cat is mentioned - how does the sentiment differ?
#nrc
hm_catdog %>%
  select(cleaned_hm,word,animal=animal.y) %>%
  inner_join(nrc_pos,relationship='many-to-many') %>%
  group_by(animal,sentiment) %>%
  summarize(n=n()) %>%
  mutate(prop=n/sum(n)) %>%
  ggplot(aes(x=reorder(sentiment,-prop),y=prop)) +
  geom_col(show.legend=F) +
  scale_y_continuous(labels = percent) + 
  labs(x="Sentiment",y="Words (%)") +
  facet_wrap(~animal)

#afinn
hm_catdog %>%
  select(cleaned_hm,word,animal=animal.y) %>%
  inner_join(afinn_pos,relationship='many-to-many') %>%
  group_by(animal,value) %>%
  summarize(n=n()) %>%
  mutate(prop=n/sum(n)) %>%
  ggplot(aes(x=value,y=prop)) +
  geom_col(show.legend=F) +
  scale_y_continuous(labels = percent) + 
  labs(x="Sentiment",y="Words (%)") +
  facet_wrap(~animal)

hm_catdog %>%
  select(cleaned_hm,word,animal=animal.y) %>%
  inner_join(afinn_pos,relationship='many-to-many') %>%
  group_by(animal) %>%
  summarize(avg=mean(value))

#example sentences
hm_catdog%>%select(cleaned_hm,word,animal=animal.y)%>%inner_join(nrc_pos,relationship='many-to-many')%>%
  filter(sentiment=="anticipation"&animal=="cat")%>%select(cleaned_hm,word)%>%print(n=10)


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
hm_dtm_c<-hm_catdog%>%filter(animal.y=="cat")%>%count(word,id)%>%cast_dtm(id,word,n) #document, term, word counts cols - casting tidy data into dtm
hm_dtm_d<-hm_catdog%>%filter(animal.y=='dog')%>%count(word,id)%>%cast_dtm(id,word,n) #document, term, word counts cols - casting tidy data into dtm
#hm_dtm_matrix<-hm_dtm[1:4,2000:2004]%>%as.matrix() #sparse matrix

#running topic model
hm_lda_d<-LDA(hm_dtm_d,k=2,method='Gibbs',control=list(seed=42)) # dtm input, number of topics model should produce, estimation method, simulation seed (consistency of topics on repeat model runs)
hm_lda_c<-LDA(hm_dtm_c,k=2,method='Gibbs',control=list(seed=42)) # dtm input, number of topics model should produce, estimation method, simulation seed (consistency of topics on repeat model runs)
hm_topics_d<-hm_lda_d%>%tidy(matrix='beta') # name and structure to tidy
hm_topics_c<-hm_lda_c%>%tidy(matrix='beta') # name and structure to tidy
hm_topics_c%>%group_by(topic)%>%arrange(desc(beta)) # casting in and out of tidy format

word_probs_d <- hm_topics_d %>%
  # Keep the top 10 highest word probabilities by topic
  group_by(topic) %>% 
  slice_max(beta,n=10) %>% 
  ungroup() %>%
  # Create term2, a factor ordered by word probability
  mutate(term2 = fct_reorder(term,beta))

word_probs_c <- hm_topics_c %>%
  group_by(topic) %>% 
  slice_max(beta,n=10) %>% 
  ungroup() %>%
  mutate(term2 = fct_reorder(term,beta))

# Plot term2 and the word probabilities
word_probs_d%>%ggplot(aes(term2,beta,fill=as.factor(topic))) +
  geom_col(show.legend=FALSE) +
  # Facet the bar plot by topic
  facet_wrap(~topic, scales = "free") +
  coord_flip()

#keep adding topics until topics appear to be duplicated
#name topics based on combo of high prob words


####ref####
# hmid (int): Happy moment ID X
# wid (int): Worker ID (id purposes)
# reflection_period (str): Reflection period used in the instructions provided to the worker (3m or 24h)
# original_hm (str): Original happy moment X
# cleaned_hm (str): Cleaned happy moment
# modified (bool): If True, original_hm is "cleaned up" to generate cleaned_hm (True or False) X
# predicted_category (str): Happiness category label predicted by our classifier (7 categories. Please see the reference for details)
# ground_truth_category (str): Ground truth category label. The value is NaN if the ground truth label is missing for the happy moment X
# num_sentence (int): Number of sentences in the happy moment X
# age (float): Age
# country (str): Country of residence (follows the ISO 3166 Country Code)
# gender (str): {Male (m), Female (f), Other (o)}
# marital (str): Marital status {single, married, divorced, separated, or widowed}
# parenthood (str): Parenthood status {yes (y) or no (n)}