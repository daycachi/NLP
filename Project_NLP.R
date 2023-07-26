library(rvest)
library(dplyr)

################################################################################
################################################################################

###############    JORA PERUVIAN FOOD

################################################################################
################################################################################

link1 = "https://www.yelp.com/biz/jora-peruvian-food-san-jose?osq=jora%20peruvian"
page1 = read_html(link1)

page_1 = page1 %>% html_nodes(".comment__09f24__gu0rG") %>% html_text()
page_1

####### page 2
link2 = "https://www.yelp.com/biz/jora-peruvian-food-san-jose?osq=jora%20peruvian&start=10"
page2 = read_html(link2)

page_2 = page2 %>% html_nodes(".comment__09f24__gu0rG") %>% html_text()
page_2

###### page 3
link3 = "https://www.yelp.com/biz/jora-peruvian-food-san-jose?osq=jora%20peruvian&start=20"
page3 = read_html(link3)

page_3 = page3 %>% html_nodes(".comment__09f24__gu0rG") %>% html_text()
page_3

####### page 4
link4 = "https://www.yelp.com/biz/jora-peruvian-food-san-jose?osq=jora%20peruvian&start=30"
page4 = read_html(link4)

page_4 = page4 %>% html_nodes(".comment__09f24__gu0rG") %>% html_text()
page_4

###### page 5
link5 = "https://www.yelp.com/biz/jora-peruvian-food-san-jose?osq=jora%20peruvian&start=40"
page5 = read_html(link3)

page_5 = page5 %>% html_nodes(".comment__09f24__gu0rG") %>% html_text()
page_5

####### page 6
link6 = "https://www.yelp.com/biz/jora-peruvian-food-san-jose?osq=jora%20peruvian&start=50"
page6 = read_html(link6)

page_6 = page6 %>% html_nodes(".comment__09f24__gu0rG") %>% html_text()
page_6

###### page 7
link7 = "https://www.yelp.com/biz/jora-peruvian-food-san-jose?osq=jora%20peruvian&start=60"
page7 = read_html(link7)

page_7 = page7 %>% html_nodes(".comment__09f24__gu0rG") %>% html_text()
page_7

####### page 8
link8 = "https://www.yelp.com/biz/jora-peruvian-food-san-jose?osq=jora%20peruvian&start=70"
page8 = read_html(link8)

page_8 = page8 %>% html_nodes(".comment__09f24__gu0rG") %>% html_text()
page_8

###### page 9
link9 = "https://www.yelp.com/biz/jora-peruvian-food-san-jose?osq=jora%20peruvian&start=80"
page9 = read_html(link9)

page_9 = page9 %>% html_nodes(".comment__09f24__gu0rG") %>% html_text()

####### page 10
link10 = "https://www.yelp.com/biz/jora-peruvian-food-san-jose?osq=jora%20peruvian&start=90"
page10 = read_html(link10)

page_10 = page10 %>% html_nodes(".comment__09f24__gu0rG") %>% html_text()
page_10

###### page 11
link11 = "https://www.yelp.com/biz/jora-peruvian-food-san-jose?osq=jora%20peruvian&start=100"
page11= read_html(link11)

page_11 = page11 %>% html_nodes(".comment__09f24__gu0rG") %>% html_text()
page_11

####### page 12
link12 = "https://www.yelp.com/biz/jora-peruvian-food-san-jose?osq=jora%20peruvian&start=110"
page12= read_html(link12)

page_12 = page12 %>% html_nodes(".comment__09f24__gu0rG") %>% html_text()
page_12

###### page 13
link13 = "https://www.yelp.com/biz/jora-peruvian-food-san-jose?osq=jora%20peruvian&start=120"
page13 = read_html(link13)

page_13 = page13 %>% html_nodes(".comment__09f24__gu0rG") %>% html_text()
page_13

####### page 14
link14 = "https://www.yelp.com/biz/jora-peruvian-food-san-jose?osq=jora%20peruvian&start=130"
page14 = read_html(link14)

page_14 = page14 %>% html_nodes(".comment__09f24__gu0rG") %>% html_text()
page_14

###### page 15
link15 = "https://www.yelp.com/biz/jora-peruvian-food-san-jose?osq=jora%20peruvian&start=140"
page15 = read_html(link15)

page_15 = page15 %>% html_nodes(".comment__09f24__gu0rG") %>% html_text()
page_15

#STRUCTURING THE DATA

library(tidytext)

list_pages = c(page_1,page_2,page_3,page_5,page_6,page_7,page_8,page_9,page_10,page_11,page_12,page_13,page_14,page_15)
df <- as.data.frame(list_pages)

colnames(df)[1] <- "text"


jora_token <- df %>%
  unnest_tokens(word, text)

#### Sentiment analysis NRC

nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise") 

#inner joining the Jora comments and the surprise sentiments
jora_token %>%
  inner_join(nrcsurprise) %>%
  count(word, sort=T)

########################################################
##### Comparing different sentiment libraries on Jora Comments ####
########################################################

library(affin)
library(nrc)
library(bing)
afinn <- jora_token %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  jora_token%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  jora_token %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

library(ggplot2)
bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

##############################################################
######## Most common positive and negative words #############
##############################################################

bing_counts <- jora_token %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts

library(ggplot2)
bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

bing_counts

#semantic structures is neeeded for semantic meanings

###########################################################
#################                           ###############
###########################################################

library(tidytext)
tidy_jora <- df %>%
  unnest_tokens(word, text)
print(tidy_jora)
#removing stop words
data(stop_words)

jora_no_stop <- tidy_jora %>%
  anti_join(stop_words)
print(jora_no_stop)
#printing the count frequencies for each token without stop words
jora_no_stop %>%
  count(word, sort=TRUE)

#plotting the token frequencies:
library(ggplot2)
freq_hist <-jora_no_stop %>%
  count(word, sort=TRUE) %>%
  filter(n>20) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist)


################################################################################
################################################################################

###############    MR KANO RESTAURANT

################################################################################
################################################################################

link1_MR = "https://www.yelp.com/biz/mr-kano-peruvian-restaurant-santa-clara?osq=peruvian+food"
page1_MR = read_html(link1_MR)

page_1_MR = page1_MR %>% html_nodes(".comment__09f24__gu0rG .raw__09f24__T4Ezm") %>% html_text()
page_1_MR

####### page 2
link2_MR = "https://www.yelp.com/biz/mr-kano-peruvian-restaurant-santa-clara?osq=peruvian%20food&start=10"
page2_MR = read_html(link2_MR)

page_2_MR = page2_MR %>% html_nodes(".comment__09f24__gu0rG .raw__09f24__T4Ezm") %>% html_text()
page_2_MR

###### page 3
link3_MR = "https://www.yelp.com/biz/mr-kano-peruvian-restaurant-santa-clara?osq=peruvian%20food&start=20"
page3_MR = read_html(link3_MR)

page_3_MR = page3_MR %>% html_nodes(".comment__09f24__gu0rG .raw__09f24__T4Ezm") %>% html_text()
page_3_MR

####### page 4
link4_MR = "https://www.yelp.com/biz/mr-kano-peruvian-restaurant-santa-clara?osq=peruvian%20food&start=30"
page4_MR = read_html(link4_MR)

page_4_MR = page4_MR %>% html_nodes(".comment__09f24__gu0rG .raw__09f24__T4Ezm") %>% html_text()
page_4_MR
###### page 5
link5_MR = "https://www.yelp.com/biz/mr-kano-peruvian-restaurant-santa-clara?osq=peruvian%20food&start=40"
page5_MR = read_html(link5_MR)

page_5_MR = page5_MR %>% html_nodes(".comment__09f24__gu0rG .raw__09f24__T4Ezm") %>% html_text()
page_5_MR

####### page 6
link6_MR = "https://www.yelp.com/biz/mr-kano-peruvian-restaurant-santa-clara?osq=peruvian%20food&start=50"
page6_MR = read_html(link6_MR)

page_6_MR = page6_MR %>% html_nodes(".comment__09f24__gu0rG .raw__09f24__T4Ezm") %>% html_text()
page_6_MR

###### page 7
link7_MR = "https://www.yelp.com/biz/mr-kano-peruvian-restaurant-santa-clara?osq=peruvian%20food&start=60"
page7_MR = read_html(link7_MR)

page_7_MR = page7_MR %>% html_nodes(".comment__09f24__gu0rG .raw__09f24__T4Ezm") %>% html_text()
page_7_MR

####### page 8
link8_MR = "https://www.yelp.com/biz/mr-kano-peruvian-restaurant-santa-clara?osq=peruvian%20food&start=70"
page8_MR = read_html(link8_MR)

page_8_MR = page8_MR %>% html_nodes(".comment__09f24__gu0rG .raw__09f24__T4Ezm") %>% html_text()
page_8_MR

###### page 9
link9_MR = "https://www.yelp.com/biz/mr-kano-peruvian-restaurant-santa-clara?osq=peruvian%20food&start=80"
page9_MR = read_html(link9_MR)

page_9_MR = page9_MR %>% html_nodes(".comment__09f24__gu0rG .raw__09f24__T4Ezm") %>% html_text()
page_9_MR
####### page 10
link10_MR = "https://www.yelp.com/biz/mr-kano-peruvian-restaurant-santa-clara?osq=peruvian%20food&start=90"
page10_MR = read_html(link10_MR)

page_10_MR = page10_MR %>% html_nodes(".comment__09f24__gu0rG .raw__09f24__T4Ezm") %>% html_text()
page_10_MR



#STRUCTURING THE DATA

list_pages_MR = c(page_1_MR,page_2_MR,page_3_MR,page_4_MR,page_5_MR,page_6_MR,page_7_MR,page_8_MR,page_9_MR,page_10_MR)
df_MR <- as.data.frame(list_pages_MR)

colnames(df_MR)[1] <- "text"


MR_token <- df_MR %>%
  unnest_tokens(word, text)

nrcsurprise_MR <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise") 

#inner joining the MR Kano comments and the surprise sentiments
MR_token %>%
  inner_join(nrcsurprise) %>%
  count(word, sort=T)

########################################################
##### Comparing different sentiment libraries on Jora Comments ####
########################################################


afinn_MR <- MR_token %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc_MR <- bind_rows(
  MR_token%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  MR_token %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

library(ggplot2)
bind_rows(afinn_MR, bing_and_nrc_MR) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

##############################################################
######## Most common positive and negative words #############
##############################################################

bing_counts_MR <- MR_token %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_MR

bing_counts_MR %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

#semantic structures is neeeded for semantic meanings

###########################################################
#################                           ###############
###########################################################

library(tidytext)
tidy_MR <- df_MR %>%
  unnest_tokens(word, text)
print(tidy_MR)
#removing stop words
data(stop_words)

MR_no_stop <- tidy_MR %>%
  anti_join(stop_words)
print(MR_no_stop)
#printing the count frequencies for each token without stop words
MR_no_stop %>%
  count(word, sort=TRUE)

#plotting the token frequencies:
library(ggplot2)
freq_hist_MR <-MR_no_stop %>%
  count(word, sort=TRUE) %>%
  filter(n>20) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist_MR)

################################################################################
################################################################################

###############    EMELINA'S PERUVIAN RESTAURANT

################################################################################
################################################################################


link1_EM = "https://www.yelp.com/biz/emelinas-peruvian-restaurant-san-carlos-2?osq=PERUVIAN%20FOOD"
page1_EM = read_html(link1_EM)

page_1_EM = page1_EM %>% html_nodes(".comment__09f24__gu0rG .raw__09f24__T4Ezm") %>% html_text()
page_1_EM

####### page 2
link2_EM = "https://www.yelp.com/biz/emelinas-peruvian-restaurant-san-carlos-2?osq=PERUVIAN%20FOOD&start=10"
page2_EM = read_html(link2_EM)

page_2_EM = page2_EM %>% html_nodes(".comment__09f24__gu0rG .raw__09f24__T4Ezm") %>% html_text()
page_2_EM

###### page 3
link3_EM = "https://www.yelp.com/biz/emelinas-peruvian-restaurant-san-carlos-2?osq=PERUVIAN%20FOOD&start=20"
page3_EM = read_html(link3_EM)

page_3_EM = page3_EM %>% html_nodes(".comment__09f24__gu0rG .raw__09f24__T4Ezm") %>% html_text()
page_3_EM

####### page 4
link4_EM = "https://www.yelp.com/biz/emelinas-peruvian-restaurant-san-carlos-2?osq=PERUVIAN%20FOOD&start=30"
page4_EM = read_html(link4_EM)

page_4_EM = page4_EM %>% html_nodes(".comment__09f24__gu0rG .raw__09f24__T4Ezm") %>% html_text()
page_4_EM
###### page 5
link5_EM = "https://www.yelp.com/biz/emelinas-peruvian-restaurant-san-carlos-2?osq=PERUVIAN%20FOOD&start=40"
page5_EM = read_html(link5_EM)

page_5_EM = page5_EM %>% html_nodes(".comment__09f24__gu0rG .raw__09f24__T4Ezm") %>% html_text()
page_5_EM

####### page 6
link6_EM = "https://www.yelp.com/biz/emelinas-peruvian-restaurant-san-carlos-2?osq=PERUVIAN%20FOOD&start=50"
page6_EM = read_html(link6_EM)

page_6_EM = page6_EM %>% html_nodes(".comment__09f24__gu0rG .raw__09f24__T4Ezm") %>% html_text()
page_6_EM

###### page 7
link7_EM = "https://www.yelp.com/biz/emelinas-peruvian-restaurant-san-carlos-2?osq=PERUVIAN%20FOOD&start=60"
page7_EM = read_html(link7_EM)

page_7_EM = page7_EM %>% html_nodes(".comment__09f24__gu0rG .raw__09f24__T4Ezm") %>% html_text()
page_7_EM

####### page 8
link8_EM = "https://www.yelp.com/biz/emelinas-peruvian-restaurant-san-carlos-2?osq=PERUVIAN%20FOOD&start=70"
page8_EM = read_html(link8_EM)

page_8_EM = page8_EM %>% html_nodes(".comment__09f24__gu0rG .raw__09f24__T4Ezm") %>% html_text()
page_8_EM

###### page 9
link9_EM = "https://www.yelp.com/biz/emelinas-peruvian-restaurant-san-carlos-2?osq=PERUVIAN%20FOOD&start=80"
page9_EM = read_html(link9_EM)

page_9_EM = page9_EM %>% html_nodes(".comment__09f24__gu0rG .raw__09f24__T4Ezm") %>% html_text()
page_9_EM
####### page 10
link10_EM = "https://www.yelp.com/biz/emelinas-peruvian-restaurant-san-carlos-2?osq=PERUVIAN%20FOOD&start=90"
page10_EM = read_html(link10_EM)

page_10_EM = page10_EM %>% html_nodes(".comment__09f24__gu0rG .raw__09f24__T4Ezm") %>% html_text()
page_10_EM



#STRUCTURING THE DATA

list_pages_EM = c(page_1_EM,page_2_EM,page_3_EM,page_4_EM,page_5_EM,page_6_EM,page_7_EM,page_8_EM,page_9_EM,page_10_EM)
df_EM <- as.data.frame(list_pages_EM)

colnames(df_EM)[1] <- "text"


EM_token <- df_EM %>%
  unnest_tokens(word, text)

nrcsurprise_EM <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise") 

#inner joining the MR Kano comments and the surprise sentiments
EM_token %>%
  inner_join(nrcsurprise_EM) %>%
  count(word, sort=T)

########################################################
##### Comparing different sentiment libraries on Emenlinda's Restautant Comments ####
########################################################


afinn_EM <- EM_token %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc_EM <- bind_rows(
  EM_token%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  EM_token %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

library(ggplot2)
bind_rows(afinn_EM, bing_and_nrc_EM) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

##############################################################
######## Most common positive and negative words #############
##############################################################

bing_counts_EM <- EM_token %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_EM

bing_counts_EM %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

#semantic structures is neeeded for semantic meanings

###########################################################
#################                           ###############
###########################################################

library(tidytext)
tidy_EM <- df_EM %>%
  unnest_tokens(word, text)
print(tidy_EM)
#removing stop words
data(stop_words)

EM_no_stop <- tidy_EM %>%
  anti_join(stop_words)
print(EM_no_stop)
#printing the count frequencies for each token without stop words
EM_no_stop %>%
  count(word, sort=TRUE)

#plotting the token frequencies:
library(ggplot2)
freq_hist_EM <-EM_no_stop %>%
  count(word, sort=TRUE) %>%
  filter(n>20) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist_EM)

####################################################################################################
############################  N-grams Analysis              ##########################################
####################################################################################################


df_res <- rbind(df, df_MR, df_EM)

#We will tokenize the data by ngram by ngram no but word

res_bigrams <- df_res %>%
  unnest_tokens(bigram, text, token = 'ngrams', n=2)
# The location information is book
# bigram = we have now pair of tokens

res_bigrams #We want to see the bigrams (words that appear together, "pairs")

res_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 


#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_separated <- res_bigrams %>%
  separate(bigram, c("word1","word2"), sep = " ") # Separating the bigrams into 2 tokens per observation
# the output is 2 separate tokens

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>% #! exclamation sign removes
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words": 
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

bigram_counts
#want to see the new bigrams

#The output is the name and the adjective, noun and adjective

###########################################################
###### What if we are interested in the most common #######
################ 4 consecutive words - quadro-gram ########
###########################################################
quadrogram <- df_res %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) 

quadrogram_counts <- quadrogram %>%
                    count(word1, word2, word3, word4,  sort = TRUE)

quadrogram_counts
######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################

#install.packages("igraph")
library(igraph)
bigram_graph <- bigram_counts %>%
  filter(n>20) %>% #for our own project n small
  graph_from_data_frame()

bigram_graph

#install.packages("ggraph")
library(ggraph)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+    #we have 2 geometrics edge and node
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1) # we are labeling our nodes

#questions for business insights what are the nodes how are conected in this semantic map

## making sure we have the data

library(dplyr)
library(tidytext)


#############################################
####We want to combine all the datasets and do frequencies 
#############################################
library(tidyr)
library(stringr)
frequency <- bind_rows(mutate(jora_no_stop, author="Jora Restaurant"),
                       mutate(MR_no_stop, author= "MR Kano Restaurant"),
                       mutate(EM_no_stop, author="Emelinda Restaurant")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `MR Kano Restaurant`, `Emelinda Restaurant`)

#let's plot the correlograms:
library(scales)
ggplot(frequency, aes(x=proportion, y=`Jora Restaurant`, 
                      color = abs(`Jora Restaurant`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Jora Restaurant", x=NULL)

##########################################
##doing the cor.test() ################
##########################################

cor.test(data=frequency[frequency$author == "MR Kano Restaurant",],
         ~proportion + `Jora Restaurant`)

cor.test(data=frequency[frequency$author == "Emelinda Restaurant",],
         ~proportion + `Jora Restaurant`)

###################################################################3

library(tidytext)
tidy_res <- df_res %>%
  unnest_tokens(word, text)
print(tidy_res)
#removing stop words
data(stop_words)

res_no_stop <- tidy_res %>%
  anti_join(stop_words)
print(res_no_stop)
#printing the count frequencies for each token without stop words
res_no_stop %>%
  count(word, sort=TRUE)

#plotting the token frequencies:
library(ggplot2)
freq_hist_res <-res_no_stop %>%
  count(word, sort=TRUE) %>%
  filter(n>100) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist_res)

