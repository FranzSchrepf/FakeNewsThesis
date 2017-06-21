######## Data Preperation ########
require(tidyverse)
require("tm")
require(XML)
require(stringr)
require(knitr)
require(bitops)
require(rvest)
require(data.table)
require(openNLP)
require(openNLPmodels.en) # which are offered by the university of vienna
require(qdap)
require("quanteda")

#Load data
# Clean list
rm(list=ls())
#Set working directory
getwd()
#setwd("")
# Get Data
df <- read.csv("raw_data.csv")
df$X <- NULL

##### Last bit of cleaning up ###
df$Article <- gsub('[????To]', '', df$Article)
#control
df %>% filter(Id == 419) %>% select(Article) %>% print()

###### Explanation Data ######
#How many fake? (fake = 170, real = 199)
df %>% filter(fake == "x") %>% count()
df %>% filter(fake == "") %>% count()
#How many publications and where published??
library(gridExtra)
n_publishers <- df %>% group_by(publication, fake) %>% summarise(total.count=n()) 

df%>% select(publication, fake) %>% filter(fake == "") %>% count(publication)

#How many shares on avg?
df %>% select(engagements, fake) %>% filter(fake == "x") %>% summary("engagements")
df_high <- df %>% filter(engagements > 2000000)
#Turn Engagements from factor to numeric
df$engagements <- as.numeric(as.character(df$engagements))
#Fake news outlets average engagements
avg_eng_publ <- df %>% 
  group_by(fake, publication, engagements) %>% summary()

#Distribution of engagements
ggplot(df, aes(x = engagements, y = publication)) + geom_point()

####### Basic Analysis #######
#Count amount of paragraphs
df$paragraphs <- sapply(gregexpr("\\|", df$Article), length)
df$Article <- gsub("\\|", "", df$Article)

#Count article length (in words)
df$article_length_words <- sapply(gregexpr("\\S+", df$Article), length) 
df$article_length_words <- as.numeric(df$article_length_words)
#df$article_length2 <- word_count(df$Article) # not as accurate

#Character count (without and with spaces) #uses qdap package
df$characters_without <- character_count(df$Article)
df$characters_with <- character_count(df$Article , count.space = TRUE)

# Number of sentences # uses quanteda packages
df$n_sentences <- nsentence(df$Article)

#Avg. Sentence length
df$avg_sentence_length <- (df$article_length / df$n_sentences)

#Count amount of expressive punctuation, NOTE: many swearwords censored with *
df$text_ex_punct <- sapply(gregexpr("[!?]", df$Article), length)
#df$more_3_excl  <- sapply(gregexpr("!!!", df$Article), length)

#count occurrences of special characters and symbols (WHICH SYMBOLS SHOULD BE COUNTED?)
df$text_symbols <- sapply(gregexpr("[$?%*#~+-@??????\\(\\)?]", df$Article), length) 

#count upper case letters for each article
df$text_upper_letters <- sapply(regmatches(df$Article, gregexpr("[A-Z]", df$Article, perl=TRUE)), length)

#count digits in text
df$text_digits <- sapply(gregexpr("[0-9]", df$Article), length)

#####Title Analysis ######
#Count headline length (in words)
df$title_length_words <- sapply(gregexpr("\\S+", df$title), length) 
df$title_length_words <- as.numeric(df$title_length_words)
#df$title_length2 <- word_count(df$title) # not as accurate

#Character count (without and with spaces) #uses qdap package
df$title_char_without <- character_count(df$title)
df$title_char_with <- character_count(df$title , count.space = TRUE)


#Count amount of expressive punctuation, NOTE: many swearwords censored with *
df$title_ex_punct <- sapply(gregexpr("[!?]", df$title), length)
#df$more_3_excl  <- sapply(gregexpr("!!!", df$title), length)

#count occurrences of special characters and symbols (WHICH SYMBOLS SHOULD BE COUNTED?)
df$title_symbols <- sapply(gregexpr("[$?%*#~+-@??????\\(\\)?]", df$title), length) 

#count upper case letters for each title
df$title_upper_letters <- sapply(regmatches(df$title, gregexpr("[A-Z]", df$title, perl=TRUE)), length)

#count digits in text
df$title_digits <- sapply(gregexpr("[0-9]", df$title), length)

### Sentiment Analysis #####
library(stringr)
library(tidyverse)
library("tidytext")

#check lexicons
get_sentiments("nrc")
get_sentiments("afinn")
get_sentiments("bing")

# Turn into tidy text readable Tibble
##### Repeat Analysis without TRUMP ####
df$Article_wt_TRUMP <- gsub('Trump', ' ', df$Article)

publication <- df$publication
articles <- df$Article_wt_TRUMP
series <- tibble()

for(i in seq_along(publication)) {
  
  clean <- tibble(article = seq_along(articles[[i]]),
                  text = articles[[i]]) %>%
    unnest_tokens(word, text) %>%
    mutate(publication = publication[i]) %>%
    select(publication, everything())
  
  series <- rbind(series, clean)
}

# Most popular words (finding: word "TRUMP" is screwing up analysis)
bing_word_counts <- series %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, n), n, fill = sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL, title = "Most common sentiment words", subtitle = "Excluding the word 'Trump' ") +
  coord_flip()

## Check sentiment in general #####
library("gridExtra")
# In General
sentiment_general <- series %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)

png("sentiment_general.png")
grid.table(sentiment_general)
dev.off()

# Factual News
real_news <- df %>% filter(fake != "x") %>% select(Id) 

#("abc news", "business insider", "buzzfeed.com",
#            "cbs news", "cnn", "fox news", "huffington post",
#           "los angeles times", "national public radio",
#          "nbc news", "new york post", "new york times",
#         "ny daily news", "the guardian", "usa today",
#        "vox", "vox news", "wall street journal",
#       "washington post")

series_real <- series %>% filter(ID <= 238)

sentiment_real <- series_real %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)

png("sentiment_real.png")
grid.table(sentiment_real)
dev.off()

#Fake news
series_fake <- series %>% filter(ID > 238)

sentiment_fake <- series_fake %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)

png("sentiment_fake.png")
grid.table(sentiment_fake)
dev.off()

series_fake %>% count(articles)
series_real %>% count(articles)

##### Sentiment of individual articles ######
df$Article_wt_TRUMP <- gsub('Trump', ' ', df$Article)

Id <- df$Id
articles <- df$Article_wt_TRUMP
series <- tibble()

for(i in seq_along(Id)) {
  
  clean <- tibble(chapter = seq_along(Id[[i]]),
                  text = articles[[i]]) %>%
    unnest_tokens(word, text) %>%
    mutate(ID = Id[i]) %>%
    select(ID, everything())
  
  series <- rbind(series, clean)
}

# Create new data frame which counts sentiment per Id
sent <- series %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment))

#Backup
#df <- read.csv("basic_analysis_words.csv")

#positive
sent_positive <- sent %>% select(ID, sentiment) %>% filter(sentiment == "positive") %>% group_by(ID) %>% count(sentiment) 
sent_positive$sentiment <- NULL
names(sent_positive)[names(sent_positive)=="n"] <- "positive_sent"
df$text_positive <- sent_positive$positive_sent[match(df$Id, sent_positive$ID)]

#negative
sent_negative <- sent %>% select(ID, sentiment) %>% filter(sentiment == "negative") %>% group_by(ID) %>% count(sentiment) %>% filter(!is.na(ID)) 
sent_negative$sentiment <- NULL
names(sent_negative)[names(sent_negative)=="n"] <- "negative_sent"
df$text_negative <- sent_negative$negative_sent[match(df$Id, sent_negative$ID)]

#trust
sent_trust <- sent %>% select(ID, sentiment) %>% filter(sentiment == "trust") %>% group_by(ID) %>% count(sentiment) %>% filter(!is.na(ID)) 
sent_trust$sentiment <- NULL
names(sent_trust)[names(sent_trust)=="n"] <- "trust_sent"
df$text_trust <- sent_trust$trust_sent[match(df$Id, sent_trust$ID)]

#fear
sent_fear <- sent %>% select(ID, sentiment) %>% filter(sentiment == "fear") %>% group_by(ID) %>% count(sentiment) %>% filter(!is.na(ID)) 
sent_fear$sentiment <- NULL
names(sent_fear)[names(sent_fear)=="n"] <- "fear_sent"
df$text_fear <- sent_fear$fear_sent[match(df$Id, sent_fear$ID)]

#anticipation
sent_anticipation <- sent %>% select(ID, sentiment) %>% filter(sentiment == "anticipation") %>% group_by(ID) %>% count(sentiment) %>% filter(!is.na(ID)) 
sent_anticipation$sentiment <- NULL
names(sent_anticipation)[names(sent_anticipation)=="n"] <- "anticipation_sent"
df$text_anticipation <- sent_anticipation$anticipation_sent[match(df$Id, sent_anticipation$ID)]

#anger
sent_anger <- sent %>% select(ID, sentiment) %>% filter(sentiment == "anger") %>% group_by(ID) %>% count(sentiment) %>% filter(!is.na(ID)) 
sent_anger$sentiment <- NULL
names(sent_anger)[names(sent_anger)=="n"] <- "anger_sent"
df$text_anger <- sent_anger$anger_sent[match(df$Id, sent_anger$ID)]

#sadness
sent_sadness <- sent %>% select(ID, sentiment) %>% filter(sentiment == "sadness") %>% group_by(ID) %>% count(sentiment) %>% filter(!is.na(ID)) 
sent_sadness$sentiment <- NULL
names(sent_sadness)[names(sent_sadness)=="n"] <- "sadness_sent"
df$text_sadness <- sent_sadness$sadness_sent[match(df$Id, sent_sadness$ID)]

#joy
sent_joy <- sent %>% select(ID, sentiment) %>% filter(sentiment == "joy") %>% group_by(ID) %>% count(sentiment) %>% filter(!is.na(ID)) 
sent_joy$sentiment <- NULL
names(sent_joy)[names(sent_joy)=="n"] <- "joy_sent"
df$text_joy <- sent_joy$joy_sent[match(df$Id, sent_joy$ID)]

#disgust
sent_disgust <- sent %>% select(ID, sentiment) %>% filter(sentiment == "disgust") %>% group_by(ID) %>% count(sentiment) %>% filter(!is.na(ID)) 
sent_disgust$sentiment <- NULL
names(sent_disgust)[names(sent_disgust)=="n"] <- "disgust_sent"
df$text_disgust <- sent_disgust$disgust_sent[match(df$Id, sent_disgust$ID)]

#surprise
sent_surprise <- sent %>% select(ID, sentiment) %>% filter(sentiment == "surprise") %>% group_by(ID) %>% count(sentiment) %>% filter(!is.na(ID)) 
sent_surprise$sentiment <- NULL
names(sent_surprise)[names(sent_surprise)=="n"] <- "surprise_sent"
df$text_surprise <- sent_surprise$surprise_sent[match(df$Id, sent_surprise$ID)]

#set NA's to 0
df[is.na(df)] <- 0

## total sentiment
df$text_sentiment <- head(df[28:37]) %>% rowSums()

##### Headline Sentiment ######
df$title_wt_TRUMP <- gsub('Trump', ' ', df$title)

Id <- df$Id
titles <- df$title_wt_TRUMP
series <- tibble()

for(i in seq_along(Id)) {
  
  clean <- tibble(chapter = seq_along(Id[[i]]),
                  text = titles[[i]]) %>%
    unnest_tokens(word, text) %>%
    mutate(ID = Id[i]) %>%
    select(ID, everything())
  
  series <- rbind(series, clean)
}

# Create new data frame which counts sentiment per Id
sent <- series %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment))

#positive
sent_positive <- sent %>% select(ID, sentiment) %>% filter(sentiment == "positive") %>% group_by(ID) %>% count(sentiment) 
sent_positive$sentiment <- NULL
names(sent_positive)[names(sent_positive)=="n"] <- "positive_sent"
df$title_positive <- sent_positive$positive_sent[match(df$Id, sent_positive$ID)]

#negative
sent_negative <- sent %>% select(ID, sentiment) %>% filter(sentiment == "negative") %>% group_by(ID) %>% count(sentiment) %>% filter(!is.na(ID)) 
sent_negative$sentiment <- NULL
names(sent_negative)[names(sent_negative)=="n"] <- "negative_sent"
df$title_negative <- sent_negative$negative_sent[match(df$Id, sent_negative$ID)]

#trust
sent_trust <- sent %>% select(ID, sentiment) %>% filter(sentiment == "trust") %>% group_by(ID) %>% count(sentiment) %>% filter(!is.na(ID)) 
sent_trust$sentiment <- NULL
names(sent_trust)[names(sent_trust)=="n"] <- "trust_sent"
df$title_trust <- sent_trust$trust_sent[match(df$Id, sent_trust$ID)]

#fear
sent_fear <- sent %>% select(ID, sentiment) %>% filter(sentiment == "fear") %>% group_by(ID) %>% count(sentiment) %>% filter(!is.na(ID)) 
sent_fear$sentiment <- NULL
names(sent_fear)[names(sent_fear)=="n"] <- "fear_sent"
df$title_fear <- sent_fear$fear_sent[match(df$Id, sent_fear$ID)]

#anticipation
sent_anticipation <- sent %>% select(ID, sentiment) %>% filter(sentiment == "anticipation") %>% group_by(ID) %>% count(sentiment) %>% filter(!is.na(ID)) 
sent_anticipation$sentiment <- NULL
names(sent_anticipation)[names(sent_anticipation)=="n"] <- "anticipation_sent"
df$title_anticipation <- sent_anticipation$anticipation_sent[match(df$Id, sent_anticipation$ID)]

#anger
sent_anger <- sent %>% select(ID, sentiment) %>% filter(sentiment == "anger") %>% group_by(ID) %>% count(sentiment) %>% filter(!is.na(ID)) 
sent_anger$sentiment <- NULL
names(sent_anger)[names(sent_anger)=="n"] <- "anger_sent"
df$title_anger <- sent_anger$anger_sent[match(df$Id, sent_anger$ID)]

#sadness
sent_sadness <- sent %>% select(ID, sentiment) %>% filter(sentiment == "sadness") %>% group_by(ID) %>% count(sentiment) %>% filter(!is.na(ID)) 
sent_sadness$sentiment <- NULL
names(sent_sadness)[names(sent_sadness)=="n"] <- "sadness_sent"
df$title_sadness <- sent_sadness$sadness_sent[match(df$Id, sent_sadness$ID)]

#joy
sent_joy <- sent %>% select(ID, sentiment) %>% filter(sentiment == "joy") %>% group_by(ID) %>% count(sentiment) %>% filter(!is.na(ID)) 
sent_joy$sentiment <- NULL
names(sent_joy)[names(sent_joy)=="n"] <- "joy_sent"
df$title_joy <- sent_joy$joy_sent[match(df$Id, sent_joy$ID)]

#disgust
sent_disgust <- sent %>% select(ID, sentiment) %>% filter(sentiment == "disgust") %>% group_by(ID) %>% count(sentiment) %>% filter(!is.na(ID)) 
sent_disgust$sentiment <- NULL
names(sent_disgust)[names(sent_disgust)=="n"] <- "disgust_sent"
df$title_disgust <- sent_disgust$disgust_sent[match(df$Id, sent_disgust$ID)]

#surprise
sent_surprise <- sent %>% select(ID, sentiment) %>% filter(sentiment == "surprise") %>% group_by(ID) %>% count(sentiment) %>% filter(!is.na(ID)) 
sent_surprise$sentiment <- NULL
names(sent_surprise)[names(sent_surprise)=="n"] <- "surprise_sent"
df$title_surprise <- sent_surprise$surprise_sent[match(df$Id, sent_surprise$ID)]

#set NA's to 0
df[is.na(df)] <- 0

## total sentiment
df$title_sentiment <- df[40:49] %>% rowSums()

#### READABILITY SCORES #####
library("readability")

read_df <- with(df, readability(Article, list(Id)))

df <- merge(df, read_df, by.x = "Id", by.y = "Id")

#### POS Tagging ######
#still rather.... experimental 
#https://stackoverflow.com/questions/43844929/count-pos-tags-by-column
library(stringr)
#Spliting into sentence based on carriage return
posText <- df$Article
s <- unlist(lapply(posText, function(x) { str_split(x, "\n") }))
#uses NLP and OPEN NLP library

tagPOS <-  function(x, ...) {
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)
}

result <- lapply(s,tagPOS)
head(result) <- as.data.frame(do.call(rbind,result))

# Using TM to create Dataframe and merge it with df
POS_data <- data.frame(ID = "", 
                       tags = c(paste("NN"), 
                                paste("DT", "NN", "VBD", ",", "WP", "VBP", "PRP", "VBG", "TO", "VB", ".")))##

corpus <- Corpus(VectorSource(result$POStags))
docs <- corpus
docs2 <- corpus
# fix issues
paste(docs2[1])
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "c")
docs <- tm_map(docs, toSpace, "[()]")

#default minimum wordlength is 3, so make sure you change this
dtm <- DocumentTermMatrix(corpus, control= list(wordLengths=c(1,Inf)))

#coerce into dataframe
dtm<- as.data.frame(as.matrix(dtm))
head(dtm)

dtm$c <- NULL
dtm$punctuation <- sapply(gregexpr("[.;:#$''(),?!??]", df$Article), length)
dtm$ID <- df$Id

## Figure out WTF is going on there and merge columns if necessary
#http://blog.dpdearing.com/2011/12/opennlp-part-of-speech-pos-tags-penn-english-treebank
dtm$VERB <- dtm$vb + dtm$vbd + dtm$vbg+ dtm$vbn + dtm$vbp + dtm$vbz + dtm$md
dtm$NOUN <- dtm$nn + dtm$nnp + dtm$nnps + dtm$nns
dtm$PRON <- dtm$prp + dtm$wp
dtm$ADJ <- dtm$jj + dtm$jjr + dtm$jjs
dtm$ADV <- dtm$rb + dtm$rbr + dtm$rbs + dtm$wrb
names(dtm)[names(dtm)=="in."] <- "ADP"
dtm$CONJ <- dtm$cc
dtm$DET <- dtm$dt + dtm$pdt + dtm$ex + dtm$wdt #predeterminer part of determiners?
dtm$NUM <- dtm$cd
dtm$PRT <- dtm$pos + dtm$rp + dtm$to
dtm$x <- dtm$fw + dtm$ls +  dtm$uh + dtm$sym
dtm <- dtm[, -c(1:4)]
dtm <- dtm[, -c(2:32)]
head(dtm)

#### Merging POS with Dataframe #####
df <- merge.data.frame(df, dtm, by.x = "Id", by.y = "ID")
colnames(df)

### Normailize columns when necessary
### Normalize all columns ###
#########TITLE
# title joy
df$norm_title_joy <- 
  df$title_joy /
  df$title_length_words

# title sadness
df$norm_title_sadness <- 
  df$title_sadness /
  df$title_length_words

# title surprise
df$norm_title_surprise <- 
  df$title_surprise /
  df$title_length_words

# title trust
df$norm_title_trust <- 
  df$title_trust /
  df$title_length_words

# title negative
df$norm_title_negative <- 
  df$title_negative /
  df$title_length_words

# title positive
df$norm_title_positive <- 
  df$title_positive /
  df$title_length_words

#title sentiment
df$norm_title_sentiment <- 
  df$title_sentiment/
  df$title_length_words
#Real stuff
# average word length # iwithout whitespaces
df$norm_title_word_length <- 
  df$title_char_without/
  df$title_length_words

# expressive punctuation per words
df$norm_title_ex_punct <- 
  df$title_ex_punct/
  df$title_length_words

# symbols title per word
df$norm_title_symbols<- 
  df$title_symbols/
  df$title_length_words

# nr uppercase words
df$norm_title_uppercase <- 
  df$title_upper_letters/
  df$title_length_words

# nr digit words
df$norm_title_digits <-  
  df$title_digits/
  df$title_length_words

#######ARTICLE
# paragraphs
df$norm_paragraphs <- 
  df$paragraphs/
  df$article_length_words

# average word length
df$norm_av_word_length <- 
  df$characters_without/
  df$article_length_words

# exclamations per words
df$norm_text_ex_punct <- 
  df$text_ex_punct/
  df$article_length_words

# symbols 
df$norm_text_symbols<- 
  df$text_symbols/
  df$article_length_words

# nr uppercase words
df$norm_nr_uppercase_letters <- 
  df$text_upper_letters/
  df$article_length_words

# nr digit words
df$norm_digit_words <- 
  df$text_digits/
  df$article_length_words

# text anger 
df$norm_text_anger <- 
  df$text_anger/
  df$article_length_words

# text anticipation
df$norm_text_anticipation <- 
  df$text_anticipation/
  df$article_length_words

# text disgust
df$norm_text_disgust <- 
  df$text_disgust/
  df$article_length_words

# text fear 
df$norm_text_fear <- 
  df$text_fear/
  df$article_length_words

# text joy 
df$norm_text_joy <- 
  df$text_joy/
  df$article_length_words

# text sadness
df$norm_text_sadness <- 
  df$text_sadness/
  df$article_length_words

# text surprise
df$norm_text_surprise <- 
  df$text_surprise/
  df$article_length_words

# text trust
df$norm_text_trust <- 
  df$text_trust/
  df$article_length_words

# text negative
df$norm_text_negative <- 
  df$text_negative/
  df$article_length_words

# text positive
df$norm_text_positive <- 
  df$text_positive/
  df$article_length_words

#text sentiment
df$norm_text_sentiment <- df$text_sentiment/df$article_length_words

#POS TAGS
# ADP 
df$norm_ADP <- 
  df$ADP/
  df$article_length_words

# punctuation 
df$norm_punctuation <- 
  df$punctuation/
  df$article_length_words

# VERB
df$norm_VERB <- 
  df$VERB/
  df$article_length_words

# NOUN
df$norm_NOUN <- 
  df$NOUN/
  df$article_length_words

# PRON
df$norm_PRON <- 
  df$PRON/
  df$article_length_words

# ADJ
df$norm_ADJ <- 
  df$ADJ/
  df$article_length_words

# ADV
df$norm_ADV <- 
  df$ADV/
  df$article_length_words

# CONJ
df$norm_CONJ <- 
  df$CONJ/
  df$article_length_words

# DET
df$norm_DET <- 
  df$DET/
  df$article_length_words

# NUM
df$norm_NUM <- 
  df$NUM/
  df$article_length_words

# PRT
df$norm_PRT <- 
  df$PRT/
  df$article_length_words

# x
df$norm_x <- 
  df$x/
  df$article_length_words

##### Word Stemming ####
#Create Corpus
m <- list(ID = "Id", content = "Article")
myReader <- readTabular(mapping = m)
mycorpus <- Corpus(DataframeSource(df), readerControl = list(reader = myReader))
for (i in 1:length(mycorpus)) {
  attr(mycorpus[[i]], "ID") <- df$Id[i]
}
docs <- mycorpus
#Transform corpus
as.character(docs[[357]])
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]"," ",x)
docs <- tm_map(docs, content_transformer(tolower)) 
docs <- tm_map(docs, removeSpecialChars)
docs <- tm_map(docs, removePunctuation) 
docs <- tm_map(docs, removeNumbers) 
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace) 
docs <- tm_map(docs, PlainTextDocument)
docs <- tm_map(docs, stemDocument, language = "english")

dtm <- DocumentTermMatrix(docs)
tdm <- TermDocumentMatrix(docs)
as.character(docs[[357]])

# Amount of different words per article (for this, first stemmed and removed numbers etc. then counted words per doc)
df$text_dif_words <- rowSums(as.matrix(dtm) != 0)

#normalizing text_dif_words
df$norm_text_dif_words <- 
  df$text_dif_words/
  df$article_length_words

inspect(dtm)
dim(dtm)
#dtm_sums <- dtm[findFreqTerms(dtm)[1:10228],] %>%
#  as.matrix() %>%
#  rowSums()

dtm_df <- as.data.frame(as.matrix(dtm))
rownames(dtm_df) <- df$Id
dtm_df_top150 <- dtm_df[,colSums(dtm_df) > 156]
df<- merge(df, dtm_df_top150, by.x = "Id", by.y = "row.names" )

colnames(dtm_df_top150)
## Formating
df$Article_wt_TRUMP <- NULL
df$title_wt_TRUMP <- NULL
colnames(df)

####### Save Data ######
write("complete_dataset.csv", x = df)

##### Prepare for analysis #######
AF <- df 
#Delete Irrelevant Columns
AF$publication <- NULL
AF$engagements <- NULL
AF$html_code <- NULL
AF$url <- NULL
AF$X <- NULL
AF$save_date <- NULL
AF$Article <- NULL
AF$title <- NULL
AF$X.x <- NULL
AF$X.1 <- NULL
AF$X.y <- NULL
#AF$Id <- NULL
#changing fake
AF$fake <- as.numeric(AF$fake) - 1

###### SAVE AF data frame #####
write.csv("prepared_for_analysis.csv", x = AF, row.names = FALSE)

