# Workspace
#setwd("./desktop/studium/semester_I/DH/dh-tagesschau-analysis")

# Library Import Statements
options(stringsAsFactors = FALSE)
library(dplyr)
library(stringr)
library(quanteda)
require(topicmodels)
library(reshape2)
library(pals)
library(ggplot2)
library(lubridate)

# CSV Datei Einlesen
dat <- read.csv("tagesschau_instagram_posts.csv",
                header = TRUE, encoding = "UTF-8")

# Reihen und Zeilen des Datensatz
dim(dat)

# Data Cleaning
df <- dat %>%
  filter(
    ownerUsername == "tagesschau",
    as.Date(timestamp) > as.Date("2024-07-31")
  ) %>%
  select(
    caption,
    commentsCount,likesCount,
    hashtags.0, hashtags.1, hashtags.2, hashtags.3, hashtags.4,
    hashtags.5, hashtags.6, hashtags.7, hashtags.8, hashtags.9,
    hashtags.10, hashtags.11,
    mentions.0, mentions.1, mentions.2,
    ownerUsername,
    taggedUsers.0.username, taggedUsers.1.username, taggedUsers.2.username, taggedUsers.3.username,
    timestamp, type, url,
    videoDuration, videoPlayCount, videoUrl, videoViewCount
  )

# Zusammenfassung aller dynamischen Reihen taggedUsers.0 etc.
# Funktion zur Zusammenfassung als kommagetrennte Liste (als String)
df_summarize <- function(strings) {
  result <- c()  
  for (item in strings) {
    if (item != "" && !is.na(item)) {  
      result <- c(result, item)        
    }
  }
  paste(result, collapse = ",")        
}

df <- df %>%
  rowwise() %>%
  mutate(
    allTaggedUsernames = df_summarize(
      c(taggedUsers.0.username, taggedUsers.1.username, taggedUsers.2.username, taggedUsers.3.username)
    ),
    allMentions = df_summarize(
      c(mentions.0, mentions.1, mentions.2)
    ),
    allHashtags = df_summarize(
      c(hashtags.0, hashtags.1, hashtags.2, hashtags.3, hashtags.4, 
        hashtags.5, hashtags.6, hashtags.7, hashtags.8, hashtags.9, 
        hashtags.10, hashtags.11)
    ),
    caption_length = nchar(caption),  # Korrekte Berechnung der Caption-Länge
    word_count = length(strsplit(caption, "\\s+")[[1]])  # Berechnung der Wortanzahl
  ) %>%
  ungroup()

# Zusammengefasste Reihen rausgefiltert und fertig gereinigtes Datenset überschreiben
df <- df %>%
  select(-starts_with("taggedUsers"), -starts_with("mentions"), -starts_with("hashtags"))

## Vorbereitung für Topic Modeling - Deskreptive Statistik zur Caption 
Q1 <- quantile(df$word_count, 0.25)
avg_caption_length <- round(mean(df$word_count),2)
min_caption_length <- round(min(df$word_count),2)
max_caption_length <- round(max(df$word_count),2)
max_caption_length <- round(max(Q1),2)
print(paste("Durchschnittliche Wörter in Posting Caption:", avg_caption_length))
print(paste("Maximale Anzahl an Wörter in Posting Caption:", max_caption_length))
print(paste("Minimale Anzahl an Wörter in Posting Caption:", min_caption_length))
print(paste("25% der Werte liegen unter:", Q1))


## basierenden darauf filtern der unteren outlier (keine iqr)
df <- df %>%
  filter(word_count > Q1) %>%
  mutate(
    caption = str_replace_all(caption, "#\\S+", ""),  # entfernt # und das folgende Wort
    caption = str_squish(caption),  
    allHashtags = str_replace_all(allHashtags, ",tagesschau", ""),
    allHashtags = str_replace_all(allHashtags, ",nachrichten", "") # entfernt tageschau und nachrichten aus #
  )


# corpus objekt erstellen
corpus_data <- corpus(df$caption, docnames = df$timestamp)

lemma_data <- read.table("lemmatization-de.txt", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
colnames(lemma_data) <- c("inflected_form", "lemma")

stopwords_de <- readLines("stopwords-de.txt",
                          encoding = "UTF-8", warn = FALSE) 

# Tokenization und Cleaning
tokens <- tokens(corpus_data, 
                 remove_punct = TRUE, 
                 remove_numbers = TRUE, 
                 remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma,
                 valuetype = "fixed") %>%
  tokens_remove(pattern = stopwords_de, padding = T)

# Erstelle DTM
dtm <- tokens %>%
  tokens_remove("") %>%
  dfm() %>%
  dfm_trim(min_docfreq = 3)

top_terms <- c("ims", "innen", "tagesschau", "nachrichten",
                 "solles", "untere", "new", "", "abern", "the", "sogar", "laute",
               "tagesschau.de", "unseren", "news", "enden", "heißt", "guten", "morgens", "infos",
               "ums", "gesollt", "gemußt")

dtm <- dtm[, !(colnames(dtm) %in% top_terms)]

sel_idx <- rowSums(dtm) > 0
dtm <- dtm[sel_idx, ]

# Anzahl der Topics (K) - kann optimiert werden!
K <- 12

# LDA-Topic Modeling mit Gibbs Sampling
topic_model <- LDA(dtm, K, method="Gibbs", control=list(
  iter = 2000,      
  alpha = 0.01,     
  seed = 42,      
  verbose = 50,
  thin = 100
))

# Ergebnisse aus dem Modell extrahieren
topic_terms <- terms(topic_model, 13)  # 10 wichtigste Wörter pro Topic
print(topic_terms)

tmResult <- posterior(topic_model)
theta <- tmResult$topics
topicNames <- apply(topic_terms, 2, paste, collapse = " ")

df$timestamp <- ymd_hms(df$timestamp)
df$kalenderwoche <- week(df$timestamp)

# Berechnung der durchschnittlichen Topic-Proportionen pro Kalenderwoche
topic_proportion_per_week <- aggregate(theta,
                                       by = list(kalenderwoche = df$kalenderwoche), 
                                       mean)

# Setzen der Topic-Namen für die aggregierten Spalten
colnames(topic_proportion_per_week)[2:(K+1)] <- topicNames

# Umformen des DataFrames für die Visualisierung
vizDataFrame <- melt(topic_proportion_per_week, id.vars = "kalenderwoche")

vizDataFrame$kalenderwoche <- factor(vizDataFrame$kalenderwoche, 
                                     levels = c(30:52, 0:29))

ggplot(vizDataFrame, aes(x = kalenderwoche, y = value, fill = variable)) +
  geom_bar(stat = "identity") + 
  ylab("Proportion") +
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "Topic") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Topic Proportions per Kalenderwoche")





