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
    ownerUsername,
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
    allHashtags = {
      # Alle Hashtags kombinieren
      hashtags_list <- c(hashtags.0, hashtags.1, hashtags.2, hashtags.3, hashtags.4, 
                         hashtags.5, hashtags.6, hashtags.7, hashtags.8, hashtags.9, 
                         hashtags.10, hashtags.11)
      
      # Die unerwünschten Hashtags herausfiltern
      hashtags_filtered <- setdiff(hashtags_list, c("tagesschau", "nachrichten"))
      
      # Die gefilterten Hashtags als Zeichenkette (Vektor) zurückgeben
      paste(hashtags_filtered, collapse = ", ")
    },
    caption_length = nchar(caption),  # Korrekte Berechnung der Caption-Länge
    word_count = length(strsplit(caption, "\\s+")[[1]])  # Berechnung der Wortanzahl
  ) %>%
  ungroup()

# Zusammengefasste Reihen rausgefiltert und fertig gereinigtes Datenset überschreiben
df <- df %>%
  select(-starts_with("taggedUsers"))

## Vorbereitung für Topic Modeling - Deskreptive Statistik zur Caption 
Q1 <- quantile(df$word_count, 0.25)
avg_caption_length <- round(mean(df$word_count),2)
min_caption_length <- round(min(df$word_count),2)
max_caption_length <- round(max(df$word_count),2)
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
topic_terms <- terms(topic_model, 13)  # 13 wichtigste Wörter pro Topic
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


# 1. Berechne die Top 10 Dokumente (Zeilen) mit der höchsten Wahrscheinlichkeit für jedes Thema
top_documents_per_topic <- apply(theta, 2, function(topic_probs) {
  order(topic_probs, decreasing = TRUE)[1:10]  # Holen der Indizes der Top 10 Dokumente
})

# 2. Erstelle eine Liste, um die Top Hashtags für jedes Thema zu speichern
top_hashtags_per_topic <- list()

# 3. Iteriere über jedes Thema und extrahiere die Hashtags für die Top 10 Dokumente
for (topic_idx in 1:K) {
  
  # Holen der Top 10 Dokumente für das aktuelle Thema
  top_docs <- top_documents_per_topic[, topic_idx]
  # Extrahiere die Hashtags für diese Top 10 Dokumente
  hashtags_list <- df$allHashtags[top_docs]
  
  # Kombiniere alle Hashtags in einem Vektor
  hashtags_combined <- unlist(hashtags_list)
  
  # Entferne duplizierte Hashtags und zeige die einzigartigen Hashtags
  unique_hashtags <- unique(hashtags_combined)
  
  # Top 13 Begriffe für das Thema aus dem LDA-Modell (aus topic_terms)
  top_topic_terms <- topic_terms[, topic_idx]  # Die Top 13 Begriffe für das Thema
  top_topic_terms_combined <- paste(top_topic_terms, collapse = ", ")
  
  # Ausgabe der Top-Hashtags für das Thema
  print(paste("Thema", topic_idx,topic_idx, "- Top Begriffe:", top_topic_terms_combined ))
  print(unique_hashtags)
  cat("\n")
}


## Hypothesentest
# H0: Die durchschnittliche Anzahl an Kommentaren/Likes ist für alle Themen gleich.
# H1: Es gibt Unterschiede in der durchschnittlichen Anzahl an Kommentaren/Likes zwischen den Themen.

# Zuweisung des wahrscheinlichsten Themas für jedes Dokument
df$topic <- apply(theta, 1, which.max)

# Berechnung der durchschnittlichen Kommentare für jedes Thema
avg_comments_per_topic <- aggregate(df$commentsCount, by = list(topic = df$topic), FUN = mean)
colnames(avg_comments_per_topic)[2] <- "avg_comments"

# Ausgabe der durchschnittlichen Kommentare pro Thema
print(avg_comments_per_topic)

# Einweg-ANOVA durchführen
anova_result <- aov(commentsCount ~ factor(topic), data = df)
summary(anova_result)


# lineare regression
lm_result <- lm(commentsCount ~ factor(topic), data = df)
summary(lm_result)




