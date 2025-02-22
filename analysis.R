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
dat <- read.csv("./auxilary/tagesschau_instagram_posts.csv",
                header = TRUE, encoding = "UTF-8")

# Reihen und Zeilen des Datensatz
dim(dat)

### Data Cleaning
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
  )

# Hashtags in einen Vector kombinieren für Qualitätskontrolle später
df <- df %>%
  rowwise() %>%
  mutate(
    allHashtags = {
      hashtags_list <- c(hashtags.0, hashtags.1, hashtags.2, hashtags.3, hashtags.4, 
                         hashtags.5, hashtags.6, hashtags.7, hashtags.8, hashtags.9, 
                         hashtags.10, hashtags.11)
      
      # inhaltliche irrelevante Hashtags herausfiltern
      hashtags_filtered <- setdiff(hashtags_list, c("tagesschau", "nachrichten"))
      paste(hashtags_filtered, collapse = ", ")
    },
    word_count = length(strsplit(caption, "\\s+")[[1]])  # Berechnung der Anzahl an Wörtern
  ) %>%
  ungroup()

# Zusammengefasste Hashtags Spalten rausfiltern und fertig gecleantes dataset überschreiben
df <- df %>%
  select(-starts_with("taggedUsers"))

## Vorbereitung für Topic Modeling - Deskreptive Statistik zur Caption 
Q1 <- quantile(df$word_count, 0.25)  
Q3 <- quantile(df$word_count, 0.75)  
iqr <- IQR(df$word_count)
avg_caption_length <- round(mean(df$word_count), 2)
median_caption_length <- round(median(df$word_count), 2)
min_caption_length <- round(min(df$word_count), 2)
max_caption_length <- round(max(df$word_count), 2)
sd_caption_length <- round(sd(df$word_count), 2)

print(paste("Durchschnittliche Wörterzahl:", avg_caption_length))
print(paste("Median der Wörter:", median_caption_length))
print(paste("Minimale Anzahl an Wörtern", min_caption_length))
print(paste("Maximale Anzahl an Wörtern:", max_caption_length))
print(paste("Standardabweichung:", sd_caption_length))
print(paste("25% der Werte liegen unter:", Q1))
print(paste("75% der Werte liegen unter:", Q3))
print(paste("Inter Quartile Range):", iqr))
library(ggplot2)

# Verteilungs Plot
hist(df$word_count, breaks = 30, col = "lightblue", main = "Verteilung der Wortanzahl in Captions", xlab = "Anzahl Wörter")
# Histogramm nach Type
ggplot(df, aes(x = word_count, fill = type)) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.6) +
  scale_fill_manual(values = c("black", "red", "green")) + 
  labs(title = "Verteilung der Wortanzahl in Captions nach Type",
       x = "Anzahl Wörter",
       y = "Häufigkeit",
       fill = "Type") +
  theme_minimal()

## basierenden darauf filtern der unteren outlier (keine iqr, weil obere Grenze das Ergebnis nicht verfälscht)
# - mindestens 55 Wörter in der Caption
df <- df %>%
  filter(word_count > 60) %>%
  mutate(
    caption = str_replace_all(caption, "#\\S+", ""),  #alle hashtags aus der caption entfernen, um Overfitting zu vermeiden
    caption = str_squish(caption),  
  )

# Neue Deskreptive Statistken jetzt wiedergeben
dim(df)
hist(df$word_count, breaks = 30, col = "lightblue", main = "Verteilung der Wortanzahl in Captions", xlab = "Anzahl Wörter")
Q1 <- quantile(df$word_count, 0.25)  
Q3 <- quantile(df$word_count, 0.75)  
iqr <- IQR(df$word_count)
print(paste("25% der Werte liegen unter:", Q1))
print(paste("75% der Werte liegen unter:", Q3))
print(paste("Inter Quartile Range):", iqr))

### Topic Model
# corpus objekt erstellen
corpus_data <- corpus(df$caption, docnames = df$timestamp)

# lemmatization data einlesen
lemma_data <- read.table("./auxilary/lemmatization-de.txt", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
colnames(lemma_data) <- c("inflected_form", "lemma")

# deutsche stopwords einlesen
stopwords_de <- readLines("./auxilary/stopwords-de.txt",
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

# oft vorkommende Wörter ohne große aussagekraft oder nicht rausgefilterte stopwords 
top_terms <- c("ims", "innen", "tagesschau", "nachrichten",
                 "solles", "untere", "new", "", "abern", "the", "sogar", "laute",
               "tagesschau.de", "unseren", "news", "enden", "heißt", "guten", "morgens", "infos",
               "ums", "gesollt", "gemußt", "hieß", "@weltspiegel", "@tagesschau")

# entfernen dieser Wörter
dtm <- dtm[, !(colnames(dtm) %in% top_terms)]
sel_idx <- rowSums(dtm) > 0
dtm <- dtm[sel_idx, ]

# Topics Anzahl
K <- 12

# LDA unsuprvised model 
topic_model <- LDA(dtm, K, method="Gibbs", control=list(
  iter = 1400,      
  alpha = 0.01,     
  seed = 42,      
  verbose = 50
))

# Ergebnisse aus dem modell extrahieren
topic_terms <- terms(topic_model, 13)  # 13 wichtigste Wörter pro Topic
print(topic_terms)


tmResult <- posterior(topic_model)
theta <- tmResult$topics
topicNames <- apply(topic_terms, 2, paste, collapse = " ")


### Ergebnis Statistiken & Visualisierungen
## Qualitiy Check mit den Hashtags
# top 10 dokumente mit höchste Wahrscheinlichkeit für nicht representativen Qualitäts Check 
top_documents_per_topic <- apply(theta, 2, function(topic_probs) {
  order(topic_probs, decreasing = TRUE)[1:10]  # Indizes der Top 10 Dokumente
})

# liste zum speichern des top hashtags 
top_hashtags_per_topic <- list()

# iteration über die themen und ausgabe des top hashtags für die themen
for (topic_idx in 1:K) {
  # top docs für aktuelles thema
  top_docs <- top_documents_per_topic[, topic_idx]
  # extraktion der hashtags aus diesen docs
  hashtags_list <- df$allHashtags[top_docs]
  
  # kombination der hashtags in einen vector
  hashtags_combined <- unlist(hashtags_list)
  
  # entfernen von möglichen duplizierten hashtags
  unique_hashtags <- unique(hashtags_combined)
  
  # Top 13 Begriffe für das Thema aus dem LDA-Modell (aus topic_terms)
  top_topic_terms <- topic_terms[, topic_idx]  # Die Top 13 Begriffe für das Thema
  top_topic_terms_combined <- paste(top_topic_terms, collapse = ", ")
  
  # Ausgabe der Top-Hashtags für das Thema
  print(paste("Thema", topic_idx,topic_idx, "- Top Begriffe:", top_topic_terms_combined ))
  print(unique_hashtags)
  cat("\n")
}

# Topic proportions over Time
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
# Start bei Woche 30 (Beginn der Daten) 
vizDataFrame$kalenderwoche <- factor(vizDataFrame$kalenderwoche, 
                                     levels = c(30:52, 0:29))

# plotten
ggplot(vizDataFrame, aes(x = kalenderwoche, y = value, fill = variable)) +
  geom_bar(stat = "identity") + 
  ylab("Proportion") +
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "Topic") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Topic Proportions per Kalenderwoche")


## Hypothesentest
# H0: Die durchschnittliche Anzahl an Kommentaren/Likes ist für alle Themen gleich.
# H1: Es gibt Unterschiede in der durchschnittlichen Anzahl an Kommentaren/Likes zwischen den Themen.

# Berechnung allgemeiner deskreptiver Statistiken bzgl. der Kommentar / Likes Verteilung
# den docs wird das thema zugeteilgt mit welchem sei am höchsten übereinstimmen
df$topic <- apply(theta, 1, which.max)
df$topic_terms <- sapply(df$topic, function(topic_idx) {
  paste(topic_terms[, topic_idx], collapse = ", ")
})
## comments
stats_comments <- aggregate(df$commentsCount, by = list(topic = df$topic_terms), 
                            FUN = function(x) c(min = min(x), max = max(x), median = median(x), 
                                                mean = mean(x), sd = sd(x), IQR = IQR(x), n = length(x)))
stats_comments <- do.call(data.frame, stats_comments)
colnames(stats_comments) <- c("topic", "min_comments", "max_comments", "median_comments", "avg_comments", 
                              "sd_comments", "iqr_comments", "n_posts")
## likes
stats_likes <- aggregate(df$likesCount, by = list(topic = df$topic_terms), 
                         FUN = function(x) c(min = min(x), max = max(x), median = median(x), 
                                             mean = mean(x), sd = sd(x), IQR = IQR(x), n = length(x)))
stats_likes <- do.call(data.frame, stats_likes)
colnames(stats_likes) <- c("topic", "min_likes", "max_likes", "median_likes", "avg_likes", 
                           "sd_likes", "iqr_likes", "n_posts")

# Einweg-ANOVA hypothesen test für comments
anova_result <- aov(commentsCount ~ factor(topic), data = df)
summary(anova_result)

# Einweg-ANOVA hypothesen test für comments
anova_result <- aov(likesCount ~ factor(topic), data = df)
summary(anova_result)


## Themen Überschneidungen als Heatmap
theta_melted <- melt(theta)
# Erstelle eine interaktive Heatmap
plot_ly(
  data = theta_melted, 
  x = ~Var2, 
  y = ~Var1, 
  z = ~value, 
  type = "heatmap", 
  colors = colorRamp(c("white", "blue"))
) %>%
  layout(
    title = "Interaktive Themenüberschneidungen", # Titel der Visualisierung
    xaxis = list(title = "Themen", showgrid = FALSE),  # Titel für x-Achse
    yaxis = list(showticklabels = FALSE),  # Keine Tick-Beschriftung auf der y-Achse
    showlegend = FALSE  # Keine Legende anzeigen
)


