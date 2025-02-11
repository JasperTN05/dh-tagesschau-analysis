# Workspace
#setwd("./desktop/studium/semester_I/DH/dh-tagesschau-analysis")

# Library Import Statement
library(dplyr)



# CSV Datei Einlesen
dat <- read.csv("tagesschau_instagram_posts.csv",
                header = TRUE, encoding = "UTF-8")

# Reihen und Zeilen des Datensatz
dim(dat)

# Data Cleaning
df <- dat %>%
  filter(
    ownerUsername == "tagesschau",
    as.Date(timestamp) > as.Date("2024-08-31")
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
caption_length = nchar(caption) # caption länge berechnung
  ) %>%
  ungroup()

# Zusammengefasste Reihen rausgefiltert und fertig gereinigtes Datenset überschreiben
df <- df %>%
  select(-starts_with("taggedUsers"), -starts_with("mentions"), -starts_with("hashtags"))

## Vorbereitung für Topic Modeling - Deskreptive Statistik zur Caption (Laenge)

avg_caption_length <- round(mean(df$caption_length),2)
min_caption_length <- round(min(df$caption_length),2)
max_caption_length <- round(max(df$caption_length),2)
print(paste("Durchschnittliche Zeichen in Posting Caption:", avg_caption_length))
print(paste("Maximale Anzahl an Zeichen in Posting Caption:", max_caption_length))
print(paste("Minimale Anzahl an Zeichen in Posting Caption:", min_caption_length))




