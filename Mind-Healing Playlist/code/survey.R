
library(dplyr)
library(randomForest)
library(tidyverse)

survey = read.csv("mxmh_survey_results.csv")
songs <- read.csv("spotify_songs.csv")
## remove NA ##
survey <- na.omit(survey)

## filter 'Improve' ##
survey <- survey %>% filter( Music.effects == "Improve")

## transform 'frequency' into 'value' ##
convert_freq <- function(x) {
  recode(as.character(x),
         "Never" = 1,
         "Rarely" = 2,
         "Sometimes" = 3,
         "Very frequently" = 4) 
}
frequency_cols <- grep("Frequency", names(survey), value = TRUE)
survey <- survey %>%
  mutate(across(all_of(frequency_cols),convert_freq, .names="num_{.col}"))%>%
  mutate(BPM=as.numeric(BPM))

## standardization ##
survey$Anxiety <- scale(survey$Anxiety)
survey$Depression <- scale(survey$Depression)
survey$Insomnia <- scale(survey$Insomnia)
survey$OCD <- scale(survey$OCD)

freq_zscore <- survey %>%
  select(starts_with("num_Frequency")) %>%
  scale()

## recluster mental state ##
set.seed(2)
kmeans_result <- kmeans(survey[,c("Anxiety", "Depression", "Insomnia", "OCD")], centers=5)
survey$cluster <- factor(kmeans_result$cluster, labels = c( "High pressure", "High anxiety", "Mild Symptom","High Anxiety Insomnia","Healthy"))

freq_zscore_df <- as.data.frame(freq_zscore)
freq_zscore_df$cluster <- survey$cluster

# long format ##
z_df <- freq_zscore_df  %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(-cluster, names_to = "genre", values_to = "z_score")

# music genre preferred ##
ggplot(z_df, aes(x = genre, y = cluster, fill = z_score)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Preference of Psychological Groups for Each Music Genre (Z-score)）",
       x = "genre", y = "mental state clusteration", fill = "Standard deviation") +
  theme_minimal()

## Plotting the Values of Music Features” ##
songs <- songs %>%
  mutate(Group = case_when(
    playlist_genre %in% c("latin", "rock", "rap", "r&b") ~ playlist_genre,
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Group))

ggplot(songs, aes(x = danceability, fill = Group)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Danceability Across Psychological Groups")

ggplot(songs, aes(x = energy, fill = Group)) +
  geom_density(alpha = 0.5) +
  labs(title = "Energy of Danceability Across Psychological Groups")

ggplot(songs, aes(x = valence, fill = Group)) +
  geom_density(alpha = 0.5) +
  labs(title = "Valence of Danceability Across Psychological Groups")

ggplot(songs, aes(x = acousticness, fill = Group)) +
  geom_density(alpha = 0.5) +
  labs(title = "Acousticness of Danceability Across Psychological Groups")

ggplot(songs, aes(x = speechiness, fill = Group)) +
  geom_density(alpha = 0.5) +
  labs(title = "Speechiness of Danceability Across Psychological Groups")

ggplot(songs, aes(x = liveness , fill = Group)) +
  geom_density(alpha = 0.5) +
  labs(title = "Liveness of Danceability Across Psychological Groups")

ggplot(songs, aes(x = tempo , fill = Group)) +
  geom_density(alpha = 0.5) +
  labs(title = "Tempo of Danceability Across Psychological Groups")

ggplot(songs, aes(x = instrumentalness , fill = Group)) +
  geom_density(alpha = 0.5) +
  labs(title = "Instrumentalness of Danceability Across Psychological Groups")










