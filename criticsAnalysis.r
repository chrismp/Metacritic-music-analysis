# install.packages('Rcpp') # Makes installation of 'dyplr' work
# install.packages('dplyr')
# install.packages("lubridate")

library(Rcpp)
library(dplyr)
library(lubridate)

## IMPORT CSV FILES INTO DATAFRAMES
df.albums <- read.csv("Albums.csv", header = TRUE, na.strings=c("NA","NULL")) %>% filter(UserScores >= 10)
df.criticReviews <- read.csv("CriticReviews.csv", header = TRUE, na.strings=c("NA","NULL"))
df.genres <- read.csv("Genres.csv", header = TRUE, na.strings=c("NA","NULL"))

## CLEAN DATA
df.albums <- mutate(df.albums, ReleaseYear = year(as.Date(ReleaseDate, format = "%b %d, %Y")))
df.albums <- filter(df.albums, !grepl("\\[",Album)) # Remove Albums containing '[', which means it's a box set, reissue, live album, etc

df.albums_criticsReviews <- merge(
  x = df.albums,
  y = df.criticReviews,
  by = "AlbumURL"
)

df.albums_genres <- merge(
  x = df.albums,
  y = df.genres,
  by = "AlbumURL"
)

eda.criticsSummary <- summarise(
  .data = group_by(df.albums_criticsReviews, Critic),
  Count = n(),
  Pearson_CriticScore_Metascore = cor(x = Score, y = Metascore, use = "complete.obs", method = "pearson"),
  Pearson_CriticScore_UserScore = cor(x = Score, y = UserScore, use = "complete.obs", method = "pearson"),
  AverageCriticScore = mean(Score), # Average score given by this critic
  AverageMetascore = mean(Metascore), # Average Metascore for albums rated by this critic
  AverageCriticScoreMetascoreDifference = mean(Score) - mean(Metascore),
  AverageUserScoreX10 = mean(UserScore*10, na.rm = TRUE),
  AverageCriticScoreUserScoreDifference = mean(Score) - mean(UserScore*10, na.rm = TRUE),
  MedianCriticScore = median(Score),
  MedianMetascore = median(Metascore),
  MedianCriticScoreMetascoreDifference = median(Score) - median(Metascore),
  MedianUserScoreX10 = median(UserScore*10, na.rm = TRUE),
  MedianCriticScoreUserScoreDifference = median(Score) - median(UserScore*10, na.rm = TRUE)
) %>%
  filter(Count >= 10)

eda.criticsSummary_2010s <- summarise(
  .data = group_by(filter(df.albums_criticsReviews, ReleaseYear >= 2010), Critic),
  Count = n(),
  Pearson_CriticScore_Metascore = cor(x = Score, y = Metascore, use = "complete.obs", method = "pearson"),
  Pearson_CriticScore_UserScore = cor(x = Score, y = UserScore, use = "complete.obs", method = "pearson"),
  AverageCriticScore = mean(Score), # Average score given by this critic
  AverageMetascore = mean(Metascore), # Average Metascore for albums rated by this critic
  AverageCriticScoreMetascoreDifference = mean(Score) - mean(Metascore),
  AverageUserScoreX10 = mean(UserScore*10, na.rm = TRUE),
  AverageCriticScoreUserScoreDifference = mean(Score) - mean(UserScore*10, na.rm = TRUE),
  MedianCriticScore = median(Score),
  MedianMetascore = median(Metascore),
  MedianCriticScoreMetascoreDifference = median(Score) - median(Metascore),
  MedianUserScoreX10 = median(UserScore*10, na.rm = TRUE),
  MedianCriticScoreUserScoreDifference = median(Score) - median(UserScore*10, na.rm = TRUE)
) %>%
  filter(Count >= 10)

eda.genreSummary <- summarise(
  .data = group_by(df.albums_genres, Genre),
  Count = n(),
  Pearson_Metascore_UserScore = cor(x = Metascore, y = UserScore, use = "pairwise.complete.obs", method = "pearson"),
  AverageMetascore = mean(Metascore, na.rm = TRUE),
  AverageUserScoreX10 = mean(UserScore*10, na.rm = TRUE),
  AverageCriticScoreUserScoreDifference = mean(Metascore) - mean(UserScore*10, na.rm = TRUE),
  MedianMetascore = median(Metascore, na.rm = TRUE),
  MedianUserScoreX10 = median(UserScore*10, na.rm = TRUE),
  MedianCriticScoreUserScoreDifference = median(Metascore) - median(UserScore*10, na.rm = TRUE)
) %>%
  filter(Count >= 50)

eda.genreSummary_2010s <- summarise(
  .data = group_by(filter(df.albums_genres, ReleaseYear >= 2010), Genre),
  Count = n(),
  Pearson_Metascore_UserScore = cor(x = Metascore, y = UserScore, use = "pairwise.complete.obs", method = "pearson"),
  AverageMetascore = mean(Metascore, na.rm = TRUE),
  AverageUserScoreX10 = mean(UserScore*10, na.rm = TRUE),
  AverageCriticScoreUserScoreDifference = mean(Metascore) - mean(UserScore*10, na.rm = TRUE),
  MedianMetascore = median(Metascore, na.rm = TRUE),
  MedianUserScoreX10 = median(UserScore*10, na.rm = TRUE),
  MedianCriticScoreUserScoreDifference = median(Metascore) - median(UserScore*10, na.rm = TRUE)
) %>%
  filter(Count >= 50)

dummy <- filter(df.albums_criticsReviews, Critic=="Village Voice (Consumer Guide)")
dummy <- NULL

# lm.CriticScore_Metascore <- lm(data = df.albums_criticsReviews, Score ~ Metascore)
# summary.CriticScore_Metascore <-summary(lm.CriticScore_Metascore)

# lm.score <- lm(data = df.albums_criticsReviews, Score ~ Metascore)
# coeffs <- coefficients(lm.score)
# dummy <- 80
# dummy <- coeffs[1] + coeffs[2]*dummy