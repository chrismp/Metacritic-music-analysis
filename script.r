## TO DO
# Convert release dates to R-readable dates DONE
# For analysis, filter out albums without a Metascore or user score DONE
# Add "year" column DONE
# Artists' ratings over time
# labels' ratings over time
# genres' ratings over time
# Correlation between Metascore and User Score DONE 
# Outliers in above correlations
# Correlation of critic-userscores over time
# Above, for genres

# install.packages('Rcpp') # Makes installation of 'dyplr' work
# install.packages('dplyr')
# install.packages('reshape2')
# install.packages('ggplot2')
# install.packages("lubridate")

library(Rcpp)
library(dplyr)
library(reshape2)
library(ggplot2)
library(lubridate)

## IMPORT CSV FILES INTO DATAFRAMES
df.albums <- read.csv("Albums.csv", header = TRUE, na.strings=c("NA","NULL"))
df.criticReviews <- read.csv("CriticReviews.csv", header = TRUE, na.strings=c("NA","NULL"))
df.genres <- read.csv("Genres.csv", header = TRUE, na.strings=c("NA","NULL"))

## CLEAN DATA
df.albums <- filter(df.albums, is.na(df.albums$Metascore)==FALSE, is.na(df.albums$UserScore)==FALSE)
df.albums <- mutate(df.albums, ReleaseYear = year(as.Date(ReleaseDate, format = "%b %d, %Y")))
df.albums <- filter(df.albums, ReleaseYear > 1999)

## ADD COLUMNS
df.albums <- mutate(df.albums, UserScoreX10 = UserScore*10)
df.albums <- mutate(df.albums, CriticUserScoreDiff = Metascore - UserScoreX10)
df.albums$MetascoreCategory <- ifelse(df.albums$Metascore < 60, "Bad",
                                  ifelse(df.albums$Metascore < 80, "Mixed",
                                         "Good"))
df.albums$UserScoreCategory <- ifelse(df.albums$UserScoreX10 < 60, "Bad",
                                  ifelse(df.albums$UserScoreX10 < 80, "Mixed",
                                         "Good"))
# df.albums$DiffRating <- ifelse(df.albums$CriticUserScoreDiff > 0, "Overrated",
#                               ifelse(df.albums$CriticUserScoreDiff == 0, "Neither",
#                               "Underrated"))


## SUMMARY STATS FOR ALBUM DATA: Min, max, median, etc
eda.summaryStats <- summary(df.albums)
eda.topMetascoreAlbums <- df.albums %>% arrange(desc(Metascore))
eda.bottomMetascoreAlbums <- df.albums %>% arrange(Metascore)
eda.topUserAlbums <- df.albums %>% arrange(desc(UserScore))
eda.bottomUserAlbums <- df.albums %>% arrange(UserScore)


## SINGLE-VARIABLE EXPLORATORY DATA ANALYSIS
eda.Labels <- summarise(
  group_by(df.albums, LabelURL, Label),
  n = n(),
  CountCriticScores = sum(CriticScores),
  CountUserScores = sum(UserScores),
  MeanMetascore = mean(Metascore),
  MeanUserScore = mean(UserScore),
  MedianMetascore = median(Metascore),
  MedianUserScore = median(UserScore),
  MeanCriticUserDiff = mean(CriticUserScoreDiff),
  MedianCriticUserDiff = median(CriticUserScoreDiff)
)
eda.LabelsSummary <- summary(eda.Labels)
eda.LabelCountCutoff <- 0
eda.LabelTop20Count <- 
  ggplot(
    eda.Labels[eda.Labels$n >= eda.LabelCountCutoff,],
    aes(
      x = reorder(Label, n), 
      y = n
    )
  ) + ggtitle("Labels") +
  labs(x=NULL, y="Count") + 
  geom_bar(stat="identity") + 
  coord_flip()

eda.Artists <- summarise(
  group_by(df.albums, ArtistURL, Artist),
  n = n(),
  CountCriticScores = sum(CriticScores),
  CountUserScores = sum(UserScores),
  MeanMetascore = mean(Metascore),
  MeanUserScore = mean(UserScore),
  MedianMetascore = median(Metascore),
  MedianUserScore = median(UserScore),
  MeanCriticUserDiff = mean(CriticUserScoreDiff),
  MedianCriticUserDiff = median(CriticUserScoreDiff)
)
eda.ArtistsSummary <- summary(eda.Artists)
eda.ArtistCountCutoff <- 0
eda.ArtistTop20Count <- 
  ggplot(
    eda.Artists[eda.Artists$n >= eda.ArtistCountCutoff,],
    aes(
      x = reorder(Artist, n), 
      y = n
    )
  ) + ggtitle("Artists") +
  labs(x=NULL, y="Count") + 
  geom_bar(stat="identity") + 
  coord_flip()

eda.ReleaseYears <- summarise(
  group_by(df.albums, ReleaseYear),
  n = n(),
  CountCriticScores = sum(CriticScores),
  CountUserScores = sum(UserScores),
  MeanMetascore = mean(Metascore),
  MeanUserScore = mean(UserScore),
  MedianMetascore = median(Metascore),
  MedianUserScore = median(UserScore),
  MeanCriticUserDiff = mean(CriticUserScoreDiff),
  MedianCriticUserDiff = median(CriticUserScoreDiff)
)
eda.ReleaseYearsSummary <- summary(eda.ReleaseYears)
eda.ReleaseYearCountCutoff <- 0
eda.ReleaseYearTop20Count <- 
  ggplot(
    eda.ReleaseYears[eda.ReleaseYears$n >= eda.ReleaseYearCountCutoff,],
    aes(
      x = reorder(ReleaseYear, n), 
      y = n
    )
  ) + ggtitle("Release years") +
  labs(x=NULL, y="Count") + 
  geom_bar(stat="identity") + 
  coord_flip()

eda.metascoreCategoryCount <- filter( count(df.albums,MetascoreCategory), is.na(MetascoreCategory)==FALSE)
eda.metascoreCategoryCountSummary <- summary(eda.metascoreCategoryCount)
eda.metascoreCategoryCountCutoff <- 1
eda.metascoreCategoryTop20Count <- 
  ggplot(
    eda.metascoreCategoryCount[eda.metascoreCategoryCount$n >= eda.metascoreCategoryCountCutoff,],
    aes(
      x = reorder(MetascoreCategory, n), 
      y = n
    )
  ) + ggtitle("Metascore category") +
  labs(x=NULL, y="Count") + 
  geom_bar(stat="identity") + 
  coord_flip() 

eda.userScoreCategoryCount <- filter( count(df.albums,UserScoreCategory), is.na(UserScoreCategory)==FALSE)
eda.userScoreCategoryCountSummary <- summary(eda.userScoreCategoryCount)
eda.userScoreCategoryCountCutoff <- 0
eda.userScoreCategoryTop20Count <- 
  ggplot(
    eda.userScoreCategoryCount[eda.userScoreCategoryCount$n >= eda.userScoreCategoryCountCutoff,],
    aes(
      x = reorder(UserScoreCategory, n), 
      y = n
    )
  ) + ggtitle("User score category") +
  labs(x=NULL, y="Count") + 
  geom_bar(stat="identity") + 
  coord_flip()

eda.MetascoreHistogram <- ggplot(df.albums, aes(x=Metascore)) + 
  geom_histogram(binwidth=10) +
  geom_vline(
    aes(xintercept=mean(Metascore)),
    color="red"
  ) +
  scale_x_continuous(breaks = seq(0,100,10)) + # seq(start, end, interval)
  scale_y_continuous(breaks = seq(0,6000,500))
eda.MetascoreHistogramData <- ggplot_build(eda.MetascoreHistogram)$data[[1]]

eda.UserScoreHistogram <- ggplot(df.albums, aes(x=UserScore)) + 
  geom_histogram(binwidth=1) +
  geom_vline(
    aes(xintercept=mean(UserScore)),
    color="red"
  ) +
  scale_x_continuous(breaks = seq(0,10,1)) +
  scale_y_continuous(breaks = seq(0,6000,500))
eda.UserScoreHistogramData <- ggplot_build(eda.UserScoreHistogram)$data[[1]]

## MULTI-VARIABLE EXPLORATORY DATA ANALYSIS
eda2.lmStats <- lm(UserScore~Metascore, data=df.albums)
eda2.lmStatsSummary <- summary(eda2.lmStats)
eda2.ScatterplotMetascoreUserScore <- ggplot(
  df.albums,
  aes(
    Metascore,
    UserScore,
    alpha = UserScores
  )
) +
  theme(legend.position='none') + 
  geom_point(position = 'jitter') + 
  geom_smooth(method=lm) +
  labs(
    title = paste(
      "Adj R2 = ",signif(eda2.lmStatsSummary$adj.r.squared, 2),
      "Intercept =",signif(eda2.lmStats$coef[[1]],2 ),
      " Slope =",signif(eda2.lmStats$coef[[2]], 2),
      " P =",signif(eda2.lmStatsSummary$coef[2,4], 2)
    )
  )