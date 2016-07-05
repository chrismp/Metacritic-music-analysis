## TO DO
# Convert release dates to R-readable dates DONE
# For analysis, filter out albums without a Metascore or user score DONE
# Add "year" column DONE
# Correlation between Metascore and User Score DONE
# Correlation of critic-userscores over time DONE
# Above, for genres
# Portion of reviews in Good/Bad/Mixed categories, for each year, for Metascore and user scores

# install.packages('Rcpp') # Makes installation of 'dyplr' work
# install.packages('dplyr')
# install.packages('reshape2')
# install.packages('ggplot2')
# install.packages("lubridate")
# install.packages("RColorBrewer")

library(Rcpp)
library(dplyr)
library(reshape2)
library(ggplot2)
library(lubridate)
library(RColorBrewer)

chartTheme <- function(){
  palette <- brewer.pal("Set1",n=9) # Run display.brewer.all() to see all RColorBrewer palettes
  color.background = "#FCFCFC"
  color.grid.major = "#DDDDDD"
  color.axis.title = "#333333"
  color.axis.text = "#333333"
  color.title = "#333333"
  
  theme_bw(base_size = 12) +
  theme(panel.background = element_rect(fill=color.background, colour=color.background)) +
  theme(plot.background = element_rect(fill=color.background, colour=color.background)) +
  theme(panel.border = element_rect(color=color.background)) +
  
  theme(legend.position="top") +
  theme(legend.background = element_rect(fill=color.background)) +
  theme(legend.text = element_text(size=7, color=color.axis.title)) +
  
  theme(plot.title = element_text(color=color.title, size=12, vjust=1.25)) + 
  theme(axis.text.x = element_text(size=10, color=color.axis.text)) +
  theme(axis.text.y = element_text(size=10, color=color.axis.text)) +
  theme(axis.title.x = element_text(size=10, color=color.axis.title, vjust=0)) +
  theme(axis.title.y = element_text(size=10, color=color.axis.title, vjust=1.25)) +
    
  theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

## IMPORT CSV FILES INTO DATAFRAMES
df.albums <- read.csv("Albums.csv", header = TRUE, na.strings=c("NA","NULL")) %>% filter(UserScores >= 10)
df.criticReviews <- read.csv("CriticReviews.csv", header = TRUE, na.strings=c("NA","NULL"))
df.genres <- read.csv("Genres.csv", header = TRUE, na.strings=c("NA","NULL"))

## CLEAN DATA
df.albums <- filter(df.albums, is.na(df.albums$Metascore)==FALSE, is.na(df.albums$UserScore)==FALSE)
df.albums <- mutate(df.albums, ReleaseYear = year(as.Date(ReleaseDate, format = "%b %d, %Y")))
df.albums <- filter(df.albums, ReleaseYear > 1999) # Useful if comparing only albums from the 21st century
df.albums <- filter(df.albums, !grepl("\\[",Album)) # Remove Albums containing '[', which means it's a box set, reissue, live album, etc

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
  MeanUserScoreX10 = mean(UserScoreX10),
  MedianMetascore = median(Metascore),
  MedianUserScoreX10 = median(UserScoreX10),
  MeanCriticUserDiff = mean(CriticUserScoreDiff),
  MedianCriticUserDiff = median(CriticUserScoreDiff)
)
eda.ReleaseYearsSummary <- summary(eda.ReleaseYears)
eda.ReleaseYearCountCutoff <- 0
eda.ReleaseYearTop20Count <- ggplot(
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
    alpha = 0.50
  )
) +
  theme(legend.position='none') + 
  geom_point() +
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
print(eda2.ScatterplotMetascoreUserScore)

# Print scatterplots for each year, Metascore x UserScore
years <- sort(unique(df.albums$ReleaseYear), decreasing = FALSE) # vector of 2000,2001...2016
for (year in years) {
  df.dummy <- filter(df.albums, ReleaseYear == year)
  eda2.lmStatsDummy <- lm(UserScore~Metascore, data=df.dummy)
  eda2.lmStatsSummaryDummy <- summary(eda2.lmStatsDummy)
  plot <- ggplot(
    df.dummy,
    aes(
      Metascore,
      UserScore#,
      # alpha = UserScores
    )
  ) +
    theme(legend.position='none') + 
    geom_point() + 
    geom_point(position = 'jitter') +
    geom_smooth(method=lm) +
    labs(
      title = paste(
        "Year = ",year,
        "; Adj R2 = ",signif(eda2.lmStatsSummaryDummy$adj.r.squared, 1),
        "; Intercept =",signif(eda2.lmStatsDummy$coef[[1]], 1),
        "; Slope =",signif(eda2.lmStatsDummy$coef[[2]], 1),
        "; P =",signif(eda2.lmStatsSummaryDummy$coef[2,4], 1)
      )
   )
  
  print(plot)
}

# Print multi-scatterplot chart for each year
eda2.lmStatsDummy <- lm(UserScore~Metascore, data=df.albums)
eda2.lmStatsSummaryDummy <- summary(eda2.lmStatsDummy)
plot <- ggplot(
  df.albums,
  aes(
    Metascore,
    UserScore
  )
) +
  theme(legend.position='none') + 
  geom_point() + 
  geom_point(position = 'jitter') +
  geom_smooth(method=lm) +
  facet_grid(ReleaseYear ~ .) +
  labs(
    title = "Metacritic music critic scores vs. user scores"
  ) +
  coord_flip()
print(plot)

# Metascores by year, boxplot
eda2.BoxPlotMetascoresByYear <- ggplot(
  df.albums, 
  aes(ReleaseYear, Metascore)
) +
  geom_boxplot(aes(group = ReleaseYear)) +
  scale_x_reverse() +
  coord_flip() +
  labs(title="Metacritic Metascores by year")
print(eda2.BoxPlotMetascoresByYear)

# User scores by year, boxplots
eda2.BoxPlotUserScoresByYear <- ggplot(
  df.albums, 
  aes(ReleaseYear, UserScore)
) +
  geom_boxplot(aes(group = ReleaseYear)) +
  scale_x_reverse() +
  coord_flip() +
  labs(title="Metacritic music user scores by year")
print(eda2.BoxPlotUserScoresByYear)

# Metascore vs user score, by year
eda2.LineScoresByYear <- ggplot(eda.ReleaseYears, aes(ReleaseYear)) +
                         geom_line(aes(y=MedianMetascore, colour="Metascore")) +
                         geom_line(aes(y=MedianUserScoreX10, colour="User score times 10")) +
                         labs(x="Year", y="Median score", title="Metacritic music scores by critics and fans") +
                         theme(legend.position="top", legend.title=element_blank()) +
                         expand_limits(y=50) +
                         scale_y_continuous(breaks = seq(0,100,10))
print(eda2.LineScoresByYear)

# Stacked bar chart, User score by year
df.albums$UserScoreCategory2 <- factor(df.albums$UserScoreCategory, levels = c("Bad","Mixed","Good"))
eda2.StackedBarUserRatingCategoriesByYear <- ggplot(data = df.albums, aes(x=ReleaseYear)) +
                                         geom_bar(aes(fill=UserScoreCategory2), position = "fill") +
                                         scale_x_reverse(breaks = seq(2000,2016,1)) +
                                         scale_y_continuous(breaks = NULL) +
                                         labs(x="Year", y=NULL, title="Metacritic music user scores", fill='') +
                                         scale_fill_manual(values=c("#FF0000","#FFCC33","#66CC33")) +
                                         chartTheme() +
                                         coord_flip()
print(eda2.StackedBarUserRatingCategoriesByYear)

df.albums$MetascoreCategory2 <- factor(df.albums$MetascoreCategory, levels = c("Bad","Mixed","Good"))
eda2.StackedBarUserRatingCategoriesByYear <- ggplot(data = df.albums, aes(x=ReleaseYear)) +
  geom_bar(aes(fill=MetascoreCategory2), position = "fill") +
  scale_fill_manual(values=c("#FF0000","#FFCC33","#66CC33")) +
  scale_x_reverse(breaks = seq(2000,2016,1)) +
  scale_y_continuous(breaks = NULL) +
  labs(x="Year", y=NULL, title="Metacritic music critic scores", fill='') +
  chartTheme() +
  coord_flip()
print(eda2.StackedBarUserRatingCategoriesByYear)





