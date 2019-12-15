## ----One-----------------------------------------------------------------
# Here, I read the .csv file and store it as a dataset called InitialGameData. This will be the initial, uncleaned version of data I will clean later. The 'header' argument reads the first row as the dataset's variables, whereas the 'sep' argument indicates what character is separating each field; in our case, this is a comma-separated value file hence ",".
InitialGameData <- read.csv("appstore_games.csv", header=TRUE, sep=",") 

# Use View to literally view the data set we just made.
View (InitialGameData)

# And run a few other functions for facts and kickers
ncol(InitialGameData) #should return 18 columns
nrow(InitialGameData) #should return 17007 observations/rows

# Now that we have our initial but unclean data set, we need to "tidy" it now. In case we make mistakes and data goes bonkers or missing, however, we should make an identical version of the data set that we can freely operate on. InitialGameData is merely our "initial" data that will act as backup.
GameData <- InitialGameData 
#View (InitialGameData)


## ----Two-----------------------------------------------------------------
#load libraries to properly plot data
library(ggplot2)
library(sjPlot)
library(tidyverse)
library(forcats)
library(plyr)


## ----Three---------------------------------------------------------------
colnames(GameData)[colnames(GameData)=="Average.User.Rating"] <- "Average User Rating"  #colnames first takes in the data set name, then specify an existing column name and give it a new handle after "<-".
colnames(GameData)[colnames(GameData)=="User.Rating.Count"] <- "Total Ratings" #a more accurate name for this column in my opinion
colnames(GameData)[colnames(GameData)=="Icon.URL"] <- "App Icon URL"
colnames(GameData)[colnames(GameData)=="Age.Rating"] <- "Age Rating" #replace period
colnames(GameData)[colnames(GameData)=="In.app.Purchases"] <- "In-App Purchases" #syntax fix
colnames(GameData)[colnames(GameData)=="Primary.Genre"] <- "Primary Genre" #replace period
colnames(GameData)[colnames(GameData)=="Genres"] <- "Game Genres" #more descriptive
colnames(GameData)[colnames(GameData)=="Original.Release.Date"] <- "Original Release Date" #replace periods
colnames(GameData)[colnames(GameData)=="Current.Version.Release.Date"] <- "Current Version Release Date" #replace periods


## ----Four----------------------------------------------------------------
is.factor(GameData$`Age Rating`) # this should return true, indicating it's already implemented as a factor variable.
is.factor(GameData$`Languages`) # should be true 
is.factor(GameData$`In-App Purchases`) # should be true 
is.factor(GameData$`Primary Genre`) # should be true 
is.factor(GameData$`Game Genres`) # should be true 

# Here, we run levels on each of the factor columns we checked to see what exactly the levels of each factor there are.

#levels(factor(GameData$`Age Rating`))  #4+, 9+, 12+, and 17+
#levels(factor(GameData$`In-App Purchases`)) #various prices
#levels(factor(GameData$`Languages`))  #various languages
#levels(factor(GameData$`Primary Genre`)) #Games and Entertainment primarily, can branch to other genres
#levels(factor(GameData$`Game Genre`))  #Same as above



## ----Six-----------------------------------------------------------------
#Here we create the Ratings table, which consists of the Game ID, Average User Rating, Total number of Ratings, and Age Ratings. Note we have to use underscores rather than whitespace in column naming for successful tibble compiling, but we're not gonna fuss over that.
Ratings <- tibble(ID = GameData$ID,
                  Average_User_Rating = GameData$`Average User Rating`,
                  Total_Ratings = GameData$`Total Ratings`,
                  Age_Ratings = GameData$`Age Rating`)
#View(Ratings) #should return a successfully-made table

#Now, we want to create a table with only games with microtransactions. We will include variables from Ratings and see if the value of the ratings corresponds with the presence of microtransactions.

nrow(GameData)  #should return 17007
gameswithpurchases <- GameData[-which(GameData$`In-App Purchases` == ""), ]  #collects data and exclude entries with no value in its In-App Purchases section.
nrow(gameswithpurchases)  #should return less than 17007; in our case, it will be 7683

GamePurchaseWithRatings <- tibble(ID = gameswithpurchases$ID,
                                  Average_User_Rating = gameswithpurchases$`Average User Rating`,
                                  Age_Ratings = gameswithpurchases$`Age Rating`,
                                  InApp_Purchases = gameswithpurchases$`In-App Purchases`)

# For example plotting purposes, we can also make a table to see if the size of a game correlates to its rating. Instead of making a new table, we can add the column to the existing dataset using the cbind function (Take a dataset and specify the name of the new column as well as source data).

Ratings <- cbind(Ratings, Size = GameData$Size)


## ----Seven---------------------------------------------------------------
# For my first visualization, I take the example plotting we established last time and attempt to see the relationship between the size of a game and its ratings. 
ggplot(Ratings, aes(x=Size, y=Average_User_Rating)) + geom_point() + geom_smooth() + labs(title="Relationship Between Size and Ratings", x="Size", y="User Ratings") + theme(plot.title=element_text(size=25, face="bold"), 
                  axis.text.x=element_text(size=15), 
                  axis.text.y=element_text(size=15),
                  axis.title.x=element_text(size=25),
                  axis.title.y=element_text(size=25))

# We can also view a glance of the frequencies of prices used as microtransactions:
counts <- table(GamePurchaseWithRatings$InApp_Purchases)
PriceDistribution <- barplot(counts, main="Price Distribution", width = 100, xlab = "Frequency", ylab = "Prices", axes = TRUE, density = 10)

