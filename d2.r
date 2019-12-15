## ----echo=FALSE, message=FALSE, error=FALSE, warning=FALSE, results='hide'----
#include function modularizes without needing the full package
include <- function(library_name){
  if( !(library_name %in% installed.packages()) )
    install.packages(library_name) 
  library(library_name, character.only=TRUE)
}

#Library inclusion
invisible( include ("tidyverse") )
invisible( include ("knitr") )
invisible( include ("tidyr") )
invisible( include ("rvest") )
invisible( include ("BBmisc") )
invisible( include ("caret") )

#Take code from deliverable 1 as a source
purl ("deliverable1.Rmd", output = "d1.r")
suppressMessages(source ("d1.r"))


## ----2-------------------------------------------------------------------
google_play_data <- read.csv("googleplaystore.csv")

glimpse(google_play_data)  #technical view
head(google_play_data)  #actual preview


## ----3-------------------------------------------------------------------
names(google_play_data)[9] <- "Content Rating"  #9 is the index of "Content.Rating" as listed above. Other renames will follow accordingly.
names(google_play_data)[11] <- "Last Updated"
names(google_play_data)[12] <- "Current Version"
names(google_play_data)[13] <- "Required Android Version" #more descriptive

#quick important variable factor checks
is.factor(google_play_data$Category)
is.factor(google_play_data$Installs)
is.factor(google_play_data$`Last Updated`)



## ----4-------------------------------------------------------------------
google_play_data <- google_play_data[-10473,]


## ----5-------------------------------------------------------------------
google_play_data$Installs <- gsub("\\D", "", google_play_data$Installs)
class(google_play_data$Installs)
google_play_data$Installs <- as.numeric(as.character(google_play_data$Installs))


## ----6-------------------------------------------------------------------
google_play_data <- na.omit(google_play_data)


## ----7-------------------------------------------------------------------
google_play_data$Price = as.numeric(gsub("[\\$,]", "", google_play_data$Price))


## ----8-------------------------------------------------------------------
cleaned_google_play_data <- google_play_data
head (cleaned_google_play_data)


## ----9-------------------------------------------------------------------
sample75 <- sample.int (
  n = nrow(cleaned_google_play_data), 
  size = .75*nrow(cleaned_google_play_data))

testingset <- cleaned_google_play_data[-sample75,]  #testing 25% of the data

trainingset <- cleaned_google_play_data[sample75,]  #testing 75% of the data

ggplot(trainingset) + geom_bar(aes(x=Category)) + coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
ggplot(trainingset) + geom_bar(aes(x=Rating)) + coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
ggplot(trainingset) + geom_bar(aes(x=Type)) + coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")


## ----10------------------------------------------------------------------
trainingmodel <- lm(Rating ~ factor(`Content Rating`), data = trainingset)
summary (trainingmodel)
predictions <- trainingmodel %>% predict(testingset)

#plotting data
ggplot(data = testingset, aes(x=predictions, y = Rating)) + 
      geom_point() + 
      geom_smooth(method = "lm")

