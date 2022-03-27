library(rtweet)
library(data.table)
#library(ggplot2)

dt <- search_tweets("#econometrics") #pulls tweets from twitter using rtweet, searches through own profile

# dt <- data.table(dt)

head(dt$text)
head(dt)

length(grep("study", dt$text, ignore.case=TRUE, value=TRUE))
#ignore case --> ignore if upper or lower case

head(dt)