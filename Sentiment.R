################
library(tm)
library(lsa)
library(ggplot2)
library(sentiment)
library(rJava)
library(SnowballC)
library(qdap)
summary(restaurant)

#format the review text
texts<-as.character(reviews$Text)
corpus <- Corpus(VectorSource(texts))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))
corpus <- tm_map(corpus, stemDocument, language = "english")
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, PlainTextDocument)
corpus<- tm_map(corpus, content_transformer(tolower))
#make it into a dataframe
dataframe<-data.frame(text=unlist(sapply(corpus, `[`, "content")), 
                      stringsAsFactors=F)
#look at term document frequency to double check
dtm<- TermDocumentMatrix(corpus)
summary(corpus)
freq <- sort(rowSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 20)   
inspect(dtm)


### polarity
rm(texts)
rm(corpus)
#make it into a vector
asvec <- unlist(dataframe)
rm(dataframe)
#find polarity
qpol <- polarity(asvec)
qpol$all$polarity
counts(qpol)[, "polarity"]
#double check to make sure polarity scores makes sense
hist(counts(qpol)[, "polarity"])
one = counts(qpol)[which(reviews$Rating==1), "polarity"]
two = counts(qpol)[which(reviews$Rating==2), "polarity"]
three = counts(qpol)[which(reviews$Rating==3), "polarity"]
four = counts(qpol)[which(reviews$Rating==4), "polarity"]
five = counts(qpol)[which(reviews$Rating==5), "polarity"]
hist(one)
hist(two)
hist(three)
hist(four)
hist(five)
qs <- quantile(reviews$all.polarity, probs = seq(0,1,0.25), na.rm = TRUE)
mean(reviews[which(reviews$all.polarity<qs[5] & reviews$all.polarity > qs[4]),]$Rating)
sum(one < 0)/length(one)
sum(two < 0)/length(two)
sum(three < 0)/length(three)
sum(four < 0)/ length(four)
sum(five < 0)/length(five)
###exploration
hist(mysample$Rating)
summary(qpol$all$polarity)
reviews <- reviews[,-8]
head(reviews)
colnames(reviews)
hist(reviews[which(counts(qpol)[, "polarity"]<0),]$Rating)
asvec[which(is.na(reviews$all.polarity))]

### lava 
library(plyr)
library(stringr)
library(e1071)
library(tm)
library(boot)


###trying a different lexicon ####
lex2 <- read.table(file = "positive-words.txt", header = FALSE, stringsAsFactors = FALSE)
lex3 <- read.table(file = "negative-words.txt", header = FALSE, stringsAsFactors = FALSE)


#function to calculate number of words in each category within a sentence
sentimentScore33 <- function(sentences, negTerms, posTerms){
  final_scores <- matrix('', 0, 3)
  scores <- laply(sentences, function(sentence, negTerms, posTerms){
    initial_sentence <- sentence
    #remove unnecessary characters and split up by word 
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('\\d+', '', sentence)
    wordList <- str_split(sentence, '\\s+')
    words <- unlist(wordList)
    #build vector with matches between sentence and each category
    negMatches <- match(words, negTerms)
    posMatches <- match(words, posTerms)
    #sum up number of words in each category
    posMatches <- sum(!is.na(posMatches))
    negMatches <- sum(!is.na(negMatches))
    score <- c(negMatches, posMatches)
    #add row to scores table
    newrow <- c(initial_sentence, score)
    final_scores <- rbind(final_scores, newrow)
    return(final_scores)
  }, negTerms, posTerms)
  return(scores)
}    

# Lexicon 1 
sampResult1 <-  as.data.frame(sentimentScore33(asvec, lex3$V1, lex2$V1))
which(is.na(sampResult1))
Pol1 <- sampResult1[,3]/sampResult1[,2]
reviews<- cbind(reviews,Pol1)
Pol1 <-(((as.numeric(sampResult1$`3`)+0.001)/(as.numeric(sampResult1$`2`)+0.001)))
lex4 <- read.csv(file = "Lexicon2.csv")
neglex4 <-lex4$word[lex4$polarity=="negative"]
poslex4 <-lex4$word[lex4$polarity=="positive"]
reviews$Pol1 <- Pol1

#Lexicon 2
sampResult2 <-  as.data.frame(sentimentScore33(asvec, neglex4, poslex4))
Pol2 <- (as.numeric(sampResult2$`3`)+0.001)/(as.numeric(sampResult2$`2`)+0.001)
reviews<- cbind(reviews,Pol2)
#Normalize all three of our sentiment scores
reviews$normPol1 <- (reviews$all.polarity - min(reviews$all.polarity, na.rm = TRUE))/(max(reviews$all.polarity, na.rm=TRUE)-min(reviews$all.polarity,na.rm = TRUE))
reviews$normPol2 <- (reviews$Pol1 - min(reviews$Pol1))/(max(reviews$Pol1)-min(reviews$Pol1))
reviews$normPol3 <- (reviews$Pol2 - min(reviews$Pol2))/(max(reviews$Pol2)-min(reviews$Pol2))
#Find the quantiles
library(boot)
?corr
notNa <- which(!is.na(reviews$all.polarity))
corr(cbind(reviews[notNa,]$Rating,reviews[notNa,]$normPol1))

#Weighted average of the scores
reviews$ave <- rowMeans(subset(reviews, select = c(normPol1,normPol2,normPol3)),na.rm=TRUE)
reviews$wave <- apply(subset(reviews, select = c(normPol1,normPol2,normPol3)), 1, function(d) weighted.mean(d, c(3,1,1), na.rm = TRUE))
which(is.na(reviews$Pol1))
hist(reviews$wave)
quantile(reviews$wave)
corr(cbind(reviews$Rating,reviews$wave))
write.csv(reviews,'reviewPol.csv')
restaurant<- restaurant[,-1]
reviews<-read.csv('reviewPol.csv')
reviews<- reviews[,-4]
colnames(restaurant)[3] <- paste("BusinessId")
all <- merge(reviews,restaurant, all.x = TRUE, by = "BusinessId")
#quantile the reviews
quants<-quantile(all$wave, probs = c(0,0.2,0.4,0.6,0.8,1))
quants
mean(all[which(all$wave<quants[6] & all$wave>quants[5]),]$Rating)
library(plyr)
#subset the reviews
one <- subset(all,wave<=quants[2])
two<- subset(all, wave>quants[2] & wave <= quants[3])
three <- subset(all,wave >quants[3] & wave <= quants[4])
four <- subset(all, wave > quants[4] & wave <= quants[5])
five <- subset(all, wave > quants[5])
write.csv(one, "one.csv")
write.csv(two, "two.csv")
write.csv(three, "three.csv")
write.csv(four, "four.csv")
write.csv(five, "five.csv")
############################
library(plyr)
#for each of these, include only the main categories which compromise more than 0.5% of the reviews
#one
counts <-(count(one,'main_cat'))
counts[,3] <- counts[,2]/sum(counts[,2])
counts[order(counts[,3]),]
sum(counts[which(counts[,3]<0.005),3])
oneother <- counts[which(counts[,3]<0.005),1]
levels(one$main_cat)<- c(levels(one$main_cat),"Other")
one[which(one$main_cat %in% oneother),]$main_cat <- "Other"
one$main_cat <- factor(one$main_cat)
write.csv(one,"one.csv")
#two
counts <-(count(two,'main_cat'))
counts[,3] <- counts[,2]/sum(counts[,2])
counts[order(counts[,3]),]
sum(counts[which(counts[,3]<0.0005),3])
other <- counts[which(counts[,3]<0.0005),1]
levels(two$main_cat)<- c(levels(two$main_cat),"Other")
two[which(two$main_cat %in% other),]$main_cat <- "Other"
two$main_cat <- factor(two$main_cat)
write.csv(two,"two.csv")
#three
counts <-(count(three,'main_cat'))
counts[,3] <- counts[,2]/sum(counts[,2])
counts[order(counts[,3]),]
sum(counts[which(counts[,3]<0.0005),3])
other <- counts[which(counts[,3]<0.0005),1]
levels(three$main_cat)<- c(levels(three$main_cat),"Other")
three[which(three$main_cat %in% other),]$main_cat <- "Other"
three$main_cat <- factor(three$main_cat)
write.csv(three,"three.csv")
#four
counts <-(count(four,'main_cat'))
counts[,3] <- counts[,2]/sum(counts[,2])
counts[order(counts[,3]),]
sum(counts[which(counts[,3]<0.0005),3])
other <- counts[which(counts[,3]<0.0005),1]
levels(four$main_cat)<- c(levels(four$main_cat),"Other")
four[which(four$main_cat %in% other),]$main_cat <- "Other"
four$main_cat <- factor(four$main_cat)
write.csv(four,"four.csv")
#five
counts <-(count(five,'main_cat'))
counts[,3] <- counts[,2]/sum(counts[,2])
counts[order(counts[,3]),]
sum(counts[which(counts[,3]<0.0005),3])
other <- counts[which(counts[,3]<0.0005),1]
levels(five$main_cat)<- c(levels(five$main_cat),"Other")
five[which(five$main_cat %in% other),]$main_cat <- "Other"
five$main_cat <- factor(five$main_cat)
write.csv(five,"five.csv")
##########################################################
one <- read.csv("one.csv")
two <- read.csv("two.csv")
three <- read.csv("three.csv")
four <- read.csv("four.csv")
five <- read.csv("five.csv")
citation(package = "base")
citation(package = "qdap")
citation(package = "lme4")
library(lme4)
library(caret)
library(plyr)
library(boot)
library(usdm)
library(MASS)
library(CRAN)
library(glmmLasso)
library(dplyr)
?glmmLasso
#make all the 
one$dessert <- as.numeric(one$dessert)
one$latenight <- as.numeric(one$latenight)
one$lunch <- as.numeric(one$lunch)
one$dinner <- as.numeric(one$dinner)
one$breakfast<- as.numeric(one$breakfast)
one$brunch <- as.numeric(one$brunch)
?sample
samp <- sample_n(one, 15000)
one$BusinessId = factor(one$BusinessId)
colnames(one)
one <- one[,-c(1,2,4,7,8,9,10,11,12,13,14,15,16,17,20,21)]
one$Median = as.numeric(one$Median)/1000
predict(onefit,one[1,])


onefit <- lmer(Rating~ review_count  + American  + Asian + Bakeries + Coffee + Fast + Greek + Nightlife  +Pubs  +Thai + Vegetarian + Vietnamese+ Bars + Breakfast + Buffets + Burgers + Chinese + Italian + Mexican + Pizza + Seafood + Steakhouse + Sushi+ Restaurant.hours.WeekendHours +CreditCard + Price + Delivery  + Reservations   + lunch + dinner    + Median   + (1| bus), data = one, REML = FALSE)
summary(onefit)
AIC(onefit)
twofit <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = two, REML = FALSE)
summary(twofit)
threefit <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = three, REML = FALSE)
summary(threefit)


####################

#fit the model for the first fifth of the data
one = read.csv("one.csv")
count(one$main_cat)

onefit <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit) #477224.6
onefit2 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch + Median + Mean + Pop + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit2) #477466
onefit3 <- lmer(Rating~ Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch + Median + Mean + Pop + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit3) #477870.2
onefit4 <- lmer(Rating~ review_count  + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch + Median + Mean + Pop + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit4) #477464
onefit5 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours  + CreditCard + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit5) #477273 
onefit6 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit6) #477234.5
onefit7 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit7) #477284
onefit8 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price  + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit8) #477264.3
onefit9 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit9) #477223.2
onefit10 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit10) #477290.9
onefit11 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit11) #477221.4
onefit12 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit12) #477220.9

##useless: desert, takeout, latenight

onefit13 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit13) #477237.2
onefit14 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit14) #477224.1
onefit15 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit15) #477219
onefit16 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner +  main_cat + Median + Mean + Pop + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit16) # 477217.1
onefit17 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner + Median + Mean + Pop + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit17) #  477460.7
onefit18 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner + main_cat  + Mean + Pop + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit18) # 477234.6
onefit19 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner +  main_cat + Median + Pop + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit19) # 477217.1

#making the executive decision to remove mean #
onefit20 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner +  main_cat + Median + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit20) # 477218.8
#removing pop#
onefit21 <- lmer(Rating~ review_count  + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner +  main_cat + Median + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit21) # 477218.4
summary(onefit21)

one$main_cat2 =one$main_cat
one$main_cat2[one$main_cat2 == "Southern"] <- "Other"

onefit22 <- lmer(Rating~ review_count  + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner +  main_cat2 + Median + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit22) #477217

one$main_cat3 =one$main_cat2
one$main_cat3[one$main_cat2 == "Taiwanese"] <- "Other"

onefit23 <- lmer(Rating~ review_count  + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner +  main_cat3 + Median + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit23) #477215.5

one$main_cat4 =one$main_cat3
one$main_cat4[one$main_cat3 == "Arts & Entertainment"] <- "Other"

onefit24 <- lmer(Rating~ review_count  + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner +  main_cat4 + Median + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit24) #477214.8

one$main_cat5 =one$main_cat4
one$main_cat5[one$main_cat4 == "Gastropubs" | one$main_cat4 == "Korean"] <- "Other"

onefit25 <- lmer(Rating~ review_count  + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner +  main_cat5 + Median + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit25) #477212.1

one$main_cat6 =one$main_cat5
one$main_cat6[one$main_cat5 == "Beer, Wine & Spirits" | one$main_cat5 == "Delis"] <- "Other"

onefit26 <- lmer(Rating~ review_count  + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner +  main_cat6 + Median + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit26) #477209.2

one$main_cat7 =one$main_cat6
one$main_cat7[one$main_cat6 == "Breweries" | one$main_cat5 == "Cafes"] <- "Other"

onefit27 <- lmer(Rating~ review_count  + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner +  main_cat7 + Median + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit27) #477206.3

one$main_cat8 =one$main_cat7
one$main_cat8[one$main_cat7 == "Dim Sum" | one$main_cat5 == "Dive Bars" |one$main_cat5 == "Ethnic Food"] <- "Other"

onefit28 <- lmer(Rating~ review_count  + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner +  main_cat8 + Median + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit28) #477200.7

one$main_cat8 =one$main_cat7
one$main_cat8[one$main_cat7 == "Wine Bars" | one$main_cat5 == "Middle Eastern" |one$main_cat5 == "Ice Cream & Frozen Yogurt"] <- "Other"

onefit28 <- lmer(Rating~ review_count  + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner +  main_cat8 + Median + (1| BusinessId), data = one, REML = FALSE)
AIC(onefit28) #477200.7

one = cbind(one, predict(onefit28, one))
write.csv(one, file = "onePredict.csv")


################################################################################
#fit for second half of the data
two = read.csv("two.csv")

twofit <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit) #477224.6
twofit2 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch + Median + Mean + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit2) #440875.9
####include review_count
twofit3 <- lmer(Rating~ Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch + Median + Mean + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit3) #441157
###drop weekday weekend
twofit4 <- lmer(Rating~ review_count  + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch + Median + Mean + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit4) #440876.6
twofit5 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours  + CreditCard + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit5) #440709.2

twofit6 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit6) #440678.5
twofit7 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit7) #440706.2

twofit7 <- lmer(Rating~ review_count + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit7) #440905.7




twofit8 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price  + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit8) #440674
twofit9 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit9) # 440674.3
twofit10 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit10) #440718.6
twofit11 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit11) #440673.5
twofit12 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + dessert+  lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit12) #440671.6

##useless: desert, takeout, latenight

twofit13 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit13) #477237.2
twofit14 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit14) #477224.1
twofit15 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit15) #477219
twofit16 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner +  main_cat + Median + Mean + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit16) # 477217.1
twofit17 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner + Median + Mean + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit17) #  477460.7
twofit18 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner + main_cat  + Mean + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit18) # 477234.6
twofit19 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner +  main_cat + Median + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit19) # 477217.1



twofit16 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner +  main_cat + Median + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit16) #440671.7


two$main_cat2 =two$main_cat
two$main_cat2[two$main_cat2 == "Brasseries"] <- "Other"

twofit22 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner +  main_cat2 + Median + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit22) #440669.7

two$main_cat3 =two$main_cat2
two$main_cat3[two$main_cat2 == "Cajun/Creol" | two$main_cat2 == "Delis" | two$main_cat2 == "Creperies" | two$main_cat2 == "Gelato"] <- "Other"

twofit23 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner +  main_cat3 + Median + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit23) #440666.4

two$main_cat4 =two$main_cat3
two$main_cat4[two$main_cat3 == "Arts & Entertainment"] <- "Other"

twofit24 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner +  main_cat4 + Median + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit24) #477214.8

two$main_cat5 =two$main_cat4
two$main_cat5[two$main_cat4 == "Gastropubs" | two$main_cat4 == "Korean"] <- "Other"

twofit25 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner +  main_cat5 + Median + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit25) #477212.1

two$main_cat6 =two$main_cat5
two$main_cat6[two$main_cat5 == "Beer, Wine & Spirits" | two$main_cat5 == "Delis"] <- "Other"

twofit26 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner +  main_cat6 + Median + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit26) #477209.2

two$main_cat7 =two$main_cat6
two$main_cat7[two$main_cat6 == "Breweries" | two$main_cat5 == "Cafes"] <- "Other"

twofit27 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner +  main_cat7 + Median + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit27) #477206.3

two$main_cat8 =two$main_cat7
two$main_cat8[two$main_cat7 == "Dim Sum" | two$main_cat5 == "Dive Bars" |two$main_cat5 == "Ethnic Food"] <- "Other"

twofit28 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Reservations + lunch + dinner +  main_cat8 + Median + Pop + (1| BusinessId), data = two, REML = FALSE)
AIC(twofit28) #477200.7


two = cbind(two, predict(twofit28, two))
write.csv(two, file = "twoPredict.csv")


###############################################
#fit for third part of the data

three = read.csv("three.csv")

threefit <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = three, REML = FALSE)
AIC(threefit) #388304.7
threefit2 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch + Median + Mean + Pop + (1| BusinessId), data = three, REML = FALSE)
AIC(threefit2) #388473.2

###review count is significant ####
threefit3 <- lmer(Rating~ Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch + Median + Mean + Pop + (1| BusinessId), data = three, REML = FALSE)
AIC(threefit3) #388632.2
threefit4 <- lmer(Rating~ review_count  + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch + Median + Mean + Pop + (1| BusinessId), data = three, REML = FALSE)
AIC(threefit4) #388473.6
threefit5 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours  + CreditCard + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = three, REML = FALSE)
AIC(threefit5) #388340.7 
threefit6 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = three, REML = FALSE)
AIC(threefit6) #388304.8


threefit7 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = three, REML = FALSE)
AIC(threefit7) #388313.5



###THIS SEEMS GOOD####
threefit6 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = three, REML = FALSE)
AIC(threefit6) #388304.8
############


threefit8 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours +  Price  + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = three, REML = FALSE)
AIC(threefit8) # 388303.1
threefit9 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + Price + Delivery + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = three, REML = FALSE)
AIC(threefit9) #388303.5
threefit10 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours  + Price + Delivery + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = three, REML = FALSE)
AIC(threefit10) #388305.6
threefit11 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours  + Price + Delivery + Reservations +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = three, REML = FALSE)
AIC(threefit11) #388302.8

threefit12 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + Price + Delivery + Reservations + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = three, REML = FALSE)
AIC(threefit12) #388301.8

threefit13 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + Price + Delivery +  Reservations+ lunch+ main_cat + Median + Mean + Pop + (1| BusinessId), data = three, REML = FALSE)
AIC(threefit13) #388299.7

##useless: all meals 

threefit14 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + Price + Delivery +  Reservations+ lunch+ main_cat + Mean + Pop + (1| BusinessId), data = three, REML = FALSE)
AIC(threefit14) #388298
threefit15 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + Price + Delivery +  Reservations+ lunch+ main_cat + Median + Pop + (1| BusinessId), data = three, REML = FALSE)
AIC(threefit15) #388298

##keeping median becauase why not, POP is significant## 
threefit16 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + Price + Delivery +  Reservations+ lunch+ main_cat + Median + (1| BusinessId), data = three, REML = FALSE)
AIC(threefit16) #388337.9


########
threefit15 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + Price +  Reservations+ lunch+ main_cat + Median + Pop + (1| BusinessId), data = three, REML = FALSE)
AIC(threefit15) #388296


three = cbind(three, predict(threefit15, three))
write.csv(three, file = "threePredict.csv")
############
#fit for the fourth fifth of the data
fourfit <- lmer(Rating~ review_count+main_cat2 + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours  + Price    + dessert  + lunch + dinner + breakfast + brunch + Median  + Pop + (1| BusinessId), data = four, REML = FALSE)
summary(fourfit)
AIC(fourfit)
anova(fourfit)
four$main_cat2 = four$main_cat3
four$main_cat2[four$main_cat == "Fondue"] <- "Other"
four = cbind(four, predict(fourfit, four))
save(fourfit,file ="fourfit.rda")
load("fourfit.rda")
summary(fourfit)
write.csv(four,'fourPredict.csv')
coef(fourfit)
#####
#fit for the fifth part of the data
five = read.csv("five.csv")

fivefit <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = five, REML = FALSE)
AIC(fivefit) #310538.8
fivefit2 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch + Median + Mean + Pop + (1| BusinessId), data = five, REML = FALSE)
AIC(fivefit2) # 310727.1

##review_count is significant!!####
fivefit3 <- lmer(Rating~ Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch + Median + Mean + Pop + (1| BusinessId), data = five, REML = FALSE)
AIC(fivefit3) # 310776

fivefit4 <- lmer(Rating~ review_count  + Restaurant.hours.WeekendHours + CreditCard + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch + Median + Mean + Pop + (1| BusinessId), data = five, REML = FALSE)
AIC(fivefit4) #310728.7
fivefit5 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours  + CreditCard + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = five, REML = FALSE)
AIC(fivefit5) # 310579.4

##Credit Card is eh### 
fivefit6 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + Price + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = five, REML = FALSE)
AIC(fivefit6) #310537.5
fivefit7 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours + CreditCard + Delivery + Takeout + Reservations + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = five, REML = FALSE)
AIC(fivefit7) #310540.5




fivefit10 <- lmer(Rating~ review_count + Restaurant.hours.WeekdayHours + Restaurant.hours.WeekendHours  + Price + Delivery + dessert +latenight + lunch + dinner + breakfast + brunch +  main_cat + Median + Mean + Pop + (1| BusinessId), data = five, REML = FALSE)
AIC(fivefit10) # 310533.6

#include them all? #

summary(fivefit10)


five = cbind(five, predict(fivefit10, five))
write.csv(five, file = "fivePredict.csv")



one = one[, c(3, 4, 5, 6, 15, 16, 17, 18, 19, 20, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 45)]

names(one)
names(two)
two = two[, c(2, 3, 4, 5, 14, 15,  16, 17, 18, 19, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 44)]

names(three)
three = three[, c(2, 3, 4, 5, 14, 15,  16, 17, 18, 19, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 44)]
names(three)
names(three)[27] = "Predicted Rating"
names(five)
five = read.csv("fivePredict.csv")
names(five)
five = five[, c(3, 4, 5,6, 15,  16, 17, 18, 19, 20, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37,  39)]
names(five)[27] = "Predicted Rating"
names(five)

four = read.csv("fourPredict.csv")
four = four[, c(3, 4, 5,6, 15,  16, 17, 18, 19, 20, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 40)]
names(four)
names(four)[27] = "Predicted Rating"

one$group = 1
two$group = 2
three$group = 3
four$group = 4
five$group = 5
#find the predicted ratings all of the reviews
predictComp = rbind(one, two, three, four, five)
#find the difference in the predicted ratings and the actual ratings

predictComp = cbind(predictComp, predictComp$`Predicted Rating` - predictComp$Rating)
names(predictComp)[29] <- "Diff"

#the final ratings
write.csv(predictComp, file = "predictComp.csv")
