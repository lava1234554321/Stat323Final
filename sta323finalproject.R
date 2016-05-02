library(jsonlite)
library(dplyr)

business <- stream_in(file('~/Yelp/yelp_academic_dataset_business.json'))
photos<-stream_in(file('~/Yelp/photo_id_to_business_id.json'))

business <- business[which(business$business_id %in% photos$business_id),]

business<-business[sapply(business$categories,function(x){return("Restaurants" %in% x)}),]

business<-business[-which(rowSums(is.na(business$hours))==14),]
sapply(lapply(business,is.na),sum)
business<-business[-which(business$open==FALSE),]
not.na <- c('Take-out','Delivery','Good For','Price Range','Takes Reservations')
not.na.data<-business$attributes[,which(names(business$attributes) %in% not.na)]
cols <- sapply(not.na.data,is.na)
keep <- which(apply(cols,1,sum)==0)
business<-business[keep,]
business <- business[-which(rowSums(is.na(business$attributes$`Good For`))>0),]

main_cat<-unlist(lapply(business$categories,
                        function(x)
                          return(x[which(x!="Restaurants"&x!="Food")[1]])))
sum(is.na(main_cat))
main_cat[which(is.na(main_cat))]<-"Restaurants"
business$main_cat <- main_cat
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
business$zip <- as.numeric(substrRight(as.character(business$full_address),5))
business<- business[-which(is.na(business$zip)),]
medMean <- read.csv('C:\\Users\\Alice\\Documents\\Stat561\\MedianZIP-3.csv')
zips <- lapply(business$zip,function(x) return(medMean[which(abs(medMean$Zip-x)==min(abs(medMean$Zip-x)))[1],1]))
business$Zip <- unlist(zips)
business<- merge(business, medMean, by="Zip", all.x = TRUE)
business$Mean <- as.numeric(gsub(",","", business$Mean))
business$Median <- as.numeric(gsub(",","", business$Median))
business$Pop <- as.numeric(gsub(",","", business$Pop))
int.var <- c('business_id','full_address','main_cat', 'city','review_count', 'name', 'longitude', 'latitude', 'state', 'stars', 'Zip','Mean','Median','Pop')
business.main <- business[,which(names(business)%in%int.var)]
colnames(business.main)<-paste(colnames(business)[which(names(business)%in% int.var)])
Restaurant.hours <- business$hours
lapply(Restaurant.hours$close,strptime,format='%H:%M')
for(i in 1:ncol(Restaurant.hours)){
  close<-strptime(Restaurant.hours[[i]]$close, format = '%H:%M')
  open<- strptime(Restaurant.hours[[i]]$open, format = '%H:%M')
  diff<- difftime(close,open,units = "hours")
  needAdd <- which(diff<=0)
  diff[needAdd] = diff[needAdd] + 24
  diff[which(is.na(diff))] = 0
  Restaurant.hours[[i]]$numHours = diff
}
Restaurant.hours$WeekdayHours = Restaurant.hours$Monday$numHours+Restaurant.hours$Tuesday$numHours+Restaurant.hours$Wednesday$numHours+Restaurant.hours$Thursday$numHours+Restaurant.hours$Friday$numHours
Restaurant.hours$WeekendHours = Restaurant.hours$Saturday$numHours+Restaurant.hours$Sunday$numHours

#attributes
Restaurant.attributes <- cbind(business$attributes$`Accepts Credit Cards`,
                               business$attributes$`Price Range`,
                               business$attributes$Delivery,
                               business$attributes$`Take-out`,
                               business$attributes$`Takes Reservations`)
ResAtNames <- c('CreditCard', 'Price', 'Delivery', 'Takeout', 'Reservations')
Restaurant.att <- NA
for(i in 1:5){
  
  Restaurant.attributes[unlist(lapply(Restaurant.attributes[,i],is.null)),i]<-NA
  Restaurant.att<-cbind(Restaurant.att,unlist(Restaurant.attributes[,i]))
}
Restaurant.att<- Restaurant.att[,-1]
colnames(Restaurant.att)<- paste(ResAtNames)
Restaurant.att
Restaurant.good<-res7$attributes$`Good For`
reviews<-stream_in(file('~/Yelp/yelp_academic_dataset_review.json'))
reviews<-reviews[which(reviews$business_id %in% business$business_id),]
photos<-photos[which(photos$business_id %in% business$business_id),]
