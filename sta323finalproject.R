library(jsonlite)
library(dplyr)

business <- stream_in(file('D:\\Yelp\\yelp_academic_dataset_business.json'))
photos<-stream_in(file('D:\\YelpPhotos\\photo_id_to_business_id.json'))

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


reviews<-stream_in(file('D:\\Yelp\\yelp_academic_dataset_review.json'))
reviews<-reviews[which(reviews$business_id %in% business$business_id),]
photos<-photos[which(photos$business_id %in% business$business_id),]
