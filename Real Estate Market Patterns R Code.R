housing <- read.csv("housing.csv")
schools <- read.csv("schools.csv")

summary(housing)
head(housing)
dim(housing)
str(housing)

#Getting rid of the missing values by removing associated rows from the dataset
housing <- housing[which(housing$levels != "?"),]
housing <- housing[which(housing$cooling != ""),]
housing <- housing[which(housing$heating != ""),]
housing <- housing[which(housing$fireplace != ""),]
housing <- housing[!is.na(housing$sqft),]
housing <- housing[!is.na(housing$lotsize),]

#Converting the categorical variables into factors and fixing spelling errors in the levels
housing$neighborhood <- factor(housing$neighborhood,levels=c("Red","Purple","Orange","Blue","Silver","Yellow","Green","Gold"))
neighcol <- c("firebrick1","orchid","tan1","skyblue1","lightgrey","khaki1","darkseagreen3","gold1")
housing$type <- factor(housing$type,levels=c("condo","condominium","town house","townhouse","single-family home","multi-family home"),labels=c("condo","condo","townhouse","townhouse","single-family home","multi-family home"))
typecol <- c("tomato","violet","paleturquoise","slateblue1")
housing$cooling <- factor(housing$cooling)
housing$heating <- factor(housing$heating)
housing$fireplace <- factor(housing$fireplace)
housing$elementary <- factor(housing$elementary)
housing$middle <- factor(housing$middle)
housing$high <- factor(housing$high)
housing$levels <- as.numeric(housing$levels)

#Getting rid of the outliers that don't make sense
boxplot(housing$beds,ylab="Number of Bedrooms",col="salmon3",main="Before Cleaning")
housing <- housing[housing$beds<900,]
boxplot(housing$beds,ylab="Number of Bedrooms",col="salmon3",main="After Cleaning")

boxplot(housing$baths,ylab="Number of Bathrooms",col="salmon3",main="Before Cleaning")
housing <- housing[housing$baths<20,]
boxplot(housing$baths,ylab="Number of Bathrooms",col="salmon3",main="After Cleaning")

boxplot(housing$year,ylab="Year",col="salmon3",main="Before Cleaning")
housing <- housing[housing$year<2024 & housing$year>1600,]
boxplot(housing$year,ylab="Year",col="salmon3",main="After Cleaning")

boxplot(housing$soldprice,ylab="House Prices in Dollars",col="salmon3",main="Before Cleaning")
housing <- housing[housing$soldprice>1000,]
housing$soldprice <- housing$soldprice/1000 #dividing house prices by a thousand to increase readability in graphs
boxplot(housing$soldprice,ylab="House Prices in Thousands of Dollars",col="salmon3",main="After Cleaning")

#Cleaning the schools dataset
summary(schools)
dim(schools)
schools$school <- factor(schools$school)

library(tidyr)
schools <- separate(schools,school,into=c("school_name","grade"),sep=" ")
schools$school_name <- factor(schools$school_name)
schools$grade <- factor(schools$grade,labels=c("Elementary","Middle","High"))

elementary_schools <- schools[schools$grade=="Elementary",]
elementary_schools$school <- paste(elementary_schools$school_name, elementary_schools$grade)
elementary_schools <- elementary_schools[,c("school","size","rating")]
elementary_schools$school <- factor(elementary_schools$school)
names(elementary_schools)[names(elementary_schools)=="size"] <- "elementary_size"
names(elementary_schools)[names(elementary_schools)=="rating"] <- "elementary_rating"
summary(elementary_schools)

middle_schools <- schools[schools$grade=="High",]
middle_schools$school <- paste(middle_schools$school_name, "Middle")
middle_schools <- middle_schools[,c("school","size","rating")]
middle_schools$school <- factor(middle_schools$school)
names(middle_schools)[names(middle_schools)=="size"] <- "middle_size"
names(middle_schools)[names(middle_schools)=="rating"] <- "middle_rating"
summary(middle_schools)

high_schools <- schools[schools$grade=="Middle",]
high_schools$school <- paste(high_schools$school_name, "High")
high_schools <- high_schools[,c("school","size","rating")]
high_schools$school <- factor(high_schools$school)
names(high_schools)[names(high_schools)=="size"] <- "high_size"
names(high_schools)[names(high_schools)=="rating"] <- "high_rating"
summary(high_schools)

### Merging schools with the housing dataset

#First, I merge the housing dataset with the elementary school dataset
me <- merge(housing,elementary_schools,by.x="elementary",by.y="school",all.x=T)
summary(me)

#Next, I merge the housing and elementary combined dataset with the middle school dataset
mm <- merge(me,middle_schools,by.x="middle",by.y="school",all.x=T)
summary(mm)

#Lastly, I combine the high school dataset with the previous dataset
m.all <- merge(mm,high_schools,by.x="high",by.y="school",all.x=T)
summary(m.all) #This is my final merged dataset that I will be using in all my further analyses
dim(m.all)

#### Single Variable Visuals

neighcount <- table(m.all$neighborhood)
barplot(neighcount,col=neighcol,ylab="Count",xlab="Neighborhood",main="Frequency Barplot of Neighborhoods")

typecount <- table(m.all$type)
barplot(typecount,col=typecol,ylab="Count",xlab="Type of House",main="Frequency Barplot of House Types")

#Comparing student body sizes of different grade levels
par(mfrow = c(1,3))	
boxplot(m.all$elementary_size,ylim=c(500,1300),main="Elementary Schools",col="palevioletred1",ylab="Number of Students")
boxplot(m.all$middle_size,ylim=c(500,1300),main="Middle Schools",col="lightgreen",ylab="Number of Students")
boxplot(m.all$high_size,ylim=c(500,1300),main="High Schools",col="paleturquoise3",ylab="Number of Students")

par(mfrow = c(1,3))	
boxplot(m.all$elementary_rating,ylim=c(0,10),main="Elementary Schools",col="palevioletred1",ylab="Rating")
boxplot(m.all$middle_rating,ylim=c(0,10),main="Middle Schools",col="lightgreen",ylab="Rating")
boxplot(m.all$high_rating,ylim=c(0,10),main="High Schools",col="paleturquoise3",ylab="Rating")

hist(m.all$soldprice,freq=F,col="darkolivegreen3",xlab="Thousands of Dollars",main="Distribution of House Selling Prices",breaks=seq(400,2400,100))
priced <- density(m.all$soldprice)
lines(priced,col="darkgoldenrod1",lwd=3)

### Bar graphs with the merged dataset

#Comparing elementary school ratings to house prices in different neighborhoods
par(mfrow = c(1,2))	
priceme <- aggregate(soldprice~neighborhood,data=m.all,FUN=mean)
barplot(priceme$soldprice,names.arg=priceme$neighborhood,col=neighcol,xlab="Neighborhood",ylab="Thousands of Dollars",main="Mean Selling Prices of Houses by Neighborhood")
ratingme <- aggregate(elementary_rating~neighborhood,data=m.all,FUN=mean)
barplot(ratingme$elementary_rating,names.arg=ratingme$neighborhood,col=neighcol,xlab="Neighborhood",ylab="Mean Elementary School Rating",main="Mean Elementary Scool Ratings by Neighborhood")

#Comparing middle school ratings to house prices in different neighborhoods
par(mfrow = c(1,2))	
pricemm <- aggregate(soldprice~neighborhood,data=m.all,FUN=mean)
barplot(pricemm$soldprice,names.arg=pricemm$neighborhood,col=neighcol,xlab="Neighborhood",ylab="Thousands of Dollars",main="Mean Prices of Sold Houses by Neighborhood")
ratingmm <- aggregate(middle_rating~neighborhood,data=m.all,FUN=mean)
barplot(ratingmm$middle_rating,names.arg=ratingmm$neighborhood,col=neighcol,xlab="Neighborhood",ylab="Mean Middle School Rating",main="Mean Middle Scool Ratings by Neighborhood")

#Comparing high school ratings to house prices in different neighborhoods
par(mfrow = c(1,2))	
pricemh <- aggregate(soldprice~neighborhood,data=m.all,FUN=mean)
barplot(pricemh$soldprice,names.arg=pricemh$neighborhood,col=neighcol,xlab="Neighborhood",ylab="Thousands of Dollars",main="Mean Prices of Sold Houses by Neighborhood")
ratingmh <- aggregate(high_rating~neighborhood,data=m.all,FUN=mean)
barplot(ratingmh$high_rating,names.arg=ratingmh$neighborhood,col=neighcol,xlab="Neighborhood",ylab="Mean High School Rating",main="Mean High Scool Ratings by Neighborhood")

#Other Bar graphs

neighprice <- aggregate(soldprice~neighborhood,data=housing,FUN=mean)
barplot(neighprice$soldprice,names.arg=neighprice$neighborhood,data=m.all,col=c("firebrick1","orchid","tan1","skyblue1","lightgrey","khaki1","darkseagreen3","gold1"),ylab="Mean Price in Thousands of Dollars",xlab="Neighborhood",main="Mean Selling House Prices by Neighborhood")

housing$type <- factor(housing$type, levels=c("condo","townhouse","single-family home","multi-family home"))
typrice <- aggregate(soldprice~type,data=m.all,FUN=mean)
barplot(typrice$soldprice,names.arg=typrice$type,data=typrice,col=typecol,ylab="Mean Price in Thousands of Dollars",xlab="Type of House",main="Mean Selling House Prices by House Type")

#Square Footage across neighborhoods
sqftneigh <- aggregate(sqft~neighborhood,data=m.all,FUN=mean)
barplot(sqftneigh$sqft,names.arg=sqftneigh$neighborhood,col=neighcol,ylab="Mean Square Footage",xlab="Neighborhood",main="Mean Square Footage By Neighborhood") #we can observe that the purple neighborhood has an higher average sqft per house compared to all other neighborhoods.

#Square footage across house types
sqftype <- aggregate(sqft~type,data=m.all,FUN=mean)
barplot(sqftype$sqft,names.arg=sqftype$neighborhood,col=typecol,ylab="Mean Square Footage",xlab="House Type",main="Mean Square Footage By House Type") 

## Regression Analysis ##
cor(m.all[,c(5:9,15:21)])
#beds, baths, sqft, and lotsize variables are all highly correlated to each other so I decided to only include one of these variables in my regression analysis to avoid multicollinearity problem. I chose to include the beds variable since it had the highest correlation to selling prices out of the 4 variables.
plot(m.all[,c(5:9,15:21)])
plot(m.all[,c(5,9,15:21)])

mprice <- lm(soldprice~neighborhood+type+levels+cooling+heating+fireplace+year+beds,data=m.all)
step(mprice,direction="backward")
mprice <- lm(soldprice~neighborhood+type+heating+fireplace+year+beds,data=m.all)
summary(mprice)
plot(mprice)

#Regression for elementary school
melr <- lm(soldprice~elementary_rating,data=m.all)
summary(melr)
plot(soldprice~elementary_rating,data=m.all,col="palevioletred1",ylab="Thousands of Dollars",xlab="Rating of Assigned Elementary School",main="House Selling Prices by Assigned Elementary School Ratings")
abline(melr,col="black")
text(3.75,2250,"Selling Price = 948.68 + 52.14 * (Elementary Rating)",cex=0.75)

#Regression for middle school 
mmir <- lm(soldprice~middle_rating,data=m.all)
summary(mmir)
plot(soldprice~middle_rating,data=m.all,col="lightgreen",ylab="Thousands of Dollars",xlab="Rating of Assigned Middle School",main="House Selling Prices by Assigned Middle School Ratings")
abline(mmir,col="black")
text(3.9,2250,"Selling Price = 716.89 + 86.30 * (Middle Rating)",cex=0.75)

#Regression for high school
mhir <- lm(soldprice~high_rating,data=m.all)
summary(mhir)
plot(soldprice~high_rating,data=m.all,col="paleturquoise3",ylab="Thousands of Dollars",xlab="Rating of Assigned High School",main="House Selling Prices by Assigned High School Ratings")
abline(mhir,col="black")
text(3.5,2250,"Selling Price = 846.96 + 67.86 * (High Rating)",cex=0.75)

### Clustering ####

#Seeded k-means clustering for house type
set.seed(900)
(kmeans <- kmeans(m.all[,c("year","soldprice")],2))
plot(m.all[,c("year","soldprice")],col=kmeans$cluster,ylab="Price in Thousands of Dollars",xlab="Year the house was built",main="Selling House Prices by Year Separated by Groupings")
points(kmeans$centers,col=c("blue","purple"),pch=8,cex=2)
legend("topleft",c("Grouping 1","Grouping 2"),col=c("black","red"),pch=1)
text(1920,1500,"Center for Grouping 1\n (1982.28 1506.63)",col="blue",cex=0.75)
text(1920,770,"Center for Grouping 2\n (1970.75, 881.18)",col="purple",cex=0.75)

m.all$type2 <- kmeans$cluster
m.all$type2 <- factor(m.all$type2)

table(m.all$type,m.all$type2)

mcl1 <- lm(soldprice~year,data=m.all[m.all$type2==1,])
mcl2 <- lm(soldprice~year,data=m.all[m.all$type2==2,])

summary(mcl1)
summary(mcl2)

#Scatterplot of selling prices by type of house
plot(m.all$soldprice[m.all$type=="condo"]~m.all$year[m.all$type=="condo"],col="red",ylim=c(400,2400),ylab="Price in Thousands of Dollars",xlab="Year the house was built",main="Selling Prices of Different Types of Houses by Year")
points(m.all$soldprice[m.all$type=="townhouse"]~m.all$year[m.all$type=="townhouse"],col="orange")
points(m.all$soldprice[m.all$type=="single-family home"]~m.all$year[m.all$type=="single-family home"],col="blue")
points(m.all$soldprice[m.all$type=="multi-family home"]~m.all$year[m.all$type=="multi-family home"],col="purple")
legend("topleft",c("condo","townhouse","single-family home","multi-family home"),col=c("red","orange","blue","purple"),pch=1)
abline(mcl1,col="turquoise4",lwd=2)
abline(mcl2,col="violetred",lwd=2)
text(1920,1400,"Grouping 1",col="turquoise4")
text(1920,800,"Grouping 2",col="violetred")


#Histogram of selling prices separated by house type
hist(m.all$soldprice[m.all$type=="condo"],col=rgb(0,1,0,0.25),freq=F,ylim=c(0,0.0025),xlim=c(400,2400),breaks=seq(400,2400,100),xlab="Thousands of Dollars",main="Distribution of Selling House Prices by Type of House")
hist(m.all$soldprice[m.all$type=="townhouse"],add=T,col=rgb(0,0,1,0.25),freq=F,breaks=seq(400,2400,100))
hist(m.all$soldprice[m.all$type=="single-family home"],add=T,col=rgb(1,1,0,0.25),freq=F,breaks=seq(400,2400,100))
hist(m.all$soldprice[m.all$type=="multi-family home"],add=T,col=rgb(1,0,1,0.25),freq=F,breaks=seq(400,2400,100))
legend("topright",c("condo","townhouse","single-family home","multi-family home"),col=c(rgb(0,1,0,0.25),rgb(0,0,1,0.25),rgb(1,1,0,0.25),rgb(1,0,1,0.25)),lwd=10,title="Type of House")
denstype<-density(m.all$soldprice,bw=50)
lines(denstype,col="red")

#Same as above but for the kmeans clusters
hist(m.all$soldprice[m.all$type2==1],col=rgb(0,0,1,0.25),freq=F,ylim=c(0,0.0025),xlim=c(400,2400),breaks=seq(400,2400,100),xlab="Thousands of Dollars",main="Distribution of Sold House Prices by Type of House")
hist(m.all$soldprice[m.all$type2==2],add=T,col=rgb(1,1,0,0.25),freq=F,breaks=seq(400,2400,100))
legend("topright",c("Grouping 1","Grouping 2"),col=c(rgb(0,0,1,0.25),rgb(1,1,0,0.25)),lwd=10)
denstype<-density(m.all$soldprice,bw=50)
lines(denstype,col="red")

##### Pie chart
table(m.all$neighborhood,m.all$type)

a.condo <- c(25,2,26,26,14,22,19,6)
a.town <- c(26,28,27,8,5,15,11)
a.single <- c(58,1,79,56,32,27,35,25)
a.multi <- c(17,8,10,22,5)
condnames <- c("Red\n 18%","Purple\n 1%","Orange\n 19%","Blue\n 19%","Silver\n 10%","Yellow\n 16%","Green\n 14%","Gold\n 4%")
townames <- c("Red\n 22%","Orange\n 23%","Blue\n 23%","Silver\n 7%","Yellow\n 4%","Green\n 12%","Gold\n 9%")
towcol <- c("firebrick1","tan1","skyblue1","lightgrey","khaki1","darkseagreen3","gold1")
singnames <- c("Red\n 19%","Purple\n <1%","Orange\n 25%","Blue\n 18%","Silver\n 10%","Yellow\n 9%","Green\n 11%","Gold\n 8%")
multinames <- c("Blue\n 27%","Silver\n 13%","Yellow\n 16%","Green\n 35%","Gold\n 8%")
mulcol <- c("skyblue1","lightgrey","khaki1","darkseagreen3","gold1")
par(mfrow = c(1,4))	
pie(a.condo,labels=condnames,col=neighcol,main="Condos")
pie(a.town,labels=townames,col=towcol,main="Townhouses")
pie(a.single,labels=singnames,col=neighcol,main="Single-Family Homes")
pie(a.multi,labels=multinames,col=mulcol,main="Multi-Family Homes")

### High Density Plot
library(car)
sp(soldprice~year,data=m.all,jitter=list(x=2,y=2),smooth=T,regLine=F,col="salmon3",ylab="Price in Thousands of Dollars",xlab="Year the house was built",main="Density Distribution of Selling House Prices By Year",boxplots=F)


##### Sensitivity Analysis ######
#Can run all analyses again after running the sensitivity analysis.

cor(housing[,c(2:6,8,15)])
housing <- read.csv("housing.csv")
schools <- read.csv("schools.csv")
summary(housing)

#### Numerical Variables:

#predicting missing values in the sqft variable
housing[is.na(housing$sqft),]
msqft0 <- lm(sqft~neighborhood+beds+baths+lotsize+year+levels+soldprice+type,data=housing)
step(msqft0,direction="backward")

msqft <- lm(sqft~beds+levels+soldprice+type,data=housing)
housing$sqft[is.na(housing$sqft)] <- predict(msqft,list(beds=housing$beds[is.na(housing$sqft)],levels=housing$levels[is.na(housing$sqft)],soldprice=housing$soldprice[is.na(housing$sqft)],type=housing$type[is.na(housing$sqft)]))

#predicting missing values in the lotsize variable
housing[is.na(housing$lotsize),]
mlot <- lm(lotsize~sqft+soldprice+type,data=housing)
housing$lotsize[is.na(housing$lotsize)] <- predict(mlot,list(sqft=housing$sqft[is.na(housing$lotsize)],soldprice=housing$soldprice[is.na(housing$lotsize)],type=housing$type[is.na(housing$lotsize)]))

#predicting missing values in the levels variable
housing$levels <- as.numeric(housing$levels)
housing[is.na(housing$levels),]
mlevels <- lm(levels~beds+sqft+type,data=housing)
housing$levels[is.na(housing$levels)] <- predict(mlevels,list(beds=housing$beds[is.na(housing$levels)],sqft=housing$sqft[is.na(housing$levels)],type=housing$type[is.na(housing$levels)]))

##### Categorical Variables:

table(housing$cooling) #67% of our non-missing observations are "No"
table(housing$heating) #78% of our non-missing observations are "No"
table(housing$fireplace) #68% of our non-missing observations are "No"

#Filling in missing values for cooling, heating, and fireplace variables using single imputation
housing$cooling[housing$cooling==""] <- "No"
housing$heating[housing$heating==""] <- "No"
housing$fireplace[housing$fireplace==""] <- "No"

