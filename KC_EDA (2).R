library(dplyr)
library(lubridate)
library(ggplot2)
library(lattice)
library(ggmap)
library(leaps) #regsubsets
library(reshape2) #melt
library(GGally) #ggcorr and ggpairs
library(car)   # vif() and qqPlot functions
library(randomForest)
library(dichromat)
library(leaflet)
library(gridExtra)
library(DT)
library(tidyverse)

#file <- "~/GMU/STAT 515/Final Project/King County House Sales/kc_house_data.csv"
#file2 <- "~/GMU/STAT 515/Final Project/King County House Sales/zipcodes.csv"
df <- read.csv(file="kc_house_data.csv")
zipcodes <- read.csv(file="zipcodes.csv", stringsAsFactors = FALSE)

ggplot(df,aes(x=bedrooms)) + geom_histogram(color="black", aes(fill=..count..))  + labs(x="Bedrooms",y="Count",title=" ")

#Plot home location map for King County
register_google(key = "AIzaSyAqdBCITsE82Xh7UsdTuyXAGdqEIhW4J3s")
myLocation <- "King County"
useLocation <- geocode(myLocation)
myMap <- get_map(myLocation, source = "google", maptype = "roadmap", 
                 crop = FALSE, zoom = 10)

k <- ggmap(myMap)

clustering<-read.csv("kc_house_data.csv", header=TRUE)

# Clustering Price for Map Index Purpose

set.seed(1)

priceclustering <- clustering %>% select(price)

# Build a kmeans model
model_km3 <- kmeans(priceclustering, centers = 5)

# Extract the cluster assignment vector from the kmeans model
clust_km3 <- model_km3$cluster

# Create a new dataframe appending the cluster assignment
price_cluster <- mutate(clustering, cluster = clust_km3)

price_index <- function(x){
  for(i in 1:nrow(x)){
    if(x[i,"cluster"] == 1){
      x[i,"index"] = '75,000 - 409,500'
    } else if(x[i,"cluster"] == 5) {
      x[i,"index"] = '409,501 - 673,500'
    } else if(x[i,"cluster"] == 3){
      x[i,"index"] = '673,501 - 1,137,500'
    } else if(x[i,"cluster"] == 2){
      x[i,"index"] = '1,137,501 - 2,140,500'
    } else {
      x[i,"index"] = '2,140,501 - 7,700,000'
    }    
  }
  return(x)
}

price_map <- price_index(price_cluster)
price_map$index <- as.factor(price_map$index)

house1 <- filter(price_map, cluster == 1)
house2 <- filter(price_map, cluster == 2)
house3 <- filter(price_map, cluster == 3)
house4 <- filter(price_map, cluster == 4)
house5 <- filter(price_map, cluster == 5)

# Color Index    
pal <- colorFactor(palette = c("blue", "red", "green", "pink", "orange"), 
                   levels = c('75,000 - 409,500', '409,501 - 673,500', '673,501 - 1,137,500', 
                              '1,137,501 - 2,140,500', '2,140,501 - 7,700,000'))

# Draw a Map
leaflet(options = leafletOptions(minZoom = 9, dragging = TRUE)) %>% 
  addProviderTiles(provider = 'CartoDB')%>%
  addCircleMarkers(data = house1, radius = 1, 
                   popup = ~paste0("<b>", 'USD ', price, "</b>", "<br/>", "House area (sqft): ", 
                                   sqft_living15, "<br/>", "Lot area (sqft): ", sqft_lot15),
                   color = ~pal(index),  group = '75,000 - 409,500') %>%
  addCircleMarkers(data = house5, radius = 1, 
                   popup = ~paste0("<b>", 'USD ', price, "</b>", "<br/>", "House area (sqft): ", 
                                   sqft_living15, "<br/>", "Lot area (sqft): ", sqft_lot15),
                   color = ~pal(index),  group = '409,501 - 673,500') %>%
  addCircleMarkers(data = house3, radius = 1, 
                   popup = ~paste0("<b>", 'USD ', price, "</b>", "<br/>", "House area (sqft): ", 
                                   sqft_living15, "<br/>", "Lot area (sqft): ", sqft_lot15),
                   color = ~pal(index),  group = '673,501 - 1,137,500') %>%
  addCircleMarkers(data = house2, radius = 1, 
                   popup = ~paste0("<b>", 'USD ', price, "</b>", "<br/>", "House area (sqft): ", 
                                   sqft_living15, "<br/>", "Lot area (sqft): ", sqft_lot15),
                   color = ~pal(index),  group = '1,137,501 - 2,140,500') %>%
  addCircleMarkers(data = house4, radius = 1, 
                   popup = ~paste0("<b>", 'USD ', price, "</b>", "<br/>", "House area (sqft): ", 
                                   sqft_living15, "<br/>", "Lot area (sqft): ", sqft_lot15),
                   color = ~pal(index),  group = '2,140,501 - 7,700,000') %>%
  setView(lng = -122.001008, lat = 47.474443, zoom = 9) %>%
  addLegend(pal = pal, 
            values = c('75,000 - 409,500', '409,501 - 673,500', '673,501 - 1,137,500', 
                       '1,137,501 - 2,140,500', '2,140,501 - 7,700,000'),
            opacity = 0.5, title = "Price Range", position = "bottomright") %>%
  addLayersControl(overlayGroups = c('75,000 - 409,500', '409,501 - 673,500', '673,501 - 1,137,500', '
                                     1,137,501 - 2,140,500', '2,140,501 - 7,700,000'), position = "bottomleft")

#Only plot half of the observations using the training set for better visualization
#Create a flag variable for home prices by quartiles
#1st quartile is $75,000-$321,950
#2nd quartile is $321,950-$450,000
#3rd quartile is $450,000-645,000
#4th quartile is $645,000-$7,700,000
df$Million <- ifelse(df$price>=1000000, "Yes", "No")
df$Quantile <- NA
for(i in 1:dim(df)[1]){
  if(df$price[i] <=321950){
    df$Quantile[i] = "1st Quartile"
  }
  if(df$price[i] > 321950 & df$price[i] <= 450000){
    df$Quantile[i] = "2nd Quartile"
  }
  if(df$price[i] > 450000 & df$price[i] <= 645000){
    df$Quantile[i] = "3rd Quartile"
  }
  if(df$price[i] > 645000){
    df$Quantile[i] = "4th Quartile"
  }  
}

#Create log of price variable
df$pricelog<-log(df$price)

k + geom_point(aes(x=long, y=lat, colour=Quantile), size = 1, data=df) + 
              scale_colour_brewer(type = "qual", palette = "Paired") + 
              labs(x = "Longitude", y = "Latitude", title = "King County Home Sales")
              
ggplot(df, aes(x=price, fill=zipcode)) + geom_histogram(binwidth = 50) + scale_fill_brewer()

#x<-seq(0, 1, by = 0.1)
#quantile(df$price, x) #75% of homes are under $645,000

#table(df$price>=1000000) #1492 homes are over $1,000,000

#Set categorical variables as factors
df$waterfront <- as.factor(df$waterfront)
levels(df$waterfront)[1] <- "Not on waterfront"
levels(df$waterfront)[2] <- "On waterfront"

df$yr_built <-as.factor(df$yr_built)
levels(df$yr_built)[1:40] <- "Pre 1940s"
levels(df$yr_built)[2:11] <- "1940s"
levels(df$yr_built)[3:12] <- "1950s"
levels(df$yr_built)[4:13] <- "1960s"
levels(df$yr_built)[5:14] <- "1970s"
levels(df$yr_built)[6:15] <- "1980s"
levels(df$yr_built)[7:16] <- "1990s"
levels(df$yr_built)[8:17] <- "2000s"
levels(df$yr_built)[9:14] <- "2010s"


df$yr_renovated <- as.factor(df$yr_renovated)
levels(df$yr_renovated)[1] <- "Not renovated"
levels(df$yr_renovated)[2:70] <- "Renovated"


##Remove trailing sequence on date variable
##Then extract month, day, and year to separate variables
df$date <- substring(df$date, first = 1, last = 8)
df <- df %>% mutate(month=month(ymd(df$date)), 
                    year=year(ymd(df$date)),
                    day=day(ymd(df$date)))

df$month <- as.factor(df$month)
levels(df$month)[1] <- "January"
levels(df$month)[2] <- "February"
levels(df$month)[3] <- "March"
levels(df$month)[4] <- "April"
levels(df$month)[5] <- "May"
levels(df$month)[6] <- "June"
levels(df$month)[7] <- "July"
levels(df$month)[8] <- "August"
levels(df$month)[9] <- "September"
levels(df$month)[10] <- "October"
levels(df$month)[11] <- "November"
levels(df$month)[12] <- "December"

#Reorder df for merge
df <- df[,c(17, 1:16, 18:27)]

##Remove characters from zipcode df
zipcodes$ZIP.Code <- substring(zipcodes$ZIP.Code, first = 10, last = 14)
zipcodes$Population <- as.numeric(gsub(",", "", zipcodes$Population))
names(zipcodes)[1] <- "zipcode"
zipcodes$zipcode <- as.numeric(zipcodes$zipcode)

#Merge KC house data with zipcodes dataframes
df <- left_join(df, zipcodes, by = "zipcode")

#Remove id, date, lat, long, year, day as they do not seem relevant
#Also reorder variables
df <- df[,c(4:17, 20:21, 25, 28:32, 1, 22:24)]

#Check correlations for continuous variables
#Exclude waterfront, yr_built, yr_renovated, zipcode, and month
corr.matrix <- round(cor(df[,c(1:6, 8:12, 15:16)]), 2)
corrMelt <- melt(corr.matrix,varnames=c("x","y"),value.name="Correlation")
corrMelt <- corrMelt[order(corrMelt$Correlation),]

corrplot<-ggplot(corrMelt,aes(x=x,y=y)) + geom_tile(aes(fill=Correlation))
corrplot

ggpairs(df[,c(1:6, 8:12, 16:17)])
ggcorr(df[,c(1:6, 8:12, 16:17)])
#ggcorr has option low,mid and high or palette to specify colors.
ggcorr(df[,c(1:5, 11:12, 16:17, 6, 8:10)],label=TRUE,label_alpha=TRUE,label_round=2)
##6 square footage variables, we want to remove all except sqft_living
ggcorr(df[,c(1:4, 6, 8:10)],label=TRUE,label_alpha=TRUE,label_round=2)

#Grab population by City
Pop <- df %>% group_by(zipcode, City, Population) %>% summarise(N = n())
Pop <- Pop %>% group_by(City) %>% summarise(TotalPop = sum(Population), N = sum(N))

#Merge Total Population onto dataset
df <- df[,c(19, 1:18, 20:26)]
df <- left_join(df, Pop, by = "City")
df <- df[, c(2:19, 1, 20:28)]

#Create factor variable based on population size
df$PopCat <- NA
for (i in 1:dim(df)[1]) {
  if(df$TotalPop[i] < 20000){
    df$PopCat[i] <- "Less than 20,000"
  }
  else if(df$TotalPop[i] < 50000){
    df$PopCat[i] <- "Between 20,000 and 50,000"
  }
  else if(df$TotalPop[i] < 100000){
    df$PopCat[i] <- "Between 50,000 and 100,000"
  }
  else{
    df$PopCat[i] <- "More than 100,000"
  }
}
df$PopCat <- as.factor(df$PopCat)
df$PopCat <- factor(df$PopCat, levels(df$PopCat)[c(3,1,2,4)])
df$City <- as.factor(df$City)              
df <- df[,c(1:17, 19, 18, 20:29)]

#Simple linear regression models
lm.p1 <- lm(price~bedrooms, df)
p1.sum <- summary(lm.p1) #significant, B1 = 121716, Rsq= .10
lm.p1 <- lm(price~bathrooms, df)
p2.sum <- summary(lm.p1) #significant, B1 = 250327, Rsq= .28
lm.p1 <- lm(price~sqft_living, df)
p3.sum <- summary(lm.p1) #significant, B1 = 280.624, Rsq= .49
lm.p1 <- lm(price~sqft_lot, df)
p4.sum <- summary(lm.p1) #significant, B1 = .7947, Rsq= .01
lm.p1 <- lm(price~floors, df)
p5.sum <- summary(lm.p1) #significant, B1 = 174589, Rsq= .07
lm.p1 <- lm(price~waterfront, df)
p6.sum <- summary(lm.p1) #significant, B1 = 1130312, Rsq= .07
lm.p1 <- lm(price~view, df)
p7.sum <- summary(lm.p1) #significant, B1 = 190335, Rsq= .16
lm.p1 <- lm(price~condition, df)
p8.sum <- summary(lm.p1) #significant, B1 = 20514, Rsq= .001
lm.p1 <- lm(price~grade, df)
p9.sum <- summary(lm.p1) #significant, B1 = 208458, Rsq= .45
lm.p1 <- lm(price~sqft_above, df)
p10.sum <- summary(lm.p1) #significant, B1 = 268.5, Rsq= .37
lm.p1 <- lm(price~sqft_basement, df)
p11.sum <- summary(lm.p1) #significant, B1 = 268.6, Rsq= .10
lm.p1 <- lm(price~yr_built, df)
p12.sum <- summary(lm.p1) #significant, different betas, Rsq= .03
lm.p1 <- lm(price~yr_renovated, df)
p13.sum <- summary(lm.p1) #significant, B1 = 230018, Rsq= .02
lm.p1 <- lm(price~sqft_living15, df)
p14.sum <- summary(lm.p1)  #significant, B1 = 313.556, Rsq= .34
lm.p1 <- lm(price~sqft_lot15, df)
p15.sum <- summary(lm.p1) #significant, B1 = 1.109, Rsq= .007
lm.p1 <- lm(price~month, df)
p16.sum <- summary(lm.p1) #significant, different betas, Rsq= .001
lm.p1 <- lm(price~City, df)
p17.sum <- summary(lm.p1) #significant, different betas, Rsq= .04

Results <- data.frame(Variable = names(df)[2:18], RSS = NA, RSq = NA)
Results[1,2] <- p1.sum$sigma
Results[1,3] <- round(p1.sum$r.squared, 3)
Results[2,2] <- p2.sum$sigma
Results[2,3] <- round(p2.sum$r.squared, 3)
Results[3,2] <- p3.sum$sigma
Results[3,3] <- round(p3.sum$r.squared, 3)
Results[4,2] <- p4.sum$sigma
Results[4,3] <- round(p4.sum$r.squared, 3)
Results[5,2] <- p5.sum$sigma
Results[5,3] <- round(p5.sum$r.squared, 3)
Results[6,2] <- p6.sum$sigma
Results[6,3] <- round(p6.sum$r.squared, 3)
Results[7,2] <- p7.sum$sigma
Results[7,3] <- round(p1.sum$r.squared, 3)
Results[8,2] <- p8.sum$sigma
Results[8,3] <- round(p8.sum$r.squared, 3)
Results[9,2] <- p9.sum$sigma
Results[9,3] <- round(p9.sum$r.squared, 3)
Results[10,2] <- p10.sum$sigma
Results[10,3] <- round(p10.sum$r.squared, 3)
Results[11,2] <- p11.sum$sigma
Results[11,3] <- round(p11.sum$r.squared, 3)
Results[12,2] <- p12.sum$sigma
Results[12,3] <- round(p12.sum$r.squared, 3)
Results[13,2] <- p13.sum$sigma
Results[13,3] <- round(p13.sum$r.squared, 3)
Results[14,2] <- p14.sum$sigma
Results[14,3] <- round(p14.sum$r.squared, 3)
Results[15,2] <- p15.sum$sigma
Results[15,3] <- round(p15.sum$r.squared, 3)
Results[16,2] <- p16.sum$sigma
Results[16,3] <- round(p16.sum$r.squared, 3)
Results[17,2] <- p17.sum$sigma
Results[17,3] <- round(p17.sum$r.squared, 3)

#Model selection. 
#From simple linear regression models remove variables with low r-squared values and
#extra square footage variables due to possible collinearity and known correlations
#from above

#Remove bedroom outlier observations
df<-df[-which(df$bedrooms==33),]
df<-df[-which(df$bedrooms==0),]

set.seed(100)
train = sample(c(TRUE,FALSE), prob=c(0.75, 0.25), nrow(df), replace = TRUE)
test = !train

##Create 5 variable model with interactions based on results from correlation plots
#and results from the simple linear regression models.

lm.seven <- lm(price~sqft_living+bedrooms+bathrooms+view+grade+waterfront+City, df[train,])
lm.summary7 <- summary(lm.seven)

lm.six <- lm(price~sqft_living+bedrooms+view+grade+waterfront+City, df[train,])
lm.summary6 <- summary(lm.six)

# Check diagnostics plot
windows()
par(mfrow = c(2,2))
plot(lm.six)

#Check test MSEs
#Obtain the test set design matrix
test.mat = model.matrix(price~sqft_living+bedrooms+
                        view+grade+waterfront+City, 
                        data = df[test,])

val.errors = rep(NA,2)

# obtain the training set coefficients
  coefi = coef(lm.six)
  # predict test set values
  pred = test.mat[,names(coefi)] %*% coefi
  # Obtain the MSE
  val.errors[1] = mean(
    (df$price[test] - pred)^2)
  
round(val.errors)
loc = which.min(val.errors)
loc

##The model with interaction terms has the lower test MSE.

##Reviewing the initial distribution of residuals we see that the model does not do well fitting 
##million dollar homes as 90% of the data are not over a million dollars.

##As we can see from the residuals plot as the fitted values get larger the variability in residuals
##increases greatly.  This also indicates non-constant variance or heteroscedasticity of error terms.


##Decision Trees and Random Forest

##Subset the dataframe to exclude unnecessary variables for RF
df2 <- df[,c(1:18)]

rf.kc=randomForest(price~.,data=df2,subset=train,mtry=4, ntree=500, importance=TRUE)

#Check how does the rf model performs on test set
yhat.rf = predict(rf.kc,newdata=df2[-train,])
kc.test <- df2[-train,"price"]
mean(sqrt((yhat.rf-kc.test)^2)) #test set MSE

#Plot predicted vs test values of medv
plot(yhat.rf, kc.test)
abline(0,1)
##A bagged tree is much more difficult to interpret than a single tree.
#We can determine which variables are important by looking at variable importance
importance(rf.kc)
varImp(rf.kc)


