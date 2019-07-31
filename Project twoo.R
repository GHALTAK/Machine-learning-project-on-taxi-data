library(tidyverse)
library(lubridate)
library(ggplot2)
library(tree)
library(rpart)
library(C50)
library(arulesCBA)
library(chron)
library(e1071)
train$pickup_datetime<-as.numeric(train$pickup_datetime)
#converting in to posix format 
newtrain$pickup_datetime<-as.POSIXct(
  as.character(newtrain$pickup_datetime))
newtrain <- newtrain %>%
  mutate(pickup_datetime = ymd_hms(pickup_datetime),
         dropoff_datetime = ymd_hms(dropoff_datetime))
newtrain <- newtrain %>%
mutate(newtrain$pickup_datetime1<-chron(
  as.character(newtrain$pickup_datetime)),format= c(dates= "y-m-d", times="h:m:s"))     

train %>%
  ggplot(aes(trip_duration)) +
  geom_histogram(fill = "red", bins = 150) +
  scale_x_log10() +
  scale_y_sqrt()
Hide
train %>%
  filter(train$pickup_datetime > ymd("2016-01-20") & 
           train$pickup_datetime < ymd("2016-02-10")) %>%
  ggplot(aes(pickup_datetime)) +
  geom_histogram(fill = "red", bins = 80)


p1 <- train %>%
  mutate(wday = wday(pickup_datetime, label = TRUE)) %>%
  group_by(wday, vendor_id) %>%
  summarise(median_duration = median(trip_duration)/60) %>%
  ggplot(aes(wday, median_duration, color = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Day of the week", y = "Median trip duration [min]")
newtrain <- train[ which(train$pickup_datetime > ymd("2016-02-01") & 
                          train$pickup_datetime < ymd("2016-03-01")),]

newtrain<-newtrain%>%
  mutate(tripdurationcat=cut(trip_duration,
            breaks = c(0,600,max(trip_duration))
          ,labels = c("0","1"),include.lowest = TRUE))
                

newtrain$pickup_date <- as.Date(newtrain$pickup_datetime) 
newtrain$pickup_time <- format(as.POSIXct(newtrain$pickup_datetime)
                               ,format = "%H:%M:%S") 
newtrain$pickup_time2<-strptime(
  newtrain$pickup_time,format = "%H:%M:%S")
newtrain$hours <- as.integer(
  as.character(newtrain$pickup_time2,format="%H"))

 newtrain$tripshifts<-cut(newtrain$hours,
                breaks = c(0,8,16,max(newtrain$hours))
                ,labels = c("1","2","3"),include.lowest = TRUE) 
  
 ggplot(newtrain,aes(newtrain$tripshifts,fill=tripshifts))+
   geom_bar(stat='count')+ coord_flip()+
   labs(title = "Barplot of Trip shifts",x = "Trip shift")
 ggplot(newtrain,aes(newtrain$tripdurationcat,fill=tripdurationcat))+
   geom_bar(stat='count')+ coord_flip()+
   labs(title = "Barplot of Trip duration based on 10min",x = "Trip duration")
 ggplot(newtrain,aes(newtrain$passenger_count,fill=passenger_count))+
   geom_bar(stat='count')+ coord_flip()+
   labs(title = "Barplot of number of passengers",x = "Number of passengers")
 newtrain$vendor_id<- as.factor(newtrain$vendor_id)
 ggplot(newtrain,aes(newtrain$vendor_id,fill=vendor_id))+
   geom_bar(stat='count')+ coord_flip()+
   labs(title = "Barplot of vendor",x = "vendor")
 ggplot(newtrain,aes(newtrain$wday1,fill=wday1))+
   geom_bar(stat='count')+ coord_flip()+
   labs(title = "Barplot of Weekdays",x = "Weekdays")
 ggplot(newtrain,aes(x="",y=newtrain$trip_duration))+
   geom_boxplot()+
   coord_flip()+
   labs(title = "Boxplot of tripduration ",
        x = "customers",
        y="tripduration")   
 
 newtrain$tripshifts<-as.numeric(newtrain$tripshifts)
 newtrain$wday<-wday(newtrain$pickup_datetime, label = FALSE)
 newtrain$wday1<- as.factor(newtrain$wday)
levels(newtrain$wday1)<-c("0","0","0","0","1","1","1")
newtrain$vendor_id<- as.factor(newtrain$vendor_id)
newtrain$passenger_count<- as.factor(newtrain$passenger_count)
newtrain$wday<- as.factor(newtrain$wday)

newtrain$wday<- as.numeric(newtrain$wday)
newtrain$vendor_id<- as.numeric(newtrain$vendor_id)
newtrain$passenger_count<- as.numeric(newtrain$passenger_count)
newtrain$tripshifts<- as.numeric(newtrain$tripshifts)
newtrain$tripshifts<- as.factor(newtrain$tripshifts)


set.seed(111)
sample <- sample.int(n = nrow(newtrain), 
                     size = floor(.60*nrow(newtrain)), replace = F)
train <- newtrain[sample, ]
newtest  <- newtrain[-sample, ]
set.seed(111)
sample1 <- sample.int(n = nrow(newtest), 
                     size = floor(.50*nrow(newtest)), replace = F)
test <- newtest[sample1, ]
eva  <- newtest[-sample1, ]
saveRDS()



newtrain$airportjfk<- vector(length=length(newtrain$pickup_datetime))

for (i in 1 :238300) {
  newtrain$airportjfk[i] <- if (newtrain$pickup_longitude[i]== -73.778889) 1
   else 0
}
newtrain$airport[newtrain$pickup_longitude = -73.778889 &  
                   newtrain$pickup_longitude= 40.639722]<- "1"
newtrain$airport[newtrain$pickup_longitude > -73.778889 &  
                   newtrain$pickup_longitude<= 40.639722]


newtrain<-newtrain%>%
  mutate(holiday=cut(pickup_datetime,
          labels = c("0","1"),include.lowest = TRUE))
ggplot2::ggplot(newtrain$passenger_count)+geom_barplot()
ggplot(newtrain,aes(newtrain$passenger_count,fill=passenger_count))+
  geom_bar(stat='count')+ coord_flip()
ggplot(newtrain,aes(newtrain$trip_durationcat,fill=trip_durationcat))+
  geom_bar(stat='count')+ coord_flip()

fit<-rpart(tripdurationcat~passenger_count+vendor_id+wday
           +tripshifts+store_and_fwd_flag,
           data=train,method="class")

plot.rpart(fit)

ctrl<-tree.control(142980, mincut= 5000, minsize= 10000, mindev= 0)
newtrain.tr <-tree(tripdurationcat~passenger_count+vendor_id+wday
                   +tripshifts+store_and_fwd_flag,
                   control=ctrl, data=train)
print(newtrain.tr)

plot(newtrain.tr,type=c("proportional", "uniform"))
text(newtrain.tr, pretty=0,cex=0.75) 


newtrain.tr <-tree(tripdurationcat~passenger_count+vendor_id+wday1+tripshifts,
                   control=ctrl,split= "gini",data=newtrain)
plot.tree(newtrain.tr,type=c("proportional", "uniform"))
text(newtrain.tr, pretty=0,cex=0.75) 
library(rattle)
library(caret)
c5<-C5.0(x = train[,c(3,6,11,18,19)],
         y=train$tripdurationcat,
         data=train,na.action = na.pass,trials = 10,
         control = C5.0Control(noGlobalPruning = FALSE,
         fuzzyThreshold=TRUE      , minCases=500,
             earlyStopping = FALSE, winnow = TRUE))
summary(c5)
C5imp(c5, metric = "splits", pct = TRUE)
p <- predict( c5, test[,c(3,6,11,18,19)], type="class" )
plot(c5,main="C5.0, min=3")
summary(p)
sum( p == test$tripdurationcat ) / length( p )
tbl4 <- table(train$tripdurationcat, p)
row.names(tbl4) <- paste("actual", row.names(tbl4))
colnames(tbl4) <- paste("predicted", colnames(tbl4))
tbl4
accuracy.percent <- 100*sum(diag(tbl4))/sum(tbl4)
print(paste("accuracy:",accuracy.percent,"%"))
caret::confusionMatrix(data = as.numeric(p),
                       reference = test$tripdurationcat)
confusion <- caret::confusionMatrix(test$tripdurationcat,p)
print(confusion)
library(caret)
tuned <- train(train[,c(3,6,11,18,19)], train$tripdurationcat ,
               method = "C5.0", tuneLength = 1, 
               trControl = trainControl(method = "cv", 
                                        number = 5,
                                        verboseIter = TRUE), 
                metric = "Kappa")
print(tuned)



folds <- split(train, cut(sample(1:nrow(train)),10))
library(plyr)
errs.c50 <- rep(NA, length(folds))
form <- "tripdurationcat~passenger_count+vendor_id+wday+tripshifts"
folds <- split(iris, cut(sample(1:nrow(train)),10))
for (i in 1:length(folds)) {
  ttest <- ldply(folds[i], data.frame)
  ttrain <- ldply(folds[-i], data.frame)
  tmp.model <- C5.0(as.formula(form), ttrain)
  tmp.predict <- predict(tmp.model, newdata=ttest)
  conf.mat <- table(ttest$tripdurationcat, tmp.predict)
  errs.c50[i] <- 1 - sum(diag(conf.mat))/sum(conf.mat)
}

print(sprintf("average error using k-fold cross validation and C5.0 decision tree algorithm: %.3f percent", 100*mean(errs.c50)))



rpart.plot::rpart.plot(fit)
fancyRpartPlot(fit)

c5caret<-train( x = train[,c(3,6,11,18,19)],
                y=train$tripdurationcat, data=train,method = "C5.0",trials=2)





classifier <- apriori(tripdurationcat~passenger_counts ,
                  data = train[,c(3)])

logmodel <- glm (tripdurationcat~passenger_count+vendor_id+wday
                 +tripshifts+store_and_fwd_flag ,
                 data = train, family = binomial(link="logit"))
summary(logmodel)
logpredict <- predict(logmodel,test[,c(3,6,11,18,19)],type='response')
table(test$tripdurationcat, logpredict>0.5)
caret::confusionMatrix(data = as.numeric(logpredict>0.5),
                reference = test$tripdurationcat)

library(ROCR)
ROCRpred <- prediction(logpredict, train$tripdurationcat)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))


tobj <- tune.svm(tripdurationcat~passenger_count+vendor_id+wday
                 +tripshifts+store_trand_fwd_flag ,
                 data = train, gamma = 10^(-3:-1),
                 cost = 10^(1:4))