library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)
library(Amelia)


###Import Data
main <- read.csv(file.choose(),na.strings = c(' ','',NA))
str(main)
main$broken <- as.factor(main$broken)


###Baseline Check
#1= yes it broke down 
#0 = didnt break down 
summary(main)

# Visualization to denote if there is any mssing value or not.
missmap(main)
?missmap
#no missing values 
prop.table(table(main$broken))
#39.4% of times it broke down(1)
#60.5 % of times it didnt break down(0)
u1 <- qplot(main$broken,fill=main$broken)+ggtitle('Breakdown')+theme_bw()+
  labs(fill='broken')
par(mfrow=c(2,2))
#Median lifetime is 60 months 
#Mean lifetime is 55.09 months 
#Distribution is sckewed towards right.
boxplot(main$lifetime,main='Boxplot of Lifetime in Months')
summary(main$lifetime)
#Median pressure at point 1 is 97.13
#Mean pressure at point 1 is 98.56
#distribution is normally distributed
boxplot(main$pressureInd_1,main='Boxplot of Pressure 1')
summary(main$pressureInd_1)

#Median Pressure at point 1 is 99.42
#Mean pressure at point 2 is 99.34
#distribution is normally distributed 
boxplot(main$pressureInd_2,main='Boxplot of Pressure2')
summary(main$pressureInd_2)
#Median Pressure at point 3 is 100.6
#Mean Pressure at point 3 is 100.6
#distribution is normally distributed 
boxplot(main$pressureInd_3,main='Boxplot of Pressure3')
summary(main$pressureInd_3)
#33.7% for team A
#35.17% for Team B 
# 30.54% for Team C
#Factory Team B has highest proportions of Machine compared to the rest.
u2 <- qplot(main$team,fill=main$team)+theme_bw()+ggtitle('Proportions of team')
prop.table(table(main$team))
#25.1% pr0vider 1
#26.8% provider 2
#23.99% provier 3
#24.01% provider 4
u3 <- qplot(main$provider,fill=main$provider)+theme_bw()+ggtitle('Proportion of Providers')
prop.table(table(main$provider))
grid.arrange(u1,u2,u3,ncol=3)
library(plotly)
#########Bivariate Analysis 
b1 <- main %>% 
  ggplot(aes(broken,lifetime,fill=broken))+geom_boxplot()+theme_bw()+ggtitle('Breakdown Vs Lifetime')+
  guides(fill=FALSE)

ggplotly(b1)
# machine without breakdown have a median lifetime value of 40
# machine with breakdown have a median lifetime value of 80
# here we can observe that normally machine doesnt breakdown if the lifetime is less than 60 months
#higher chances are there for the machine to breakdown if the lifetime is >60.
b2 <- main %>% 
  ggplot(aes(broken,pressureInd_1,fill=broken))+theme_bw()+
  ggtitle('broken Vs Pressure1')+guides(fill=FALSE)+geom_boxplot()
ggplotly(b2)
# machine without breakdown (0) have a median pressure of 98.23
# machine with breakdown (1) have a median pressure of 96.11
#no significant changes are observed.
b3 <- main %>%
  ggplot(aes(broken,pressureInd_2,fill=broken))+theme_bw()+
  ggtitle('Broken Vs Pressure2')+guides(fill=FALSE)+geom_boxplot()
ggplotly(b3)
#Machine without breakdown (0) have a median pressure at point 2 to be 99.52
#Machine with breakdown (1) have a median pressure at point 2 to be 99.04


b4 <- main %>% 
  ggplot(aes(broken,pressureInd_3,fill=broken))+theme_bw()+
  ggtitle('Broken Vs pressure3')+guides(fill=FALSE)+geom_boxplot()
ggplotly(b4)
# Machine without breakdwn have a median pressure at point 3 to be 100.62
# machine with breakdown have a median pressure at point 3 to be 100.56

b5 <- qplot(main$broken,main$team,geom = 'bin2d')+theme_bw()+ggtitle('Count of team Vs Breakdown')
table(main$broken,main$team)
ggplotly(b5)

b6 <- qplot(main$broken,main$provider,geom='bin2d')+theme_classic()+ggtitle('count of Proviers Vs Broken ')

ggplotly(b6)

library(gridExtra)
grid.arrange(b1,b2,b3,b4,b5,b6,nrow=2)

library(caTools)
sample_split <- sample.split(main$broken,SplitRatio = .8)
train <- filter(main,sample_split==TRUE)
train <- train[,-1]
test <- filter(main,sample_split==FALSE)
test <- test[,-1]
### Decision tree  using Gini 

model.rpart <- rpart(broken~.,data = train,method = 'class',parms = list(split='gini'))
model.rpart
summary(model.rpart)

###Plotting
library(rattle)

par(mfrow=c(1,1))
help(package = 'rattle')
fancyRpartPlot(model.rpart)
plotcp(model.rpart,minline = TRUE)
### Pruning using Plot 
set.seed(101)
model.rpart.1 <- prune(model.rpart,cp = .01)
fancyRpartPlot(model.rpart.1,main = 'Decision tree',sub = 'Preventive Maintaince')
prp(model.rpart.1)
model.rpart.1
prp(model.rpart.1, main="Desicion Tree",
    extra=106,           # display prob of survival and percent of obs
    nn=TRUE,             # display the node numbers
    fallen.leaves=TRUE,  # put the leaves on the bottom of the page
    shadow.col="gray",   # shadows under the leaves
    branch.lty=1,        # draw branches using staright lines
    branch=.5,           # change angle of branch lines
    faclen=0)
##Predict 
predict.y <-predict(model.rpart.1,newdata = test[,-2],type = 'class') 
predict.y <- as.numeric(predict.y)
predict.y <- ifelse(predict.y==2,1,0)
##Evaluation
library(caret)
confusionMatrix(predict.y,test$broken)
?confusionMatrix
library(ROCR)
rocrpred <- prediction(predict.y,test$broken)
rocrperf <- performance(rocrpred,'tpr','fpr')
plot(rocrperf)
auc <- performance(rocrpred,'auc')
auc <- unlist(auc@y.values)
auc


###Using Ensemble Method- Random forest
set.seed(200)

model.rf <- randomForest(broken~.,data = train,ntree=250)
#Predict 
pred.rpart <- predict(model.rf,type = 'class',newdata = test[,-2])
pred.rpart <- as.numeric(pred.rpart)
pred.rpart <- ifelse(pred.rpart==2,1,0)
plot(model.rf)

#### Important variable

Title=c('lifetime','pressure1','pressure2','pressure3','team','provider')
Title <- as.data.frame(Title)
var.imp <- as.data.frame(model.rf$importance)
var.imp <- cbind(Title,var.imp)

#### Important Variable 
var.imp$Title <- factor(var.imp$Title,levels = var.imp$Title[order(var.imp$MeanDecreaseGini)])
ggplot(var.imp,aes(x =Title,y = MeanDecreaseGini,fill=MeanDecreaseGini))+
  theme_bw()+ggtitle('Important Variable')+
  geom_col()+
  coord_flip()+
  guides(fill= FALSE)+
  ylab('Importance')

#### predicted vs Actual 
pred.y <- as.data.frame(predict(model.rf,test[,-2]))
names(pred.y) <- 'pred.broken'
test <- cbind(test,pred.y)
table(test$broken,test$pred.broken)
confusionMatrix(test$pred.broken,test$broken)
### 100% accuracy 






