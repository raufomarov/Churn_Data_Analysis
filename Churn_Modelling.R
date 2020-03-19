library(dplyr)
library(DataExplorer)
df = read.csv("Churn_Modelling.csv")
df %>% glimpse()

library(inspectdf)

inspect_na(df)

typeof(df)

numcol = select_if(df,is.numeric)

numcolnames=names(numcol)

#Histogram

DataExplorer::plot_histogram(numcol)


#there are no NAs in dataset

inc=1

par(mfrow=c(3,4))
for(i in numcolnames) {
  
  boxplot(df[i], main=numcolnames[inc])
  inc=inc+1
}

#As wee see there so many outliers in Age column

#Removing outliers

Q <- quantile(df$Age, probs=c(.25, .75))

iqr <- IQR(df$Age)


df2<- subset(df,df$Age> (Q[1] - 1.5*iqr) & 
               df$Age < (Q[2]+1.5*iqr))

boxplot(df2$Age)

#Correlations

library(corrplot)

cor<-cor(df3)
cor
corrplot(cor, method = 'number')



library(caTools)
set.seed(123)
split <- df2$Exited %>% sample.split(SplitRatio = 0.8)
train <- df2 %>% subset(split == TRUE)
test <- df2 %>% subset(split == FALSE)

glm <- glm(Exited ~ CreditScore+Age+Tenure+Balance+NumOfProducts+
           +EstimatedSalary+IsActiveMember+HasCrCard, 
               data = train, family = binomial)
summary(glm)

step(glm)
step(glm)$call
 
#Feature selecting
#Selecting significant features

step(glm)
step(glm)$call
#Building our final model

glm2<-glm(formula = Exited ~ Age + Balance + IsActiveMember, family = binomial, 
          data = train)

#Calculating train set accuracy

glm2.probs <- predict(glm2,type = "response")
glm2.probs

glm2.pred <- ifelse(glm2.probs > 0.5, "1", "0")
glm2.pred

tb2<-table(glm2.pred,train$Exited)

#Accuracy for train data

acc<-(tb2[1]+tb2[2,2])/(nrow(train))
acc


library(ROCR)

p <- predict(glm2, newdata = test, type = "response")
pr <- prediction(p, test$Exited)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,colorize=TRUE)


auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc



#Test set accuracy

test.pred <- ifelse(p > 0.5, "1", "0")
test.pred

tb3<-table(test.pred,test$Exited)
tb3

#Accuracy for train data

acctest<-(tb3[1]+tb3[2,2])/(nrow(test))
acctest

#Comparing train and test set accuracy
accs<- c(acc,acctest)
names <- c("train acc","test acc")

dt<-data.frame(train_acc=acc,
               test_acc=acctest)
dt

#There is no big difference between train and test set accuracy,it shows good fit.


