library(caret)
library(sqldf)
library(ggplot2)
set.seed(2364)
dtrain <- read.csv('Minih/train.csv',na.strings = '')
darticle <- read.csv('Minih/article.csv',na.strings = '')
duser <- read.csv('Minih/user.csv',na.strings = '')
dtest <- read.csv('Minih/test.csv',na.strings = '')


names(darticle) <- c("Article_ID","VintageMonths","SameAuthor","SameCategory")

#merging using merging train article and user
newtrain <- sqldf("select t.User_ID,t.Article_ID,u.Var1,u.Age,a.VintageMonths,a.SameAuthor,a.SameCategory,t.Rating
                  from dtrain t 
                  join duser u on u.User_ID = t.User_ID 
                  join darticle a on a.Article_ID = t.Article_ID")
newtest <- sqldf("select t.User_ID,t.Article_ID,u.Var1,u.Age,a.VintageMonths,a.SameAuthor,a.SameCategory 
                  from dtest t 
                 join duser u on u.User_ID = t.User_ID 
                 join darticle a on a.Article_ID = t.Article_ID")

summary(newtrain)
summary(newtest)

sapply(newtrain, function(x) sum(is.na(x)))
sapply(newtest, function(x) sum(is.na(x)))

str(newtrain)
str(newtest)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#cleaning the VintageMonths
hist(newtrain$VintageMonths)
boxplot((newtrain$VintageMonths))
boxplot.stats(newtrain$VintageMonths)

newtrain$VintageMonths[is.na(newtrain$VintageMonths)] <- -99
newtest$VintageMonths[is.na(newtest$VintageMonths)] <- -99

#cleaning the Age
newtrain$Age[is.na(newtrain$Age)] <- 'Unknown'
newtrain$Age <- as.factor(newtrain$Age)

newtest$Age[is.na(newtest$Age)] <- 'Unknown'
newtest$Age <- as.factor(newtest$Age)

getmode(newtrain$Age)
table(newtrain$Age)


#simple linear regression
set.seed(28365)
tr <- trainControl(method="repeatedcv",number=5, repeats = 3)


grid <- expand.grid(nrounds=50,lambda=0.1,alpha=0.1,eta=0.8)

xgbLinear_model <- train(Rating~.,newtrain,'xgbLinear',trControl = tr,tuneGrid = grid)
plot(xgbLinear_model)

prediction <- predict(xgbLinear_model,newtrain)
RMSE(prediction,newtrain$Rating)

#testing
prediction <- predict(xgbLinear_model,newtest)

submission <- data.frame('ID'=dtest$ID,'Rating'=prediction)
write.csv(submission, 'Submission_xgb_v6.csv',row.names = F)
