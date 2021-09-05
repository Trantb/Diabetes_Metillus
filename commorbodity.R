commorbodity <- df[,c(174:180)]

for (i in 1:7){
  commorbodity[,i] <- as.factor(commorbodity[,i])
}
target <- as.factor(df$diabetes_mellitus)

df <- cbind(target, demographic, apache, vital, lab, commorbodity)

rm(dictionary)
rm(apache)
rm(commorbodity)
rm(demographic)
rm(lab)
rm(vital)
rm(i)
rm(target)

df <- df[,-53]

train.index <- sample(nrow(df), nrow(df)*0.7)
train <- df[train.index,]
test <- df[-train.index,]

library(caret)


str(df)
library(ggplot2)
ggplot(df, aes(x = target, fill = target)) + geom_bar()

library(caret)
library(rpart)
library(rpart.plot)



set.seed(24)
fit <- rpart(target~., train, method = 'class',
             control = rpart.control(xval = 10,
                                     minbucket = 2,
                                     cp = 0),
             parms = list(split = 'information'))
rpart.plot(fit, extra = 100)

t(t(names(df)))

model_rf <- caret::train(target~., train,
                         method = 'rf', 
                         preProcess = c('scale','center'),
                         trControl = trainControl(method = 'repeatedcv',
                                                  number = 3,
                                                  repeats = 1,
                                                  savePredictions = T,
                                                  verboseIter = F), 
                         na.action = na.exclude)
summary(df)
