library(caret)
library(dplyr)
library(pROC)

vis_miss(df[,c(18:45)], large_data_size = 150000,
         warn_large_data = F)




t(t(names(df)))
df <- df[,-c(1:3)]
df <- df[,-c(4:5,9)]
df <- dummy_cols(df, factor, remove_first_dummy = T,
                 ignore_na = T)

factor <- c('hospital_admit_source', 'icu_admit_source',
            'icu_stay_type','icu_type')
df <- df[,-c(5:8)]
target <- df$diabetes_mellitus
df <- df[,-171]

df <- cbind(target, df)

train.index <- sample(nrow(df), nrow(df)*0.7)
train <- df[train.index,]
test <- df[-train.index,]
train.target <- target[train.index]
test.target <- target[-train.index]

test <- test[,-1]

fitControl <- trainControl(method = 'cv',
                           number = 5,
                           savePredictions = 'final',
                           classProbs = T)

model.gbm <- train(train, train.target, method = 'gbm',
                   trControl = fitControl,
                   tuneLength = 2)

df$heart_rate_apache <- as.numeric(cut(df$heart_rate_apache,
                                       breaks = c(-Inf,39,49,99,109,119,139,154,Inf),
                                       labels = c('8','5','0','1','5','7','13','17')))
df$map_apache <- as.numeric(cut(df$map_apache,
                                breaks = c(-Inf,39,59,69,79,99,119,129,139,Inf),
                                labels = c('23','15','7','6','0','4','7','9','10')))
df$temp_apache <- as.numeric(cut(df$temp_apache, 
                                 breaks = c(-Inf,32.9,33.4,33.9,34.9,35.9,36.9,Inf),
                                 labels = c('20','16','13','8','2','0','4')))
df$resprate_apache <- as.numeric(cut(df$resprate_apache,
                                     breaks = c(-Inf,5,11,13,24,34,39,49,Inf),
                                     labels = c('17','8','7','0','6','9','11','18')))
df$pao2_apache <- as.numeric(cut(df$pao2_apache,
                                 breaks = c(-Inf,49,69,79,Inf),
                                 labels = c('15','5','2','0')))
#
df$hematocrit_apache <- as.numeric(cut(df$hematocrit_apache,
                                       breaks = c(-Inf,40.9,49,Inf),
                                       labels = c('3','0','3')))
summary(df$hematocrit_apache)
df$wbc_apache <- as.numeric(cut(df$wbc_apache,
                                breaks = c(-Inf,0.99,2.9,19.9,24.9,Inf),
                                labels = c(19,5,0,1,5)))
df$creatinine_apache <- as.numeric(cut(df$creatinine_apache,
                                       breaks = c(-Inf,0.4,1.4,1.94,Inf),
                                       labels = c(3,0,4,7)))
#df$arf_apache <- as.numeric(cut(df$arf_apache,
#                                breaks = c(-Inf,0,Inf),
#                                labels = c(0,10)))
df$arf_apache[df$arf_apache == 1] <- 10

df$urineoutput_apache <- as.numeric(cut(df$urineoutput_apache,
                                        breaks = c(-Inf,399,599,899,1499,1999,3999,Inf),
                                        labels = c(15,8,7,5,4,0,1)))
df$bun_apache <- as.numeric(cut(df$bun_apache,
                                breaks = c(-Inf,16.9,19,39,79,Inf),
                                labels = c(0,2,7,11,12)))
df$sodium_apache <- as.numeric(cut(df$sodium_apache,
                                   breaks = c(-Inf,119,134,154,Inf),
                                   labels = c(3,2,0,4)))
df$albumin_apache <- as.numeric(cut(df$albumin_apache,
                                    breaks = c(-Inf,1.9,2.4,4.4,Inf),
                                    labels = c(11,6,0,4)))
df$bilirubin_apache <- as.numeric(cut(df$bilirubin_apache,
                                      breaks = c(-Inf,1.9,2.9,4.9,7.9,Inf),
                                      labels = c(0,5,6,8,16)))
df$glucose_apache <- as.numeric(cut(df$glucose_apache,
                                    breaks = c(-Inf,39,59,199,349,Inf),
                                    labels = c(8,9,0,3,5)))
df$age <- as.numeric(cut(df$age, breaks = c(-Inf,44,59,64,69,74,84,Inf),
                         labels = c(0,5,11,13,16,17,24)))


#
#df$aids <- as.numeric(cut(df$aids, breaks = c(-Inf,0,Inf),
#                          labels = c(0,23)))
#df$hepatic_failure <- as.numeric(cut(df$hepatic_failure,
#                                     breaks = c(-Inf,0,Inf),
#                                     labels = c(0,16)))
#df$lymphoma <- as.numeric(cut(df$lymphoma, 
#                              breaks = c(-Inf,0,Inf),
#                              labels = c(0,13)))
#df$solid_tumor_with_metastasis <- as.numeric(cut(df$solid_tumor_with_metastasis,
#                                                 breaks = c(-Inf,0,Inf),
#                                                 labels = c(0,11)))
#df$leukemia <- as.numeric(cut(df$leukemia,
#                              breaks = c(-Inf,0,Inf),
#                              labels = c(0,10)))
#df$immunosuppression <- as.numeric(cut(df$immunosuppression,
#                                       breaks = c(-Inf,0,Inf),
#                                       labels = c(0,10)))
#df$cirrhosis <- as.numeric(cut(df$cirrhosis,
#                               breaks = c(0,1,2),
#                               labels = c(0,4)))
df$aids[df$aids == 1] <- 23
df$hepatic_failure[df$hepatic_failure == 1] <- 16
df$lymphoma[df$lymphoma == 1] <- 13
df$solid_tumor_with_metastasis[df$solid_tumor_with_metastasis == 1] <- 11
df$leukemia[df$leukemia == 1] <- 10
df$immunosuppression[df$immunosuppression == 1] <- 10 
df$cirrhosis[df$cirrhosis == 1] <- 4


df$arf_apache <- as.numeric(df$arf_apache)
df$aids <- as.numeric(df$aids)
df$hepatic_failure <- as.numeric(df$hepatic_failure)
df$lymphoma <- as.numeric(df$lymphoma)
df$solid_tumor_with_metastasis <- as.numeric(df$solid_tumor_with_metastasis)
df$leukemia <- as.numeric(df$leukemia)
df$immunosuppression <- as.numeric(df$immunosuppression)
df$cirrhosis <- as.numeric(df$cirrhosis)

co2<- data.frame(df$paco2_for_ph_apache)
ph <- data.frame(df$ph_apache)
ap <- data.frame(matrix(0, nrow = 130157, ncol = 1))
summary(ap)

for (i in 1:nrow(ap)){
  if (is.na(ph[i,]) || is.na(co2[i,])){
    ap[i,] <- NA
  } else if (ph[i,] < 7.20 && co2[i,] < 50){
    ap[i,] <- 12
  } else if (ph[i,] < 7.20 && co2[i,] >= 50){
    ap[i,] <- 4
  } else if (ph[i,] >= 7.20 && ph[i,] < 7.35 && co2[i,] <30){
    ap[i,] <- 9
  } else if (ph[i,] >= 7.20 && ph[i,] < 7.27 && co2[i,] >=30 && co2[i,] < 35){
    ap[i,] <- 6
  } else if (ph[i,] >= 7.20 && ph[i,] < 7.27 && co2[i,] >=35 && co2[i,] < 50){
    ap[i,] <- 3
  } else if (ph[i,] >= 7.20 && ph[i,] < 7.27 && co2[i,] >=50){
    ap[i,] <- 2
  } else if (ph[i,] >= 7.35 && ph[i,] < 7.50 && co2[i,] <30){
    ap[i,] <- 5
  } else if (ph[i,] >= 7.27 && ph[i,] < 7.45 && co2[i,] >=30 && co2[i,] < 45){
    ap[i,] <- 0
  } else if (ph[i,] >= 7.27 && ph[i,] < 7.45 && co2[i,] >=45){
    ap[i,] <- 1
  } else if (ph[i,] >= 7.45 && ph[i,] < 7.50 && co2[i,] >= 30 && co2[i,] <35){
    ap[i,] <- 0
  } else if (ph[i,] >= 7.45 && ph[i,] < 7.50 && co2[i,] >= 35 && co2[i,] <45){
    ap[i,] <- 2
  } else if (ph[i,] >= 7.45 && ph[i,] < 7.50 && co2[i,] >= 45){
    ap[i,] <- 12
  } else if (ph[i,] >= 7.50 && ph[i,] < 7.65 && co2[i,] < 35){
    ap[i,] <- 3
  } else if (ph[i,] >= 7.65 && co2[i,] < 25){
    ap[i,] <- 0
  } else if (ph[i,] >= 7.65 && co2[i,] >= 25 && co2[i,] < 35){
    ap[i,] <- 3
  } else if (ph[i,] >= 7.50 && co2[i,] >= 35){
    ap[i,] <- 12
  } else {
    ap[i,] <- NA
  }
}

colnames(ap)[1] <- 'ap'
summary(ap)
cor(df$diabetes_mellitus, df$apache_2_diagnosis, use = 'complete.obs')
summary(df$apache_2_diagnosis)
df$acute_physiology <- ap

rm(ap); rm(co2); rm(ph); rm(i)

t(t(names(df)))
summary(df[,16])
df <- df[,-c(1:3,7:8,16)]
df <- df[,-c(7,13,14)]



factor <- c('elective_surgery','hospital_admit_source',
            'icu_admit_source','icu_stay_type','icu_type',
            'apache_post_operative','arf_apache','gcs_unable_apache',
            'intubated_apache','ventilated_apache')
df <- fastDummies::dummy_cols(df, factor, remove_first_dummy = T,
                              ignore_na = T)
df <- df[,-c(3,5:8,12:13,20,25,35)]

t(t(names(df)))

df[,c(164:196)] <- as.factor(df[,c(164:196)])

target <- df$diabetes_mellitus
df <- df[,-162]

target <- as.factor(target)
levels(target) <- c('X0','X1')

#test
df <- df[,-163]
df <- df[,-194]
df <- df[,-c(53:78)]
df <- df[,-c(83:112)]
df <- df[,-c(91:98)]
summary(df)

train.index <- sample(nrow(df), nrow(df)*0.7)
train <- df[train.index,]
test <- df[-train.index,]

train.target <- target[train.index]
test.target <- target[-train.index]

fitControl <- trainControl(method = 'repeatedcv',
                           number = 5,
                           repeats = 5,
                           classProbs = T,
                           summaryFunction = twoClassSummary)
gbm.grid <- expand.grid(interaction.depth = c(1,5,9),
                   n.trees = (1:30)*20,
                   shrinkage = 0.1,
                   n.minobsinnode = 20)
set.seed(424)
model.gbm <- train(train, train.target, method = 'gbm',
                   trControl = fitControl,
                   verbose = T,
                   tuneGrid = gbm.grid,
                   metric = "ROC")
gc()
model.gbm
pred <- predict(model.gbm, test)
confusionMatrix(pred,test.target)

roc(test.target, pred, smoothed = T, ci = T, ci.alpha = 0.9,
    startified = F, plot = T, auc.polygon = T, max.auc.polygon = T,
    print.auc = T, show.thres = T)

roc(as.numeric(test.target), as.numeric(pred))

summary(df$bmi)
