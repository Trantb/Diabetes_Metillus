setwd('C:/Users/HP/Desktop/Study/WDIS')
df <- read.csv('TrainingWiDS2021.csv', header = T)

target <- df$diabetes_mellitus
target <- as.factor(target)
levels(target) <- c('X0','X1')
#levels(target) <- c('0','1')
t(t(names(df)))

#remove some columns
df <- df[,-c(72:97)]
df <- df[,-c(102:131)]
df <- df[,-c(110:117)]
df <- df[,-c(1:3,7:8,12,16,19:20,117)]

#create dummy
factor <- c('elective_surgery','hospital_admit_source',
            'icu_admit_source','icu_stay_type','icu_type',
            'apache_post_operative','arf_apache','gcs_unable_apache',
            'intubated_apache','ventilated_apache')
df <- fastDummies::dummy_cols(df, factor, remove_first_dummy = T,
                              ignore_na = T)
df <- df[,-c(3,5:8,12,13,20,25,35)]

summary(df)
t(t(names(df)))

for (i in 91:130){
  df[,i] <- as.factor(df[,i])
}

summary(df)
rm(factor)
rm(i)

#partition
train.index <- sample(nrow(df), nrow(df)*0.7)
train <- df[train.index,]
train.target <- target[train.index]
test <- df[-train.index,]
test.target <- target[-train.index]

#model
controlfit <- trainControl(method = 'repeatedcv',
                           number = 3,
                           repeats = 2,
                           adaptive = list(min = 3, alpha = 0.05,
                                           method = 'BT',
                                           complete = F),
                           search = 'random',
                           classProbs = T,
                           summaryFunction = twoClassSummary)
model.weights <- ifelse(train.target == 'X0',
                        (1/table(train.target)[1])*0.5,
                        (1/table(train.target)[2])*0.5)


gbm.grid <- expand.grid(interaction.depth = c(17,20),
                        n.trees = 1500,
                        shrinkage = 0.05,
                        n.minobsinnode = 20)
set.seed(419)
model.gbm <- train(train, train.target, method = 'gbm',
                   trControl = controlfit,
                   verbose = T,
                   tuneGrid = gbm.grid,
                   metric = 'ROC', tuneLength = 10)

controlfit$seeds <- model.gbm$control$seeds

model.gbm <- train(train, train.target, method = 'gbm',
                   trControl = controlfit,
                   verbose = T,
                   weights = model.weights,
                   tuneGrid = gbm.grid,
                   metric = 'ROC', tuneLength = 10)


model.gbm

pred <- predict(model.gbm, test)
confusionMatrix(pred, test.target)$byClass['F1']
a <- pROC::roc(as.numeric(test.target), as.numeric(pred), smoothed = T,
               ci = T, ci.alpha = 0.9, stratified = F,
               plot = T, auc.polygon = T, max.auc.polygon = T, grid = T,
               print.auc = T, show.thres = T)
a <- ci.se(a)
plot(a, type = 'bars')

pROC::auc(as.numeric(test.target), as.numeric(pred))
library(pROC)

#Light gbm
library(lightgbm)
library(smotefamily)
library(Matrix)


#lgb.test <- lgb.Dataset.create.valid(lgb.train, as.matrix(test), test.target)
#valids <- list(lgb.train, lgb.test)

#model <- lightgbm(lgb.train, num_leaves = 4L, learning_rate = 1.0,
#                  nrounds = 2L, objective = 'binary')
#lgb.train <- lgb.Dataset()
#list <- list(data = as.matrix(train), label = as.numeric(train.target))
##agaricus.train$data
#list$data
#Matrix::Matrix(train, sparse = T)
#smote.params.lgb = list(objective = 'binary',
#                        metric = 'auc',
#                        min_data_in_leaf = 30,
#                        min_sum_hessian_in_leaf = 100,
#                        feature_fraction = .9,
#                        bagging_fraction = 1,
#                        bagging_freq = 0,
#                        lambda_l1 = 8,
#                        lambda_l2 = 1.4,
#                        min_gain_to_split = 15,
#                        num_boost_round = 30000)
#smote.lgb.model <- lgb.train(params = smote.params.lgb,
#                             data = lgb.train,
#                             valids = valids,
#                             learning_rate = 0.088,
#                             num_leaves = 31,
#                             num_threads = 2,
#                             nrounds = 500,
#                             early_stopping_rounds = 50,
#                             eval_freq = 20)


lgb.train <- Matrix(as.matrix(train), sparse = T)
lgb.train <- lgb.Dataset(lgb.train, label = train.target,
                         free_raw_data = F, feature_pre_filter = F)
lgb.test <- Matrix(as.matrix(test), sparse = T)
lgb.test <- lgb.Dataset(lgb.test, label = test.target,
                        free_raw_data = F, feature_pre_filter = F)

params.lgb <- list(objective = 'binary',
                   metric = 'auc',
                   boosting = 'gbdt',
                   subsample_freq = 10,
                   min_data_in_leaf = 20,
                   min_sum_hessian_in_leaf = 100,
                   feature_pre_filter = F,
                   feature_fraction = 0.8,
                   bagging_fraction = 1,
                   #bagging_freq = 0,
                   num_iterations = 15000,
                   #scale_pos_weight = 6,
                   is_unbalance = T,
                   lambda_l1 = 2.5,
                   lambda_l2 = 2.5,
                   num_boost_round = 30000,
                   learning_rate = 0.02)
lgb.model <- lgb.train(params = params.lgb,
                       data = lgb.train,
                       valids = list(test = lgb.test),
                       num_leaves = 42,
                       num_threads = 2,
                       nrounds = 500,
                       early_stopping_round = 50,
                       eval_freq = 20,
                       force_col_wise = T)

lgb.Bayes
rm(params.lgb)

pred <- lgb.model$predict(Matrix(as.matrix(test), sparse = T))
pred <- lgb.model$predict(Matrix(as.matrix(unlab), sparse = T),
                          predict_disable_shape_check = T)
write.csv(data.frame(pred),'C:/Users/HP/Desktop/Study/WDIS/pred.csv',
          row.names = F)
confusionMatrix(as.factor(ifelse(pred >= 0.5, '1', '0')),
                as.factor(test.target), positive = '1')
pred <- ifelse(pred >= 0.5,1,0)

pROC::auc(as.numeric(test.target), as.numeric(pred))
               #,smoothed = T,
               #ci = T, ci.alpha = 0.9, stratified = F,
               #plot = T, auc.polygon = T, max.auc.polygon = T, grid = T,
               #print.auc = T, show.thres = T)
a <- pROC::ci.se(a)
plot(a)


library(rBayesianOptimization)

params.lgb <- list(objective = 'binary',
                   metric = 'auc',
                   boosting = 'goss',
                   subsample_freq = 10,
                   min_data_in_leaf = 2000,
                   min_sum_hessian_in_leaf = 100,
                   feature_fraction = 0.6,
                   bagging_fraction = 1,
                   #bagging_freq = 0,
                   num_iterations = 10000,
                   scale_pos_weight = 3,
                   lambda_l1 = 0,
                   lambda_l2 = 0,
                   num_boost_round = 30000,
                   learning_rate = 0.01)
lgb.model <- lgb.train(params = params.lgb,
                       data = lgb.train,
                       valids = list(test = lgb.test),
                       num_leaves = 31,
                       num_threads = 2,
                       nrounds = 500,
                       early_stopping_round = 50,
                       eval_freq = 20,
                       force_col_wise = T)


bayes_lgb <- function(subsample_freq,
                      learning_rate,
                      feature_fraction,
                      lambda_l1,
                      lambda_l2,
                      scale_pos_weight){
  params <- list(objective = 'binary',
                 metric = 'auc',
                 boosting = 'gbdt',
                 subsample_freq = subsample_freq,
                 learning_rate = learning_rate,
                 feature_fraction = feature_fraction,
                 lambda_l1 = lambda_l1,
                 lambda_l2 = lambda_l2,
                 scale_pos_weight = scale_pos_weight,
                 min_data_in_leaf = 2000,
                 min_sum_hessian_in_leaf = 100,
                 num_iterations = 10000,
                 num_boost_round = 30000)
  
  lgb.model <- lgb.train(params = params,
                         data = lgb.train,
                         valids = list(test = lgb.test),
                         num_leaves = 31,
                         num_threads = 2,
                         nrounds = 500,
                         early_stopping_round = 50,
                         eval_freq = 20,
                         force_col_wise = T)
}
bayes_lgb(10,0.02,1,0,0,3)

bound <- list(subsample_freq = c(1,10),
              learning_rate = c(0.2,0.4),
              feature_fraction = c(0.5, 1),
              lambda_l1 = c(0, 5),
              lambda_l2 = c(0, 5),
              scale_pos_weight = c(1,10))

grid <- data.frame(subsample_freq = round(runif(20, 1, 10)),
                   learning_rate = runif(20, 0.02, 0.4),
                   feature_fraction = runif(20, 0.5, 1),
                   lambda_l1 = runif(20, 0, 5),
                   lambda_l2 = runif(20, 0, 5),
                   scale_pos_weight = runif(20, 1, 10))

library(ParBayesianOptimization)

search <- bayesOpt(bayes_lgb, bound,
                   grid, iters.n = 2)
search <- BayesianOptimization(bayes_lgb, bounds = bound, 
                               init_grid_dt = grid, init_points = 2,
                               acq = 'ucb', verbose = T, n_iter = 5)

sum(complete.cases(test))
library(data.table)
?getDTthreads
sum(complete.cases(df))
t(t(names(df)))
summary(df[,c(101:190)])
library(mice)
df <- complete(mice(df, method = 'pmm', maxit = 5, m = 5, verbose = T))
colnames(df)[1:196] <- paste('x', 1:196, sep = '')
t(t(names(df)))
