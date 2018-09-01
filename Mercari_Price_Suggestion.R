#Mercari Price Suggestion Challenge
#Team members: Joshua Mckenny, Junyan Shao, Yuqi Jiang

options(java.parameters = "-Xmx64048m")
memory.limit(size=10000000000024)
library(ggplot2)
library(dplyr)
library(caTools)
library(stringr)
library(xgboost)
library(quanteda)
library(SnowballC)
library(tm)
library(gridExtra)
library(corrplot)
library(caret)
library(cluster)
library(data.table)
library(quanteda)
getwd()
setwd("C:\\Users\\yan_m\\Desktop")
tr <- read.csv(file="train.tsv", header=T, sep="\t")
te <- read.csv("test.tsv", sep='\t')
names(tr)[1] <- paste("train_id")
names(te)[1] <- paste("test_id")
tr$train_id <- NULL
str(te)

tr$name <- as.character(tr$name)
te$name <- as.character(te$name)
tr$category_name <- as.character(tr$category_name)
te$category_name <- as.character(te$category_name)
tr$brand_name <- as.character(tr$brand_name)
te$brand_name <- as.character(te$brand_name)
tr$item_description <- as.character(tr$item_description)
te$item_description <- as.character(te$item_description)
str(tr)
str(te)
summary(tr)


#transforming price
hist(tr$price, breaks=1000)
tr$price <- log(tr$price+1)
hist(tr$price, breaks=1000)
summary(tr$price)



#Category

#Category seems to have 3 parts seperated by “/”

splitVar <- str_split(tr$category_name, "/")
splitVarte <- str_split(te$category_name, "/")
cat1 <- sapply(splitVar,'[',1)
cat1te <- sapply(splitVarte,'[',1)
cat2 <- sapply(splitVar,'[',2)
cat2te <- sapply(splitVarte,'[',2)
cat3 <- sapply(splitVar,'[',3)
cat3te <- sapply(splitVarte,'[',3)

tr$cat1 <- cat1
te$cat1 <- cat1te
tr$cat2 <- cat2
te$cat2 <- cat2te
tr$cat3 <- cat3
te$cat3 <- cat3te
tr$cat1[is.na(tr$cat1)] <- -1
te$cat1[is.na(te$cat2)] <- -1
tr$cat2[is.na(tr$cat2)] <- -1
te$cat2[is.na(te$cat2)] <- -1
tr$cat3[is.na(tr$cat3)] <- -1
te$cat3[is.na(te$cat3)] <- -1
tr$category_name <- NULL
te$category_name <- NULL

str(tr)
sort(summary(tr$cat1), decreasing=T)
cat2_top20 <- head(sort(summary(tr$cat2), decreasing=T), 20)
cat2_top20
cat3_top20 <- head(sort(summary(tr$cat3), decreasing=T), 20)
cat3_top20



##Dummy features for desc/name

#item_description only using 2 gram for right now
#will add 3 gram
tr$comes_with <- (str_detect(tr$item_description, 'comes with'))*1
tr$in_box <- (str_detect(tr$item_description, 'in box'))*1
tr$like_new <- (str_detect(tr$item_description, 'like new'))*1
tr$for_rm <- (str_detect(tr$item_description, 'for rm'))*1
tr$to_save <- (str_detect(tr$item_description, 'to save'))*1
tr$free_shipping <- (str_detect(tr$item_description, 'free shipping'))*1
#3gram
tr$in_excellent_condition <- (str_detect(tr$item_description, 'in excellent condition'))*1
tr$new_with_tags <- (str_detect(tr$item_description, 'new with tags'))*1
tr$in_perfect_condition <- (str_detect(tr$item_description, 'in perfect condition'))*1
tr$new_never_worn <- (str_detect(tr$item_description, 'new never worn'))*1
tr$bundle_and_save <- (str_detect(tr$item_description, 'bundle and save'))*1
tr$save_on_shipping <- (str_detect(tr$item_description, 'save on shipping'))*1


#name only using 1 gram for right now
#will add 2 gram
tr$leggings <- (str_detect(tr$name, 'leggings'))*1
tr$boots <- (str_detect(tr$name, 'boots'))*1
tr$jacket <- (str_detect(tr$name, 'jacket'))*1
tr$case <- (str_detect(tr$name, 'case'))*1
tr$shirt <- (str_detect(tr$name, 'shirt'))*1
tr$baby <- (str_detect(tr$name, 'baby'))*1
tr$shorts <- (str_detect(tr$name, 'shorts'))*1
tr$brand_name[str_detect(tr$name,'lularo')] = 'Lularoe'
#2 gram
tr$tank_top <- (str_detect(tr$name, 'tank top'))*1
tr$free_shipping <- (str_detect(tr$name, 'free shipping'))*1
tr$free_ship <- (str_detect(tr$name, 'free ship'))*1
tr$body_works <- (str_detect(tr$name, 'body works'))*1
tr$iphone_plus <- (str_detect(tr$name, 'iphone plus'))*1




#XGBoost

set.seed(1234)
str(tr)
tr$item_description <- NULL
tr$name <- NULL

funTime <- tr

features <- names(funTime)
for(f in features){
  if(class(funTime[[f]])=="character"){
    levels=sort(unique(funTime[[f]]))
    funTime[[f]]=as.integer(factor(funTime[[f]],levels = levels))}
}



trainIndex <- createDataPartition(funTime$price # target variable vector
                                  , p = 0.80    # % of data for training
                                  , times = 1   # Num of partitions to create
                                  , list = F    # should result be a list (T/F)
)
train <- funTime[trainIndex,]
test <- funTime[-trainIndex,]
yTrainXG <- train$price
yTestXG <- test$price
trainXG <- train %>% select(-price)
testXG <- test %>% select(-price)


trainXG[] <- lapply(trainXG,as.numeric)
testXG[] <- lapply(testXG, as.numeric)
xgTrain <- xgb.DMatrix(as.matrix(trainXG),label=yTrainXG)
xgTest <- xgb.DMatrix(as.matrix(testXG), label=yTestXG)

xgPrm_tree <- list(boost='gbtree',objective='reg:linear',colsample_bytree=1,
                   eta=0.12,max_depth=9,min_child_weight=1,alpha=0.3,
                   lambda=0.4,gamma=0.2,subsample=0.8,seed=5,silent=TRUE)
xgPrm_linear <- list(boost='gblinear',objective='reg:linear',seed=5,silent=TRUE)
xgPrm_dart <- list(boost='dart.gbtree',objective='reg:linear',normalized_type='forest',sample_type='weighted',
                   seed=5,silent=TRUE)

xgbModel_tree <- xgb.train(xgPrm_tree,xgTrain,nrounds=275)
xgbModel_linear <- xgb.train(xgPrm_linear,xgTrain,nrounds=275)
xgbModel_dart <- xgb.train(xgPrm_dart,xgTrain,nrounds=275)


yhat_xg_tree <- predict(xgbModel_tree, newdata=xgTrain)
yhat_xg_linear <- predict(xgbModel_linear, newdata=xgTrain)
yhat_xg_dart <- predict(xgbModel_dart, newdata=xgTrain)
yhat_xg_tree[yhat_xg_tree<0] <- 0
yhat_xg_linear[yhat_xg_linear<0] <- 0
yhat_xg_dart[yhat_xg_dart<0] <- 0


yhat_xgt_tree <- predict(xgbModel_tree, newdata=xgTest)
yhat_xgt_linear <- predict(xgbModel_linear, newdata=xgTest)
yhat_xgt_dart <- predict(xgbModel_dart, newdata=xgTest)
yhat_xgt_tree[yhat_xgt_tree<0] <- 0
yhat_xgt_linear[yhat_xgt_linear<0] <- 0
yhat_xgt_dart[yhat_xgt_dart<0] <- 0

par(mfrow=c(1,3))
plot(yTestXG, yhat_xgt_tree)
plot(yTestXG, yhat_xgt_linear)
plot(yTestXG, yhat_xgt_dart)

R2(pred=yhat_xg_tree, obs=yTrainXG)
R2(pred=yhat_xgt_tree, obs=yTestXG)
R2(pred=yhat_xg_linear, obs=yTrainXG)
R2(pred=yhat_xgt_linear, obs=yTestXG)
R2(pred=yhat_xg_dart, obs=yTrainXG)
R2(pred=yhat_xgt_dart, obs=yTestXG)


RMSE(pred=yhat_xg_tree, obs=yTrainXG)
RMSE(pred=yhat_xgt_tree, obs=yTestXG)
RMSE(pred=yhat_xg_linear, obs=yTrainXG)
RMSE(pred=yhat_xgt_linear, obs=yTestXG)
RMSE(pred=yhat_xg_dart, obs=yTrainXG)
RMSE(pred=yhat_xgt_dart, obs=yTestXG)


names = names(trainXG)
importance_matrix <- xgb.importance(names, model = xgbModel_tree)
options(repr.plot.width=8, repr.plot.height=8)
ggplot(importance_matrix,aes(x=reorder(Feature,Gain),y=Gain))+
  geom_bar(stat='identity',aes(fill = Gain > 0.003))+
  scale_fill_manual(values=c('red','grey'),guide=FALSE) +
  coord_flip()+
  xlab('Features')+
  ylab('Importance')+
  ggtitle('Feature Importance')