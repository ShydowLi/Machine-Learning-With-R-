#----随机森林-----------------

library(randomForest)
library(caret)
library(readr)

credit<-read_csv('F:\\r帮助文档\\MLwR-master\\Machine Learning with R (2nd Ed.)\\Chapter 05\\credit.csv')

#----1、使用randomforest()函数训练数据集

#randomforest(train,class,ntree=500,mtry=sqrt(p))  train：训练集数据框 class:每一行的类别 ntree:树的数目
#mtry:每次划分中随机选择特征的数量，默认值为sqrt(p)

set.seed(300)
rf<-randomForest(default ~ .,data = credit)


#----2、使用caret包进行参数调整，提高模型性能

ctrl<-trainControl(method = 'repeatedcv',number = 10,repeats = 10)
grid<-expand.grid(.mtry=c(2,4,8,16))
rf_model<-train(default~.,data = credit,method='rf',metric='Kappa',trControl=ctrl,tuneGrid=grid)