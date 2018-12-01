#----使用caret包进行自动调整参数

library(caret)
library(readr)

#当你想查看某个模型的参数时，你可以使用modellookup()函数进行查找
#当模型由p个参数时，caret会自动进行3^p次调整，对每一个参数进行三次调整
modelLookup('C5.0')

#--1、创建简单的模型

data<-read_csv('F:\\r帮助文档\\MLwR-master\\Machine Learning with R (2nd Ed.)\\Chapter 05\\credit.csv')

set.seed(300)
model<-train(default~.,data = data,method='C5.0')           #模型会选择自助抽样法进行数据的选取


pre<-predict(model,data)          #如果想要得到概率的话，可以在后面添加参数type='prob'
table(pre,data$default)           #在这里可能正确率很高，但是不能作为是未来预测的能力


#----定制调整过程

#使用traincontrol()函数进行创建配置选项，控制对象   traincontrol(method,selectfunction)
#其中method参数用来设置重抽样的方法  selectfunction参数用来选择最优模型

ctrl<-trainControl(method = 'cv',number = 10,selectionFunction = 'oneSE')

grid<-expand.grid(.model='tree',.trials=c(1,5,10,15,20,25,30,35),.winnow='FALSE')  #expand.grid多种形式组合

#自定义模型
set.seed(300)
m<-train(default~.,data = data,method='C5.0',metric='Kappa',trControl=ctrl,tuneGrid=grid)


