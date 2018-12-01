#----使用元学习来提高模型的性能----------------------------------------------

library(ipred)  #加载ipred包，使用bagging()函数
library(caret)

data<-read_csv('F:\\r帮助文档\\MLwR-master\\Machine Learning with R (2nd Ed.)\\Chapter 05\\credit.csv')

set.seed(300)
#建立一个10折cv的控制队形

#在caret包中有bagging的函数--treebag
ctrl<-trainControl(method = 'cv',number = 10)
train(default~.,data = data,method='treebag',trControl=ctrl)


#建立一个基于baggging的svmbag的支持向量机模型，适用前面的ksvm()函数，caret包中自带svmbag

str(svmBag)          #这里可以看到三个bagging指定的三个函数：拟合模型、预测、聚集投票

#创建一个bagging控制对象
bagctrl<-bagControl(fit = svmBag$fit,predict = svmBag$pred,aggregate = svmBag$aggregate)

model<-train(default~.,data=data,'bag',trControl=ctrl,bagControl=bagctrl)