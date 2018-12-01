#----独而治之-规则分类----------------------------------------------------------------------------------------------------

#--1R算法：

data<-read.csv('F:\\r帮助文档\\MLwR-master\\Machine Learning with R (2nd Ed.)\\Chapter 05\\mushrooms.csv')

#veil_type特征只有一个因子水平，属于异常值，直接删除这列
data$veil_type<-NULL

library(RWeka)   #Rweak包：1R算法 oneR()   RIPPER算法：JRIP（）

#----1R算法
model1<-OneR(type~.,data =data)
summary(model1)                #查看总体结果

#----PIPPER算法

model2<-JRip(type~.,data = data)

