#------------用回归树与模型树估计葡萄酒的质量------------------------------------------------------

data<-read.csv('F:\\r帮助文档\\MLwR-master\\Machine Learning with R (2nd Ed.)\\Chapter 06\\whitewines.csv')

hist(data$quality)   #查看葡萄酒质量的一个分布情况，排除极端情况

dat<-sample(2,nrow(data),replace = T,prob = c(0.75,0.25))
train_data<-data[dat==1,]
test_data<-data[dat==2,]

#加载rpart包，调用rpart（）函数
library(rpart)
library(rpart.plot)

model<-rpart(quality~.,data = train_data)

#可视化回归书
rpart.plot(model,digits = 3)

rpart.plot(model,digits = 3,fallen.leaves = T,type = 3,extra = 101)  


#--2、评估模型的性能(数值型的预测值评估)

pre<-predict(model,test_data)

summary(pre)
summary(test_data$quality)
#由以上两个看出模型不能识别极端值

#查看预测值对于真实值的程度有多好，cor（）
cor(pre,test_data$quality)

#用平均绝对误差度量性能
mean(abs(pre-test_data$quality))


#--3、提高模型的性能（构建模型树）   ---好像显著啊
library(RWeka)

model1<-M5P(quality~.,data = test_data)

pre1<-predict(model1,test_data)

mean(abs(pre-test_data$quality))

cor(pre1,test_data$quality)



