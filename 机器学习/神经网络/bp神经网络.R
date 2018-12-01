#-----BP神经网络---对混泥土的强度进行建模--------------------------------------

data<-read.csv('F:\\r帮助文档\\MLwR-master\\Machine Learning with R (2nd Ed.)\\Chapter 07\\concrete.csv')

#对数据进行标准化（如果数据呈现正态分布使用z分数标准化，如果处于均匀分布或者非正态分布则最大最小标准化）
#在这里数据不满足正态分布，最大最小标准化，定义公式

normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

data_normalize<-as.data.frame(lapply(data,normalize))

#训练集与测试集
dat<-sample(2,nrow(data_normalize),replace = T,prob = c(0.75,0.25))
train_data<-data_normalize[dat==1,]
test_data<-data_normalize[dat==2,]


#这里用neuralnet包，也可以用nnet包、RSNNS包

library(neuralnet)

model<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data=train_data,hidden = 1)

#---评估模型的性能

pre_strength<-compute(model,test_data[1:8])

#cor()相关系数获取实际值与预测值之间的关系
cor(pre_strength$net.result,test_data$strength)
#或者使用平均绝对误差
mean(abs(test_data$strength-pre_strength$net.result))   #平均绝对误差0.08，已经很高了



#==----------------提高模型的性能-----------------------
#增加隐藏层，hidden=5时
model2<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data=train_data,hidden = 5)
plot(model2)

pre_strength2<-compute(model2,test_data[1:8])

cor(pre_strength2$net.result,test_data$strength)

mean(abs(test_data$strength-pre_strength2$net.result))