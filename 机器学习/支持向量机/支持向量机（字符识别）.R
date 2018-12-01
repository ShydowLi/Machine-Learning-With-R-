#--------支持向量机进行光学字符的识别--------------------------------------------

#读入数据‘
data<-read.csv('F:\\r帮助文档\\MLwR-master\\Machine Learning with R (2nd Ed.)\\Chapter 07\\letterdata.csv')

#训练集与测试集

train_data<-data[1:16000,]
test_data<-data[16001:20000,]

#采用kernlab包，待用ksvm()函数

library(kernlab)

model<-ksvm(letter~.,data=train_data,kernel='vanilladot')

pred<-predict(model,test_data)

table(pred,test_data$letter)

agreement<-pred==test_data$letter
table(agreement)
prop.table(table(agreement))


#--------------提高模型的性能-----------------------

#==-----采用不同的核函数，或者添加参数c，改变成本约束参数修改边界的宽度。
