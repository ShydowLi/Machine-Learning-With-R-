#------------使用C5.0识别高风险银行贷款——————————————————————————————————
library(C50)
library(gmodels)


#导入数据
data<-read.csv('F:\\r帮助文档\\MLwR-master\\Machine Learning with R (2nd Ed.)\\Chapter 05\\credit.csv',header = TRUE)

#按9：1构建训练集与测试集
set.seed(123)
dat<-sample(2,length(data),replace = T,prob = c(0.9,0.1))
train_data<-data[dat==1,]
test_data<-data[dat==2,]

#构建模型
model<-C5.0(default~.,data = train_data,control=C5.0Control(noGlobalPruning = FALSE))

#评估模型的性能
credit_pred<-predict(model,test_data,type = 'class')
CrossTable(test_data$default,credit_pred,prop.chisq = F,prop.c = F,prop.r = F,dnn = c('actual','predict'))


#------------提高模型的性能------------------------------------------------------
    #1、提高决策树的准确性：自适应增强算法：通过投票表决的方法为每一个案例选择最优的分类，即BOOSTING算法
credit_boost10<-C5.0(default~.,data = train_data,trials=10)  #参数trials:添加boosting算法
summary(credit_boost10)
credit_boost_pred<-predict(credit_boost10,test_data)
CrossTable(test_data$default,credit_boost_pred,prop.chisq = F,prop.r = F,prop.c = F,dnn = c('actual','predict'))
  
    #--boosting算法：将学习能力弱的算法组合在一起，创建一个团队，使他们的优点和缺点互补的多种学习方法的组合，可以显著的提高分类的准确性

     
    #2、犯一些比其他错误更严重的错误
    #--拒绝大量处于边界线的申请者，将一个惩罚因子分配到不同类型的错误上，该惩罚因子设定在一个代价矩阵中，用来指定每种错误相对于任何其他预测的严重程度
    
    #在这里因为预测值与实际值都是两个变量，需要构建一个2X2的矩阵
matrix_dim<-list(c('no','yes'),c('no','yes'))
names(matrix_dim)<-c('predict','actual')
error_cost<-matrix(c(0,1,4,0),nrow = 2,dimnames = matrix_dim)

credit_cost<-C5.0(default~.,data = train_data,costs=error_cost)
credit_cost_pred<-predict(credit_cost,test_data)
CrossTable(credit_cost_pred,test_data$default,prop.chisq = F,prop.c = F,prop.r = F,dnn = c('predict','actual'))

    #增加惩罚因子，实际上是以假阳性为代价，减少假阴性，这种这种是可以接受的。关注的只是召唤率
 
   




