#------多元线性回归中最有特征系数求解------------------------

reg<-function(y,x){
  x<-as.matrix(x)
  x<-cbind(intercept=1,x)
  b<-solve(t(x) %*% x) %*% t(x) %*% y
  colnames(b)<-'estimate'
  print(b)
}

#--函数solve()求解逆矩阵  函数t()求转置矩阵  %*%矩阵相乘


#--利用线性回归预测医疗费用

data<-read.csv('F:\\r帮助文档\\MLwR-master\\Machine Learning with R (2nd Ed.)\\Chapter 06\\insurance.csv',stringsAsFactors = F)

#看一下数值分布
hist(data$expenses)

#--1、探索特征之间的关系--相关系数矩阵
cor(data[c('age','bmi','children','expenses')])

#--2、可视化特征之间的关系--散点矩阵图
pairs(data[c('age','bmi','children','expenses')])   #该散点图看着不舒服

library(psych)
pairs.panels(data[c('age','bmi','children','expenses')])   #在散点矩阵下方的椭圆中：椭圆越扁，相关强度越大
#--在相关散点图中，圆表示相关椭圆，越扁越相关强度越大  图中的线表示局部回归曲线：x轴与y轴变量之间的一般关系


#--3、基于数据训练模型
model<-lm(expenses~.,data = data)          #在实际生活中，当特征值全为0时，也就是预测值等于截距时，往往忽略


#--4、评估模型的性能
  #--一般考虑残差、p值、R^2；残差可以分析误差的分布  p值越小且小于显著性水平，表示该特征越显著 R^2越接近1，模型性能越好

#--5、模型设定--加入相互作用的影响
  #以上只是研究因变量与每个特征的单独的关系，并不考虑特征之间的关系，特征之间可能存在着相互作用
  #①、添加非线性关系：在线性回归中，一般认为变量之间的关系是线性的，而这并不是确定的
  #将年龄的平方添加到自变量中，这将是模型对年龄的的线性和非线性影响区分开
data$age2<-data$age^2
 
  #②、转换--将一个数值变量转换为二元指标，也就是将连续型变量（看起来相关较小）离散化为二值型数据
data$bmi30<-ifelse(data$bmi>=30,1,0)

  #③、模型设定--加入相互作用的影响
  #如果哪两个特征具有相互作用，则可以为他们创建一个相互作用特征
expenses~bmi30*smoker   #添加后这个公式代表着三个影响的：2+1

  #④全部放在一起--改进的回归模型

model<-lm(expenses~age+age2+children+bmi+sex+bmi30*smoker+region,data = data)


  






