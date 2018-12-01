#--------用KNN算法诊断乳腺癌

#--------knn常用于：计算机视觉：面部识别，光学字符识别  一个人是否喜欢会喜欢推荐的电影或音乐

#适用于分类任务，其中特征值和目标类之间的关系是众多的、复杂的，但是具有相似类的项目有非常接近
#加载class包：knn()算法  加载gmodels包：Crosstable（）交叉表

library(class)
library(gmodels)
#读入数据
wbcd<-read.csv('F:\\r帮助文档\\MLwR-master\\Machine Learning with R (2nd Ed.)\\Chapter 03\\wisc_bc_data.csv',header=TRUE)

#剔除第一列，第一列没有意义
wbcd<-wbcd[-1]
wbcd$diagnosis<-factor(wbcd$diagnosis,levels = c('B','M'),labels = c('Benign','Malignant'))

#这里先采用最大-最小标准化
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

#调用lapply()函数将所有的特征标准化,这里使全部的特征向量标准化
wbcd_n<-as.data.frame(lapply(wbcd[2:31],normalize))

#将数据分为训练集与测试集
data<-sample(2,nrow(wbcd_n),replace = T,prob = c(0.7,0.3))
#标准化训练集与测试机
#训练集
wbcd_train<-wbcd_n[data==1,]
#测试集
wbcd_test<-wbcd_n[data==2,]

#存储类标签
wbcd_train_lables<-wbcd[data==1,1]
wbcd_test_lables<-wbcd[data==2,1]


#建立模型   用法：knn(train,test,class,k)  class表示分类的因子变量  这里k值取：sqrt（训练集个数383）最好选奇数

wbcd_pred<-knn(wbcd_train,wbcd_test,wbcd_train_lables,k=21)

#混淆矩阵
m1<-table(wbcd_pred,wbcd_test_lables)
sum(diag(m1)/sum(m1))

#-----------------评估模型新性能--------------------

#建立交叉表
CrossTable(wbcd_pred,wbcd_test_lables,prop.chisq = F)


#-----------------性能优化————————-------------------
 --1、采用Z分数标准化 scale()
 --2、测试不同的k值








      




