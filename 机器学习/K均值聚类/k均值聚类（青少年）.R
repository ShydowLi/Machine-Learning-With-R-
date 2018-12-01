#==---K均值聚类-----------------------

data<-read.csv('F:\\r帮助文档\\MLwR-master\\Machine Learning with R (2nd Ed.)\\Chapter 09\\snsdata.csv')

#查看某一特征的缺失值数量
table(data$gender,useNA = 'ifany')    #或者可以用sum(is.na(data$gender))

summary(data$age)

#限制年龄，青少年合理的年龄
data$age<-ifelse(data$age>=13 & data$age <=20,data$age,NA)

#将性别特征作为一个单独的类别

data$female<-ifelse(data$gender=='F' & !is.na(data$gender),1,0)
data$no_gender<-ifelse(is.na(data$gender),1,0)

#--插补缺失值

mean(data$age,na.rm = T)    #这里如果不处理缺失值，均值是无法计算的
#按照毕业年份进行插补，而不是直接用总的平均

aggregate(data=data,age~gradyear,mean,na.rm=T)    #为数据的子组计算统计量
ave_age<-ave(data$age,data$gradyear,FUN = function(x) mean(x,na.rm = T))

data$age<-ifelse(is.na(data$age),ave_age,data$age)


#==3、训练模型-----------------------------

#归一化
dat<-data[5:40]
dat<-scale(dat)
model<-kmeans(dat,5)


#评估模型性能
model$size    #分为5组，每组的数量长度

model$centers    #聚类质心的坐标


#将模型中的分类结果加入到原始数据中

data$cluster<-model$cluster

#用aggregate()函数分组，了解每一类不同特征的关系
aggregate(data=data,age~cluster,mean)


