#协同过滤算法（基于用户推荐UBCF）

#对于realRatingMatrix数据类型，recommenderlab包一共提供了6种模型，分别是基于项目协同过滤(IBCF)、主成分分析(PCA)、基于流行度推荐(POPULAR)、随机推荐(RANDOM)、奇异值分解(SVD)和基于用户推荐(UBCF)，这里采用基于用户推荐(UBCF)。

dat<-read.csv('E:\\R WORK SPACE\\Data Files\\ratings.csv',header = TRUE)
dat<-dat[-4]
#画图
library(ggplot2)
library(reshape2)
library(reshape)
library(recommenderlab)
p<-ggplot(data = dat,aes(x=rating))+geom_histogram(binwidth = 0.1)
dat<-cast(data = dat,userId~movieId,value = 'rating')
#去掉多余的列
dat<-dat[,-1]
class(dat)<-'data.frame'
dat<-as.matrix(dat)
dat<-as(dat,'realRatingMatrix')

#建模分析
colnames(dat)<-paste('movie',1:9066,sep = '')
model<-Recommender(data = dat[1:600,],method='UBCF')
pre<-predict(model,dat[601:606,],type='ratings')
pre2<-predict(model,dat[601:606],n=6)
as(pre,'matrix')[1:6,1:6]
as(pre2,'list')

