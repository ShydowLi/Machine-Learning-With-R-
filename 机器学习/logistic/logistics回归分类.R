#回归分析之logistics回归（0/1回归）
mydata<-read.csv('E:\\新建文件夹\\R数据挖掘\\chapter5\\示例程序\\data\\bankloan.csv',header=TRUE)
colnames(mydata)<-c('x1','x2','x3','x4','x5','x6','x7','x8','y')
#构建logistics回归模型
model<-glm(y~.,family = binomial(link = 'logit'),data = mydata)
summary(model)

#逐步寻优法(可以选择前向、后向、逐步)
model.step<-step(model,direction = 'both')
summary(model.step)

#听过逐步寻优得出显著性模型
model1<-glm(y~x1+x3+x4+x6+x7,family = binomial(link = 'logit'),data = mydata)
test<-mydata[c(1:5),]
pre<-predict(model1,test,type = 'response')
class<-pre>0.5
summary(class)


#将结果转化为0，1值 用ifelse函数
result<-ifelse(pre>0.5,1,0)

#生成混淆矩阵
table(result,test$y)


#roc曲线
library(pROC)
pre1<-predict(model,mydata,type = 'response')
summary(pre1)
modelroc<-roc(mydata$y,pre1)
plot(modelroc,print.auc=TRUE,auc.polygon=TRUE,grid=c(0.1,0.2),grid.col=c("green","red"),max.auc.polygon=TRUE,auc.polygon.col="skyblue",print.thres=TRUE)


