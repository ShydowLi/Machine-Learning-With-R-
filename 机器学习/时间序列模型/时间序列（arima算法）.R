#时间序列分析
#应用系统负载分析与磁盘容量预测
dat<-read.csv('E:\\新建文件夹\\R数据挖掘\\chapter11\\示例程序\\data\\discdata.csv',header=T,encoding='utf-8')
#数据清洗，去除缺失值
dat<-na.omit(dat)
#删除重复值
index1<-which(dat$VALUE==52323324)
index2<-which(dat$VALUE==157283328)
dat<-dat[-c(index1,index2),]
#提取c\d盘数据
cdat<-subset(dat,dat$ENTITY=='C:\\')
ddat<-dat[which(dat$ENTITY=='D:\\'),]

#对D盘数据进行平稳性检验 1、时序图 2、ADF表
#ADF表
library(fUnitRoots)
adfTest(ddat$VALUE)
adfTest(diff(ddat$VALUE))  #diff()函数一阶差分  在进行adf测试时p值越大越不平稳

#白噪声识别：验证数据中有用的信息是否已被提取完毕
Box.test(ddat$VALUE,type = 'Ljung-Box')  #p值越小越好

#用自相关图与偏相关图确定p、q参数
acf(diff(ddat$VALUE),lag.max = 20)  #自相关图：看有多少阶超出置信边界 确定p值 0
pacf(diff(ddat$VALUE),lag.max = 20)  #偏相关图：同上 确定q值  2
#以上可以确定模型为arima(0,1,2)

#建构arima模型，加载forecast 包
library(forecast)
model<-arima(ddat$VALUE,order = c(0,1,2))
summary(model)

#未来相同时间间隔内的5次预测
fore<-forecast(model,5)

#对残差进行平稳性检验
r1<-model$residuals
adfTest(r1)

#模型评价：
#平均绝对误差、均方根误差、平均百分比误差


 
