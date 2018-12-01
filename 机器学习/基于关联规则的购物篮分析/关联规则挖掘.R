#==-----------------用关联规则确定购买食品杂货----------------------------------

#加载arules包
library(arules)

data<-read.transactions('F:\\r帮助文档\\MLwR-master\\Machine Learning with R (2nd Ed.)\\Chapter 08\\groceries.csv',sep = ',')

summary(data)  #可以查看输入信息的记录数，商品总数，最频繁的项集等数据

#可以用inspect查看数据
inspect(data[1:5])

#若要研究某一特定的商品（一列数据），可以使用itemfrequency()函数查看

itemFrequency(data[,1:3])   #给出的是前三个商品的支持度

#可视化商品的支持度，可以使用itemfrequencyplot()

itemFrequencyPlot(data,support=0.1)  #这里规定支持度至少为0.1

itemFrequencyPlot(data,topN=20)   #根据支持度降序排列前20

#可视化交易数据-----绘制稀疏矩阵  image()

image(data[1:100])


#==--------训练模型-----------------------------------------
myrules<-apriori(data = data,parameter = list(support=0.006,confidence=0.25,minlen=2))

summary(myrules)
inspect(myrules[1:3])  #查看前三条规则


#==----------提高模型的性能-------------------------------------

#根据不同标准对规则进行排序，提取出来，并进行整理
#--1、对关联规则进行集合排序
#-------通过lift排序：这里by只后可以换成 support confidence
inspect(sort(myrules,by='lift')[1:5])

#--2、-----提取关联规则的子集：subset()
#假设想要知道某一商品是否与其他商品一起被购买--这里以berries为例

berriesRules<-subset(myrules,items %in% 'berries')   #这里items项也可以用lhs，rhs来代替，将某一项固定在左边或者右边
inspect(berriesRules)

berriesRules<-subset(myrules,lhs %in% 'berries' & lift>2)  #可以与与、或、非结合

#提取规则时： items代表所有的规则；可以使用%pin%（部分匹配），%ain%（完全匹配）；也可以加入支持度，置信度等条件

#--3、将规则写入数据框或者文件中

write(myrules,file = 'rules.csv',sep=',',quote=T,row.names=F)  #写入文件，csv格式

dat<-as(myrules,'data.frame') #变为数据框



