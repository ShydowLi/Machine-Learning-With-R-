#---------概率学习--朴素贝叶斯分类------------------------------------------------------------------------------
library(tm)   #--文本处理包
library(SnowballC)  #与tm包一起使用，用于词干提取
library(wordcloud2)
library(wordcloud)
library(e1071) 
library(gmodels)
#读入数据集
data<-read.csv('F:\\r帮助文档\\MLwR-master\\Machine Learning with R (2nd Ed.)\\Chapter 04\\sms_spam.csv',header = TRUE,encoding = 'utf-8')
data<-data[-1072,]
data$type<-factor(data$type)
#文本拆分--词袋
#查看垃圾短信各有多少
table(data$type)


#数据准备--清理和标准化文本数据
data_corpus<-VCorpus(VectorSource(data$text))   #函数vectorsource指定文档来源  函数vcorpus构建词库

#清理文本，去除标点符号，所有字母转化为小写
#data_corpus$text<-iconv(data_corpus$text,"WINDOWS-1252","UTF-8") 
#data_clean<-tm_map(data_corpus,content_transformer(tolower))    --转化为小写错误  

data_clean<-tm_map(data_corpus,removeNumbers)   #去除数字

#去除填充词，类似and，or等  使用stopword()函数
data_clean<-tm_map(data_clean,removeWords,stopwords())   #removeword是指移除填充词

#去除标点符哈皮
data_clean<-tm_map(data_clean,removePunctuation)

#词干提取
data_clean<-tm_map(data_clean,stemDocument)

#去除多余空格
data_clean<-tm_map(data_clean,stripWhitespace)


#将文本文档拆解成词语
data_dtm<-DocumentTermMatrix(data_clean)   #类似于稀疏矩阵

#当然如果你不想创建以上麻烦的步骤，你也可以直接建立DTM矩阵
# data_DTM1<-DocumentTermMatrix(data_clean,control = list(
#   tolower=TRUE,
#   removeNumbers=TRUE,
#   stopwords=TRUE,             #这里可以变为stopwords=function(x){removeWords(x,stopwords())}
#   removePunctuation=TRUE,
#   stemming=TRUE
# ))


#建立测试集与训练集
data_train<-data_dtm[1:4200,]
data_test<-data_dtm[4201:5558,]
#建立标签向量
data_train_lables<-data[1:4200,]$type
data_test_lables<-data[4201:5558,]$type

#可视化词云，描述词语出现的频率
wordcloud(data_clean,min.freq = 50,random.order = F)  #信息中所有词出现的频率
#找出垃圾短信与非垃圾短信中的词，用词云表示
sapm<-subset(data,type=='spam')      #垃圾短信
ham<-subset(data,type=='ham')        #非垃圾短息n
wordcloud(sapm$text,max.words = 40,scale = c(3,0.5))  #垃圾短信词云
wordcloud(ham$text,max.words = 40,scale = c(3,0.5))   #非垃圾短信

 

#减少特征的数量，找出次数不少于指定次数的单词
data_freq_words<-findFreqTerms(data_train,5)           #findfreqTerms()找出词频
data_dtm_freq_train<-data_train[,data_freq_words]
data_dtm_freq_test<-data_test[,data_freq_words]


#将数值变量转化为分类变量（yes，no）
convert_counts<-function(x){
  x<-ifelse(x>0,'yes','no')
}

ac_train<-apply(data_dtm_freq_train,2,convert_counts)
ac_test<-apply(data_dtm_freq_test,2,convert_counts)

#建立数据模型 （e1071）
naivemodel<-naiveBayes(ac_train,data_train_lables)  #参数拉普拉斯估计laplace设为默认，因为这里我们已经选取出现次数为5

pred<-predict(naivemodel,ac_test,type = 'class')

CrossTable(pred,data_test_lables,prop.chisq = F,prop.t = F,dnn = c('预测','实际'))

#--------------提高模型的性能---------------------
# 在建立的朴素贝叶斯模型中将laplace参数设置为1

naivemodel2<-naiveBayes(ac_train,data_train_lables,laplace = 1)
pred2<-predict(naivemodel2,ac_test,type = 'class')
CrossTable(pred2,data_test_lables,prop.chisq = F,prop.t = F,dnn = c('预测','实际'))






#------------提取部分DTM矩阵（稀疏）转化为数据框----------------------------------------------------------------------


# 将某一列列转换为utf-8格式，不能转换的字符使用其十六进制形式替换
# enc2utf8()将该列的内容转换为utf-8格式
# iconv(sub = 'byte')指定不能转换的字符用其十六进制形式替换
#类似----sms$message <- sapply(sms$message, function(x) iconv(enc2utf8(x), sub = "byte"))





