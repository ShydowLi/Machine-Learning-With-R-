#---------------文本处理函数----------------------

library(tm)
library(SnowballC)

create_corpus <- function(x) {
  result_corpus <- VCorpus(VectorSource(x)) %>% 
    tm_map(tolower) %>%                    # 把所有字母转换为小写字母
    tm_map(PlainTextDocument) %>%          # 把文本转换为纯文本文档
    tm_map(removePunctuation) %>%          # 删除标点符号
    tm_map(removeWords, stopwords()) %>%   # 剔除停用词
    tm_map(removeNumbers) %>%              # 剔除数字
    tm_map(stripWhitespace) %>%            # 剔除空格
    tm_map(stemDocument)                   # 采用Porter's stemming算法提取词干
  return(result_corpus)
}



#-------------文本处理值后转变为稀疏矩阵--------------
# 创建稀疏矩阵

dtm_ham <- DocumentTermMatrix(corpus_ham) %>%
  removeSparseTerms(0.99) # 降维，相当于剔除文本中的低频词汇

## 展示稀疏矩阵

inspect(dtm_ham[35:45, 1:10])
as.matrix(dtm_ham[35:45, 1:10]) # 展示稀疏矩阵的一部分


#----创建一个函数，用来将稀疏矩阵转置为矩阵，在计算每一个词出现的频率--------------------------
create_term_frequency_counts <- function(dtm) {
  m <- as.matrix(t(dtm))
  v <- sort(rowSums(m), decreasing = TRUE)
  d <- data.frame(word = names(v), freq = v, stringsAsFactors = FALSE)
  return(d)
}
