library(data.table)
library(tidyverse)
library(arules)
library(arulesViz)
library(arulesSequences)
# install.packages('data.table',dependencies = TRUE)
# install.packages('tidyverse',dependencies = TRUE)
# install.packages('arules',dependencies = TRUE)
# install.packages('arulesViz',dependencies = TRUE)
# install.packages('arulesSequences',dependencies = TRUE)
#data translate to apriori----
# list to apriori
a_list <- list(c("a","b","c"),c("a","b"), c("a","b","d"), c("c","e"), c("a","b","d","e"))
names(a_list) <- paste("Tr",c(1:5), sep = "")
trans1 <- as(a_list, "transactions")
summary(trans1)
image(trans1)
size(trans1)

# matrix
a_matrix <- matrix(c(1,1,1,0,0, 1,1,0,0,0,1,1,0,1,0,0,0,1,0,1,1,1,0,1,1), ncol = 5)
dimnames(a_matrix) <- list(c("a","b","c","d","e"),paste("Tr",c(1:5), sep = ""))
trans2 <- as(a_matrix, "transactions")
inspect(trans2)

# data.frame type1 
a_df <- data.frame(age   = as.factor(c(6, 8, NA, 9, 16)), grade = as.factor(c("A", "C", "F", NA, "C")), pass  = c(TRUE, TRUE, FALSE, TRUE, TRUE)) 
trans3 <- as(a_df, "transactions") 
inspect(trans3)

# data.frame type2 ID,value
a_df3 <- data.frame( TID = c(1,1,2,2,2,3), item=c("a","b","a","b","c", "b"))
trans4 <- as(split(a_df3[,"item"], a_df3[,"TID"]), "transactions")
inspect(trans4)

#  data.frame  continuous variables into a categorical variables 
data(iris)
irisDisc <- discretizeDF(iris)
head(irisDisc)
trans5 <- as(irisDisc, "transactions")
inspect(head(trans5))

# data.frame  translate datefrem to list
a_df6 <- a_df3%>% bind_rows(data.frame(TID=1,item='a')) %>% as.data.table() 
a_df6 <- a_df6[!duplicated(a_df6),,]
trans6 <- as(a_df6[,.(val = list(item)),by=.(TID)][,.(val),][[1]],'transactions')
inspect()

# data trabsactions 
# a,c,d
# b,c,e
# a,b,c,e
# b,e
Trans7 <- read.transactions(file = 'xx.txt',format = 'basket',sep=',')
# TID ITEMS
# 1 a
# 1 c
# 1 d
# 2 b
# 2 c
# 2 e
Trans8 <- read.transactions(file = 'xx.txt', format = 'single',cols = c('TID','ITEMS',sep=" "))

# apriori----
# parameter
## support 0.1
## minlen  1
## maxlen 10
## target  frequent itemsets,maximally frequent itemsets,closed frequent itemsets,rules,hyperedgesets
## ext
## confidence
## smax
## arem

# APappearance
## lhs,rhs,both,items,none
## set
## items
## labels
## default

# control
## sort
## verbose
## filter
## tree
## heap
## memopt
## load

data("Adult")

# not contains
is <- apriori(Adult, parameter = list(support= 0.1, target="frequent"), 
              appearance = list(none = c("income=small", "income=large")))
itemFrequency(items(is))["income=small"]
itemFrequency(items(is))["income=large"]

# only contain
is <- apriori(Adult, parameter = list(support= 0.1, target="frequent"), 
              appearance = list(items = c("income=small", "income=large", "age=Young")))
inspect(head(is))
inspect(is)

# rhs  filter
incomeItems <- grep("^income=", itemLabels(Adult), value = TRUE)
incomeItems
rules <- apriori(Adult, parameter = list(support=0.2, confidence = 0.5), 
                 appearance = list(rhs = incomeItems))
inspect(head(rules))

# other params
rules <- apriori(Adult, parameter = list(support=0.2, confidence = 0.5,ext = FALSE), 
                 appearance = list(rhs = incomeItems),control = list(verbose = FALSE))
image(sample(Adult,100))
summary(rules)

#  48842个交易记录，115个项目
#  最频频的商品，以及出现次数
#  每笔交易的分位数，19条记录包含9个商品，971条记录包含10条商品
#  20%的交易不超过12个item,50的交易不超过13个item
#  如果数据集合中除了itemid和item之外其他的列,显示:labels是item名字
# other functions----
# itemFrequency(data) # 各项集的频率:支持度
# itemFrequencyPlot(data,support,topN) # 稀疏矩阵图
# inspect(rules[seq]) # 查看数据或者关联规则 
# inspect(sort()) inspect(subset())
# quality() # 提取规则中的支持度,置信度,提升度
# interestMeasure(rules,measure,transations = NULL) # 规则的附加信息
# sort()  # 规则排序,递减
# subset() # 提取符合条件的规则 items%in%c('x','xx') rhs/lhs%in%c('x') %pin%部分匹配 %ain%完全匹配 &| 
# plot(rules,method,measure,shading,interactive) # 规则可视化 
# method:scatter,graph,group,paracoord,matrix,matrix3D ;measure:support,confidence,left,order
# shading:support,confidence,lift
# as(xx,'data.frame')
# write() 规则保存
# size() 交易包含的商品数目

itemFrequency(Adult[1:3])
itemFrequency(Adult[,c('age=Young')])
itemFrequencyPlot(Adult,support = 0.1,topN = 20)
inspect(Adult[1:3])
inspect(rules[1:3])
quality(rules)
qualityMeasures <- interestMeasure(rules,measure = c("coverage","fishersExactTest","conviction","chiSquared"),transactions = Adult)
summary(qualityMeasures)
sort(rules,by='support')[1:5]
inspect(sort(rules,by='support')[1:5])
subset(rules,subset=rhs%in%'income=small'&lift>=0.1)
inspect(subset(rules,subset=rhs%in%'income=small'&lift>=0.1)[1:3])
plot(rules,method ='scatter',interactive=T)

write(rules,file='C:\\Users\\hasee\\Desktop\\test.csv',sep=',',quote=TRUE,row.names=FALSE)
rule_df <- as(rules,'data.frame')
str(rule_df)
# demo---- 
data('Groceries')
summary(Groceries) # 数据集的汇总情况
inspect(Groceries[1:10]) # 数据集前10个
size(Groceries) #交易包含的商品数量
itemFrequency(Groceries) # 各项集的频率:支持度
itemFrequency(Groceries[,c('whole milk','other vegetables'),]) # 特定项目的支持度
itemFrequencyPlot(Groceries,topN=20) # 前20项集的稀疏矩阵
rules <- apriori(Groceries,parameter = list(support=0.001,confidence=0.5)) # 限定条件的规则
inspect(rules[1:10]) # 规则前10
qualityMeasures <- interestMeasure(rules,measure = c('coverage','fishersExactTest','conviction','chiSquared'),transactions = Groceries) # 计算其他闲逛指标
quality(rules) <- cbind(quality(rules),qualityMeasures)
quality(rules) <- round(quality(rules),digits = 3)  # 保留小数点后3位
inspect(head(rules)) # 新的关联规则
rules.sorted <- sort(rules,by='lift')
inspect(rules.sorted[1:4])
rules.subset <- subset(rules,subset = rhs%in%'whole milk'&lift>=1.2)
inspect(rules.subset[1:5])
plot(rules,method = 'scatter',interactive = T)

library(arulesViz)
plot(x=rules,method='graph',control = list(main = '频繁项集可视化'))#频繁项目集合可视化 圈大小 支持度  颜色 提升度 
plot(rules,method = 'grouped') # 横标规则前项 纵标规则后项 圈大小支持度大小 颜色提升度高低
plot(rules,method = 'paracoord') # 从左到右带箭头折线表示规则前后项 粗细表示表示支持度大小 颜色表示提升度高低
plot(rules,method = 'graph',control = list(arrowSize =2,main='关联规则可视化')) #指定箭头和标题 箭头宽度：支持度大小 灰度：提升度高低

SuperSetF <- is.subset(rules,rules) # 判断是否存在冗余规则
inspect(rules[-which(colSums(SuperSetF)>1)])  # 浏览非冗余规则
subset_Rules <- subset(x=rules,subset=quality(rules)$lift>1)
# Fptree----
# 删除元素：每个元素的频率表，不满足的删除
# 更新数据：数据集合每条记录去掉删除的元素
# 构造FP树：更新头指针表
# 递归构造FP数
# 生成频繁项目集
# eclat----
# 倒排思想
# 数据库的事务id划分到每个item下面
MyFSets<- arules::eclat(Groceries,parameter = list(support = 0.01,target = 'maximally frequent itemsets')) # 最大频繁k项集
inspect(MyFSets)
MyFSets<- arules::eclat(Groceries,parameter = list(support = 0.01,target = 'frequent itemsets')) # 最大频繁k项集
inspect(MyFSets)
plot(MyFSets)
MyRules <- ruleInduction(x=MyFSets,transactions = Groceries,confidence = 0.02)
inspect(MyRules)
inspect(sort(x=MyRules,by='lift'))
# sequence arules----
# SPADE
# 事务序列sid 一个顾客的n次购买行为 
# 事务号eid 每一个单独的事务
# 事务序列sid包含的项目个数为k，成为k序列
# 频繁1项目集合:纵向查找：纵轴：item/sid&eid
# 频繁2项目集合：横轴：sid,纵轴 :(项目,eid)  类似eclat
# 连接方式：事务&事务，事务&序列，序列&序列
# install.packages("arulesSequences")
# install.packages("arules")
library(arulesSequences)
library(arules)
# 1,10,C,D
# 1,15,A,B,C
# 1,20,A,B,F
MyTrans <- read_baskets(con= xx.csv,sep=',',info = c('sequenceID','eventID'))
# demo
item <- factor(c("A","B","B","A","B","A","C","A","B","C","B","A","B","A","A","B","A","B"))
seqid <- c(1,1,1,1,1,2,2,2,2,2,2,3,3,3,4,4,4,4)
eventid <- c(101,102,203,304,305,2001,2002,3003,3004,3005,5006,10001,30002,40003,300004,300005,400006,500007)
data <- data.frame(item = item)
data.trans <- as(data,'transactions')
transactionInfo(data.trans)$sequenceID <- seqid
transactionInfo(data.trans)$eventID <- eventid
transactionInfo(data.trans)
result <- cspade(data.trans,parameter = list(support=0.5))

inspect(result)

example("ruleInduction",package = 'arulesSequences')
