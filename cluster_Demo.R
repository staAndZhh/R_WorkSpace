library(factoextra)
library(cluster)
library(data.table)
library(tidyverse)
library(fpc)

data <-read.csv("C:\\Users\\hasee\\Desktop\\聚类数据_个数.csv") %>% as.data.table()
data_ori <- data %>% select(购物服务便民商店.便利店:购物服务综合市场)
data_ori_used <- scale(data_ori)
data_initial <- data_ori_used %>% as.matrix()

# method 1 -- bic----
library(mclust)
# bic准则
m_clust <- Mclust(as.matrix(data_initial), G=1:30) #聚类数目从1一直试到30  15
m_clust <- Mclust(as.matrix(data_ori), G=1:30) # 18
summary(m_clust)
plot(m_clust,"BIC")

# method 2 -- support kpi----
library(NbClust)
set.seed(1234) #因为method选择的是kmeans，所以如果不设定种子，每次跑得结果可能不同
nb_clust <- NbClust(data_initial, distance = "euclidean",
                    min.nc=2, max.nc=15, method = "kmeans",
                    index = "alllong", alphaBeale = 0.1)
nb_clust <- NbClust(data_ori, distance = "euclidean",
                    min.nc=2, max.nc=15, method = "kmeans",
                    index = "alllong", alphaBeale = 0.1)
barplot(table(nb_clust$Best.nc[1,]),xlab = "聚类数",ylab = "支持指标数")

# method 3 --  wss plot ----
wssplot <- function(data, nc=30, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
    }
  plot(1:nc, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
  }
wssplot(data = data_initial)
# 4

# method 3.1 --  wss plot auto----
library(factoextra)
library(ggplot2)
set.seed(12234)
fviz_nbclust(data_ori, kmeans, method = "wss") +geom_vline(xintercept = 3, linetype = 2)
fviz_nbclust(data_initial, kmeans, method = "wss") +geom_vline(xintercept = 3, linetype = 2)
km.res <- kmeans(data_initial,6)
fviz_cluster(km.res, data = data_initial)

# method 4 -- pam----
library(fpc)
pamk.best <- pamk(data_initial)
pamk.best$nc
library(cluster)
clusplot(pam(data_ori, pamk.best$nc))
# 2
# method 5 -- ssw/sse ----
library(vegan)
ca_clust <- cascadeKM(data_initial, 1, 30, iter = 1000)
ca_clust$results
calinski.best <- as.numeric(which.max(ca_clust$results[2,]))
calinski.best
# plot(fit, sortg = TRUE, grpmts.plot = TRUE)

calinski<-as.data.frame(ca_clust$results[2,])
calinski$cluster <- c(1:10)
library(ggplot2)
ggplot(calinski,aes(x = calinski[,2], y = calinski[,1]))+geom_line()
# method 6 -- ap 吸引度/归属度 ----
library(apcluster)
ap_clust <- apcluster(negDistMat(r=2), data_ori)
length(ap_clust@clusters)
heatmap(ap_clust)
# method 7 -- 轮廓系数----
require(cluster)
library(factoextra)
fviz_nbclust(data_ori, kmeans, method = "silhouette")
# method 8 -- gap statistic 肘点跌落最快----
library(cluster)
set.seed(123)
gap_clust <- clusGap(data_ori, kmeans, 10, B = 500, verbose = interactive())
library(factoextra)
fviz_gap_stat(gap_clust)

km_result<-kmeans(data_ori,3,nstart = 25)
win.graph(width = 20,height = 20,pointsize = 8)
fviz_cluster(km_result,data_ori)
# method 9 -- 层次聚类----
h_dist <- dist(as.matrix(data_ori))
h_clust<-hclust(h_dist)
plot(h_clust, hang = -1, labels = FALSE)
rect.hclust(h_clust,8)
# method 10 -- clustergram----
