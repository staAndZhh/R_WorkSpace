library(ggplot2)
## 柱状图----
#离散单变量的条形图，数据为原始数据：stat= 'identity'
x <- c('A','B','C','D','E') 
y <- c(13,22,16,31,8) 
df <- data.frame(x= x, y = y)
ggplot(data = df, mapping = aes(x = x, y = y)) + geom_bar(stat= 'identity')

#离散单变量的条形图,数据为原始数据：stat= 'count'
set.seed(1234) 
x <- sample(c('A','B','C','D'), size = 1000, replace= TRUE, prob = c(0.2,0.3,0.3,0.2)) 
y <- rnorm(1000) * 1000 
df = data.frame(x= x, y = y) 
ggplot(data = df, mapping = aes(x = x)) + geom_bar(stat = 'count')

# #离散单变量的条形图,x轴为离散型
set.seed(1234) 
x <- sample(c(1,2,4,6,7), size = 1000, replace = TRUE,prob = c(0.1,0.2,0.2,0.3,0.2))
ggplot(data = data.frame(x = x), mapping= aes(x = x, y = ..count..)) + geom_bar(stat = 'count')
ggplot(data = data.frame(x = x), mapping = aes(x = factor(x), y = ..count..))+ geom_bar(stat = 'count') #离散型
ggplot(data = data.frame(x = x), mapping = aes(x = factor(x), y = ..count..))+ geom_bar(stat = 'count', fill = 'steelblue', colour = 'darkred') # 更改颜色

# 簇状条形图：在ggplot()函数的aes()参数中将其他离散变量赋给fill参数
# position参数表示条形图的摆放形式，默认为堆叠式(stack)，还可以是百分比的堆叠式
x <- rep(1:5, each = 3) 
y <- rep(c('A','B','C'),times = 5) 
set.seed(1234)
z <- round(runif(min = 10, max = 20, n = 15)) 
df <- data.frame(x= x, y = y, z = z) 
ggplot(data = df, mapping = aes(x = factor(x), y = z,fill = y)) + geom_bar(stat = 'identity', position = 'dodge')  # 分开柱状图
ggplot(data = df, mapping = aes(x = factor(x), y = z, fill = y)) + geom_bar(stat= 'identity', position = 'stack') # 堆叠柱状图
ggplot(data = df, mapping = aes(x = factor(x), y = z, fill = y)) + geom_bar(stat= 'identity', position = 'stack') + guides(fill = guide_legend(reverse= TRUE)) # 条形图的堆叠顺序与图例顺序相同
ggplot(data = df, mapping = aes(x = factor(x), y = z, fill = y)) + geom_bar(stat= 'identity', position = 'fill') # 百分比堆叠图

# 更改调色方案
ggplot(data = df, mapping = aes(x = factor(x), y = z, fill = y)) + geom_bar(stat= 'identity', position = 'dodge') + scale_fill_brewer(palette = 'Accent')
# 自定义调色方案
col <- c('darkred','skyblue','purple')
ggplot(data = df, mapping =aes(x = factor(x), y = z, fill = y)) + geom_bar(stat = 'identity', colour= 'black', position = 'dodge') +scale_fill_manual(values = col,limits= c('B','C','A')) + xlab('x')
# 按z值的大小，重新排列条形图的顺序，只需将aes()中x的属性用reorder()函数
ggplot(data = df, mapping = aes(x = reorder(x, z), y = z, fill = y)) +geom_bar(stat = 'identity') + xlab('x')

# 添加标签
# 堆叠粗壮柱形图
ggplot(data = df, mapping = aes(x = x, y = z, fill = y)) + geom_bar(stat
                                                                    = 'identity', position = 'stack') + geom_text(mapping = aes(label = z),
                                                                                                                 size = 5, colour = 'black', vjust = 3.5, hjust = .5, position = position_stack())
#分开粗壮柱形图 
ggplot(data = df, mapping = aes(x = x, y = z, fill = y)) + geom_bar(stat
                                                                    = 'identity', position = 'dodge') + geom_text(mapping = aes(label = z),
                                                                                                                  size = 5, colour = 'black', vjust = 1, hjust = .5, position = position_dodge(0.9))

ggplot(data = df, mapping = aes(x = interaction(x,y),
                               y = z, fill = y)) + geom_bar(stat = 'identity') + geom_text(mapping = aes(label= z))
