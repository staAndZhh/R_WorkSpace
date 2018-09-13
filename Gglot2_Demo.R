library(ggplot2)
## ��״ͼ----
#��ɢ������������ͼ������Ϊԭʼ���ݣ�stat= 'identity'
x <- c('A','B','C','D','E') 
y <- c(13,22,16,31,8) 
df <- data.frame(x= x, y = y)
ggplot(data = df, mapping = aes(x = x, y = y)) + geom_bar(stat= 'identity')

#��ɢ������������ͼ,����Ϊԭʼ���ݣ�stat= 'count'
set.seed(1234) 
x <- sample(c('A','B','C','D'), size = 1000, replace= TRUE, prob = c(0.2,0.3,0.3,0.2)) 
y <- rnorm(1000) * 1000 
df = data.frame(x= x, y = y) 
ggplot(data = df, mapping = aes(x = x)) + geom_bar(stat = 'count')

# #��ɢ������������ͼ,x��Ϊ��ɢ��
set.seed(1234) 
x <- sample(c(1,2,4,6,7), size = 1000, replace = TRUE,prob = c(0.1,0.2,0.2,0.3,0.2))
ggplot(data = data.frame(x = x), mapping= aes(x = x, y = ..count..)) + geom_bar(stat = 'count')
ggplot(data = data.frame(x = x), mapping = aes(x = factor(x), y = ..count..))+ geom_bar(stat = 'count') #��ɢ��
ggplot(data = data.frame(x = x), mapping = aes(x = factor(x), y = ..count..))+ geom_bar(stat = 'count', fill = 'steelblue', colour = 'darkred') # ������ɫ

# ��״����ͼ����ggplot()������aes()�����н�������ɢ��������fill����
# position������ʾ����ͼ�İڷ���ʽ��Ĭ��Ϊ�ѵ�ʽ(stack)���������ǰٷֱȵĶѵ�ʽ
x <- rep(1:5, each = 3) 
y <- rep(c('A','B','C'),times = 5) 
set.seed(1234)
z <- round(runif(min = 10, max = 20, n = 15)) 
df <- data.frame(x= x, y = y, z = z) 
ggplot(data = df, mapping = aes(x = factor(x), y = z,fill = y)) + geom_bar(stat = 'identity', position = 'dodge')  # �ֿ���״ͼ
ggplot(data = df, mapping = aes(x = factor(x), y = z, fill = y)) + geom_bar(stat= 'identity', position = 'stack') # �ѵ���״ͼ
ggplot(data = df, mapping = aes(x = factor(x), y = z, fill = y)) + geom_bar(stat= 'identity', position = 'stack') + guides(fill = guide_legend(reverse= TRUE)) # ����ͼ�Ķѵ�˳����ͼ��˳����ͬ
ggplot(data = df, mapping = aes(x = factor(x), y = z, fill = y)) + geom_bar(stat= 'identity', position = 'fill') # �ٷֱȶѵ�ͼ

# ���ĵ�ɫ����
ggplot(data = df, mapping = aes(x = factor(x), y = z, fill = y)) + geom_bar(stat= 'identity', position = 'dodge') + scale_fill_brewer(palette = 'Accent')
# �Զ����ɫ����
col <- c('darkred','skyblue','purple')
ggplot(data = df, mapping =aes(x = factor(x), y = z, fill = y)) + geom_bar(stat = 'identity', colour= 'black', position = 'dodge') +scale_fill_manual(values = col,limits= c('B','C','A')) + xlab('x')
# ��zֵ�Ĵ�С��������������ͼ��˳��ֻ�轫aes()��x��������reorder()����
ggplot(data = df, mapping = aes(x = reorder(x, z), y = z, fill = y)) +geom_bar(stat = 'identity') + xlab('x')

# ���ӱ�ǩ
# �ѵ���׳����ͼ
ggplot(data = df, mapping = aes(x = x, y = z, fill = y)) + geom_bar(stat
                                                                    = 'identity', position = 'stack') + geom_text(mapping = aes(label = z),
                                                                                                                 size = 5, colour = 'black', vjust = 3.5, hjust = .5, position = position_stack())
#�ֿ���׳����ͼ 
ggplot(data = df, mapping = aes(x = x, y = z, fill = y)) + geom_bar(stat
                                                                    = 'identity', position = 'dodge') + geom_text(mapping = aes(label = z),
                                                                                                                  size = 5, colour = 'black', vjust = 1, hjust = .5, position = position_dodge(0.9))

ggplot(data = df, mapping = aes(x = interaction(x,y),
                               y = z, fill = y)) + geom_bar(stat = 'identity') + geom_text(mapping = aes(label= z))