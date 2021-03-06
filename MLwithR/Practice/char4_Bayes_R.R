sms_raw <- read.csv('xx',stringsAsFactors = FALSE)
str(sms_raw)
sms_raw$type <- factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)

install.packages('tm')
library(tm)
sms_corpus <- Corpus(VectorSource(sms_raw$text))
print(sms_corpus)
inspect(sms_corpus[1:3])
corpus_clean <- tm_map(sms_corpus,tolower)
corpus_clean <- tm_map(sms_corpus,removeNumber)
corpus_clean <- tm_map(corpus_clean,removeWords,stopwords())
corpus_clean <- tm_map(corpus_clean,removePunctuation)
sms_dtm <- DocumentTermMatrix(corpus_clean)

sms_raw_train <- sms_raw[1:4169,]
sms_raw_test <- sms_raw[4170:5559,]

sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4170:5559,]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test <- corpus_clean[4170:5559]

prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

library(wordcloud)
wordcloud::wordcloud(sms_corpus_train,min.freq=40,random.order=FALSE)
spam <- subset(sms_raw_train,type=='spam')
ham <- subset(sms_raw_train,type=='ham')
wordcloud::wordcloud(span$text,min.freq=40,scale=c(3,0.5))
wordcloud::wordcloud(ham$text,min.freq=40,scale=c(3,0.5))

sms_dict <- Dictionary(findFreqTerms(sms_dtm_train,5))
sms_train <- DocumnetTermMaxrix(sms_corpus_train,list(dictionary = sms_dict))
sms_test <- DocumnetTermMaxrix(sms_corpus_test,list(dictionary = sms_dict))

convert_counts <- function(x){
  x <- ifelse(x>0,1,0)
  x <- factor(x,levels = c(0,1),labels = c("No","Yes"))
  return (x)
}
sms_train <- apply(sms_train, margin=2, convert_counts)
sms_test <- apply(sms_test, margin=2, convert_counts)

library(e1071)
sms_classifier <- naiveBayes(sms_train,sms_raw_train$type)
sms_test_pred <- predict(sms_classifier,sms_test)

library(gmodels)
CrossTable(sms_test_pred,sms_raw_test$type,prop.chisq = FALSE,prop.t = FALSE,dnn =c('predicted','actual'))

sms_classifier2 <- naiveBayes(sms_train,sms_raw_train$type,laplace = 1)
sms_test_pred2 <- predict(sms_classifier2,sms_test)

library(gmodels)
CrossTable(sms_test_pred2,sms_raw_test$type,prop.chisq = FALSE,prop.t = FALSE,dnn =c('predicted','actual'))