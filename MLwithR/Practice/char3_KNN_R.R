# min-max normalize 
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
as.data.frame(lapply(data,normalize))
# z-score
data_z <- scale(data)
# data_train <-
# data_test <- 
# data_train_labels <-
# data_test_labels <-
install.packages('class')
install.packages('gmodels')
library(gmodels)
library(class)
data_test_pred <- knn(data_train,data_test,data_train_labels,k=21)
CrossTable(data_test_labels,data_test_pred,prop.chisq = FALSE)