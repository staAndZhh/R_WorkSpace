# confuse matrix
table(sms_results$actual_type,sms_results$predict_type)
library(gmodels)
CrossTable(sms_results$actual_type,sms_results$predict_type)

library(caret)
confusionMatrix(sms_results$actual_type,sms_results$predict_type,positive='spam')

# kappa
library(vcd)
kappa(table(sms_results$actual_type,sms_results$predict_type))

library(irr)
kappa2(sms_results[1:2])

# real True & real false
library(caret)
sensitivity(sms_results$actual_type,sms_results$predict_type,positive='spam')
specificity(sms_results$actual_type,sms_results$predict_type,negative='ham')

# real true/ predit real &  real true/true all library(caret)
posPredValue(sms_results$actual_type,sms_results$predict_type,positive='spam')
sensitivity(sms_results$actual_type,sms_results$predict_type,positive='spam')

# F

# visiual of measure
library(ROCR)
pred <- prediction(predictions = sms_results$prob_spam,labels=sms_results$actual_type)

# ROC
perf <- performance(pred,measure='tpr',x.measure ='fpr')
plot(perf,main="ROC for sms spam filter",col='blue',lwd=3)

perf.auc <- performance(pred,measure='auc')
str(perf.auc)
unlist(perf.auc@y.values)

# keep
# k-method
# cross valia
