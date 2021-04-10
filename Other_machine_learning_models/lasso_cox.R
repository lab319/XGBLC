

lasso_cox <- function(x.train,fold){

time = as.double(x.train$time)
status = as.double(x.train$status)

set.seed(4321)
fit = cv.glmnet(as.matrix(x.train[,c(2:(num_feature-2))]), Surv(time,status), family = "cox")

pred_cox = predict(fit, as.matrix(x.test[,c(2:(num_feature-2))]),s = c("lambda.min"))
cindex_cox = 1-rcorr.cens(pred_cox,Surv(t.test, s.test))[[1]]
print(sprintf("cox_%d",fold))
print(cindex_cox)

### 函数输出 参数、模型、预测值、cindex、1年auc、3年auc、5年auc

result <- list()
result$model = fit
result$pred = predict(fit, as.matrix(x.test[,c(2:(num_feature-2))]),s = c("lambda.min"))
pred_cox = predict(fit, as.matrix(x.test[,c(2:(num_feature-2))]),s = c("lambda.min"))
result$cindex = 1-rcorr.cens(pred_cox,Surv(t.test, s.test))[[1]]
#cindex_cox = 1-rcorr.cens(pred_cox,Surv(t.test, s.test))[[1]]


cutoff = 365*1
if ( min(x.test$time) < cutoff )
{
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_cox, predict.time = cutoff,lambda=0.01,method = "NNE")
result$fp_1 = y$FP
result$tp_1 = y$TP
result$auc_1 = y$AUC
}else
{
result$fp_1 = NA
result$tp_1 = NA
result$auc_1 = NA
}

cutoff=365*3
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_cox, predict.time = cutoff,lambda=0.01,method = "NNE")
result$fp_3 = y$FP
result$tp_3 = y$TP
result$auc_3 = y$AUC

cutoff=365*5
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_cox, predict.time = cutoff,lambda=0.01,method = "NNE")
result$fp_5 = y$FP
result$tp_5 = y$TP
result$auc_5 = y$AUC

cutoff=365*10
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_cox, predict.time = cutoff,lambda=0.01,method = "NNE")
result$fp_10 = y$FP
result$tp_10 = y$TP
result$auc_10 = y$AUC


cutoff = 365*1
if ( min(x.test$time) < cutoff )
{
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_cox, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
result$fp_span_1 = y_span$FP
result$tp_span_1 = y_span$TP
result$auc_span_1 = y_span$AUC
}else
{
result$fp_span_1 = NA
result$tp_span_1 = NA
result$auc_span_1 = NA
}


cutoff=365*3
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_cox, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
result$fp_span_3 = y_span$FP
result$tp_span_3 = y_span$TP
result$auc_span_3 = y_span$AUC

cutoff=365*5
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_cox, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
result$fp_span_5 = y_span$FP
result$tp_span_5 = y_span$TP
result$auc_span_5 = y_span$AUC

cutoff=365*10
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_cox, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
result$fp_span_10 = y_span$FP
result$tp_span_10 = y_span$TP
result$auc_span_10 = y_span$AUC



save("result", file = sprintf("%d_lasso_cox_result.RData", fold))

}




