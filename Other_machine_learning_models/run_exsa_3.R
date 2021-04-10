library(Hmisc)
library(survivalROC)

for (fold in c(1,5)){

  source("ratio_sample.R")
  cross_va_sample(x,1234,fold)

  x.train <- read.table(file=sprintf("x.train_%d.txt",fold),T)
  x.test <- read.table(file=sprintf("x.test_%d.txt",fold),T)
  t.train <- x.train$time
  t.test <- x.test$time
  s.train <- x.train$status
  s.test <- x.test$status

pred_exsa = read.table(file=sprintf("pre_%d.txt",fold))
cindex_exsa = 1-rcorr.cens(pred_exsa[[1]],Surv(t.test, s.test))[[1]]



### 函数输出 参数、模型、预测值、cindex、1年auc、3年auc、5年auc

result <- list()
result$pred = pred_exsa
result$cindex = cindex_exsa


cutoff = 365*1
if ( min(x.test$time) < cutoff )
{
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_exsa, predict.time = cutoff,lambda=0.01,method = "NNE")
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
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_exsa, predict.time = cutoff,lambda=0.01,method = "NNE")
result$fp_3 = y$FP
result$tp_3 = y$TP
result$auc_3 = y$AUC

cutoff=365*5
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_exsa, predict.time = cutoff,lambda=0.01,method = "NNE")
result$fp_5 = y$FP
result$tp_5 = y$TP
result$auc_5 = y$AUC

cutoff=365*10
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_exsa, predict.time = cutoff,lambda=0.01,method = "NNE")
result$fp_10 = y$FP
result$tp_10 = y$TP
result$auc_10 = y$AUC

### another AUC


cutoff = 365*1
if ( min(x.test$time) < cutoff )
{
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_exsa, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
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
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_exsa, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
result$fp_span_3 = y_span$FP
result$tp_span_3 = y_span$TP
result$auc_span_3 = y_span$AUC

cutoff=365*5
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_exsa, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
result$fp_span_5 = y_span$FP
result$tp_span_5 = y_span$TP
result$auc_span_5 = y_span$AUC

cutoff=365*10
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_exsa, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
result$fp_span_10 = y_span$FP
result$tp_span_10 = y_span$TP
result$auc_span_10 = y_span$AUC

save("result", file = sprintf("%d_exsa_result.RData", fold))

}


##5次交叉验证结果——EXSA
load("1_exsa_result.RData")
result_exsa_1 = result
load("2_exsa_result.RData")
result_exsa_2 = result
load("3_exsa_result.RData")
result_exsa_3 = result
load("4_exsa_result.RData")
result_exsa_4 = result
load("5_exsa_result.RData")
result_exsa_5 = result

##交叉验证结果求平均  平均cindex、平均auc、画auc图
source("result_mean.R")
result_mean("exsa",result_exsa_1,result_exsa_2,result_exsa_3,result_exsa_4,result_exsa_5)
load("exsa_fin_result.RData")
exsa_fin_result = data_fin




