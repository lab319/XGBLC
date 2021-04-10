xgblc_param_2 <- function(x.train, best_param, nround, fold, nthreads){

## 自定义目标函数
mylossobj<-function(preds, dtrain) {
labels <- getinfo(dtrain, "label")
ord<-order(abs(labels))	
ran=rank(abs(labels))
grad=c()
hess=c()
label <- labels[ord]  
ls <- lsfit(preds,Xtrain,tolerance = 1e-07,intercept = FALSE)
beta <- ls$coefficients
beta[which(beta>0)]=1
beta[which(beta<0)]=-1
exp_p_sum = sum(exp(preds[ord]))
rk=0
sk=0
last_exp_p=0
last_abs_y=0
acc_sum=0
for(i in 1:length(label)){   
p=preds[ord][i]
exp_p=exp(p)
y=label[i]   
abs_y=abs(y)
acc_sum=acc_sum+last_exp_p
if(last_abs_y < abs_y){
	exp_p_sum = exp_p_sum - acc_sum
	acc_sum=0
}
if (y>0){
rk = rk+1/exp_p_sum
sk = sk+1/(exp_p_sum*exp_p_sum)
}
xx <- 1/Xtrain[i,which(Xtrain[i,]>0)]
g_beta <- sum(beta[which(Xtrain[i,]>0)]*xx)
###
grad[i] <- (exp_p * rk - as.numeric(y>0)) - lambda2*g_beta/length(beta)
hess[i] <- (exp_p * rk - exp_p*exp_p*sk ) 
last_abs_y = abs_y
last_exp_p = exp_p
}
grad=grad[ran]		
hess=hess[ran]			
return(list(grad = grad, hess = hess))
}

###########
num_feature = dim(x.train)[2]
Xtrain = x.train[,c(2:(num_feature-2))]
best_lambda = 0
best_cindex = 0

lambda_1 = c(0.00001,0.0001,0.001,0.01,0.1,1,2,5,10 )
#lambda_1 = c(0.01,0.1,1000)

for (lambda2 in lambda_1){
  print (lambda2)
  set.seed(4321)
  best_param$objective=mylossobj
  md <- try(xgboost(data=Dtrain, params=best_param, nrounds=nround,nthread=nthreads, verbose = F, watchlist))
  if("try-error" %in% class(md))
      {
        next
      }   
  xgb_pred1=predict(md,Dtest)
  cindex <- try(1-rcorr.cens(xgb_pred1,Surv(t.test, s.test))[[1]])
  if("try-error" %in% class(cindex))
      {
        next
      } 
  print(cindex)
  try(
  if (best_cindex<cindex){
   best_cindex=cindex
   best_lambda=lambda2
   })
  }

print(best_lambda)
lambda_2 = c(seq(best_lambda/5,best_lambda,best_lambda/10),seq(best_lambda*2,best_lambda*10,best_lambda))

 for (lambda2 in lambda_2){
  print (lambda2)
  set.seed(4321)
  best_param$objective=mylossobj
  md <- try(xgboost(data=Dtrain, params=best_param, nrounds=nround,nthread=nthreads, verbose = F, watchlist))
  if("try-error" %in% class(md))
      {
        next
      }   
  xgb_pred1=predict(md,Dtest)
  cindex <- try(1-rcorr.cens(xgb_pred1,Surv(t.test, s.test))[[1]])
  if("try-error" %in% class(cindex))
      {
        next
      } 
  print(cindex)
  try(
  if (best_cindex<cindex){
   best_cindex=cindex
   best_lambda=lambda2
   })
  }

lambda_3 = c(seq(best_lambda/5,best_lambda,best_lambda/10),seq(best_lambda*2,best_lambda*10,best_lambda))

 for (lambda2 in lambda_3){
  print (lambda2)
  set.seed(4321)
  best_param$objective=mylossobj
  md <- try(xgboost(data=Dtrain, params=best_param, nrounds=nround,nthread=nthreads, verbose = F, watchlist))
  if("try-error" %in% class(md))
      {
        next
      }   
  xgb_pred1=predict(md,Dtest)
  cindex <- try(1-rcorr.cens(xgb_pred1,Surv(t.test, s.test))[[1]])
  if("try-error" %in% class(cindex))
      {
        next
      } 
  print(cindex)
  try(
  if (best_cindex<cindex){
   best_cindex=cindex
   best_lambda=lambda2
   })
  }

set.seed(4321)
lambda2 = best_lambda
best_param$objective=mylossobj
Xtrain = x.train[,c(2:(num_feature-2))]
model_my <- xgboost(data=Dtrain, params=best_param, nrounds=nround,nthread=nthreads, verbose = F, watchlist)
  
### 函数输出 参数、模型、预测值、cindex、1年auc、3年auc、5年auc

result <- list()
result$best_param = best_param
result$nround = nround
result$model = model_my
result$pred = predict(model_my,Dtest)
pred_my = predict(model_my,Dtest)
result$cindex = 1-rcorr.cens(pred_my,Surv(t.test, s.test))[[1]]
#cindex_my = 1-rcorr.cens(pred_my,Surv(t.test, s.test))[[1]]

cutoff = 365*1
if ( min(x.test$time) < cutoff )
{
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_my, predict.time = cutoff,lambda=0.01,method = "NNE")
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
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_my, predict.time = cutoff,lambda=0.01,method = "NNE")
result$fp_3 = y$FP
result$tp_3 = y$TP
result$auc_3 = y$AUC

cutoff=365*5
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_my, predict.time = cutoff,lambda=0.01,method = "NNE")
result$fp_5 = y$FP
result$tp_5 = y$TP
result$auc_5 = y$AUC

cutoff=365*10
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_my, predict.time = cutoff,lambda=0.01,method = "NNE")
result$fp_10 = y$FP
result$tp_10 = y$TP
result$auc_10 = y$AUC


cutoff = 365*1
if ( min(x.test$time) < cutoff )
{
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_my, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
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
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_my, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
result$fp_span_3 = y_span$FP
result$tp_span_3 = y_span$TP
result$auc_span_3 = y_span$AUC

cutoff=365*5
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_my, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
result$fp_span_5 = y_span$FP
result$tp_span_5 = y_span$TP
result$auc_span_5 = y_span$AUC

cutoff=365*10
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_my, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
result$fp_span_10 = y_span$FP
result$tp_span_10 = y_span$TP
result$auc_span_10 = y_span$AUC



save("result", file = sprintf("%d_xgblc_2_result.RData", fold))

}


