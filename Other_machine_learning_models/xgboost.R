xgb_param <- function(best_param,best_loss,best_lost_index,seed.number,watchlist,fold,nthreads){

########
########	a,b
########	max_depth
########	min_child_weight
########	

max_depth. <- c(3:11)
min_child_weight. <- c(3:18)

for (a in max_depth.){
for (b in min_child_weight.){
print("max_depth, min_child_weight")
print(a)
print(b)
	 param <- list(objective = "survival:cox",
                eval_metric = "cox-nloglik",
                max_depth = a,
                eta = 0.01,
                gamma = 0, 
                subsample = 0.8,
                colsample_bytree = 0.8, 
                min_child_weight = b,
                lambda=1,
                alpha=0
)
  cv.nround = 5000
  cv.nfold = 5
  set.seed(seed.number)
  mdcv <- xgb.cv(data=Dtrain, params = param, nthread=nthreads, 
                 nfold=cv.nfold, nrounds=cv.nround,watchlist,
                 verbose = T, early.stop.round=30, maximize=FALSE )
 
  if("try-error" %in% class(mdcv))
      {
        next
      } 
  
  min_loss = min(mdcv$evaluation_log[,'test_cox_nloglik_mean'])
  min_loss_index = which.min(as.numeric(unlist(mdcv$evaluation_log[,'test_cox_nloglik_mean'])))
  
 if (min_loss < best_loss) {
    best_loss = min_loss
    best_loss_index = min_loss_index
    best_param = param
    write.table(best_loss, file = "best_loss.txt", append=F)
    write.table(best_loss_index, file = "best_loss_index.txt", append=F)
    write.table(best_param, file = "best_param.txt", append=F)
  }
write.table(a,file='max_depth.txt',append=FALSE)
write.table(b,file='min_child_weight.txt',append=FALSE)

}}

########
########	c
########	gamma
########	

max_depth. = best_param$max_depth  ##8
min_child_weight. = best_param$min_child_weight  ##13

gamma. <- seq(0,0.15,0.01)

for (c in gamma.){
print("gamma")
print(c)
	 param <- list(objective = "survival:cox",
                eval_metric = "cox-nloglik",
		    max_depth = max_depth.,
                eta = 0.01,
                gamma = c, 
                subsample = 0.8,
                colsample_bytree = 0.8, 
                min_child_weight = min_child_weight.,
                lambda=1,
                alpha=0
)
  cv.nround = 5000
  cv.nfold = 5
  set.seed(seed.number)
  mdcv <- xgb.cv(data=Dtrain, params = param, nthread=nthreads, 
                 nfold=cv.nfold, nrounds=cv.nround,watchlist,
                 verbose = T, early.stop.round=30, maximize=FALSE )
  
  if("try-error" %in% class(mdcv))
      {
        next
      } 

  min_loss = min(mdcv$evaluation_log[,'test_cox_nloglik_mean'])
  min_loss_index = which.min(as.numeric(unlist(mdcv$evaluation_log[,'test_cox_nloglik_mean'])))
  
  if (min_loss < best_loss) {
    best_loss = min_loss
    best_loss_index = min_loss_index
    best_param = param
    write.table(best_loss, file = "best_loss.txt", append=F)
    write.table(best_loss_index, file = "best_loss_index.txt", append=F)
    write.table(best_param, file = "best_param.txt", append=F)
  }
write.table(c,file='gamma.txt',append=FALSE)

}

########
########	d,e
########	subsample
########	colsample_bytree
########	

gamma. = best_param$gamma  ##1.4

subsample. <- seq(0.4,0.95,0.05)
colsample_bytree. <- seq(0.4,0.95,0.05)

for (d in subsample.){
for (e in colsample_bytree.){
print("subsample, colsample_bytree")
print(d)
print(e)
	 param <- list(objective = "survival:cox",
                eval_metric = "cox-nloglik",
                max_depth = max_depth.,
                eta = 0.01,
                gamma = gamma., 
                subsample = d,
                colsample_bytree = e, 
                min_child_weight = min_child_weight.,
                lambda=1,
                alpha=0
                )
  cv.nround = 5000
  cv.nfold = 5
  set.seed(seed.number)
  mdcv <- xgb.cv(data=Dtrain, params = param, nthread=nthreads, 
                 nfold=cv.nfold, nrounds=cv.nround,watchlist,
                 verbose = T, early.stop.round=30, maximize=FALSE )

  if("try-error" %in% class(mdcv))
      {
        next
      } 
  
  min_loss = min(mdcv$evaluation_log[,'test_cox_nloglik_mean'])
  min_loss_index = which.min(as.numeric(unlist(mdcv$evaluation_log[,'test_cox_nloglik_mean'])))
    
  if (min_loss < best_loss) {
    best_loss = min_loss
    best_loss_index = min_loss_index
    best_param = param
    write.table(best_loss, file = "best_loss.txt", append=F)
    write.table(best_loss_index, file = "best_loss_index.txt", append=F)
    write.table(best_param, file = "best_param.txt", append=F)
  }
write.table(d,file='subsample.txt',append=FALSE)
write.table(e,file='colsample_bytree.txt',append=FALSE)
}}

########
########	f,g
########	alpha
########	lambda
########

subsample. = best_param$subsample
colsample_bytree. = best_param$colsample_bytree

alpha. <- seq(0,1.5,0.1)
lambda. <- seq(0,1.5,0.1)

for (f in alpha.){
for (g in lambda.){
print("alpha, lambda")
print(f)
print(g)
	 param <- list(objective = "survival:cox",
                eval_metric = "cox-nloglik",
                max_depth = max_depth.,
                eta = 0.01,
                gamma = gamma., 
                subsample = subsample.,
                colsample_bytree = colsample_bytree., 
                min_child_weight = min_child_weight.,
                lambda=g,
                alpha=f

)
  cv.nround = 5000
  cv.nfold = 5
  set.seed(seed.number)
  mdcv <- xgb.cv(data=Dtrain, params = param, nthread=nthreads, 
                 nfold=cv.nfold, nrounds=cv.nround,watchlist,
                 verbose = T, early.stop.round=30, maximize=FALSE )

  if("try-error" %in% class(mdcv))
      {
        next
      } 
  
  min_loss = min(mdcv$evaluation_log[,'test_cox_nloglik_mean'])
  min_loss_index = which.min(as.numeric(unlist(mdcv$evaluation_log[,'test_cox_nloglik_mean'])))
  
  
  if (min_loss < best_loss) {
    best_loss = min_loss
    best_loss_index = min_loss_index
    best_param = param
    write.table(best_loss, file = "best_loss.txt", append=F)
    write.table(best_loss_index, file = "best_loss_index.txt", append=F)
    write.table(best_param, file = "best_param.txt", append=F)
  }
write.table(f,file='alpha.txt',append=FALSE)
write.table(g,file='lambda.txt',append=FALSE)
}}

########
########	h
########	eta
########

alpha. = best_param$alpha   ##1
lambda. = best_param$lambda   ##1.85

eta. <- seq(0.01,0.13,0.01)

for (h in eta.){
print("eta")
print(h)
	 param <- list(objective = "survival:cox",
                eval_metric = "cox-nloglik",
                max_depth = max_depth.,
                eta = h,
                gamma = gamma., 
                subsample = subsample.,
                colsample_bytree = colsample_bytree., 
                min_child_weight = min_child_weight.,
                lambda=lambda.,
                alpha=alpha.
)
  cv.nround = 5000
  cv.nfold = 5
  set.seed(seed.number)
  mdcv <- xgb.cv(data=Dtrain, params = param, nthread=nthreads, 
                 nfold=cv.nfold, nrounds=cv.nround,watchlist,
                 verbose = T, early.stop.round=30, maximize=FALSE )

  if("try-error" %in% class(mdcv))
      {
        next
      } 
  
  min_loss = min(mdcv$evaluation_log[,'test_cox_nloglik_mean'])
  min_loss_index = which.min(as.numeric(unlist(mdcv$evaluation_log[,'test_cox_nloglik_mean'])))
  
  write.table(param, file="param_now.txt", append=F)
  
  if (min_loss < best_loss) {
    best_loss = min_loss
    best_loss_index = min_loss_index
    best_param = param
    write.table(best_loss, file = "best_loss.txt", append=F)
    write.table(best_loss_index, file = "best_loss_index.txt", append=F)
    write.table(best_param, file = "best_param.txt", append=F)
  }
write.table(h,file='eta.txt',append=FALSE)

}

nround = best_loss_index
set.seed(seed.number)
model_xgb <- xgboost(data=Dtrain, params=best_param, nrounds=nround, nthread=nthreads)

### 函数输出 参数、模型、预测值、cindex、1年auc、3年auc、5年auc

result <- list()
result$best_param = best_param
result$nround = nround
result$model = model_xgb
result$pred = predict(model_xgb,Dtest)
pred_xgb = predict(model_xgb,Dtest)
result$cindex = 1-rcorr.cens(pred_xgb,Surv(t.test, s.test))[[1]]
#cindex_xgb = 1-rcorr.cens(pred_xgb,Surv(t.test, s.test))[[1]]


cutoff = 365*1
if ( min(x.test$time) < cutoff )
{
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_xgb, predict.time = cutoff,lambda=0.01,method = "NNE")
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
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_xgb, predict.time = cutoff,lambda=0.01,method = "NNE")
result$fp_3 = y$FP
result$tp_3 = y$TP
result$auc_3 = y$AUC

cutoff=365*5
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_xgb, predict.time = cutoff,lambda=0.01,method = "NNE")
result$fp_5 = y$FP
result$tp_5 = y$TP
result$auc_5 = y$AUC

cutoff=365*10
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_xgb, predict.time = cutoff,lambda=0.01,method = "NNE")
result$fp_10 = y$FP
result$tp_10 = y$TP
result$auc_10 = y$AUC


cutoff = 365*1
if ( min(x.test$time) < cutoff )
{
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_xgb, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
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
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_xgb, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
result$fp_span_3 = y_span$FP
result$tp_span_3 = y_span$TP
result$auc_span_3 = y_span$AUC

cutoff=365*5
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_xgb, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
result$fp_span_5 = y_span$FP
result$tp_span_5 = y_span$TP
result$auc_span_5 = y_span$AUC

cutoff=365*10
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_xgb, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
result$fp_span_10 = y_span$FP
result$tp_span_10 = y_span$TP
result$auc_span_10 = y_span$AUC



save("result", file = sprintf("%d_xgb_result.RData", fold))

}



