
xgblc_param_1 <- function(best_param,best_loss,best_lost_index,seed.number,watchlist,fold,nthreads){

### 自定义目标函数
mylossobj<-function(preds, dtrain) {
labels <- getinfo(dtrain, "label")

ord<-order(abs(labels))	
ran=rank(abs(labels))
	

grad=c()
hess=c()

label <- labels[ord]  

#ls <- lsfit(preds,Xtrain,tolerance = 1e-07,intercept = FALSE)
#beta <- ls$coefficients

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


#xx <- 1/Xtrain[i,which(Xtrain[i,]>0)]
#xx2 <- xx^2
#g_beta <- sum(beta[which(Xtrain[i,]>0)]*xx)
#h_beta <- sum(xx2)

###
grad[i] <- (exp_p * rk - as.numeric(y>0))# - 2*lambda2*g_beta/1e9
hess[i] <- (exp_p * rk - exp_p*exp_p*sk )# - 2*lambda2*h_beta/1e9

last_abs_y = abs_y
last_exp_p = exp_p
}

grad=grad[ran]		
hess=hess[ran]			
return(list(grad = grad, hess = hess))
}


evalerror <- function(preds, dtrain) {
    labels <- getinfo(dtrain, "label") #labels<-dtrain$label
    censor= labels>0
    labels=abs(labels)
    ord<-order(labels)
    d=censor[ord]  #status
    etas=preds[ord] #linear predictor
    haz<-as.numeric(exp(etas)) #w[i]
    rsk<-rev(cumsum(rev(haz)))
    err <- -2*sum(d*(etas-log(rsk)))/length(labels)
    return(list(metric = "cindex",value = err))
}



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
	 param <- list(objective = mylossobj,
                eval_metric = evalerror,
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
  mdcv <- try(xgb.cv(data=Dtrain, params = param, nthread=nthreads, 
                   nfold=cv.nfold, nrounds=cv.nround,watchlist,
                   verbose = T, early.stop.round=30, maximize=FALSE ))
  if("try-error" %in% class(mdcv))
      {
        next
      } 
  min_loss = min(mdcv$evaluation_log[,'test_cindex_mean'])
  min_loss_index = which.min(as.numeric(unlist(mdcv$evaluation_log[,'test_cindex_mean'])))
  
  if (min_loss < best_loss) {
    best_loss = min_loss
    best_loss_index = min_loss_index
    best_param = param
aaa = best_param
aaa$objective=""
aaa$eval_metric=""
write.table(aaa, file="best_param.txt",append=FALSE,sep = "\t" )
write.table(best_loss_index,file='best_loss_index.txt',append=FALSE)
write.table(best_loss,file='best_loss.txt',append=FALSE)
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
	 param <- list(objective = mylossobj,
                eval_metric = evalerror,
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
  mdcv <- try(xgb.cv(data=Dtrain, params = param, nthread=nthreads, 
                   nfold=cv.nfold, nrounds=cv.nround,watchlist,
                   verbose = T, early.stop.round=30, maximize=FALSE ))
  if("try-error" %in% class(mdcv))
      {
        next
      } 

  min_loss = min(mdcv$evaluation_log[,'test_cindex_mean'])
  min_loss_index = which.min(as.numeric(unlist(mdcv$evaluation_log[,'test_cindex_mean'])))
  
  if (min_loss < best_loss) {
    best_loss = min_loss
    best_loss_index = min_loss_index
    best_param = param
aaa = best_param
aaa$objective=""
aaa$eval_metric=""
write.table(aaa, file="best_param.txt",append=FALSE,sep = "\t" )
write.table(best_loss_index,file='best_loss_index.txt',append=FALSE)
write.table(best_loss,file='best_loss.txt',append=FALSE)
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
	 param <- list(objective = mylossobj,
                eval_metric = evalerror,
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
  mdcv <- try(xgb.cv(data=Dtrain, params = param, nthread=nthreads, 
                   nfold=cv.nfold, nrounds=cv.nround,watchlist,
                   verbose = T, early.stop.round=30, maximize=FALSE ))
  if("try-error" %in% class(mdcv))
      {
        next
      } 
  
  min_loss = min(mdcv$evaluation_log[,'test_cindex_mean'])
  min_loss_index = which.min(as.numeric(unlist(mdcv$evaluation_log[,'test_cindex_mean'])))
  
  if (min_loss < best_loss) {
    best_loss = min_loss
    best_loss_index = min_loss_index
    best_param = param
aaa = best_param
aaa$objective=""
aaa$eval_metric=""
write.table(aaa, file="best_param.txt",append=FALSE,sep = "\t" )
write.table(best_loss_index,file='best_loss_index.txt',append=FALSE)
write.table(best_loss,file='best_loss.txt',append=FALSE)
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
	 param <- list(objective = mylossobj,
                eval_metric = evalerror,
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
  mdcv <- try(xgb.cv(data=Dtrain, params = param, nthread=nthreads, 
                   nfold=cv.nfold, nrounds=cv.nround,watchlist,
                   verbose = T, early.stop.round=30, maximize=FALSE ))
  if("try-error" %in% class(mdcv))
      {
        next
      } 
  
  min_loss = min(mdcv$evaluation_log[,'test_cindex_mean'])
  min_loss_index = which.min(as.numeric(unlist(mdcv$evaluation_log[,'test_cindex_mean'])))
  
  if (min_loss < best_loss) {
    best_loss = min_loss
    best_loss_index = min_loss_index
    best_param = param
	aaa = best_param
aaa$objective=""
aaa$eval_metric=""
write.table(aaa, file="best_param.txt",append=FALSE,sep = "\t" )
write.table(best_loss_index,file='best_loss_index.txt',append=FALSE)
write.table(best_loss,file='best_loss.txt',append=FALSE)
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
	 param <- list(objective = mylossobj,
                eval_metric = evalerror,
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
  mdcv <- try(xgb.cv(data=Dtrain, params = param, nthread=nthreads, 
                   nfold=cv.nfold, nrounds=cv.nround,watchlist,
                   verbose = T, early.stop.round=30, maximize=FALSE ))
  if("try-error" %in% class(mdcv))
      {
        next
      } 
  
  min_loss = min(mdcv$evaluation_log[,'test_cindex_mean'])
  min_loss_index = which.min(as.numeric(unlist(mdcv$evaluation_log[,'test_cindex_mean'])))

  if (min_loss < best_loss) {
    best_loss = min_loss
    best_loss_index = min_loss_index
    best_param = param
aaa = best_param
aaa$objective=""
aaa$eval_metric=""
write.table(aaa, file="best_param.txt",append=FALSE,sep = "\t" )
write.table(best_loss_index,file='best_loss_index.txt',append=FALSE)
write.table(best_loss,file='best_loss.txt',append=FALSE)
  }
write.table(h,file='eta.txt',append=FALSE)
}


nround = best_loss_index
set.seed(seed.number)
model_my_xgb <- xgboost(data=Dtrain, params=best_param, nrounds=nround, nthread=nthreads)

### 函数输出 参数、模型、预测值、cindex、1年auc、3年auc、5年auc

result <- list()
result$best_param = best_param
result$nround = nround
result$model = model_my_xgb
result$pred = predict(model_my_xgb,Dtest)
pred_my_xgb = predict(model_my_xgb,Dtest)
result$cindex = 1-rcorr.cens(pred_my_xgb,Surv(t.test, s.test))[[1]]
#cindex_my_xgb = 1-rcorr.cens(pred_my_xgb,Surv(t.test, s.test))[[1]]


cutoff = 365*1
if ( min(x.test$time) < cutoff )
{
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_my_xgb, predict.time = cutoff,lambda=0.01,method = "NNE")
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
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_my_xgb, predict.time = cutoff,lambda=0.01,method = "NNE")
result$fp_3 = y$FP
result$tp_3 = y$TP
result$auc_3 = y$AUC

cutoff=365*5
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_my_xgb, predict.time = cutoff,lambda=0.01,method = "NNE")
result$fp_5 = y$FP
result$tp_5 = y$TP
result$auc_5 = y$AUC

cutoff=365*10
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_my_xgb, predict.time = cutoff,lambda=0.01,method = "NNE")
result$fp_10 = y$FP
result$tp_10 = y$TP
result$auc_10 = y$AUC


cutoff = 365*1
if ( min(x.test$time) < cutoff )
{
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_my_xgb, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
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
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_my_xgb, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
result$fp_span_3 = y_span$FP
result$tp_span_3 = y_span$TP
result$auc_span_3 = y_span$AUC

cutoff=365*5
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_my_xgb, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
result$fp_span_5 = y_span$FP
result$tp_span_5 = y_span$TP
result$auc_span_5 = y_span$AUC

cutoff=365*10
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_my_xgb, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
result$fp_span_10 = y_span$FP
result$tp_span_10 = y_span$TP
result$auc_span_10 = y_span$AUC



save("result", file = sprintf("%d_xgblc_1_result.RData", fold))

}
