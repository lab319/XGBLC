svm_param <- function(x.train, fold){

CVgroup <- function(k,datasize,seed){
  cvlist <- list()
  set.seed(seed)
  n <- rep(1:k,ceiling(datasize/k))[1:datasize]    #将数据分成K份，并生成的完成数据集n
  temp <- sample(n,datasize)   #把n打乱
  x <- 1:k
  dataseq <- 1:datasize
  cvlist <- lapply(x,function(x) dataseq[temp==x])  #dataseq中随机生成k个随机有序数据列
  return(cvlist)
}

k <- 5
datasize <- nrow(x.train)
cvlist <- CVgroup(k = k,datasize = nrow(x.train),seed = 1234)
cvlist

cindex_fin <- c()
j=1
best_cindex = 0
best_C = 0
num_feature = dim(x.train)[2]
for(C in c(0.0001,0.001,0.01,0.1,1,2,5,10)){   #j指的是随机森林的数量
  print("C")
  print(C)
  cindex_svm <- c()
  for (i in 1:k){
    xx.train <- x.train[-cvlist[[i]],]  #刚才通过cvgroup生成的函数
    xx.test <- x.train[cvlist[[i]],]
    tt.train <- t.train[-cvlist[[i]]]
    ss.train <- s.train[-cvlist[[i]]]
    tt.test <- t.train[cvlist[[i]]]
    ss.test <- s.train[cvlist[[i]]]
    print(i)
    model <-survivalsvm(formula = Surv(tt.train, ss.train) ~ ., 
		data = xx.train,type = "regression", 
		gamma.mu = C,opt.meth="quadprog",kernel="add_kernel")

    pred <- predict(model,xx.test)   #预测
    cindex_svm[i] <- 1-rcorr.cens(pred$predicted,Surv(tt.test, ss.test))[[1]]
    print(cindex_svm[i])
    }
  cindex_fin[j] = (cindex_svm[1]+cindex_svm[2]+cindex_svm[3]+cindex_svm[4]+cindex_svm[5])/5
  if (cindex_fin[i] > best_cindex) {
    best_cindex = cindex_svm[i]
    best_C = C
    }
  j = j+1
  }

#### 细化参数_1

print(best_lambda)
C_2 = c(seq(best_lambda/5,best_lambda,best_lambda/10),seq(best_lambda*2,best_lambda*10,best_lambda))
for(C in C_2){   #j指的是随机森林的数量
  print(C)
  cindex_svm <- c()
  for (i in 1:k){
    xx.train <- x.train[-cvlist[[i]],]  #刚才通过cvgroup生成的函数
    xx.test <- x.train[cvlist[[i]],]
    tt.train <- t.train[-cvlist[[i]]]
    ss.train <- s.train[-cvlist[[i]]]
    tt.test <- t.train[cvlist[[i]]]
    ss.test <- s.train[cvlist[[i]]]

    model <-survivalsvm(formula = Surv(tt.train, ss.train) ~ ., 
		data = xx.train,type = "regression", 
		gamma.mu = C,opt.meth="quadprog",kernel="add_kernel")

    pred <- predict(model,xx.test)   #预测
    cindex_svm[i] <- 1-rcorr.cens(pred$predicted,Surv(tt.test, ss.test))[[1]]
    print(cindex_svm[i])
    }
  cindex_fin[j] = (cindex_svm[1]+cindex_svm[2]+cindex_svm[3]+cindex_svm[4]+cindex_svm[5])/5
  if (cindex_fin[i] > best_cindex) {
    best_cindex = cindex_svm[i]
    best_C = C
    }
  j = j+1
  }






xx <- as.data.frame(x.train)
num_feature = dim(x.train)[2]
model_svm <- rfsrc(formula=as.formula(Surv(time,status)~.),
		data=as.data.frame(xx[2:num_feature]),ntree = best_ntree,
		seed=1234,nodesize=3,splitrule = "logrank",tree.err=TRUE)
pred <- predict(model_svm,as.data.frame(x.test[,c(2:num_feature)]))
cindex_svm <- 1 - pred$err.rate[best_ntree]


### 函数输出 参数、模型、预测值、cindex、1年auc、3年auc、5年auc

result <- list()
result$best_param = best_ntree
result$model = model_svm
result$pred = pred$predicted
pred_svm = pred$predicted
result$cindex = cindex_svm


cutoff = 365*1
if ( min(x.test$time) < cutoff )
{
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_svm, predict.time = cutoff,lambda=0.01,method = "NNE")
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
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_svm, predict.time = cutoff,lambda=0.01,method = "NNE")
result$fp_3 = y$FP
result$tp_3 = y$TP
result$auc_3 = y$AUC

cutoff=365*5
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_svm, predict.time = cutoff,lambda=0.01,method = "NNE")
result$fp_5 = y$FP
result$tp_5 = y$TP
result$auc_5 = y$AUC

cutoff=365*10
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_svm, predict.time = cutoff,lambda=0.01,method = "NNE")
result$fp_10 = y$FP
result$tp_10 = y$TP
result$auc_10 = y$AUC

### another AUC


cutoff = 365*1
if ( min(x.test$time) < cutoff )
{
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_svm, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
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
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_svm, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
result$fp_span_3 = y_span$FP
result$tp_span_3 = y_span$TP
result$auc_span_3 = y_span$AUC

cutoff=365*5
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_svm, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
result$fp_span_5 = y_span$FP
result$tp_span_5 = y_span$TP
result$auc_span_5 = y_span$AUC

cutoff=365*10
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_svm, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
result$fp_span_10 = y_span$FP
result$tp_span_10 = y_span$TP
result$auc_span_10 = y_span$AUC

save("result", file = sprintf("%d_svm_result.RData", fold))

}
