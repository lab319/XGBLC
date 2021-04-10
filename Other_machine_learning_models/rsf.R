library(plyr)

rsf_param <- function(x.train, fold){

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

#m <- seq(200,1600,200)  #如果数据量大尽量间隔大点，间隔过小没有实际意义
cindex_fin <- c()
j=1
best_cindex = 0
best_ntree = 0
num_feature = dim(x.train)[2]
for(ntree in seq(500,2500,500)){   #j指的是随机森林的数量
  print("ntree")
  print(ntree)
  cindex_rsf_cross <- c()
  for (i in 1:k){
    print(i)
    train <- x.train[-cvlist[[i]],]  #刚才通过cvgroup生成的函数
    test <- x.train[cvlist[[i]],]
    model <-rfsrc(formula=as.formula(Surv(time,status)~.),data=as.data.frame(train[,c(2:num_feature)]),
			ntree=ntree,seed=1234,nodesize=3,splitrule = "logrank",tree.err=TRUE)
    pred <- predict(model,as.data.frame(test[,c(2:num_feature)]))   #预测
    cindex_rsf_cross[i] <- 1 - pred$err.rate[ntree]
    print(cindex_rsf_cross[i])
    }
  cindex_fin[j] = (cindex_rsf_cross[1]+cindex_rsf_cross[2]+cindex_rsf_cross[3]+cindex_rsf_cross[4]+cindex_rsf_cross[5])/5
  if (cindex_fin[j] > best_cindex) {
    best_cindex = cindex_fin[j]
    best_ntree = ntree
    }
  print("cindex_fin")
  print(cindex_fin[j])
  print("best_ntree_now")
  print(best_ntree)
  j = j+1
  }
xx <- as.data.frame(x.train)
num_feature = dim(x.train)[2]
model_rsf <- rfsrc(formula=as.formula(Surv(time,status)~.),
		data=as.data.frame(xx[2:num_feature]),ntree = best_ntree,
		seed=1234,nodesize=3,splitrule = "logrank",tree.err=TRUE)
pred <- predict(model_rsf,as.data.frame(x.test[,c(2:num_feature)]))
cindex_rsf <- 1 - pred$err.rate[best_ntree]


### 函数输出 参数、模型、预测值、cindex、1年auc、3年auc、5年auc

result <- list()
result$best_param = best_ntree
#result$model = model_rsf
result$pred = pred$predicted
pred_rsf = pred$predicted
result$cindex = cindex_rsf


cutoff = 365*1
if ( min(x.test$time) < cutoff )
{
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_rsf, predict.time = cutoff,lambda=0.01,method = "NNE")
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
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_rsf, predict.time = cutoff,lambda=0.01,method = "NNE")
result$fp_3 = y$FP
result$tp_3 = y$TP
result$auc_3 = y$AUC

cutoff=365*5
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_rsf, predict.time = cutoff,lambda=0.01,method = "NNE")
result$fp_5 = y$FP
result$tp_5 = y$TP
result$auc_5 = y$AUC

cutoff=365*10
y <- survivalROC(Stime = t.test, status = s.test, marker = pred_rsf, predict.time = cutoff,lambda=0.01,method = "NNE")
result$fp_10 = y$FP
result$tp_10 = y$TP
result$auc_10 = y$AUC

### another AUC


cutoff = 365*1
if ( min(x.test$time) < cutoff )
{
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_rsf, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
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
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_rsf, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
result$fp_span_3 = y_span$FP
result$tp_span_3 = y_span$TP
result$auc_span_3 = y_span$AUC

cutoff=365*5
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_rsf, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
result$fp_span_5 = y_span$FP
result$tp_span_5 = y_span$TP
result$auc_span_5 = y_span$AUC

cutoff=365*10
y_span <- survivalROC(Stime = t.test, status = s.test, marker = pred_rsf, predict.time = cutoff,method = "NNE",span = 0.25*NROW(s.test)^(-0.2))
result$fp_span_10 = y_span$FP
result$tp_span_10 = y_span$TP
result$auc_span_10 = y_span$AUC

save("result", file = sprintf("%d_rsf_result.RData", fold))

}
