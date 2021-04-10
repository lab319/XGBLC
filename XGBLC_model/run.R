
library(rbsurv)
library(survival)
library(survivalROC)
library(Hmisc)
library(xgboost)
library(Matrix)


## 读取数据集
clinical <- read.table(file="TCGA clinical data.txt",T) ## 读取临床数据
clinical <- data.frame(clinical)
dim(clinical)		#497*3
head(clinical)
time <- clinical$time
status <- clinical$status
RNA <- read.table(file="TCGA RNA data.txt",T)  ## 读取RNA-Seq基因表达数据
RNA <- data.frame(RNA,stringsAsFactors=TRUE)
dim(RNA)		#497*16321
RNA1 <- cbind(RNA,time,status)	
x <- RNA1

### 5折交叉验证划分训练集和测试集，生存和死亡样本比例保持一致
### for循环5次做不同折数
###
for (fold in c(1,2,3,4,5)){

  source("ratio_sample.R")
  cross_va_sample(x,1234,fold)

  x.train <- read.table(file=sprintf("x.train_%d.txt",fold),T)
  x.test <- read.table(file=sprintf("x.test_%d.txt",fold),T)
  t.train <- x.train$time
  t.test <- x.test$time
  s.train <- x.train$status
  s.test <- x.test$status

  nthreads <- 8

### 模型调参 同采用5折交叉验证

### XGB

  num_feature <- dim(x.train)[2]
  #head(x.train[1:5,(num_feature-2):num_feature])

  x.train.xgb <- data.matrix(x.train) # 将自变量转化为矩阵
  dtrain<-list(data=x.train.xgb[,c(2:(num_feature-2))],label=x.train.xgb[,(num_feature-1)]*(-(-1)^(as.numeric(x.train.xgb[,num_feature]))))	#time*（-status） 为删失数据是为负数
  Dtrain<-xgb.DMatrix(dtrain$data,label=dtrain$label)
  x.test.xgb <- data.matrix(x.test) # 将自变量转化为矩阵
  dtest<-list(data=x.test.xgb[,c(2:(num_feature-2))],label=x.test.xgb[,(num_feature-1)]*(-(-1)^(as.numeric(x.test.xgb[,num_feature]))))	#time
  Dtest<-xgb.DMatrix(dtest$data,label=dtest$label)

  best_param = list()
  best_loss = Inf
  best_loss_index = 0
  seed.number = 4321
  watchlist = list(train = Dtrain, test = Dtest)

  source("xgblc_1.R")
  xgblc_param_1(best_param,best_loss,best_lost_index,seed.number,watchlist,fold, nthreads)

  ### 调参lambda2
  load(file = sprintf("%d_xgblc_1_result.RData",fold))
 ## 
 #  
 #  CVgroup <- function(k,datasize,seed){
 #    cvlist <- list()
 #    set.seed(seed)
 #    n <- rep(1:k,ceiling(datasize/k))[1:datasize]    #将数据分成K份，并生成的完成数据集n
 #    temp <- sample(n,datasize)   #把n打乱
 #    x <- 1:k
 #    dataseq <- 1:datasize
 #    cvlist <- lapply(x,function(x) dataseq[temp==x])  #dataseq中随机生成k个随机有序数据列
 #    return(cvlist)
 #  }
 #  
 #  k <- 5
 #  datasize <- nrow(x.train)
 #  cvlist <- CVgroup(k = k,datasize = nrow(x.train),seed = 1234)
 #  x.train <- x.train[-cvlist[[i]],]  
 #  x.test <- x.train[cvlist[[i]],]
 #  
 #  
 #   num_feature <- dim(x.train)[2]
 #    #head(x.train[1:5,(num_feature-2):num_feature])
 #  
 #    x.train.xgb <- data.matrix(x.train) # 将自变量转化为矩阵
 #   dtrain<-list(data=x.train.xgb[,c(2:(num_feature-2))],label=x.train.xgb[,(num_feature-1)]*(-(-1)^(as.numeric(x.train.xgb[,num_feature]))))	#time*（-status） 为删失数据是为负数
 #    Dtrain<-xgb.DMatrix(dtrain$data,label=dtrain$label)
 #    x.test.xgb <- data.matrix(x.test) # 将自变量转化为矩阵
 #    dtest<-list(data=x.test.xgb[,c(2:(num_feature-2))],label=x.test.xgb[,(num_feature-1)]*(-(-1)^(as.numeric(x.test.xgb[,num_feature]))))	#time
 #    Dtest<-xgb.DMatrix(dtest$data,label=dtest$label)
 ##
  source("xgblc_2.R")
  xgblc_param_2(x.train.xgb, result$best_param, result$nround, fold, nthreads)
  }
}

