
library(rbsurv)
library(survival)
#library(Biobase)
library(randomForestSRC)
library(survivalROC)
library(Hmisc)
library(xgboost)
library(Matrix)
library(glmnet)
library(survivalsvm)

## ��ȡ���ݼ�
clinical <- read.table(file="TCGA clinical data.txt",T) ## ��ȡ�ٴ�����
clinical <- data.frame(clinical)
dim(clinical)		#497*3
head(clinical)
time <- clinical$time
status <- clinical$status
RNA <- read.table(file="TCGA RNA data.txt",T)  ## ��ȡRNA-Seq�����������
RNA <- data.frame(RNA,stringsAsFactors=TRUE)
dim(RNA)		#497*16321
RNA1 <- cbind(RNA,time,status)	
x <- RNA1

### 5�۽�����֤����ѵ�����Ͳ��Լ������������������������һ��
### forѭ��5������ͬ����
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

### ģ�͵��� ͬ����5�۽�����֤

### XGB

  num_feature <- dim(x.train)[2]
  #head(x.train[1:5,(num_feature-2):num_feature])

  x.train.xgb <- data.matrix(x.train) # ���Ա���ת��Ϊ����
  dtrain<-list(data=x.train.xgb[,c(2:(num_feature-2))],label=x.train.xgb[,(num_feature-1)]*(-(-1)^(as.numeric(x.train.xgb[,num_feature]))))	#time*��-status�� Ϊɾʧ������Ϊ����
  Dtrain<-xgb.DMatrix(dtrain$data,label=dtrain$label)
  x.test.xgb <- data.matrix(x.test) # ���Ա���ת��Ϊ����
  dtest<-list(data=x.test.xgb[,c(2:(num_feature-2))],label=x.test.xgb[,(num_feature-1)]*(-(-1)^(as.numeric(x.test.xgb[,num_feature]))))	#time
  Dtest<-xgb.DMatrix(dtest$data,label=dtest$label)


  best_param = list()
  best_loss = Inf
  best_loss_index = 0
  seed.number = 4321
  watchlist = list(train = Dtrain, test = Dtest)

  source("xgboost.R")
  xgb_param(best_param,best_loss,best_lost_index,seed.number,watchlist,fold, nthreads)


### RSF

  source("rsf.R")
  rsf_param(x.train.xgb,fold)


### EXSA

#source("exsa.R")
#exsa_param(best_param,best_loss,best_lost_index,seed.number,watchlist,fold)

### LASSO-COX
  x.train.cox <- x.train

  x.train.cox$time[x.train.cox$time==0]<-NA		#��NA�滻�հ�ֵ
  x.train.cox <- na.omit(x.train.cox)		#ɾ������NA����

  source("lasso_cox.R")
  lasso_cox(x.train.cox,fold)

### SVM

source("svm.R")
svm_param(x.train,fold)

}













