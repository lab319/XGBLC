
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

for (fold in c(1,2,3,4,5)){

  source("ratio_sample.R")
  cross_va_sample(x,1234,fold)
}