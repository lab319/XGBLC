
rm(list=ls())
gc()

### 数据预处理
source("1 normalize+20precent.R")
RNA <- read.table(file='Stomach Adenocarcinoma RNA-Seq.txt',header=T,sep="\t")   
preprocess(RNA)

source("2 TCGAprocess+clinical.R")
clinical <- read.table(file='Stomach Adenocarcinoma clinic.txt',header=T,sep="\t",quote = "")   #TCGA下载得到的原始临床数据
cli_rna(clinical)






