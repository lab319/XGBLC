
rm(list=ls())
gc()

### ����Ԥ����
source("1 normalize+20precent.R")
RNA <- read.table(file='Stomach Adenocarcinoma RNA-Seq.txt',header=T,sep="\t")   
preprocess(RNA)

source("2 TCGAprocess+clinical.R")
clinical <- read.table(file='Stomach Adenocarcinoma clinic.txt',header=T,sep="\t",quote = "")   #TCGA���صõ���ԭʼ�ٴ�����
cli_rna(clinical)





