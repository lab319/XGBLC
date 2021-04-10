
rm(list=ls())
gc()

cli_rna <- function(clinical){
######################处理临床数据##################################

dim(clinical)   #508*227
names(clinical)
#head(clinical)
clinical <- clinical[,c(1,2,3)] ##分别为 编号、生存时间、生存状态
#clinical <- clinical[,c(1,2,3,7,8,9,36)]    #去掉缺失值很多的列
names(clinical)
dim(clinical)                        #528*3
clinical[clinical==""]<-NA		#用NA替换空白值
dim(clinical)
head(clinical)
clinical <- na.omit(clinical)		#删掉含有NA的行
colnames(clinical)[1] <- "Tags"
colnames(clinical)[2] <- "time"
colnames(clinical)[3] <- "status"


dim(clinical)   #507*3            #只要含有NA，就把这个样本去掉，剩下495
#View(clinical)
head(clinical)

clinical[,1] <- paste(substr(clinical[,1],0,4),substr(clinical[,1],6,7),substr(clinical[,1],9,12),sep=".")

#View(clinical)

#######把临床数据中的因子型变量变成数值型变量
clinical$status
index_Alive=which(as.character(clinical$status)=="Alive")
index_Dead=which(as.character(clinical$status)=="Dead")
clinical$status=NA
clinical$status[index_Alive]=0
clinical$status[index_Dead]=1
clinical$status

#View(clinical)
write.table(clinical,file="clinical.txt",row.names=FALSE,col.names=TRUE,append=FALSE,sep="\t")

#################读取肺腺癌的RNA-Seq分子数据,分子数据和临床数据求交集###############

RNA <- read.table(file='TCGA RNA log2.txt',header=T,sep="\t")
dim(RNA)	#15936*506  行是基因  列是样本
dim(clinical)	#457*3

colnames(RNA)[1] <- "Tags"
head(RNA[1:5,1:5])
head(clinical)

RNA1<-t(RNA)     #行是样本  列是特征
RNA1=as.data.frame(RNA1)
head(RNA1[1:5,1:5])

write.table(RNA1,file="RNA未删癌旁.txt",row.names=TRUE,col.names=FALSE,append=FALSE,sep="\t")
rm(list=ls())
clinical <- read.table(file="clinical.txt",T)
RNA2 <- read.table(file="RNA未删癌旁.txt",T)    #行是样本  列是特征

a = which(substr(RNA2[,1],13,15)==".01")

RNA2 <- RNA2[a,]

RNA2[,1] <- substr(RNA2[,1],0,12)
dim(RNA2)		#462*15937
head(RNA2[1:5,1:5])

#id <- as.character(RNA2$Tags)
#id
#newid=substr(id,1,12)
#newid
#RNA2$Tags <- newid
#RNA2$Tags
#RNA2$Tags <- as.factor(RNA2$Tags)
#RNA2$Tags			#513 levels

#head(RNA2[1:5,1:5])
#dim(RNA2)		#533*24992
head(clinical)
head(RNA2[1:5,1:5])
#dim(data)		#495*6

#########################################

jiaoji <- merge(RNA2,clinical,by = 'Tags')
dim(jiaoji)			#497*16323
head(jiaoji[1:5,1:5])
write.table(jiaoji,file="合并.txt",row.names=FALSE,col.names=TRUE,append=FALSE,sep="\t")
rm(a)
rm(jiaoji)
gc()
f <- read.table(file="合并.txt",T)
g <- t(f)
h <- as.data.frame(g)
dim(h)		#16761*496
head(h[1:5,1:5])
write.table(h,file="合并_行基因列样本.txt",row.names=TRUE,col.names=FALSE,append=FALSE,sep="\t")
rm(list=c("f","g","h"))
m <- read.table(file="合并_行基因列样本.txt",T)
num_feature = dim(m)[1]		#16760*497
num_sample = dim(m)[2]
head(m[1:5,1:5])
#View(m[(num_feature-3):num_feature,1:5])
xcli <- m[c((-1):(-(num_feature-2))),]	#留临床
dim(xcli)               #2*497
head(xcli[,1:5])
xcli2 <- t(xcli)
xcli2 <- as.data.frame(xcli2)
head(xcli2[1:5,])
write.table(xcli2,file="TCGA clinical data.txt",row.names=TRUE,col.names=FALSE,append=FALSE,sep="\t")
#x3 <- read.table(file="TCGA clinical data.txt",T)      #####用于后续分析的临床数据
#dim(x3)       #488*7
#names(x3)
#head(x3[1:5,1:5])

RNA3 <- m[c((-(num_feature-1)):(-num_feature)),]  ##留分子数据
dim(RNA3)			#16758*497
head(RNA3[(num_feature-4):num_feature,1:5])
write.table(RNA3,file="RNA_行基因列样本.txt",row.names=FALSE,col.names=TRUE,append=FALSE,sep="\t")

RNA4 <- read.table(file='RNA_行基因列样本.txt',header=T)
RNA4 <- t(RNA4)
RNA4 = as.data.frame(RNA4)
dim(RNA4)		#347*19754
head(RNA4[1:5,1:5])
class(RNA4)
write.table(RNA4,file="TCGA RNA data.txt",row.names=TRUE,col.names=FALSE,append=FALSE,sep="\t")   #####用于后续分析的分子数据

}


