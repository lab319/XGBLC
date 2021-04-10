preprocess <- function(RNA){

#######	去除缺失值多的样本（删除列）
#rm(list=ls())

bililie <- c()
for (i in 1:dim(RNA)[2]){
	num0 = 0
	for (j in 1:dim(RNA)[1]){
		if (RNA[j,i] == 0){
			num0 <- num0+1
		}
	}
	bililie[i] <- num0/dim(RNA)[1]
}

bilie <- which(bililie > 0.2)
f <- RNA[,-bilie]

contrast <- integer(0)
if (setequal(bilie,contrast)){
write.table(RNA,file="lie.txt",row.names=FALSE,col.names=TRUE,append=FALSE,sep="\t")
} else {
write.table(f,file="lie.txt",row.names=FALSE,col.names=TRUE,append=FALSE,sep="\t")
}



############	去除缺失值多的基因（删除行）

rm(list=ls())
RNA <- read.table(file='lie.txt',header=T,sep="\t")   

bilihang <- c()
for (i in 1:dim(RNA)[1]){
	num0 = 0
	for (j in 2:dim(RNA)[2]){
		if (RNA[i,j] == 0){
			num0 <- num0+1
		}
	}
	bilihang[i] <- num0/(dim(RNA)[2]-1)
}

bihang <- which(bilihang > 0.2)
f <- RNA[-bihang,]

write.table(f,file="hanglie.txt",row.names=FALSE,col.names=TRUE,append=FALSE,sep="\t")


##########		归一化log2（X+1）
rm(list=ls())
RNA <- read.table(file='hanglie.txt',header=T,sep="\t")
RNA1 <- c()
RNA1 <- as.data.frame(RNA1)
for (i in 1:dim(RNA)[1]){
	RNA1[i,1] <- RNA[i,1]
	for(j in 2:dim(RNA)[2]){
		RNA1[i,j] <- log2(RNA[i,j]+1)
#print(RNA1[i,j])
}
}
colnames(RNA1) <- colnames(RNA)

write.table(RNA1,file="TCGA RNA log2.txt",row.names=FALSE,col.names=TRUE,append=FALSE,sep="\t")

######
#rm(list=ls())
#RNA <- read.table(file='TCGA RNA log2.txt',header=T,sep="\t")

}







