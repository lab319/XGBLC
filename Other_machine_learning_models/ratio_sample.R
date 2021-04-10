cross_va_sample <- function(x,seed,fold){

xalive <- which(x$status==0)
xdead <- which(x$status==1)

####生存样本

x.alive <- x[xalive,]
n.alive<-nrow(x.alive)	#列的个数346
n.alive	 #346
k.alive <- rep((1:5), 225)[1:n.alive]
k.alive	#1到3重复120次(最终得到564个）

set.seed(seed)
i.alive <- sample(k.alive, size=n.alive, replace = FALSE)	#不放回的在随机在向量k中抽取346个元素
#i.alive
k.alive.test <-(1:n.alive)[i.alive==fold]
k.alive.test			#（测试集有115个,训练集有115*2个）
x.alive.train <- x.alive[(-k.alive.test),]
dim(x.alive.train)                  #训练集 231*19757
x.alive.test <- x.alive[(k.alive.test),]
dim(x.alive.test)                   #验证集 115*19757

a=dim(x)[2]
t.alive.train <- x.alive[(-k.alive.test),a-1]    #时间
t.alive.test <- x.alive[(k.alive.test),a-1]
s.alive.train <- x.alive[(-k.alive.test),a]   #状态
s.alive.test <- x.alive[(k.alive.test),a]

####死亡样本

x.dead <- x[xdead,]
n.dead<-nrow(x.dead)	#列的个数346
n.dead	 #346
k.dead <- rep((1:5), 125)[1:n.dead]
k.dead	#1到3重复120次(最终得到564个）

set.seed(seed)
i.dead <- sample(k.dead, size=n.dead, replace = FALSE)	#不放回的在随机在向量k中抽取346个元素
#i.dead
k.dead.test <-(1:n.dead)[i.dead==fold]
k.dead.test			#（测试集有115个,训练集有115*2个）
x.dead.train <- x.dead[(-k.dead.test),]
dim(x.dead.train)                  #训练集 87*15536
x.dead.test <- x.dead[(k.dead.test),]
dim(x.dead.test)                   #验证集 43*15536


##### 样本 <- 生存：死亡相等

x.train <- rbind(x.alive.train, x.dead.train)
x.train <- as.data.frame(x.train)
x.test <- rbind(x.alive.test, x.dead.test)
x.test <- as.data.frame(x.test)
write.table(x.train,file=sprintf("x.train_%d.txt",fold),row.names=TRUE,col.names=TRUE,append=FALSE,sep="\t")
write.table(x.test,file=sprintf("x.test_%d.txt",fold),row.names=TRUE,col.names=TRUE,append=FALSE,sep="\t")

}






