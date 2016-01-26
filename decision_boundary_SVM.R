## To do run scripts for this.

## double circle
dc<-read.table("double_circle.txt",header=T) #get this dataset.
par(mfrow=c(2,2))
plot(dc[,-3],col=c(rep('blue',300),rep('red',300)),xlim=c(-4,4),ylim=c(-4,4),pch=19,cex=2)
plot(c(),type='n',xlim=c(-4,4),ylim=c(-4,4),axes=F,xlab='',ylab='')
zdc<-rep(0,600)
for (i in 1:600){
  +     for (j in 1:600){
    +         zdc[i]<-zdc[i]+exp(-((dc[i,1]-dc[j,1])^2+(dc[i,2]-dc[j,2])^2))
    +     }
  + }
library(scatterplot3d)
scatterplot3d(dc$x,dc$y,zdc,color=c(rep('blue',300),rep('red',300)),pch=19,cex.symbols=1.5)
scatterplot3d(dc$x,dc$y,zdc,color=c(rep('blue',300),rep('red',300)),pch=19,cex.symbols=1.5,angle=0)


### Prection plot on double circle
par(mfrow)
library(e1071) # similar to LIBSVM
dc$label<-as.factor(dc$label)
dc.svm<-svm(label~.,dc)
px<-seq(-4,4,0.03)
py<-seq(-4,4,0.03)
pgrid<-expand.grid(px,py)
names(pgrid)<-names(dc)[-3]
plot(dc[,-3],col=c(rep('blue',300),rep('red',300)),xlim=c(-4,4),ylim=c(-4,4),pch=19,cex=2)
par(new=T)
contour(px,py,array(predict(dc.svm,newdata=pgrid),dim = c(length(px),length(py))),col='purple',lwd=5,levels=0.5,drawlabels=F,xlim=c(-4,4),ylim=c(-4,4))

#XOR Patterns
xorc<-read.table("xor_complex.txt",header=T)
xors$label<-as.factor(xors$label-1)
xorc$label<-as.factor(xorc$label-1)
xors.svm<-svm(label~.,xors)
xorc.svm<-svm(label~.,xorc)
par(mfrow=c(1,2))
# Plot for simple XOR
plot(c(),type='n',xlim=c(-4,4),ylim=c(-4,4))
par(new=T)
rect(0,0,4,4,col='#aaaaff')
par(new=T)
rect(-4,0,0,4,col='#ffaaaa')
par(new=T)
rect(-4,-4,0,0,col='#aaaaff')
par(new=T)
rect(0,-4,4,0,col='#ffaaaa')
par(new=T)
plot(xors[,-3],col=c(rep('blue',50),rep('red',50)),xlim=c(-4,4),ylim=c(-4,4),pch=19,cex=2.5)
par(new=T)
contour(px,py,array(predict(xors.svm,newdata=pgrid),dim = c(length(px),length(py))),levels = 0.5,drawlabels = T,col='purple',lwd=3,xlim=c(-4,4),ylim=c(-4,4))
# Plot for complex XOR
plot(c(),type='n',xlim=c(-4,4),ylim=c(-4,4))
par(new=T)
rect(0,0,4,4,col='#aaaaff')
par(new=T)
rect(-4,0,0,4,col='#ffaaaa')
par(new=T)
rect(-4,-4,0,0,col='#aaaaff')
par(new=T)
rect(0,-4,4,0,col='#ffaaaa')
par(new=T)
plot(xorc[,-3],col=c(rep('blue',50),rep('red',50)),xlim=c(-4,4),ylim=c(-4,4),pch=19,cex=2.5)
par(new=T)
contour(px,py,array(predict(xorc.svm,newdata=pgrid),dim = c(length(px),length(py))),levels = 0.5,drawlabels = T,col='purple',lwd=3,xlim=c(-4,4),ylim=c(-4,4))


xorc.svm2<-svm(label~.,xorc,cost=10,gamma=10)
xorc.svm3<-svm(label~.,xorc,cost=0.5,gamma=(1/2)/10)
# *************Plot scripts omitted


#### Linearly separable


dbi<-read.table("linear_bi.txt",header=T)
dml<-read.table("linear_multi.txt",header=T)
dbi$label<-as.factor(dbi$label)
dml$label<-as.factor(dml$label)
dbi.svm1<-svm(label~.,dbi,kernel='radial')
dbi.svm2<-svm(label~.,dbi,kernel='linear')
px2<-seq(-0.5,4.5,0.1)
py2<-seq(-0.5,4.5,0.1)
pgrid2<-expand.grid(px2,py2)
names(pgrid2)<-names(dbi)[-3]
par(mfrow=c(1,2))
# RBF kernel
plot(c(),type='n',xlim=c(-0.5,3.5),ylim=c(-0.5,3.5))
par(new=T)
polygon(c(-0.5,-0.5,3.5),c(3.5,-0.5,-0.5),col='#aaaaff')
par(new=T)
polygon(c(-0.5,3.5,3.5),c(3.5,-0.5,3.5),col='#ffaaaa')
par(new=T)
plot(dbi[,-3],pch=19,col=c(rep('blue',25),rep('red',25)),cex=3,xlim=c(-0.5,3.5),ylim=c(-0.5,3.5))
par(new=T)
contour(px2,py2,array(predict(dbi.svm1,newdata=pgrid2),dim = c(length(px2),length(py2))),xlim=c(-0.5,3.5),ylim=c(-0.5,3.5),lwd=6,col='purple',levels = 0.5,drawlabels = T)
# Linear kernel
plot(c(),type='n',xlim=c(-0.5,3.5),ylim=c(-0.5,3.5))
par(new=T)
polygon(c(-0.5,-0.5,3.5),c(3.5,-0.5,-0.5),col='#aaaaff')
par(new=T)
polygon(c(-0.5,3.5,3.5),c(3.5,-0.5,3.5),col='#ffaaaa')
par(new=T)
plot(dbi[,-3],pch=19,col=c(rep('blue',25),rep('red',25)),cex=3,xlim=c(-0.5,3.5),ylim=c(-0.5,3.5))
par(new=T)
contour(px2,py2,array(predict(dbi.svm2,newdata=pgrid2),dim = c(length(px2),length(py2))),xlim=c(-0.5,3.5),ylim=c(-0.5,3.5),lwd=6,col='purple',levels = 0.5,drawlabels = T)


## after plot


dml.svm1<-svm(label~.,dml,kernel='radial')
dml.svm2<-svm(label~.,dml,kernel='linear')
par(mfrow=c(1,2))
plot(c(),type='n',xlim=c(-0.5,4.5),ylim=c(-0.5,4.5))
par(new=T)
polygon(c(-0.5,-0.5,3.5),c(3.5,-0.5,-0.5),col='#aaaaff')
par(new=T)
polygon(c(-0.5,3.5,4.5,4.5,1.0,-0.5),c(3.5,-0.5,-0.5,1.0,4.5,4.5),col='#ffaaaa')
par(new=T)
polygon(c(1.0,4.5,4.5),c(4.5,1.0,4.5),col='#ccffcc')
par(new=T)
plot(dml[,-3],pch=19,col=c(rep('blue',25),rep('red',25),rep('green',25)),cex=3,xlim=c(-0.5,4.5),ylim=c(-0.5,4.5))
par(new=T)
contour(px2,py2,array(predict(dml.svm1,newdata=pgrid2),dim=c(length(px2),length(py2))),xlim=c(-0.5,4.5),ylim=c(-0.5,4.5),col="purple",lwd=6,drawlabels=T,levels=c(0.5,1.5))
plot(c(),type='n',xlim=c(-0.5,4.5),ylim=c(-0.5,4.5))
par(new=T)
polygon(c(-0.5,-0.5,3.5),c(3.5,-0.5,-0.5),col='#aaaaff')
par(new=T)
polygon(c(-0.5,3.5,4.5,4.5,1.0,-0.5),c(3.5,-0.5,-0.5,1.0,4.5,4.5),col='#ffaaaa')
par(new=T)
polygon(c(1.0,4.5,4.5),c(4.5,1.0,4.5),col='#ccffcc')
par(new=T)
plot(dml[,-3],pch=19,col=c(rep('blue',25),rep('red',25),rep('green',25)),cex=3,xlim=c(-0.5,4.5),ylim=c(-0.5,4.5))
par(new=T)
contour(px2,py2,array(predict(dml.svm2,newdata=pgrid2),dim=c(length(px2),length(py2))),xlim=c(-0.5,4.5),ylim=c(-0.5,4.5),col="purple",lwd=6,drawlabels=T,levels=c(0.5,1.5))
