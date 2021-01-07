library(smacof)
library(ggplot2)

data("USArrests")

fit = mds(delta = dist(USArrests), type = "ratio")

if (fit$stress >= 0.2) {
  print("Bad Model")
} else if (fit$stress >= 0.1) {
  print("Reasonable Minimum Score")
} else if (fit$stress >= 0.05) {
    print("Good Model")
} else if (fit$stress >= 0.025) {
    print("Excellent Model")
} else {
    print("Perfect Model")
}

ggplot() + 
  geom_text(data = as.data.frame(fit$conf), mapping = aes(x = D1, y = D2, color = "green", alpha = 0.5, label = rownames(USArrests)))  

par(mfrow = c(2,3))
plot(fit,plot.type="Shepard",plot.dim=c(1,2),shpere=T,bubscale=.1,col=1,
     label.conf=list(label=T,pos=3,col=1,cex=.8,shepard.x=NULL,identify=F,type="p",pch=20,asp=1,col.hist=NULL))
plot(fit,plot.type="confplot",plot.dim=c(1,2),shpere=T,bubscale=.1,col=1,
     label.conf=list(label=T,pos=3,col=1,cex=.8,shepard.x=NULL,identify=F,type="p",pch=20,asp=1,col.hist=NULL))
plot(fit,plot.type="resplot",plot.dim=c(1,2),shpere=T,bubscale=.1,col=1,
     label.conf=list(label=T,pos=3,col=1,cex=.8,shepard.x=NULL,identify=F,type="p",pch=20,asp=1,col.hist=NULL))
plot(fit,plot.type="stressplot",plot.dim=c(1,2),shpere=T,bubscale=.1,col=1,
     label.conf=list(label=T,pos=3,col=1,cex=.8,shepard.x=NULL,identify=F,type="p",pch=20,asp=1,col.hist=NULL))
plot(fit,plot.type="bubbleplot",plot.dim=c(1,2),shpere=T,bubscale=.1,col=1,
     label.conf=list(label=T,pos=3,col=1,cex=.8,shepard.x=NULL,identify=F,type="p",pch=20,asp=1,col.hist=NULL))

ggplot() + 
  geom_point(data = as.data.frame(fit$conf) , mapping = aes(x = -D1 , y = -D2), alpha = 0.5 , color = "blue", size = 10 ) + 
  geom_text(data = as.data.frame(fit$conf), mapping = aes(x = -D1,y= -D2), label = rownames(USArrests)) 
