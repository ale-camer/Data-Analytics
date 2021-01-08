library(smacof)
library(ggplot2)
library(cluster)

# Multidimensional Scaling (MDS)

data("USArrests")

data_norm = scale(USArrests)
data_dist = dist(data_norm, method = "euclidean", diag = T, upper = T)

data_mat = as.matrix(data_dist)
rownames(data_mat) = paste("Z", 1:50)
colnames(data_mat) = paste("Z", 1:50)
data_dist2 = as.dist(data_mat)

fit = mds(delta = data_dist2, ndim = 2, type = "ratio")
fit$stress

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

grouping = hclust(fit$dhat)
(groups <- cutree(grouping, k=2))
plot(grouping, hang = -1, cex = 0.7, labels = rownames(USArrests))
rect.hclust(grouping, k=2, border="red")

plot(fit,plot.type="stressplot",plot.dim=c(1,2),shpere=T,bubscale=.1,col=1,
     label.conf=list(label=T,pos=3,col=1,cex=.8,shepard.x=NULL,identify=F,type="p",pch=20,asp=1,col.hist=NULL))

dist = cbind(c(fit$dhat))
dism = cbind(c(fit$confdist))
model = summary(lm(dist~dism))
model$r.squared  # model performance

fit2 = mds(delta = data_dist2, ndim = 3, type = "ratio")
fit2$stress

if (fit2$stress >= 0.2) {
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

grouping = hclust(fit2$dhat)
(groups <- cutree(grouping, k=3))
plot(grouping, hang = -1, cex = 0.7, labels = rownames(USArrests))
rect.hclust(grouping, k=3, border="red")

dist2 = cbind(c(fit2$dhat))
dism2 = cbind(c(fit2$confdist))
model2 = summary(lm(dist2~dism2))
model2$r.squared  # model performance

plot(fit2,plot.type="stressplot",plot.dim=c(1,2),shpere=T,bubscale=.1,col=1,
     label.conf=list(label=T,pos=3,col=1,cex=.8,shepard.x=NULL,identify=F,type="p",pch=20,asp=1,col.hist=NULL))

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

# Weighted Multidimensional Scaling (WMDS)

data("volcano")

data1 = data.matrix(volcano)
data2 = data.matrix(volcano)
data = list(dist(data1), dist(data2))
fit = smacofIndDiff(delta = data, type = "ordinal", constraint = "indscal")
fit$cweights

# Classical Multidimensional Scaling Unfolding (CMDSU)

data("attitude")

fit = smacofRect(attitude, itmax = 1000)
plot(fit, joint = T, asp = 0.6)
fit$conf.row
fit$conf.col
