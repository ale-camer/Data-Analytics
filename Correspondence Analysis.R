library(ca)
library(rgl)

# Simple Correspondence Analysis

data("smoke")

summary(ca(smoke), scree = T, rows = T, columns = T)

fit = ca(smoke)
fit_sum = summary(fit)
fit_sum$scree[,3]

a = fit_sum$scree[,3]
b = seq(1, length(a), 1)
ggplot(data = as.data.frame(cbind(a,b)), aes(x = b, y = a)) + 
  geom_line(linetype="dashed", color="blue", size=1.2)+
  geom_point(color="red", size=3) + geom_smooth(method = "loess", se=FALSE)

plot.ca(fit)
plot3d.ca(fit)

# Multiple Correspondence Analysis

data("wg93")

fit2 = mjca(wg93[,1:4], lambda = "Burt", reti = TRUE)
summary(fit2)

a = fit2$inertia.e
b = seq(1, length(a), 1)
ggplot(data = as.data.frame(cbind(a,b)), aes(x = b, y = a)) + 
  geom_line(linetype="dashed", color="blue", size=1.2)+
  geom_point(color="red", size=3) + geom_smooth(method = "loess", se=FALSE)

plot.mjca(fit2)
