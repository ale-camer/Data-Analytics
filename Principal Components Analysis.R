data("Seatbelts")
str(Seatbelts)
dim(Seatbelts)

library(corrplot)
library(psych)

r = cor(Seatbelts, method = "pearson")
corrplot(r, type = "lower", sig.level = 0.05, method = "number")
cor_test = cortest.bartlett(r, n = nrow(Seatbelts))
ifelse(cor_test$p.value <= 0.05, "There are principal components in this matrix", "There aren't principal components in this matrix")

Seatbelts_pca = princomp(Seatbelts, cor = T, scores = T)
pca_sum = summary(Seatbelts_pca)
var_prop = Seatbelts_pca$sdev^2 / sum(Seatbelts_pca$sdev^2)
var_df = data.frame(Dimensions = 1:length(var_prop), varExp = var_prop)
cum_var_prop = cumsum(var_prop)
cum_var_df = data.frame(Dimensions = 1:length(var_prop), varExp = cum_var_prop)

draft = cum_var_df %>% subset(varExp <= 80 & max(Dimensions)) %>% tail(n = 1) # dimensiones to retain
ndim = as.numeric(draft[1,1])

library(ggplot2)
library(paran)
library(factoextra)

ggplot(var_df, aes(x = Dimensions, y = var_prop)) + geom_col(fill = "steelblue") + geom_line(aes(y = cum_var_prop)) + scale_x_continuous(breaks = 1:nrow(var_df)) +
  geom_hline(yintercept = 80, color = "red") + geom_point(aes(y = cum_var_prop, size = var_prop, fill = "steelblue"))

eigen_val = paran(Seatbelts, iterations = 1000, graph = T)
ndim2 = eigen_val$Retained

fa_met = fa.parallel(r, fm = "ml", n.obs = nrow(Seatbelts))
ndim3 = fa_met$ncomp

scores = data.frame(Seatbelts_pca$scores)
rownames(scores) = rownames(Seatbelts)

library(NbClust)

datak = kmeans(scores, ndim)

biplot(Seatbelts_pca)

fviz_pca_biplot(Seatbelts_pca, geom.ind = "point", col.ind = datak$cluster, palette = c("#00AFBB", "#E7B800", "#FC4E07"))