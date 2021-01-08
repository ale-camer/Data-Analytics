library(rvest)
library(dplyr)
library(tidyr)
library(wpp2019)

# Obtaining, storing and cleaning data

GDP_per_capita = read_html("https://tradingeconomics.com/country-list/gdp-per-capita")
GDP_per_capita_table = GDP_per_capita %>% html_node("table") %>% html_table()
GDP_per_capita_list = GDP_per_capita_table[,1:2]

population = read_html("https://tradingeconomics.com/country-list/population")
population_table = population %>% html_node("table") %>% html_table()
population_list = population_table[,1:2]

df = GDP_per_capita_list %>% left_join(x = population_list, y = GDP_per_capita_list, by = "Country")
colnames(df) = c("Country", "Population", "GDP_per_capita")

apply(is.na(df), 2, sum)

is_na = df %>% filter(is.na(GDP_per_capita))

GDP = read_html("https://tradingeconomics.com/country-list/gdp")
GDP_table = GDP %>% html_node("table") %>% html_table()
GDP_list = GDP_table[,1:2]

apply(is.na(GDP_list), 2, sum)

df2 = df %>% left_join(x = df, y = GDP_list, by = "Country")
colnames(df2) = c("Country", "Population", "GDP_per_capita", "GDP")

df2$GDP_per_capita = replace_na(df2$GDP * 1000000000) / (df2$Population * 1000000)
apply(is.na(df2), 2, sum)
df3 = na.omit(df2)
apply(is.na(df3), 2, sum)
df4 = df3[,-4]
head(df4)

rownames(df4) = df4[,1]
df5 = df4[,-1]
head(df5)

data(e0F)
data(e0M)
life_expec = data.frame(e0F[,2], (e0F[,16]+e0M[,16])/2)
colnames(life_expec) = c("Country", "life_expec")

df6 = df4 %>% left_join(x = df4, y = life_expec, by = "Country")
rownames(df6) = df6[,1]
df7 = df6[,-1]
head(df7)

apply(is.na(df7), 2, sum)

life_expec[249,1] = "United States"
life_expec[212,1] =  "Russia"
life_expec[146,1] =  "Vietnam"
life_expec[121,1] =  "Iran"
life_expec[47,1] =  "Tanzania"
life_expec[134,1] =  "South Korea"
life_expec[187,1] =  "Venezuela"
life_expec[130,1] =  "Taiwan"
life_expec[178,1] =  "Bolivia"
life_expec[207,1] =  "Czech Republic"
life_expec[128,1] =  "Hong Kong"
life_expec[55,1] =  "Republic of the Congo"
life_expec[105,1] =  "Palestine"
life_expec[210,1] =  "Moldova"
life_expec[235,1] =  "Macedonia"
life_expec[74,1] =  "Guinea Bissau"
life_expec[145,1] =  "East Timor"
life_expec[248,1] =  "Swaziland"
life_expec[69,1] =  "Cape Verde"
life_expec[136,1] =  "Brunei"

df6 = df4 %>% left_join(x = df4, y = life_expec, by = "Country")
apply(is.na(df6), 2, sum)
df8 = na.omit(df6)
apply(is.na(df8), 2, sum)
rownames(df8) = df8[,1]
df9 = df8[,-1]
head(df9)

# Clustering

library(NbClust)
library(ggplot2)

data_scale = scale(df9)
data_dist = dist(data_scale, method = "euclidean", diag = T)

data_clust = hclust(data_dist, method = "ward.D2")
res = NbClust(df9, distance = "euclidean", method = "ward.D2")
nclust = max(res$Best.partition)

plot(data_clust, hang = -1, labels = row.names(df9))
rect.hclust(data_clust, k = nclust, border = "red")

datak = kmeans(data_scale, nclust)

ggplot(df9, aes(life_expec, GDP_per_capita, color = datak$cluster)) + geom_point()

ggplot(as.data.frame(data_scale), aes(x=life_expec, y=GDP_per_capita, size = Population, color = datak$cluster)) + geom_point(alpha=0.7) 

ggplot(as.data.frame(data_scale), aes(x=life_expec, y=GDP_per_capita, size = 10, color = datak$cluster)) + geom_text(alpha=0.7, label = rownames(df9)) 

ggplot(as.data.frame(data_scale), aes(x=life_expec, y=GDP_per_capita, size = Population, color = datak$cluster)) + geom_point() +
  scale_fill_gradient(low = 'palegreen', high = 'palegreen4')

ggplot(as.data.frame(data_scale), aes(x=life_expec, y=GDP_per_capita, size = 10, color = datak$cluster)) + geom_text(label = rownames(df9)) +
  scale_fill_gradient(low = 'palegreen', high = 'palegreen4')

a = subset(data_scale[,2:3], rownames(data_scale) == "Argentina")
ggplot(as.data.frame(data_scale), aes(x=life_expec, y=GDP_per_capita, size = 10, color = datak$cluster)) + 
  geom_point(data = as.data.frame(a), color = "red", size = 10) +
  geom_text(label = rownames(df9)) +
  scale_fill_gradient(low = 'palegreen', high = 'palegreen4')