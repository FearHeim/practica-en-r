library(readr)
xy <- read_delim("E:/DiscoRecuperado/ULEAM/9no/INTELIGENCIA DN/Tarea/xy.csv", 
                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(xy)
colMeans(xy)
colMeans(scale(xy))

dist_xy <- dist(scale(xy))

hc_xy <- hclust(dist_xy, method = "complete")

hc_xy_single <- hclust(dist_xy, method = "single")

hc_xy_average <- hclust(dist_xy, method = "average")

hc <- hclust(dist_xy, method = "ward.D")


# Gráfico de dispersión 
plot(x=xy$X,y=xy$Y)

# Dendrograma 1
plot(hc_xy)
abline(h=2)
rect.hclust(hc_xy, k = 2)

# Dendrograma 2
plot(hc_xy_single)
rect.hclust(hc_xy_single, k = 2)

# Dendrograma 3
plot(hc_xy_average)
rect.hclust(hc_xy_average, k = 2)

# Dendrograma 4
plot(as.dendrogram(hc), main = "ward.D")


clust_xy <- cutree(hc_xy, h=2)
clust_xy
table(clust_xy)

clust_xy_s <- scale(xy)
km_xy <- kmeans(clust_xy_s, centers=2,
                nstart= 20, iter.max = 50)
km_xy
table(km_xy$cluster)

plot(xy, col = km_xy$cluster)
plot(scale(xy[,c(1,2)]), col=km_xy$cluster)
points(km_xy$centers,cex=2, col=13, pch=19)



