install.packages("dplyr")
library("dplyr")
install.package("mice")
library("mice")
library("VIM")
install.packages("corrgram")
library("corrgram")
library("mice")
library("fastcluster")
library("cluster")
library("NbClust")
library("ggplot2")
install.packages("ggdendro")
library("ggdendro")
install.packages("ape")
library("ape")

?mice

Beer <- read.csv("BrewdogNew.csv", header=TRUE)
Beer
md.pattern(Beer, plot = TRUE, rotate.names = TRUE)
matrixplot("Beer")
md.pattern(Beer, plot = TRUE, rotate.names = FALSE)
agg<-aggr(Beer, numbers=TRUE, prop=F,cex.axis = .5, sortVars=T)
?aggr
BeerImputSimp <- Beer
BeerImputSimp$EBC[is.na(BeerImputSimp$EBC)] <- mean(Beer$EBC , na.rm = TRUE)
hist(BeerImputSimp$EBC)
hist(Beer$EBC)
mean(BeerImputSimp$EBC)
mean(Beer$EBC , na.rm = TRUE)
var(Beer$EBC , na.rm = TRUE)
var(BeerImputSimp$EBC)
var.test(BeerImputSimp$EBC, Beer$EBC, alternative = "two.sided")
EBCFRAME<- as.matrix(Beer$EBC)
ks.test(Beer$EBC,BeerImputSimp$EBC)

miceEBC <- mice(EBCCSV, m=20, maxit=20)
miceEBC$imp
miceEBC2 <- complete(impabv)
mean(miceEBC$EBC)
write.csv(miceEBC2,"C:\\Users\\mojim\\OneDrive - University of Leeds\\Business analytics and decision scinece-Moji-Surface\\Assignment\\Question 2\\IMPUTEDebctest.csv", row.names = TRUE)

#Question2#
ABVCSV <- read.csv("ABV-CSV.csv")
corrgram(ABVCSV)
EBCCSV <- read.csv("EBC-CSV.csv")
corrgram(EBCCSV)
ABVCSV

#mice(ABVCSV, m=10)#
ABVCSV2 <- ABVCSV
impabv <- mice(ABVCSV2, m=20, maxit=100)
impabv$imp
ABVCSV2COM <- complete(impabv)
hist(ABVCSV2COM$ABV)
hist(ABVCSV2$ABV)
var.test(ABVCSV2COM$ABV, ABVCSV2$ABV, alternative = "two.sided")
summary(ABVCSV2COM$ABV)
summary(ABVCSV2$ABV)
t.test(ABVCSV2$ABV, ABVCSV2COM$ABV, alternative = "two.sided", var.equal = FALSE)
var.test(ABVCSV2$ABV, ABVCSV2COM$ABV, alternative = "two.sided")
write.csv(ABVCSV2COM,"C:\\Users\\mojim\\OneDrive - University of Leeds\\Business analytics and decision scinece-Moji-Surface\\Assignment\\Question 2\\IMPUTED.csv", row.names = TRUE)
?mice

ks.test(ABVCSV2COM$ABV,ABVCSV2$ABV)
#clustering#

CLUSTCSV <- read.csv("CLUSTCSV.CSV", header=T, stringsAsFactors=TRUE)
CLUSTCSV$Yeast <- as.factor(CLUSTCSV$Yeast)
CLUSTCSV$FermentationTempCelsius <- as.numeric(CLUSTCSV$FermentationTempCelsius)
str(CLUSTCSV)
dist <- daisy(CLUSTCSV[2:9], metric = "gower")
clust <- agnes(dist, diss = TRUE, method="ward")
clust <- hclust(dist, "ward.D2")
plot(clust, labels=CLUSTCSV$ï..Name)
plot(clust, hang = -2, labels=CLUSTCSV$ï..Name, cex=0.5, which.plot = 2)
rect.hclust(clust, 2)
str(dist)
?agnes
?rotate
#test from blog
#to test: aggl.clust.c <- hclust(dist, method = "complete")
#to test: plot(aggl.clust.c, main = "Divisive, complete linkages", labels = CLUSTCSV$ï..Name, cex=0.3, which.plot = 2)

resfrey <- NbClust(data=NULL, diss=dist, distance=NULL, min.nc=2, 
                   max.nc=50, method="ward.D2", index="frey")
resmcclain <- NbClust(data=NULL, diss=dist, distance=NULL, min.nc=2, 
                      max.nc=50, method="ward.D2", index="mcclain")
rescindex <- NbClust(data=NULL, diss=dist, distance=NULL, min.nc=2,
                     max.nc=50, method="ward.D2", index="cindex")
ressilhouette <- NbClust(data=NULL, diss=dist, distance=NULL, min.nc=2, 
                         max.nc=50, method="ward.D2", index="silhouette")
resdunn <- NbClust(data=NULL, diss=dist, distance=NULL, min.nc=2,
                   max.nc=50, method="ward.D2", index="dunn")

clustfin <- cbind(resfrey$Best.partition, resmcclain$Best.partitio, 
                  rescindex$Best.partitio, ressilhouette$Best.partitio, resdunn$Best.partitio)
write.csv(clustfin,"C:\\Users\\mojim\\OneDrive - University of Leeds\\Business analytics and decision scinece-Moji-Surface\\Assignment\\Question 2\\CLUSTFIN.csv", row.names = TRUE)
?gower

#NbClust(data = NULL, diss = diss_matrix, distance = "NULL", min.nc = 2, max.nc = 10, method = "complete", index = "alllong")
?NbClust
ggdendrogram(clust, labels = FALSE, rotate = TRUE, theme_dendro = FALSE)

#test dendo

install.packages("dendextend")
install.packages("circlize")
library(dendextend)
library(circlize)

# create a dendrogram

dend <- as.dendrogram(clust)
clust
# modify the dendrogram to have some colors in the branches and labels
dend <- dend %>% 
  color_branches(k=2) %>% 
  color_labels(k=2) %>%


# plot the radial plot
par(mar = rep(0,4))
# circlize_dendrogram(dend, dend_track_height = 0.8) 
circlize_dendrogram(dend, labels_track_height = 0.2, dend_track_height = 0.5,labels_cex = 0.1, labels=T)
?circlize_dendrogram
clust
order


#fviz
install.packages("factoextra")
library(factoextra)
fviz_nbclust(CLUSTCSV[2:9], hcut, method = "silhouette") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
?fviz_nbclust


#atest for reducing teh features

CLUSTCSV <- read.csv("CLUSTCSV.CSV", header=T, stringsAsFactors=TRUE)
CLUSTCSV$Yeast <- as.factor(CLUSTCSV$Yeast)
CLUSTCSV$FermentationTempCelsius <- as.numeric(CLUSTCSV$FermentationTempCelsius)
str(CLUSTCSV)
dist2 <- daisy(CLUSTCSV, metric = "gower")
clust2 <- agnes(dist, diss = TRUE, method="ward")
clust2 <- hclust(dist, "ward.D2")
plot(clust2, labels=CLUSTCSV$ï..Name)
plot(clust2, hang = -2, labels=CLUSTCSV$ï..Name, cex=0.5, which.plot = 2)
rect.hclust(clust2, 2)
str(dist)



?agnes

#test from blog
#to test: aggl.clust.c <- hclust(dist, method = "complete")
#to test: plot(aggl.clust.c, main = "Divisive, complete linkages", labels = CLUSTCSV$ï..Name, cex=0.3, which.plot = 2)

resfrey2 <- NbClust(data=NULL, diss=dist2, distance=NULL, min.nc=2, 
                   max.nc=50, method="ward.D2", index="frey")
resmcclain2 <- NbClust(data=NULL, diss=dist2, distance=NULL, min.nc=2, 
                      max.nc=50, method="ward.D2", index="mcclain")
rescindex2 <- NbClust(data=NULL, diss=dist2, distance=NULL, min.nc=2,
                     max.nc=50, method="ward.D2", index="cindex")
ressilhouette2 <- NbClust(data=NULL, diss=dist2, distance=NULL, min.nc=2, 
                         max.nc=50, method="ward.D2", index="silhouette")
resdunn2 <- NbClust(data=NULL, diss=dist2, distance=NULL, min.nc=2,
                   max.nc=50, method="ward.D2", index="dunn")

clustfin2 <- cbind(resfrey2$Best.partition, resmcclain2$Best.partitio, 
                  rescindex2$Best.partitio, ressilhouette2$Best.partitio, resdunn2$Best.partitio)
write.csv(clustfin,"C:\\Users\\mojim\\OneDrive - University of Leeds\\Business analytics and decision scinece-Moji-Surface\\Assignment\\Question 2\\CLUSTFIN.csv", row.names = TRUE)
