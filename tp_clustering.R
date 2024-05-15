# demianenko veronique, emilie caillerie

rm(list = ls())

library(stats)
library(graphics)
library(cluster)
library(fpc)
library(class)
library(caret)


library(cluster.datasets)

data(mammal.dentition)
# View(mammal.dentition)

# On applique différentes méthodes de clustering : k-means, clustering hiérarchique, DBSCAN


kmoy <- function(data){
  plot(data, col=6)
  title('Distribution initiale')
  
  n <- dim(data)[1]
  K = 2:10;
  J<- matrix(0,length(K),1);
  JJ<- matrix(0,length(K),1);
  for (k in K){
    cl <- kmeans(data,k)
    plot(data, col=cl$cluster)
    
    title(paste("Apres le k-means pour k =", k))
    #points(cl$centers, col = 'yellow', pch = 8, cex=2)
    
    J[k-1] <- 1/n * cl$tot.withinss
    xx <- data - cl$center[cl$cluster]
    JJ[k-1] <- 1/n * sum(xx * xx)}}


hierarchique <- function(data){
  md <- dist(data)
  
  hh <- hclust(md,'complete')
  plot(hh)
  
  hh <- hclust(md,'ward')
  plot(hh)}


hierarchique_methodes <- function(data){
  plot(data, col = 6)
  title('Distribution initiale')
  
  n <- dim(data)[1]
  md <- dist(data)
  hclust_methods <- c("ward", "single", "complete", "average", "mcquitty", 
                      "median", "centroid")
  for(i in seq_along(hclust_methods)) {
    hh <- hclust(md, method = hclust_methods[i])   
    plot(hh)
  }
}


dbscan <- function(data,e){
  db_x <- fpc::dbscan(data,eps=e,MinPts=3)
  db_x
  db_x$cluster
  plot(data, col = db_x$cluster + 1)
  title("DBSCAN pour eps = ",e)
}

data<-mammal.dentition[,-1]

kmoy(data)
hierarchique(data)
hierarchique_methodes(data)
























##  Question 2 sur les données réelles
View(iris)

summary(iris)

irisbis <- iris[,-ncol(iris)]
colMeans(irisbis,na.rm=TRUE) # Moyenne de chaque colonne
sapply(irisbis,function(col)sd(col,na.rm=TRUE)) # Écart-type de chaque colonne 
sapply(irisbis,function(col)var(col,na.rm=TRUE)) # Variance de chaque colonne 
for (col in colnames(irisbis)){
  boxplot(irisbis[[col]], main=paste("Boxplot de",col)) }

# Méthode de K-moyennes
kmoy(irisbis)

# Méthode clustering hiérarchique


hierarchique(irisbis)

# on prend moins de données sinon illisible
set.seed(1234)
indices_aleatoires <- sample(nrow(irisbis), 20)
irisrandom <- iris[indices_aleatoires, ]

hierarchique(irisrandom)



hierarchique_methodes(irisrandom)

# Méthode DBSCAN


dbscan(irisbis,0.9)


## Question 2 sur les données centrées-réduites
iriscr <- scale(irisbis)
iriscr <- as.data.frame(iriscr)
View(iriscr)

kmoy(iriscr)

irisrandomcr <- iriscr[indices_aleatoires, ]
hierarchique_methodes(irisrandomcr)

dbscan(iriscr,1)


## Question 3
# On va créer donner train et test
set.seed(1234)
n <- nrow(iris)
I <- sample(1:n,(2*n)/3)
J <- setdiff(1:n,I)

# on prépare les données : on construit le classifieur K-NN
# pour les valeurs numérique et on extrait explicitement la classe 
# à predire

cl <- iris[I,5]

dtrain <- iris[I,1:4]
dtest <- iris[J,1:4]

methodeknn <- function(data,train,test, cl, ind){
  mknn <- knn(train, test, cl, k=ind)
  mknn
  table(mknn, data[J,5])}

methodeknn(iris, dtrain, dtest, cl, 1)
methodeknn(iris, dtrain, dtest, cl, 2)
methodeknn(iris, dtrain, dtest, cl, 3)
methodeknn(iris, dtrain, dtest, cl, 9)

# validation croisée
train <- iris[,1:4]
cl <- iris[,5]
model <- knn.cv(train,cl,k=9)
model

# Matrice de confution
library(caret)
confusionMatrix(cl,model)






