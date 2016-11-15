#  classifieur euclidien, classifieur bayésien, k-means, kppv

# ici decrip

# AFD et ACP puis analyse facteur discriminant (ACP supprime le bruit, AFD -> selection des variables les plus discriminantes)



phoneme <- read.table("phoneme.data", header=TRUE, sep=",")
library(MASS)

n <- nrow(phoneme)
pho_val <- phoneme[,2:257]
pho_val_g <- phoneme[,2:258]
pho_val_g_num <- abind(pho_val, as.numeric(pho_val_g[,257]), along=2)

pho_app <- pho_val_g_num[1:3006,]
pho_test <- pho_val_g_num[3007:n,]
pho_app <- as.data.frame(pho_app)
pho_test <- as.data.frame(pho_test)
ntst <- nrow(pho_test)

# regression linéaire

lda.pho<- lda(V257~. , data=pho_app)
pred.pho<-predict(lda.pho , newdata=pho_test)
perf_lda <-table(pho_test$V257, pred.pho$class)
1-sum(diag(perf))/ntst # 8%

# kppv 
pho_kmean_g <- kmeans(as.matrix(pho_val_g_num), 5, 10)
class_g <- pho_kmean_g$cluster
perf_kmean_g <- table(as.data.frame(pho_val_g_num)$V257, class_g)
1-sum(diag(perf_kmean_g))/n # 40% pour 6 17% pour 5 mais sinon des pourcentages très élevés souvent autour de 85%

#sans la dernière colonne
pho_kmean <- kmeans(as.matrix(pho_val), 5, 100)
class <- pho_kmean$cluster
perf_kmean <- table(as.data.frame(pho_val_g_num)$V257, class)
1-sum(diag(perf_kmean))/n 

# en utilisant les données après l'ACP


# ACP avec ou sans la dernière colonne ??
pho_acp <- princomp(pho_val_g_num)
biplot(pho_acp) # 3 groupes

pho_acp_scores <- pho_acp$scores
pho_acp_app <- pho_acp_scores[1:3006,]
pho_acp_test <- pho_acp_scores[3007:n,]
pho_acp_app <- as.data.frame(pho_acp_app)
pho_acp_test <- as.data.frame(pho_acp_test)

#regression linéaire -> 

lda.pho_acp<- lda(Comp.257~. , data=pho_acp_app) # Error in lda.default(x, grouping, ...) : 
pred.pho_acp<-predict(lda.pho_acp , newdata=pho_acp_test)
perf_lda_acp <-table(pho_test_acp$V257, pred.pho_acp$class)
1-sum(diag(perf_lda_acp))/ntst


#kmean
pho_acp_kmean_g <- kmeans(as.matrix(pho_acp_scores), 5, 10)
class_acp_g <- pho_acp_kmean_g$cluster
perf_kmean_acp <- table(as.data.frame(pho_val_g_num)$V257, class_acp)
1-sum(diag(perf_kmean_acp))/n

# sans la dernière colonne pour l'ACP
pho_acp_kmean <- princomp(pho_val)
pho_acp_kmean_scores <- pho_acp_kmean$scores 
pho_acp_kmean <- kmeans(as.matrix(pho_acp_kmean_scores), 5, 10)
class_acp <- pho_acp_kmean$cluster
perf_kmean_acp <- table(as.data.frame(pho_val_g_num)$V257, class_acp)
1-sum(diag(perf_kmean_acp))/n

