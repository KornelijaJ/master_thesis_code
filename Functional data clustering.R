
library(funFEM)
####################### DATA USAGE ############################
set.seed(123456)
gb_clustering_5_DkBk <- funFEM(gb_fda$fd,K=5,model="DkBk",init="kmeans",lambda=0,disp=TRUE,graph=FALSE)
gb_clustering_l2_5 <- fda.usc::kmeans.fd(gb_fda$fd,ncl=5,metric=metric.lp)

gb_clustering_4_DkBk <- funFEM(gb_fda$fd,K=4,model="DkBk",init="kmeans",lambda=0,disp=TRUE,graph=FALSE)
gb_clustering_l2_4 <- fda.usc::kmeans.fd(gb_fda$fd,ncl=4,metric=metric.lp)

table(gb_clustering_l2_5$cluster)

table(gb_clustering_l2_4$cluster)

#funFEM grafikai:
par(mfrow=c(1,2))
plot_col = ifelse(gb_clustering_5_DkBk$cls == 3, "purple",
                  ifelse(gb_clustering_5_DkBk$cls == 2, "green",
                         ifelse(gb_clustering_5_DkBk$cls == 4, "orange",
                                ifelse(gb_clustering_5_DkBk$cls == 5, "blue","brown"))))
plot(gb_fda$fd);
lines(gb_fda$fd,col=plot_col,lwd=2,lty=1)
lines(mean.fd(gb_fda$fd[as.vector(which(gb_clustering_5_DkBk$cls == 3))]),col="#886eaf",lwd=2,lty=1)
lines(mean.fd(gb_fda$fd[as.vector(which(gb_clustering_5_DkBk$cls == 2))]),col="#006400",lwd=2,lty=1)
lines(mean.fd(gb_fda$fd[as.vector(which(gb_clustering_5_DkBk$cls == 4))]),col="#ab6e00",lwd=2,lty=1)
lines(mean.fd(gb_fda$fd[as.vector(which(gb_clustering_5_DkBk$cls == 5))]),col="#040461",lwd=2,lty=1)
lines(mean.fd(gb_fda$fd[as.vector(which(gb_clustering_5_DkBk$cls == 1))]),col="#4c332e",lwd=2,lty=1)

plot(gb_fda$fd, col="grey")
lines(mean.fd(gb_fda$fd[as.vector(which(gb_clustering_5_DkBk$cls == 3))]),col="#886eaf",lwd=2,lty=1)
lines(mean.fd(gb_fda$fd[as.vector(which(gb_clustering_5_DkBk$cls == 2))]),col="#006400",lwd=2,lty=1)
lines(mean.fd(gb_fda$fd[as.vector(which(gb_clustering_5_DkBk$cls == 4))]),col="#ab6e00",lwd=2,lty=1)
lines(mean.fd(gb_fda$fd[as.vector(which(gb_clustering_5_DkBk$cls == 5))]),col="#040461",lwd=2,lty=1)
lines(mean.fd(gb_fda$fd[as.vector(which(gb_clustering_5_DkBk$cls == 1))]),col="#4c332e",lwd=2,lty=1)

####################### VOICE MINUTES ############################
set.seed(123456)
voice_clustering_5_DkBk <- funFEM(voice_fda$fd,K=5,model="DkBk",init="kmeans",lambda=0,disp=TRUE,graph=FALSE)
voice_clustering_l2_5 <- fda.usc::kmeans.fd(voice_fda$fd,ncl=5,metric=metric.lp)

voice_clustering_6_DkBk <- funFEM(voice_fda$fd,K=6,model="DkBk",init="kmeans",lambda=0,disp=TRUE,graph=FALSE)
voice_clustering_l2_6 <- fda.usc::kmeans.fd(voice_fda$fd,ncl=6,metric=metric.lp)

table(voice_clustering_l2_5$cluster)
table(voice_clustering_l1_5$cluster)

#funFEM grafikai:
par(mfrow=c(1,2))
plot_col = ifelse(voice_clustering_6_DkBk$cls == 3, "purple",
                  ifelse(voice_clustering_6_DkBk$cls == 2, "green",
                         ifelse(voice_clustering_6_DkBk$cls == 4, "orange",
                                ifelse(voice_clustering_6_DkBk$cls == 5, "blue",
                                       ifelse(voice_clustering_6_DkBk$cls == 6, "brown", "pink")))))
plot(voice_fda$fd);
lines(voice_fda$fd,col=plot_col,lwd=2,lty=1)
lines(mean.fd(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 3))]),col="#886eaf",lwd=2,lty=1)
lines(mean.fd(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 2))]),col="#006400",lwd=2,lty=1)
lines(mean.fd(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 4))]),col="#ab6e00",lwd=2,lty=1)
lines(mean.fd(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 5))]),col="#040461",lwd=2,lty=1)
lines(mean.fd(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 6))]),col="#8c2645",lwd=2,lty=1)
lines(mean.fd(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 1))]),col="#4c332e",lwd=2,lty=1)

plot(voice_fda$fd, col="grey")
lines(mean.fd(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 3))]),col="#886eaf",lwd=2,lty=1)
lines(mean.fd(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 2))]),col="#006400",lwd=2,lty=1)
lines(mean.fd(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 4))]),col="#ab6e00",lwd=2,lty=1)
lines(mean.fd(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 5))]),col="#040461",lwd=2,lty=1)
lines(mean.fd(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 6))]),col="#8c2645",lwd=2,lty=1)
lines(mean.fd(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 1))]),col="#4c332e",lwd=2,lty=1)

####################### USAGE INDEX ############################

set.seed(123456)
index_clustering_5_DkBk <- funFEM(usage_index_05_fda$fd,K=5,model="DkBk",init="kmeans",lambda=0,disp=TRUE,graph=FALSE)
index_clustering_l2_5 <- fda.usc::kmeans.fd(usage_index_05_fda$fd,ncl=5,metric=metric.lp)
index_clustering_6_DkBk <- funFEM(usage_index_05_fda$fd,K=6,model="DkBk",init="kmeans",lambda=0,disp=TRUE,graph=FALSE)
index_clustering_l2_6 <- fda.usc::kmeans.fd(usage_index_05_fda$fd,ncl=6,metric=metric.lp,cluster.size=0)

#funFEM grafikai:
par(mfrow=c(1,2))
plot_col = ifelse(index_clustering_6_DkBk$cls == 3, "purple",
                  ifelse(index_clustering_6_DkBk$cls == 2, "green",
                         ifelse(index_clustering_6_DkBk$cls == 4, "orange",
                                ifelse(index_clustering_6_DkBk$cls == 5, "blue",
                                       ifelse(index_clustering_6_DkBk$cls == 6, "brown", "pink")))))
plot(usage_index_05_fda$fd);
lines(usage_index_05_fda$fd,col=plot_col,lwd=2,lty=1)
lines(mean.fd(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 3))]),col="#886eaf",lwd=2,lty=1)
lines(mean.fd(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 2))]),col="#006400",lwd=2,lty=1)
lines(mean.fd(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 4))]),col="#ab6e00",lwd=2,lty=1)
lines(mean.fd(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 5))]),col="#040461",lwd=2,lty=1)
lines(mean.fd(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 6))]),col="#8c2645",lwd=2,lty=1)
lines(mean.fd(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 1))]),col="#4c332e",lwd=2,lty=1)

plot(usage_index_05_fda$fd, col="grey")
lines(mean.fd(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 3))]),col="#886eaf",lwd=2,lty=1)
lines(mean.fd(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 2))]),col="#006400",lwd=2,lty=1)
lines(mean.fd(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 4))]),col="#ab6e00",lwd=2,lty=1)
lines(mean.fd(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 5))]),col="#040461",lwd=2,lty=1)
lines(mean.fd(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 6))]),col="#8c2645",lwd=2,lty=1)
lines(mean.fd(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 1))]),col="#4c332e",lwd=2,lty=1)






