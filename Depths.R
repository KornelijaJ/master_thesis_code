
library(funFEM)

####################### DATA USAGE ############################
set.seed(123456)
# gb_clustering_l2_4$cluster -> gb_clustering_4_DkBk$cls
depth_FM_gb_4_1 <- depth.FM(fdata(gb_fda$fd[as.vector(which(gb_clustering_4_DkBk$cls == 1))]))$median
depth_FM_gb_4_2 <- depth.FM(fdata(gb_fda$fd[as.vector(which(gb_clustering_4_DkBk$cls == 2))]))$median
depth_FM_gb_4_3 <- depth.FM(fdata(gb_fda$fd[as.vector(which(gb_clustering_4_DkBk$cls == 3))]))$median
depth_FM_gb_4_4 <- depth.FM(fdata(gb_fda$fd[as.vector(which(gb_clustering_4_DkBk$cls == 4))]))$median

depth_mode_gb_4_1 <- depth.mode(fdata(gb_fda$fd[as.vector(which(gb_clustering_4_DkBk$cls == 1))]))$median
depth_mode_gb_4_2 <- depth.mode(fdata(gb_fda$fd[as.vector(which(gb_clustering_4_DkBk$cls == 2))]))$median
depth_mode_gb_4_3 <- depth.mode(fdata(gb_fda$fd[as.vector(which(gb_clustering_4_DkBk$cls == 3))]))$median
depth_mode_gb_4_4 <- depth.mode(fdata(gb_fda$fd[as.vector(which(gb_clustering_4_DkBk$cls == 4))]))$median

depth_RPD_gb_4_1 <- depth.RPD(fdata(gb_fda$fd[as.vector(which(gb_clustering_4_DkBk$cls == 1))]))$median
depth_RPD_gb_4_2 <- depth.RPD(fdata(gb_fda$fd[as.vector(which(gb_clustering_4_DkBk$cls == 2))]))$median
depth_RPD_gb_4_3 <- depth.RPD(fdata(gb_fda$fd[as.vector(which(gb_clustering_4_DkBk$cls == 3))]))$median
depth_RPD_gb_4_4 <- depth.RPD(fdata(gb_fda$fd[as.vector(which(gb_clustering_4_DkBk$cls == 4))]))$median

plot(depth_FM_gb_4_1,ylim=c(3,45),col="blue",lwd=2,main=NULL)
lines(depth_FM_gb_4_2,col="green",lwd=2)
lines(depth_FM_gb_4_3,col="pink",lwd=2)
lines(depth_FM_gb_4_4,col="orange",lwd=2)
legend("topleft", horiz=TRUE, legend=c("1","2","3","4"), 
       lwd=2,col=c("blue", "green", "pink", "orange"), title="Cluster")

plot(depth_mode_gb_4_1,ylim=c(3,45),col="blue",lwd=2,main=NULL)
lines(depth_mode_gb_4_2,col="green",lwd=2)
lines(depth_mode_gb_4_3,col="pink",lwd=2)
lines(depth_mode_gb_4_4,col="orange",lwd=2)
legend("topleft", horiz=TRUE, legend=c("1","2","3","4"), 
       lwd=2,col=c("blue", "green", "pink", "orange"), title="Cluster")

plot(depth_RPD_gb_4_1,ylim=c(3,45),col="blue",lwd=2,main=NULL)
lines(depth_RPD_gb_4_2,col="green",lwd=2)
lines(depth_RPD_gb_4_3,col="pink",lwd=2)
lines(depth_RPD_gb_4_4,col="orange",lwd=2)
legend("topleft", horiz=TRUE, legend=c("1","2","3","4"), 
       lwd=2,col=c("blue", "green", "pink", "orange"), title="Cluster")

set.seed(123456)
depth_FM_gb_5_1 <- depth.FM(fdata(gb_fda$fd[as.vector(which(gb_clustering_l2_5$cluster == 1))]))$median
depth_FM_gb_5_2 <- depth.FM(fdata(gb_fda$fd[as.vector(which(gb_clustering_l2_5$cluster == 2))]))$median
depth_FM_gb_5_3 <- depth.FM(fdata(gb_fda$fd[as.vector(which(gb_clustering_l2_5$cluster == 3))]))$median
depth_FM_gb_5_4 <- depth.FM(fdata(gb_fda$fd[as.vector(which(gb_clustering_l2_5$cluster == 4))]))$median
depth_FM_gb_5_5 <- depth.FM(fdata(gb_fda$fd[as.vector(which(gb_clustering_l2_5$cluster == 5))]))$median

depth_mode_gb_5_1 <- depth.mode(fdata(gb_fda$fd[as.vector(which(gb_clustering_l2_5$cluster == 1))]))$median
depth_mode_gb_5_2 <- depth.mode(fdata(gb_fda$fd[as.vector(which(gb_clustering_l2_5$cluster == 2))]))$median
depth_mode_gb_5_3 <- depth.mode(fdata(gb_fda$fd[as.vector(which(gb_clustering_l2_5$cluster == 3))]))$median
depth_mode_gb_5_4 <- depth.mode(fdata(gb_fda$fd[as.vector(which(gb_clustering_l2_5$cluster == 4))]))$median
depth_mode_gb_5_5 <- depth.mode(fdata(gb_fda$fd[as.vector(which(gb_clustering_l2_5$cluster == 5))]))$median

depth_RPD_gb_5_1 <- depth.RPD(fdata(gb_fda$fd[as.vector(which(gb_clustering_l2_5$cluster == 1))]))$median
depth_RPD_gb_5_2 <- depth.RPD(fdata(gb_fda$fd[as.vector(which(gb_clustering_l2_5$cluster == 2))]))$median
depth_RPD_gb_5_3 <- depth.RPD(fdata(gb_fda$fd[as.vector(which(gb_clustering_l2_5$cluster == 3))]))$median
depth_RPD_gb_5_4 <- depth.RPD(fdata(gb_fda$fd[as.vector(which(gb_clustering_l2_5$cluster == 4))]))$median
depth_RPD_gb_5_5 <- depth.RPD(fdata(gb_fda$fd[as.vector(which(gb_clustering_l2_5$cluster == 5))]))$median

plot(depth_FM_gb_5_1,ylim=c(3,55),col="blue",main=NULL)
lines(depth_FM_gb_5_2,col="green",lwd=2)
lines(depth_FM_gb_5_3,col="pink",lwd=2)
lines(depth_FM_gb_5_4,col="orange",lwd=2)
lines(depth_FM_gb_5_5,col="purple",lwd=2)
legend("topleft", horiz=TRUE, legend=c("1","2","3","4","5"), 
       lwd=2,col=c("blue", "green", "pink", "orange", "purple"), title="Cluster")

plot(depth_mode_gb_5_1,ylim=c(3,58),col="blue",lwd=2,main=NULL)
lines(depth_mode_gb_5_2,col="green",lwd=2)
lines(depth_mode_gb_5_3,col="pink",lwd=2)
lines(depth_mode_gb_5_4,col="orange",lwd=2)
lines(depth_mode_gb_5_5,col="purple",lwd=2)
legend("topleft", horiz=TRUE, legend=c("1","2","3","4","5"), 
       lwd=2,col=c("blue", "green", "pink", "orange", "purple"), title="Cluster")

plot(depth_RPD_gb_5_1,ylim=c(3,58),col="blue",lwd=2,main=NULL)
lines(depth_RPD_gb_5_2,col="green",lwd=2)
lines(depth_RPD_gb_5_3,col="pink",lwd=2)
lines(depth_RPD_gb_5_4,col="orange",lwd=2)
lines(depth_RPD_gb_5_5,col="purple",lwd=2)
legend("topleft", horiz=TRUE, legend=c("1","2","3","4","5"), 
       lwd=2,col=c("blue", "green", "pink", "orange", "purple"), title="Cluster")


####################### VOICE MINUTES ############################
set.seed(123456)
depth_FM_voice_5_1 <- depth.FM(fdata(voice_fda$fd[as.vector(which(voice_clustering_l2_5$cluster == 1))]))$median
depth_FM_voice_5_2 <- depth.FM(fdata(voice_fda$fd[as.vector(which(voice_clustering_l2_5$cluster == 2))]))$median
depth_FM_voice_5_3 <- depth.FM(fdata(voice_fda$fd[as.vector(which(voice_clustering_l2_5$cluster == 3))]))$median
depth_FM_voice_5_4 <- depth.FM(fdata(voice_fda$fd[as.vector(which(voice_clustering_l2_5$cluster == 4))]))$median
depth_FM_voice_5_5 <- depth.FM(fdata(voice_fda$fd[as.vector(which(voice_clustering_l2_5$cluster == 5))]))$median

depth_mode_voice_5_1 <- depth.mode(fdata(voice_fda$fd[as.vector(which(voice_clustering_l2_5$cluster == 1))]))$median
depth_mode_voice_5_2 <- depth.mode(fdata(voice_fda$fd[as.vector(which(voice_clustering_l2_5$cluster == 2))]))$median
depth_mode_voice_5_3 <- depth.mode(fdata(voice_fda$fd[as.vector(which(voice_clustering_l2_5$cluster == 3))]))$median
depth_mode_voice_5_4 <- depth.mode(fdata(voice_fda$fd[as.vector(which(voice_clustering_l2_5$cluster == 4))]))$median
depth_mode_voice_5_5 <- depth.mode(fdata(voice_fda$fd[as.vector(which(voice_clustering_l2_5$cluster == 5))]))$median

depth_RPD_voice_5_1 <- depth.RPD(fdata(voice_fda$fd[as.vector(which(voice_clustering_l2_5$cluster == 1))]))$median
depth_RPD_voice_5_2 <- depth.RPD(fdata(voice_fda$fd[as.vector(which(voice_clustering_l2_5$cluster == 2))]))$median
depth_RPD_voice_5_3 <- depth.RPD(fdata(voice_fda$fd[as.vector(which(voice_clustering_l2_5$cluster == 3))]))$median
depth_RPD_voice_5_4 <- depth.RPD(fdata(voice_fda$fd[as.vector(which(voice_clustering_l2_5$cluster == 4))]))$median
depth_RPD_voice_5_5 <- depth.RPD(fdata(voice_fda$fd[as.vector(which(voice_clustering_l2_5$cluster == 5))]))$median

plot(depth_FM_voice_5_1,ylim=c(200,3000),col="blue",lwd=2,main=NULL)
lines(depth_FM_voice_5_2,col="green",lwd=2)
lines(depth_FM_voice_5_3,col="pink",lwd=2)
lines(depth_FM_voice_5_4,col="orange",lwd=2)
lines(depth_FM_voice_5_5,col="purple",lwd=2)
legend("topright", horiz=TRUE, legend=c("1","2","3","4","5"), 
       lwd=2,col=c("blue", "green", "pink", "orange","purple"), title="Cluster")

plot(depth_mode_voice_5_1,ylim=c(200,3000),col="blue",lwd=2,main=NULL)
lines(depth_mode_voice_5_2,col="green",lwd=2)
lines(depth_mode_voice_5_3,col="pink",lwd=2)
lines(depth_mode_voice_5_4,col="orange",lwd=2)
lines(depth_mode_voice_5_5,col="purple",lwd=2)
legend("topright", horiz=TRUE, legend=c("1","2","3","4","5"), 
       lwd=2,col=c("blue", "green", "pink", "orange","purple"), title="Cluster")

plot(depth_RPD_voice_5_1,ylim=c(300,3000),col="blue",lwd=2,main=NULL)
lines(depth_RPD_voice_5_2,col="green",lwd=2)
lines(depth_RPD_voice_5_3,col="pink",lwd=2)
lines(depth_RPD_voice_5_4,col="orange",lwd=2)
lines(depth_RPD_voice_5_5,col="purple",lwd=2)
legend("topright", horiz=TRUE, legend=c("1","2","3","4","5"), 
       lwd=2,col=c("blue", "green", "pink", "orange","purple"), title="Cluster")

# voice_clustering_l2_6$cluster -> voice_clustering_6_DkBk$cls
set.seed(123456)
depth_FM_voice_6_1 <- depth.FM(fdata(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 1))]))$median
depth_FM_voice_6_2 <- depth.FM(fdata(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 2))]))$median
depth_FM_voice_6_3 <- depth.FM(fdata(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 3))]))$median
depth_FM_voice_6_4 <- depth.FM(fdata(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 4))]))$median
depth_FM_voice_6_5 <- depth.FM(fdata(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 5))]))$median
depth_FM_voice_6_6 <- depth.FM(fdata(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 6))]))$median

depth_mode_voice_6_1 <- depth.mode(fdata(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 1))]))$median
depth_mode_voice_6_2 <- depth.mode(fdata(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 2))]))$median
depth_mode_voice_6_3 <- depth.mode(fdata(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 3))]))$median
depth_mode_voice_6_4 <- depth.mode(fdata(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 4))]))$median
depth_mode_voice_6_5 <- depth.mode(fdata(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 5))]))$median
depth_mode_voice_6_6 <- depth.mode(fdata(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 6))]))$median

depth_RPD_voice_6_1 <- depth.RPD(fdata(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 1))]))$median
depth_RPD_voice_6_2 <- depth.RPD(fdata(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 2))]))$median
depth_RPD_voice_6_3 <- depth.RPD(fdata(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 3))]))$median
depth_RPD_voice_6_4 <- depth.RPD(fdata(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 4))]))$median
depth_RPD_voice_6_5 <- depth.RPD(fdata(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 5))]))$median
depth_RPD_voice_6_6 <- depth.RPD(fdata(voice_fda$fd[as.vector(which(voice_clustering_6_DkBk$cls == 6))]))$median

plot(depth_FM_voice_6_1,ylim=c(300,3000),col="blue",lwd=2,main=NULL)
lines(depth_FM_voice_6_2,col="green",lwd=2)
lines(depth_FM_voice_6_3,col="pink",lwd=2)
lines(depth_FM_voice_6_4,col="orange",lwd=2)
lines(depth_FM_voice_6_5,col="purple",lwd=2)
lines(depth_FM_voice_6_6,col="brown",lwd=2)
legend("topright", horiz=TRUE, legend=c("1","2","3","4","5","6"), 
       lwd=2,col=c("blue", "green", "pink", "orange","purple","brown"), title="Cluster")

plot(depth_mode_voice_6_1,ylim=c(300,3000),col="blue",lwd=2,main=NULL)
lines(depth_mode_voice_6_2,col="green",lwd=2)
lines(depth_mode_voice_6_3,col="pink",lwd=2)
lines(depth_mode_voice_6_4,col="orange",lwd=2)
lines(depth_mode_voice_6_5,col="purple",lwd=2)
lines(depth_mode_voice_6_6,col="brown",lwd=2)
legend("topright", horiz=TRUE, legend=c("1","2","3","4","5","6"), 
       lwd=2,col=c("blue", "green", "pink", "orange","purple","brown"), title="Cluster")

plot(depth_RPD_voice_6_1,ylim=c(300,3200),col="blue",lwd=2,main=NULL)
lines(depth_RPD_voice_6_2,col="green",lwd=2)
lines(depth_RPD_voice_6_3,col="pink",lwd=2)
lines(depth_RPD_voice_6_4,col="orange",lwd=2)
lines(depth_RPD_voice_6_5,col="purple",lwd=2)
lines(depth_RPD_voice_6_6,col="brown",lwd=2)
legend("topright", horiz=TRUE, legend=c("1","2","3","4","5","6"), 
       lwd=2,col=c("blue", "green", "pink", "orange","purple","brown"), title="Cluster")

####################### USAGE INDEX ############################
set.seed(123456)
depth_FM_index_5_1 <- depth.FM(fdata(usage_index_fda$fd[as.vector(which(index_clustering_l2_5$cluster == 1))]))$median
depth_FM_index_5_2 <- depth.FM(fdata(usage_index_fda$fd[as.vector(which(index_clustering_l2_5$cluster == 2))]))$median
depth_FM_index_5_3 <- depth.FM(fdata(usage_index_fda$fd[as.vector(which(index_clustering_l2_5$cluster == 3))]))$median
depth_FM_index_5_4 <- depth.FM(fdata(usage_index_fda$fd[as.vector(which(index_clustering_l2_5$cluster == 4))]))$median
depth_FM_index_5_5 <- depth.FM(fdata(usage_index_fda$fd[as.vector(which(index_clustering_l2_5$cluster == 5))]))$median

depth_mode_index_5_1 <- depth.mode(fdata(usage_index_fda$fd[as.vector(which(index_clustering_l2_5$cluster == 1))]))$median
depth_mode_index_5_2 <- depth.mode(fdata(usage_index_fda$fd[as.vector(which(index_clustering_l2_5$cluster == 2))]))$median
depth_mode_index_5_3 <- depth.mode(fdata(usage_index_fda$fd[as.vector(which(index_clustering_l2_5$cluster == 3))]))$median
depth_mode_index_5_4 <- depth.mode(fdata(usage_index_fda$fd[as.vector(which(index_clustering_l2_5$cluster == 4))]))$median
depth_mode_index_5_5 <- depth.mode(fdata(usage_index_fda$fd[as.vector(which(index_clustering_l2_5$cluster == 5))]))$median

depth_RPD_index_5_1 <- depth.RPD(fdata(usage_index_fda$fd[as.vector(which(index_clustering_l2_5$cluster == 1))]))$median
depth_RPD_index_5_2 <- depth.RPD(fdata(usage_index_fda$fd[as.vector(which(index_clustering_l2_5$cluster == 2))]))$median
depth_RPD_index_5_3 <- depth.RPD(fdata(usage_index_fda$fd[as.vector(which(index_clustering_l2_5$cluster == 3))]))$median
depth_RPD_index_5_4 <- depth.RPD(fdata(usage_index_fda$fd[as.vector(which(index_clustering_l2_5$cluster == 4))]))$median
depth_RPD_index_5_5 <- depth.RPD(fdata(usage_index_fda$fd[as.vector(which(index_clustering_l2_5$cluster == 5))]))$median

plot(depth_FM_index_5_1,ylim=c(100,1500),col="blue",lwd=2,main=NULL)
lines(depth_FM_index_5_2,col="green",lwd=2)
lines(depth_FM_index_5_3,col="pink",lwd=2)
lines(depth_FM_index_5_4,col="orange",lwd=2)
lines(depth_FM_index_5_5,col="purple",lwd=2)
legend("topright", horiz=TRUE, legend=c("1","2","3","4","5"), 
       lwd=2,col=c("blue", "green", "pink", "orange","purple"), title="Cluster")

plot(depth_mode_index_5_1,ylim=c(100,1500),col="blue",lwd=2,main=NULL)
lines(depth_mode_index_5_2,col="green",lwd=2)
lines(depth_mode_index_5_3,col="pink",lwd=2)
lines(depth_mode_index_5_4,col="orange",lwd=2)
lines(depth_mode_index_5_5,col="purple",lwd=2)
legend("topright", horiz=TRUE, legend=c("1","2","3","4","5"), 
       lwd=2,col=c("blue", "green", "pink", "orange","purple"), title="Cluster")

plot(depth_RPD_index_5_1,ylim=c(100,1500),col="blue",lwd=2,main=NULL)
lines(depth_RPD_index_5_2,col="green",lwd=2)
lines(depth_RPD_index_5_3,col="pink",lwd=2)
lines(depth_RPD_index_5_4,col="orange",lwd=2)
lines(depth_RPD_index_5_5,col="purple",lwd=2)
legend("topright", horiz=TRUE, legend=c("1","2","3","4","5"), 
       lwd=2,col=c("blue", "green", "pink", "orange","purple"), title="Cluster")

# index_clustering_l2_6$cluster -> index_clustering_6_DkBk$cls
set.seed(123456)
depth_FM_index_6_1 <- depth.FM(fdata(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 1))]))$median
depth_FM_index_6_2 <- depth.FM(fdata(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 2))]))$median
depth_FM_index_6_3 <- depth.FM(fdata(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 3))]))$median
depth_FM_index_6_4 <- depth.FM(fdata(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 4))]))$median
depth_FM_index_6_5 <- depth.FM(fdata(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 5))]))$median
depth_FM_index_6_6 <- depth.FM(fdata(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 6))]))$median

depth_mode_index_6_1 <- depth.mode(fdata(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 1))]))$median
depth_mode_index_6_2 <- depth.mode(fdata(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 2))]))$median
depth_mode_index_6_3 <- depth.mode(fdata(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 3))]))$median
depth_mode_index_6_4 <- depth.mode(fdata(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 4))]))$median
depth_mode_index_6_5 <- depth.mode(fdata(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 5))]))$median
depth_mode_index_6_6 <- depth.mode(fdata(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 6))]))$median

depth_RPD_index_6_1 <- depth.RPD(fdata(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 1))]))$median
depth_RPD_index_6_2 <- depth.RPD(fdata(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 2))]))$median
depth_RPD_index_6_3 <- depth.RPD(fdata(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 3))]))$median
depth_RPD_index_6_4 <- depth.RPD(fdata(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 4))]))$median
depth_RPD_index_6_5 <- depth.RPD(fdata(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 5))]))$median
depth_RPD_index_6_6 <- depth.RPD(fdata(usage_index_05_fda$fd[as.vector(which(index_clustering_6_DkBk$cls == 6))]))$median

plot(depth_FM_index_6_1,ylim=c(100,1500),col="blue",lwd=2,main=NULL)
lines(depth_FM_index_6_2,col="green",lwd=2)
lines(depth_FM_index_6_3,col="pink",lwd=2)
lines(depth_FM_index_6_4,col="orange",lwd=2)
lines(depth_FM_index_6_5,col="purple",lwd=2)
lines(depth_FM_index_6_6,col="brown",lwd=2)
legend("topright", horiz=TRUE, legend=c("1","2","3","4","5","6"), 
       lwd=2,col=c("blue", "green", "pink", "orange","purple","brown"), title="Cluster")

plot(depth_mode_index_6_1,ylim=c(100,1500),col="blue",lwd=2,main=NULL)
lines(depth_mode_index_6_2,col="green",lwd=2)
lines(depth_mode_index_6_3,col="pink",lwd=2)
lines(depth_mode_index_6_4,col="orange",lwd=2)
lines(depth_mode_index_6_5,col="purple",lwd=2)
lines(depth_mode_index_6_6,col="brown",lwd=2)
legend("topright", horiz=TRUE, legend=c("1","2","3","4","5","6"), 
       lwd=2,col=c("blue", "green", "pink", "orange","purple","brown"), title="Cluster")

plot(depth_mode_index_6_1,ylim=c(100,1500),col="blue",lwd=2,main=NULL)
lines(depth_RPD_index_6_2,col="green",lwd=2)
lines(depth_RPD_index_6_3,col="pink",lwd=2)
lines(depth_RPD_index_6_4,col="orange",lwd=2)
lines(depth_RPD_index_6_5,col="purple",lwd=2)
lines(depth_RPD_index_6_6,col="purple",lwd=2)
legend("topright", horiz=TRUE, legend=c("1","2","3","4","5","6"), 
       lwd=2,col=c("blue", "green", "pink", "orange","purple","brown"), title="Cluster")




