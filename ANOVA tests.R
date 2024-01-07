library(fda)
library(fdANOVA)
library(dplyr)

fANOVA.pointwise <- function(data, groups, t.seq, alpha=0.05) {
  # data is matrix with time in rows and variables in columns
  # group is a list names separating columns into different groups, a factor
  # time scale for measures
  n <- nrow(data)
  pvals <- numeric(n)
  lv <- levels(groups)
  k <- length(lv)
  mean.p <- matrix(NA, ncol=k, nrow=n)
  perm <- factorial(k)/(factorial(2)*(factorial(k-2)))
  Tukey.posthoc <- matrix(NA, ncol=perm, nrow=n)
  for(i in 1:n) {
    dt <- data.frame((data[i,]), groups)
    names(dt) <- c("values", "groups")
    av <- aov(values~groups, data = dt)
    pvals[i] <- summary(av)[[1]]["Pr(>F)"][1,1]
    mean.p[i,]  <- as.matrix((dt %>% group_by(groups) %>% summarise(mean(values)))[,2])
    colnames(Tukey.posthoc) <- rownames(TukeyHSD(av)$groups)
    Tukey.posthoc[i,] <- TukeyHSD(av)$groups[,4]
  }
  
  overall_mean <- apply(data, 1, mean)
  
  opar1 <- par(mfrow=c(2,1))
  
  plot(t.seq, pvals, type="l", main = "Pointwise ANOVA p-values",
       xlab = "Time", ylab="p-value", ylim=c(0,1))
  lines(t.seq, rep(0.05, n), col="blue", lty=2)
  
  mn <- min(mean.p, overall_mean)
  mx <- max(mean.p, overall_mean)
  
  plot(t.seq, overall_mean, type = "l", main = "Group means",
       xlab = "Time", ylab = "Mean", ylim = c(mn-0.05, mx+0.05))
  for(i in 1:k) {
    lines(t.seq, mean.p[,i], col=i+1, lty=i+1)
  }
  
  legend("topright", legend=c("Overall", lv), lty=1:(k+1), col=1:(k+1), title="Group")
  
  par(opar1)
  
  
  opar2 <- par(mfrow=c(1,1), ask = TRUE)
  
  for(i in 1:perm) {
    plot(t.seq, Tukey.posthoc[,i], type="l", main = paste("Tukey HSD p-values", rownames(TukeyHSD(av)$groups)[i]),
         xlab = "Time", ylab = "p-value", ylim = c(0,1))
    lines(t.seq, rep(0.05, n), col="blue", lty=2)
  }
  
  par(opar2)
  
  #return(list(p.values=pvals, TukeyHSD=Tukey.posthoc, gr.means = mean.p, overal.mean=overall_mean))
  return(summary(av))
  #return(TukeyHSD(av))
} 

######################## GB Basis #######################################
dta_mb = gb_fda$fd
dta_months.eval_mb <- eval.fd(0:35, dta_mb)

# segments
group_label_gb_l2_4 <- factor(gb_clustering_l2_4$cluster)
group_label_gb_l2_5 <- factor(gb_clustering_l2_5$cluster)

fANOVA.pointwise(data=dta_months.eval_mb, groups=group_label_gb_l2_4, 
                 t.seq=0:35, alpha=0.05)
fANOVA.pointwise(data=dta_months.eval_mb, groups=group_label_gb_l2_5, 
                 t.seq=0:35, alpha=0.05)

# model
group_label_gb_model_4 <- factor(gb_clustering_4_DkBk$cls)
group_label_gb_model_5 <- factor(gb_clustering_5_DkBk$cls) # imam 4

fANOVA.pointwise(data=dta_months.eval_mb, groups=group_label_gb_model_4, 
                 t.seq=0:35, alpha=0.05)
fANOVA.pointwise(data=dta_months.eval_mb, groups=group_label_gb_model_5, 
                 t.seq=0:35, alpha=0.05)

######################## Voice Basis #######################################
dta_voice = voice_fda$fd
dta_months.eval_voice <- eval.fd(0:35, dta_voice)

# segments
group_label_voice_l2_5 <- factor(voice_clustering_l2_5$cluster)
group_label_voice_l2_6 <- factor(voice_clustering_l2_6$cluster)

fANOVA.pointwise(data=dta_months.eval_voice, groups=group_label_voice_l2_5, 
                 t.seq=0:35, alpha=0.05)
fANOVA.pointwise(data=dta_months.eval_voice, groups=group_label_voice_l2_6, 
                 t.seq=0:35, alpha=0.05)

# model
group_label_voice_model_5 <- factor(voice_clustering_5_DkBk$cls)
group_label_voice_model_6 <- factor(voice_clustering_6_DkBk$cls) # imam 6

fANOVA.pointwise(data=dta_months.eval_voice, groups=group_label_voice_model_5, 
                 t.seq=0:35, alpha=0.05)
fANOVA.pointwise(data=dta_months.eval_voice, groups=group_label_voice_model_6, 
                 t.seq=0:35, alpha=0.05)

######################## Index Basis #######################################

dta_index_05 = usage_index_05_fda$fd
dta_months.eval_indicator <- eval.fd(0:35, dta_index_05)

# segments
group_label_usage_index_5 <- factor(index_clustering_l2_5$cluster)
group_label_usage_index_6 <- factor(index_clustering_l2_6$cluster)

fANOVA.pointwise(data=dta_months.eval_indicator, groups=group_label_usage_index_5, 
                 t.seq=0:35, alpha=0.05)
fANOVA.pointwise(data=dta_months.eval_indicator, groups=group_label_usage_index_6, 
                 t.seq=0:35, alpha=0.05)

# model
group_label_index_model_5 <- factor(index_clustering_5_DkBk$cls)
group_label_index_model_6 <- factor(index_clustering_6_DkBk$cls) # imam 6

fANOVA.pointwise(data=dta_months.eval_indicator, groups=group_label_index_model_5, 
                 t.seq=0:35, alpha=0.05)
fANOVA.pointwise(data=dta_months.eval_indicator, groups=group_label_index_model_6, 
                 t.seq=0:35, alpha=0.05)











