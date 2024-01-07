
########################### OFFER GBs ###########################
gb_offers_dist <- gb_offers_wo_unlim_fda$y[1:21,]

# 5 clusters: FM
gb_depths_cluster1 <- depth_FM_gb_4_1$data
gb_depths_cluster2 <- depth_FM_gb_4_2$data
gb_depths_cluster3 <- depth_FM_gb_4_3$data
gb_depths_cluster4 <- depth_FM_gb_4_4$data
# gb_depths_cluster5 <- depth_FM_gb_5_5$data

gb_dist_cluster1 <- numeric(ncol(gb_offers_dist))
gb_dist_cluster2 <- numeric(ncol(gb_offers_dist))
gb_dist_cluster3 <- numeric(ncol(gb_offers_dist))
gb_dist_cluster4 <- numeric(ncol(gb_offers_dist))
# gb_dist_cluster5 <- numeric(ncol(gb_offers_dist))

gb_dist_cluster1_2 <- numeric(ncol(gb_offers_dist))
gb_dist_cluster2_2 <- numeric(ncol(gb_offers_dist))
gb_dist_cluster3_2 <- numeric(ncol(gb_offers_dist))
gb_dist_cluster4_2 <- numeric(ncol(gb_offers_dist))
# gb_dist_cluster5_2 <- numeric(ncol(gb_offers_dist))

for(i in 1:ncol(gb_offers_dist)){
  gb_dist_cluster1[i] <- sqrt(sum((gb_depths_cluster1 - gb_offers_dist[,i])^2))
  gb_dist_cluster2[i] <- sqrt(sum((gb_depths_cluster2 - gb_offers_dist[,i])^2))
  gb_dist_cluster3[i] <- sqrt(sum((gb_depths_cluster3 - gb_offers_dist[,i])^2))
  gb_dist_cluster4[i] <- sqrt(sum((gb_depths_cluster4 - gb_offers_dist[,i])^2))
  # gb_dist_cluster5[i] <- sqrt(sum((gb_depths_cluster5 - gb_offers_dist[,i])^2))
}

for(i in 1:ncol(gb_offers_dist)){
  gb_dist_cluster1_2[i] <- max(abs(gb_depths_cluster1 - gb_offers_dist[,i]))
  gb_dist_cluster2_2[i] <- max(abs(gb_depths_cluster2 - gb_offers_dist[,i]))
  gb_dist_cluster3_2[i] <- max(abs(gb_depths_cluster3 - gb_offers_dist[,i]))
  gb_dist_cluster4_2[i] <- max(abs(gb_depths_cluster4 - gb_offers_dist[,i]))
  # gb_dist_cluster5_2[i] <- max(abs(gb_depths_cluster5 - gb_offers_dist[,i]))
}

gb_FM_5_clusters1 <- list(gb_offers_dist[,which.min(gb_dist_cluster1)][1],
                          gb_offers_dist[,which.min(gb_dist_cluster2)][1],
                          gb_offers_dist[,which.min(gb_dist_cluster3)][1],
                          gb_offers_dist[,which.min(gb_dist_cluster4)][1])
                          # gb_offers_dist[,which.min(gb_dist_cluster5)][1])
gb_FM_5_clusters2 <- list(gb_offers_dist[,which.min(gb_dist_cluster1_2)][1],
                          gb_offers_dist[,which.min(gb_dist_cluster2_2)][1],
                          gb_offers_dist[,which.min(gb_dist_cluster3_2)][1],
                          gb_offers_dist[,which.min(gb_dist_cluster4_2)][1])
                          # gb_offers_dist[,which.min(gb_dist_cluster5_2)][1])

# 5 clusters: mode
gb_depths_cluster1 <- depth_mode_gb_4_1$data
gb_depths_cluster2 <- depth_mode_gb_4_2$data
gb_depths_cluster3 <- depth_mode_gb_4_3$data
gb_depths_cluster4 <- depth_mode_gb_4_4$data
# gb_depths_cluster5 <- depth_mode_gb_5_5$data

gb_dist_cluster1 <- numeric(ncol(gb_offers_dist))
gb_dist_cluster2 <- numeric(ncol(gb_offers_dist))
gb_dist_cluster3 <- numeric(ncol(gb_offers_dist))
gb_dist_cluster4 <- numeric(ncol(gb_offers_dist))
# gb_dist_cluster5 <- numeric(ncol(gb_offers_dist))

gb_dist_cluster1_2 <- numeric(ncol(gb_offers_dist))
gb_dist_cluster2_2 <- numeric(ncol(gb_offers_dist))
gb_dist_cluster3_2 <- numeric(ncol(gb_offers_dist))
gb_dist_cluster4_2 <- numeric(ncol(gb_offers_dist))
# gb_dist_cluster5_2 <- numeric(ncol(gb_offers_dist))

for(i in 1:ncol(gb_offers_dist)){
  gb_dist_cluster1[i] <- sqrt(sum((gb_depths_cluster1 - gb_offers_dist[,i])^2))
  gb_dist_cluster2[i] <- sqrt(sum((gb_depths_cluster2 - gb_offers_dist[,i])^2))
  gb_dist_cluster3[i] <- sqrt(sum((gb_depths_cluster3 - gb_offers_dist[,i])^2))
  gb_dist_cluster4[i] <- sqrt(sum((gb_depths_cluster4 - gb_offers_dist[,i])^2))
  # gb_dist_cluster5[i] <- sqrt(sum((gb_depths_cluster5 - gb_offers_dist[,i])^2))
}

for(i in 1:ncol(gb_offers_dist)){
  gb_dist_cluster1_2[i] <- max(abs(gb_depths_cluster1 - gb_offers_dist[,i]))
  gb_dist_cluster2_2[i] <- max(abs(gb_depths_cluster2 - gb_offers_dist[,i]))
  gb_dist_cluster3_2[i] <- max(abs(gb_depths_cluster3 - gb_offers_dist[,i]))
  gb_dist_cluster4_2[i] <- max(abs(gb_depths_cluster4 - gb_offers_dist[,i]))
  # gb_dist_cluster5_2[i] <- max(abs(gb_depths_cluster5 - gb_offers_dist[,i]))
}

gb_mode_5_clusters1 <- list(gb_offers_dist[,which.min(gb_dist_cluster1)][1],
                          gb_offers_dist[,which.min(gb_dist_cluster2)][1],
                          gb_offers_dist[,which.min(gb_dist_cluster3)][1],
                          gb_offers_dist[,which.min(gb_dist_cluster4)][1])
                          # gb_offers_dist[,which.min(gb_dist_cluster5)][1])
gb_mode_5_clusters2 <- list(gb_offers_dist[,which.min(gb_dist_cluster1_2)][1],
                          gb_offers_dist[,which.min(gb_dist_cluster2_2)][1],
                          gb_offers_dist[,which.min(gb_dist_cluster3_2)][1],
                          gb_offers_dist[,which.min(gb_dist_cluster4_2)][1])
                          # gb_offers_dist[,which.min(gb_dist_cluster5_2)][1])

# 5 clusters: RPD
gb_depths_cluster1 <- depth_RPD_gb_4_1$data
gb_depths_cluster2 <- depth_RPD_gb_4_2$data
gb_depths_cluster3 <- depth_RPD_gb_4_3$data
gb_depths_cluster4 <- depth_RPD_gb_4_4$data
# gb_depths_cluster5 <- depth_RPD_gb_5_5$data

gb_dist_cluster1 <- numeric(ncol(gb_offers_dist))
gb_dist_cluster2 <- numeric(ncol(gb_offers_dist))
gb_dist_cluster3 <- numeric(ncol(gb_offers_dist))
gb_dist_cluster4 <- numeric(ncol(gb_offers_dist))
# gb_dist_cluster5 <- numeric(ncol(gb_offers_dist))

gb_dist_cluster1_2 <- numeric(ncol(gb_offers_dist))
gb_dist_cluster2_2 <- numeric(ncol(gb_offers_dist))
gb_dist_cluster3_2 <- numeric(ncol(gb_offers_dist))
gb_dist_cluster4_2 <- numeric(ncol(gb_offers_dist))
# gb_dist_cluster5_2 <- numeric(ncol(gb_offers_dist))

for(i in 1:ncol(gb_offers_dist)){
  gb_dist_cluster1[i] <- sqrt(sum((gb_depths_cluster1 - gb_offers_dist[,i])^2))
  gb_dist_cluster2[i] <- sqrt(sum((gb_depths_cluster2 - gb_offers_dist[,i])^2))
  gb_dist_cluster3[i] <- sqrt(sum((gb_depths_cluster3 - gb_offers_dist[,i])^2))
  gb_dist_cluster4[i] <- sqrt(sum((gb_depths_cluster4 - gb_offers_dist[,i])^2))
  # gb_dist_cluster5[i] <- sqrt(sum((gb_depths_cluster5 - gb_offers_dist[,i])^2))
}

for(i in 1:ncol(gb_offers_dist)){
  gb_dist_cluster1_2[i] <- max(abs(gb_depths_cluster1 - gb_offers_dist[,i]))
  gb_dist_cluster2_2[i] <- max(abs(gb_depths_cluster2 - gb_offers_dist[,i]))
  gb_dist_cluster3_2[i] <- max(abs(gb_depths_cluster3 - gb_offers_dist[,i]))
  gb_dist_cluster4_2[i] <- max(abs(gb_depths_cluster4 - gb_offers_dist[,i]))
  # gb_dist_cluster5_2[i] <- max(abs(gb_depths_cluster5 - gb_offers_dist[,i]))
}

gb_RPD_5_clusters1 <- list(gb_offers_dist[,which.min(gb_dist_cluster1)][1],
                          gb_offers_dist[,which.min(gb_dist_cluster2)][1],
                          gb_offers_dist[,which.min(gb_dist_cluster3)][1],
                          gb_offers_dist[,which.min(gb_dist_cluster4)][1])
                          # gb_offers_dist[,which.min(gb_dist_cluster5)][1])

gb_RPD_5_clusters2 <- list(gb_offers_dist[,which.min(gb_dist_cluster1_2)][1],
                          gb_offers_dist[,which.min(gb_dist_cluster2_2)][1],
                          gb_offers_dist[,which.min(gb_dist_cluster3_2)][1],
                          gb_offers_dist[,which.min(gb_dist_cluster4_2)][1])
                          # gb_offers_dist[,which.min(gb_dist_cluster5_2)][1])


########################### OFFER VOICE minutes ###########################
voice_offers_dist <- voice_offers_fda$y[1:21,]
fic_voice_offers_dist <- fic_voice_offers_wo_unlim_fda$y[1:21,]

# 5 clusters: FM
voice_depths_cluster1 <- depth_FM_voice_6_1$data
voice_depths_cluster2 <- depth_FM_voice_6_2$data
voice_depths_cluster3 <- depth_FM_voice_6_3$data
voice_depths_cluster4 <- depth_FM_voice_6_4$data
voice_depths_cluster5 <- depth_FM_voice_6_5$data
voice_depths_cluster6 <- depth_FM_voice_6_6$data

voice_dist_cluster1 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster2 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster3 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster4 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster5 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster6 <- numeric(ncol(fic_voice_offers_dist))

voice_dist_cluster1_2 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster2_2 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster3_2 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster4_2 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster5_2 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster6_2 <- numeric(ncol(fic_voice_offers_dist))

for(i in 1:ncol(fic_voice_offers_dist)){
  voice_dist_cluster1[i] <- sqrt(sum((voice_depths_cluster1 - fic_voice_offers_dist[,i])^2))
  voice_dist_cluster2[i] <- sqrt(sum((voice_depths_cluster2 - fic_voice_offers_dist[,i])^2))
  voice_dist_cluster3[i] <- sqrt(sum((voice_depths_cluster3 - fic_voice_offers_dist[,i])^2))
  voice_dist_cluster4[i] <- sqrt(sum((voice_depths_cluster4 - fic_voice_offers_dist[,i])^2))
  voice_dist_cluster5[i] <- sqrt(sum((voice_depths_cluster5 - fic_voice_offers_dist[,i])^2))
  voice_dist_cluster6[i] <- sqrt(sum((voice_depths_cluster6 - fic_voice_offers_dist[,i])^2))
}

for(i in 1:ncol(fic_voice_offers_dist)){
  voice_dist_cluster1_2[i] <- max(abs(voice_depths_cluster1 - fic_voice_offers_dist[,i]))
  voice_dist_cluster2_2[i] <- max(abs(voice_depths_cluster2 - fic_voice_offers_dist[,i]))
  voice_dist_cluster3_2[i] <- max(abs(voice_depths_cluster3 - fic_voice_offers_dist[,i]))
  voice_dist_cluster4_2[i] <- max(abs(voice_depths_cluster4 - fic_voice_offers_dist[,i]))
  voice_dist_cluster5_2[i] <- max(abs(voice_depths_cluster5 - fic_voice_offers_dist[,i]))
  voice_dist_cluster6_2[i] <- max(abs(voice_depths_cluster6 - fic_voice_offers_dist[,i]))
}

voice_FM_6_clusters1 <- list(fic_voice_offers_dist[,which.min(voice_dist_cluster1)][1],
                             fic_voice_offers_dist[,which.min(voice_dist_cluster2)][1],
                             fic_voice_offers_dist[,which.min(voice_dist_cluster3)][1],
                             fic_voice_offers_dist[,which.min(voice_dist_cluster4)][1],
                             fic_voice_offers_dist[,which.min(voice_dist_cluster5)][1],
                             fic_voice_offers_dist[,which.min(voice_dist_cluster6)][1])
voice_FM_6_clusters2 <- list(fic_voice_offers_dist[,which.min(voice_dist_cluster1_2)][1],
                             fic_voice_offers_dist[,which.min(voice_dist_cluster2_2)][1],
                             fic_voice_offers_dist[,which.min(voice_dist_cluster3_2)][1],
                             fic_voice_offers_dist[,which.min(voice_dist_cluster4_2)][1],
                             fic_voice_offers_dist[,which.min(voice_dist_cluster5_2)][1],
                             fic_voice_offers_dist[,which.min(voice_dist_cluster6_2)][1])

# 5 clusters: mode
voice_depths_cluster1 <- depth_mode_voice_6_1$data
voice_depths_cluster2 <- depth_mode_voice_6_2$data
voice_depths_cluster3 <- depth_mode_voice_6_3$data
voice_depths_cluster4 <- depth_mode_voice_6_4$data
voice_depths_cluster5 <- depth_mode_voice_6_5$data
voice_depths_cluster6 <- depth_mode_voice_6_6$data

voice_dist_cluster1 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster2 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster3 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster4 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster5 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster6 <- numeric(ncol(fic_voice_offers_dist))

voice_dist_cluster1_2 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster2_2 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster3_2 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster4_2 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster5_2 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster6_2 <- numeric(ncol(fic_voice_offers_dist))

for(i in 1:ncol(fic_voice_offers_dist)){
  voice_dist_cluster1[i] <- sqrt(sum((voice_depths_cluster1 - fic_voice_offers_dist[,i])^2))
  voice_dist_cluster2[i] <- sqrt(sum((voice_depths_cluster2 - fic_voice_offers_dist[,i])^2))
  voice_dist_cluster3[i] <- sqrt(sum((voice_depths_cluster3 - fic_voice_offers_dist[,i])^2))
  voice_dist_cluster4[i] <- sqrt(sum((voice_depths_cluster4 - fic_voice_offers_dist[,i])^2))
  voice_dist_cluster5[i] <- sqrt(sum((voice_depths_cluster5 - fic_voice_offers_dist[,i])^2))
  voice_dist_cluster6[i] <- sqrt(sum((voice_depths_cluster6 - fic_voice_offers_dist[,i])^2))
}

for(i in 1:ncol(fic_voice_offers_dist)){
  voice_dist_cluster1_2[i] <- max(abs(voice_depths_cluster1 - fic_voice_offers_dist[,i]))
  voice_dist_cluster2_2[i] <- max(abs(voice_depths_cluster2 - fic_voice_offers_dist[,i]))
  voice_dist_cluster3_2[i] <- max(abs(voice_depths_cluster3 - fic_voice_offers_dist[,i]))
  voice_dist_cluster4_2[i] <- max(abs(voice_depths_cluster4 - fic_voice_offers_dist[,i]))
  voice_dist_cluster5_2[i] <- max(abs(voice_depths_cluster5 - fic_voice_offers_dist[,i]))
  voice_dist_cluster6_2[i] <- max(abs(voice_depths_cluster6 - fic_voice_offers_dist[,i]))
}

voice_mode_5_clusters1 <- list(fic_voice_offers_dist[,which.min(voice_dist_cluster1)][1],
                              fic_voice_offers_dist[,which.min(voice_dist_cluster2)][1],
                              fic_voice_offers_dist[,which.min(voice_dist_cluster3)][1],
                              fic_voice_offers_dist[,which.min(voice_dist_cluster4)][1],
                              fic_voice_offers_dist[,which.min(voice_dist_cluster5)][1],
                              fic_voice_offers_dist[,which.min(voice_dist_cluster6)][1])
voice_mode_5_clusters2 <- list(fic_voice_offers_dist[,which.min(voice_dist_cluster1_2)][1],
                              fic_voice_offers_dist[,which.min(voice_dist_cluster2_2)][1],
                              fic_voice_offers_dist[,which.min(voice_dist_cluster3_2)][1],
                              fic_voice_offers_dist[,which.min(voice_dist_cluster4_2)][1],
                              fic_voice_offers_dist[,which.min(voice_dist_cluster5_2)][1],
                              fic_voice_offers_dist[,which.min(voice_dist_cluster6_2)][1])

# 5 clusters: RPD
voice_depths_cluster1 <- depth_RPD_voice_6_1$data
voice_depths_cluster2 <- depth_RPD_voice_6_2$data
voice_depths_cluster3 <- depth_RPD_voice_6_3$data
voice_depths_cluster4 <- depth_RPD_voice_6_4$data
voice_depths_cluster5 <- depth_RPD_voice_6_5$data
voice_depths_cluster6 <- depth_RPD_voice_6_6$data

voice_dist_cluster1 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster2 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster3 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster4 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster5 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster6 <- numeric(ncol(fic_voice_offers_dist))

voice_dist_cluster1_2 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster2_2 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster3_2 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster4_2 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster5_2 <- numeric(ncol(fic_voice_offers_dist))
voice_dist_cluster6_2 <- numeric(ncol(fic_voice_offers_dist))

for(i in 1:ncol(fic_voice_offers_dist)){
  voice_dist_cluster1[i] <- sqrt(sum((voice_depths_cluster1 - fic_voice_offers_dist[,i])^2))
  voice_dist_cluster2[i] <- sqrt(sum((voice_depths_cluster2 - fic_voice_offers_dist[,i])^2))
  voice_dist_cluster3[i] <- sqrt(sum((voice_depths_cluster3 - fic_voice_offers_dist[,i])^2))
  voice_dist_cluster4[i] <- sqrt(sum((voice_depths_cluster4 - fic_voice_offers_dist[,i])^2))
  voice_dist_cluster5[i] <- sqrt(sum((voice_depths_cluster5 - fic_voice_offers_dist[,i])^2))
  voice_dist_cluster6[i] <- sqrt(sum((voice_depths_cluster6 - fic_voice_offers_dist[,i])^2))
}

for(i in 1:ncol(fic_voice_offers_dist)){
  voice_dist_cluster1_2[i] <- max(abs(voice_depths_cluster1 - fic_voice_offers_dist[,i]))
  voice_dist_cluster2_2[i] <- max(abs(voice_depths_cluster2 - fic_voice_offers_dist[,i]))
  voice_dist_cluster3_2[i] <- max(abs(voice_depths_cluster3 - fic_voice_offers_dist[,i]))
  voice_dist_cluster4_2[i] <- max(abs(voice_depths_cluster4 - fic_voice_offers_dist[,i]))
  voice_dist_cluster5_2[i] <- max(abs(voice_depths_cluster5 - fic_voice_offers_dist[,i]))
  voice_dist_cluster6_2[i] <- max(abs(voice_depths_cluster6 - fic_voice_offers_dist[,i]))
}

voice_RPD_6_clusters1 <- list(fic_voice_offers_dist[,which.min(voice_dist_cluster1)][1],
                             fic_voice_offers_dist[,which.min(voice_dist_cluster2)][1],
                             fic_voice_offers_dist[,which.min(voice_dist_cluster3)][1],
                             fic_voice_offers_dist[,which.min(voice_dist_cluster4)][1],
                             fic_voice_offers_dist[,which.min(voice_dist_cluster5)][1],
                             fic_voice_offers_dist[,which.min(voice_dist_cluster6)][1])
voice_RPD_6_clusters2 <- list(fic_voice_offers_dist[,which.min(voice_dist_cluster1_2)][1],
                             fic_voice_offers_dist[,which.min(voice_dist_cluster2_2)][1],
                             fic_voice_offers_dist[,which.min(voice_dist_cluster3_2)][1],
                             fic_voice_offers_dist[,which.min(voice_dist_cluster4_2)][1],
                             fic_voice_offers_dist[,which.min(voice_dist_cluster5_2)][1],
                             fic_voice_offers_dist[,which.min(voice_dist_cluster6_2)][1])





