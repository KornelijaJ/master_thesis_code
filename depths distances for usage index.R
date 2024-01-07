
combinations <- expand.grid(a = gb_offers_dist, b = fic_voice_offers_dist) %>%
  arrange(a,b) %>% distinct()

########################### OFFER on Usage Index ###########################

alpha = 0.5

# 5 clusters: FM
usage_index_depths_cluster1 <- depth_FM_index_6_1$data
usage_index_depths_cluster2 <- depth_FM_index_6_2$data
usage_index_depths_cluster3 <- depth_FM_index_6_3$data
usage_index_depths_cluster4 <- depth_FM_index_6_4$data
usage_index_depths_cluster5 <- depth_FM_index_6_5$data
usage_index_depths_cluster6 <- depth_FM_index_6_6$data

cluster1_test <- sqrt(sum((usage_index_depths_cluster1 - 
                             (alpha*gb_offers_dist[,1]+
                                (1-alpha)*fic_voice_offers_dist[,1]))^2))
cluster2_test <- sqrt(sum((usage_index_depths_cluster2 - 
                             (alpha*gb_offers_dist[,1]+
                                (1-alpha)*fic_voice_offers_dist[,1]))^2))
cluster3_test <- sqrt(sum((usage_index_depths_cluster3 - 
                             (alpha*gb_offers_dist[,1]+
                                (1-alpha)*fic_voice_offers_dist[,1]))^2))
cluster4_test <- sqrt(sum((usage_index_depths_cluster4 - 
                             (alpha*gb_offers_dist[,1]+
                                (1-alpha)*fic_voice_offers_dist[,1]))^2))
cluster5_test <- sqrt(sum((usage_index_depths_cluster5 - 
                             (alpha*gb_offers_dist[,1]+
                                (1-alpha)*fic_voice_offers_dist[,1]))^2))
cluster6_test <- sqrt(sum((usage_index_depths_cluster6 - 
                             (alpha*gb_offers_dist[,1]+
                                (1-alpha)*fic_voice_offers_dist[,1]))^2))

cluster1_test2 <- max(abs(usage_index_depths_cluster1 - 
                             (alpha*gb_offers_dist[,1]+
                                (1-alpha)*fic_voice_offers_dist[,1])))
cluster2_test2 <- max(abs(usage_index_depths_cluster2 - 
                            (alpha*gb_offers_dist[,1]+
                               (1-alpha)*fic_voice_offers_dist[,1])))
cluster3_test2 <- max(abs(usage_index_depths_cluster3 - 
                            (alpha*gb_offers_dist[,1]+
                               (1-alpha)*fic_voice_offers_dist[,1])))
cluster4_test2 <- max(abs(usage_index_depths_cluster4 - 
                            (alpha*gb_offers_dist[,1]+
                               (1-alpha)*fic_voice_offers_dist[,1])))
cluster5_test2 <- max(abs(usage_index_depths_cluster5 - 
                            (alpha*gb_offers_dist[,1]+
                               (1-alpha)*fic_voice_offers_dist[,1])))
cluster6_test2 <- max(abs(usage_index_depths_cluster6 - 
                            (alpha*gb_offers_dist[,1]+
                               (1-alpha)*fic_voice_offers_dist[,1])))

for(i in 1:nrow(combinations)){
    cluster1_test <- append(cluster1_test, 
                            sqrt(sum((usage_index_depths_cluster1 - 
                                        ((1-alpha)*combinations[i,2]+
                                           alpha*combinations[i,1]))^2)))
    cluster2_test <- append(cluster2_test, 
                            sqrt(sum((usage_index_depths_cluster2 - 
                                        ((1-alpha)*combinations[i,2]+
                                           alpha*combinations[i,1]))^2)))
    cluster3_test <- append(cluster3_test, 
                            sqrt(sum((usage_index_depths_cluster3 - 
                                        ((1-alpha)*combinations[i,2]+
                                           alpha*combinations[i,1]))^2)))
    cluster4_test <- append(cluster4_test, 
                            sqrt(sum((usage_index_depths_cluster4 - 
                                        ((1-alpha)*combinations[i,2]+
                                           alpha*combinations[i,1]))^2)))
    cluster5_test <- append(cluster5_test, 
                            sqrt(sum((usage_index_depths_cluster5 - 
                                        ((1-alpha)*combinations[i,2]+
                                           alpha*combinations[i,1]))^2)))
    cluster6_test <- append(cluster6_test, 
                            sqrt(sum((usage_index_depths_cluster6 - 
                                        ((1-alpha)*combinations[i,2]+
                                           alpha*combinations[i,1]))^2)))
}
for(i in 1:nrow(combinations)){
    cluster1_test2 <- append(cluster1_test2, 
                             max(abs(usage_index_depths_cluster1 - 
                                       (alpha*combinations[i,1]+
                                          (1-alpha)*combinations[i,2]))))
    cluster2_test2 <- append(cluster2_test2, 
                             max(abs(usage_index_depths_cluster2 - 
                                       (alpha*combinations[i,1]+
                                          (1-alpha)*combinations[i,2]))))
    cluster3_test2 <- append(cluster3_test2, 
                             max(abs(usage_index_depths_cluster3 - 
                                       (alpha*combinations[i,1]+
                                          (1-alpha)*combinations[i,2]))))
    cluster4_test2 <- append(cluster4_test2, 
                             max(abs(usage_index_depths_cluster4 - 
                                       (alpha*combinations[i,1]+
                                          (1-alpha)*combinations[i,2]))))
    cluster5_test2 <- append(cluster5_test2, 
                             max(abs(usage_index_depths_cluster5 - 
                                       (alpha*combinations[i,1]+
                                          (1-alpha)*combinations[i,2]))))
    cluster6_test2 <- append(cluster6_test2, 
                             max(abs(usage_index_depths_cluster6 - 
                                       (alpha*combinations[i,1]+
                                          (1-alpha)*combinations[i,2]))))
}

usage_index_FM_6_clusters_combinations1 <- list(combinations[which.min(cluster1_test[-1]),],
                                                combinations[which.min(cluster2_test[-1]),],
                                                combinations[which.min(cluster3_test[-1]),],
                                                combinations[which.min(cluster4_test[-1]),],
                                                combinations[which.min(cluster5_test[-1]),],
                                                combinations[which.min(cluster6_test[-1]),])

usage_index_FM_6_clusters_combinations2 <- list(combinations[which.min(cluster1_test2[-1]),],
                                                combinations[which.min(cluster2_test2[-1]),],
                                                combinations[which.min(cluster3_test2[-1]),],
                                                combinations[which.min(cluster4_test2[-1]),],
                                                combinations[which.min(cluster5_test2[-1]),],
                                                combinations[which.min(cluster6_test2[-1]),])

# 5 clusters: mode
usage_index_depths_cluster1 <- depth_mode_index_6_1$data
usage_index_depths_cluster2 <- depth_mode_index_6_2$data
usage_index_depths_cluster3 <- depth_mode_index_6_3$data
usage_index_depths_cluster4 <- depth_mode_index_6_4$data
usage_index_depths_cluster5 <- depth_mode_index_6_5$data
usage_index_depths_cluster6 <- depth_mode_index_6_6$data

cluster1_test <- sqrt(sum((usage_index_depths_cluster1 - 
                             (alpha*gb_offers_dist[,1]+
                                (1-alpha)*fic_voice_offers_dist[,1]))^2))
cluster2_test <- sqrt(sum((usage_index_depths_cluster2 - 
                             (alpha*gb_offers_dist[,1]+
                                (1-alpha)*fic_voice_offers_dist[,1]))^2))
cluster3_test <- sqrt(sum((usage_index_depths_cluster3 - 
                             (alpha*gb_offers_dist[,1]+
                                (1-alpha)*fic_voice_offers_dist[,1]))^2))
cluster4_test <- sqrt(sum((usage_index_depths_cluster4 - 
                             (alpha*gb_offers_dist[,1]+
                                (1-alpha)*fic_voice_offers_dist[,1]))^2))
cluster5_test <- sqrt(sum((usage_index_depths_cluster5 - 
                             (alpha*gb_offers_dist[,1]+
                                (1-alpha)*fic_voice_offers_dist[,1]))^2))
cluster6_test <- sqrt(sum((usage_index_depths_cluster6 - 
                             (alpha*gb_offers_dist[,1]+
                                (1-alpha)*fic_voice_offers_dist[,1]))^2))

cluster1_test2 <- max(abs(usage_index_depths_cluster1 - 
                            (alpha*gb_offers_dist[,1]+
                               (1-alpha)*fic_voice_offers_dist[,1])))
cluster2_test2 <- max(abs(usage_index_depths_cluster2 - 
                            (alpha*gb_offers_dist[,1]+
                               (1-alpha)*fic_voice_offers_dist[,1])))
cluster3_test2 <- max(abs(usage_index_depths_cluster3 - 
                            (alpha*gb_offers_dist[,1]+
                               (1-alpha)*fic_voice_offers_dist[,1])))
cluster4_test2 <- max(abs(usage_index_depths_cluster4 - 
                            (alpha*gb_offers_dist[,1]+
                               (1-alpha)*fic_voice_offers_dist[,1])))
cluster5_test2 <- max(abs(usage_index_depths_cluster5 - 
                            (alpha*gb_offers_dist[,1]+
                               (1-alpha)*fic_voice_offers_dist[,1])))
cluster6_test2 <- max(abs(usage_index_depths_cluster6 - 
                            (alpha*gb_offers_dist[,1]+
                               (1-alpha)*fic_voice_offers_dist[,1])))


for(i in 1:nrow(combinations)){
  cluster1_test <- append(cluster1_test, 
                          sqrt(sum((usage_index_depths_cluster1 - 
                                      ((1-alpha)*combinations[i,2]+
                                         alpha*combinations[i,1]))^2)))
  cluster2_test <- append(cluster2_test, 
                          sqrt(sum((usage_index_depths_cluster2 - 
                                      ((1-alpha)*combinations[i,2]+
                                         alpha*combinations[i,1]))^2)))
  cluster3_test <- append(cluster3_test, 
                          sqrt(sum((usage_index_depths_cluster3 - 
                                      ((1-alpha)*combinations[i,2]+
                                         alpha*combinations[i,1]))^2)))
  cluster4_test <- append(cluster4_test, 
                          sqrt(sum((usage_index_depths_cluster4 - 
                                      ((1-alpha)*combinations[i,2]+
                                         alpha*combinations[i,1]))^2)))
  cluster5_test <- append(cluster5_test, 
                          sqrt(sum((usage_index_depths_cluster5 - 
                                      ((1-alpha)*combinations[i,2]+
                                         alpha*combinations[i,1]))^2)))
  cluster6_test <- append(cluster6_test, 
                          sqrt(sum((usage_index_depths_cluster6 - 
                                      ((1-alpha)*combinations[i,2]+
                                         alpha*combinations[i,1]))^2)))
}
for(i in 1:nrow(combinations)){
  cluster1_test2 <- append(cluster1_test2, 
                           max(abs(usage_index_depths_cluster1 - 
                                     (alpha*combinations[i,1]+
                                        (1-alpha)*combinations[i,2]))))
  cluster2_test2 <- append(cluster2_test2, 
                           max(abs(usage_index_depths_cluster2 - 
                                     (alpha*combinations[i,1]+
                                        (1-alpha)*combinations[i,2]))))
  cluster3_test2 <- append(cluster3_test2, 
                           max(abs(usage_index_depths_cluster3 - 
                                     (alpha*combinations[i,1]+
                                        (1-alpha)*combinations[i,2]))))
  cluster4_test2 <- append(cluster4_test2, 
                           max(abs(usage_index_depths_cluster4 - 
                                     (alpha*combinations[i,1]+
                                        (1-alpha)*combinations[i,2]))))
  cluster5_test2 <- append(cluster5_test2, 
                           max(abs(usage_index_depths_cluster5 - 
                                     (alpha*combinations[i,1]+
                                        (1-alpha)*combinations[i,2]))))
  cluster6_test2 <- append(cluster6_test2, 
                           max(abs(usage_index_depths_cluster6 - 
                                     (alpha*combinations[i,1]+
                                        (1-alpha)*combinations[i,2]))))
}
usage_index_mode_6_clusters_combinations1 <- list(combinations[which.min(cluster1_test[-1]),],
                                                combinations[which.min(cluster2_test[-1]),],
                                                combinations[which.min(cluster3_test[-1]),],
                                                combinations[which.min(cluster4_test[-1]),],
                                                combinations[which.min(cluster5_test[-1]),],
                                                combinations[which.min(cluster6_test[-1]),])
usage_index_mode_6_clusters_combinations2 <- list(combinations[which.min(cluster1_test2[-1]),],
                                                combinations[which.min(cluster2_test2[-1]),],
                                                combinations[which.min(cluster3_test2[-1]),],
                                                combinations[which.min(cluster4_test2[-1]),],
                                                combinations[which.min(cluster5_test2[-1]),],
                                                combinations[which.min(cluster6_test2[-1]),])

# 5 clusters: RPD
usage_index_depths_cluster1 <- depth_RPD_index_6_1$data
usage_index_depths_cluster2 <- depth_RPD_index_6_2$data
usage_index_depths_cluster3 <- depth_RPD_index_6_3$data
usage_index_depths_cluster4 <- depth_RPD_index_6_4$data
usage_index_depths_cluster5 <- depth_RPD_index_6_5$data
usage_index_depths_cluster6 <- depth_RPD_index_6_6$data

cluster1_test <- sqrt(sum((usage_index_depths_cluster1 - 
                             (alpha*gb_offers_dist[,1]+
                                (1-alpha)*fic_voice_offers_dist[,1]))^2))
cluster2_test <- sqrt(sum((usage_index_depths_cluster2 - 
                             (alpha*gb_offers_dist[,1]+
                                (1-alpha)*fic_voice_offers_dist[,1]))^2))
cluster3_test <- sqrt(sum((usage_index_depths_cluster3 - 
                             (alpha*gb_offers_dist[,1]+
                                (1-alpha)*fic_voice_offers_dist[,1]))^2))
cluster4_test <- sqrt(sum((usage_index_depths_cluster4 - 
                             (alpha*gb_offers_dist[,1]+
                                (1-alpha)*fic_voice_offers_dist[,1]))^2))
cluster5_test <- sqrt(sum((usage_index_depths_cluster5 - 
                             (alpha*gb_offers_dist[,1]+
                                (1-alpha)*fic_voice_offers_dist[,1]))^2))
cluster6_test <- sqrt(sum((usage_index_depths_cluster6 - 
                             (alpha*gb_offers_dist[,1]+
                                (1-alpha)*fic_voice_offers_dist[,1]))^2))

cluster1_test2 <- max(abs(usage_index_depths_cluster1 - 
                            (alpha*gb_offers_dist[,1]+
                               (1-alpha)*fic_voice_offers_dist[,1])))
cluster2_test2 <- max(abs(usage_index_depths_cluster2 - 
                            (alpha*gb_offers_dist[,1]+
                               (1-alpha)*fic_voice_offers_dist[,1])))
cluster3_test2 <- max(abs(usage_index_depths_cluster3 - 
                            (alpha*gb_offers_dist[,1]+
                               (1-alpha)*fic_voice_offers_dist[,1])))
cluster4_test2 <- max(abs(usage_index_depths_cluster4 - 
                            (alpha*gb_offers_dist[,1]+
                               (1-alpha)*fic_voice_offers_dist[,1])))
cluster5_test2 <- max(abs(usage_index_depths_cluster5 - 
                            (alpha*gb_offers_dist[,1]+
                               (1-alpha)*fic_voice_offers_dist[,1])))
cluster6_test2 <- max(abs(usage_index_depths_cluster6 - 
                            (alpha*gb_offers_dist[,1]+
                               (1-alpha)*fic_voice_offers_dist[,1])))


for(i in 1:nrow(combinations)){
  cluster1_test <- append(cluster1_test, 
                          sqrt(sum((usage_index_depths_cluster1 - 
                                      ((1-alpha)*combinations[i,2]+
                                         alpha*combinations[i,1]))^2)))
  cluster2_test <- append(cluster2_test, 
                          sqrt(sum((usage_index_depths_cluster2 - 
                                      ((1-alpha)*combinations[i,2]+
                                         alpha*combinations[i,1]))^2)))
  cluster3_test <- append(cluster3_test, 
                          sqrt(sum((usage_index_depths_cluster3 - 
                                      ((1-alpha)*combinations[i,2]+
                                         alpha*combinations[i,1]))^2)))
  cluster4_test <- append(cluster4_test, 
                          sqrt(sum((usage_index_depths_cluster4 - 
                                      ((1-alpha)*combinations[i,2]+
                                         alpha*combinations[i,1]))^2)))
  cluster5_test <- append(cluster5_test, 
                          sqrt(sum((usage_index_depths_cluster5 - 
                                      ((1-alpha)*combinations[i,2]+
                                         alpha*combinations[i,1]))^2)))
  cluster6_test <- append(cluster6_test, 
                          sqrt(sum((usage_index_depths_cluster6 - 
                                      ((1-alpha)*combinations[i,2]+
                                         alpha*combinations[i,1]))^2)))
}
for(i in 1:nrow(combinations)){
  cluster1_test2 <- append(cluster1_test2, 
                           max(abs(usage_index_depths_cluster1 - 
                                     (alpha*combinations[i,1]+
                                        (1-alpha)*combinations[i,2]))))
  cluster2_test2 <- append(cluster2_test2, 
                           max(abs(usage_index_depths_cluster2 - 
                                     (alpha*combinations[i,1]+
                                        (1-alpha)*combinations[i,2]))))
  cluster3_test2 <- append(cluster3_test2, 
                           max(abs(usage_index_depths_cluster3 - 
                                     (alpha*combinations[i,1]+
                                        (1-alpha)*combinations[i,2]))))
  cluster4_test2 <- append(cluster4_test2, 
                           max(abs(usage_index_depths_cluster4 - 
                                     (alpha*combinations[i,1]+
                                        (1-alpha)*combinations[i,2]))))
  cluster5_test2 <- append(cluster5_test2, 
                           max(abs(usage_index_depths_cluster5 - 
                                     (alpha*combinations[i,1]+
                                        (1-alpha)*combinations[i,2]))))
  cluster6_test2 <- append(cluster6_test2, 
                           max(abs(usage_index_depths_cluster6 - 
                                     (alpha*combinations[i,1]+
                                        (1-alpha)*combinations[i,2]))))
}
usage_index_RPD_6_clusters_combinations1 <- list(combinations[which.min(cluster1_test[-1]),],
                                                 combinations[which.min(cluster2_test[-1]),],
                                                 combinations[which.min(cluster3_test[-1]),],
                                                 combinations[which.min(cluster4_test[-1]),],
                                                 combinations[which.min(cluster5_test[-1]),],
                                                 combinations[which.min(cluster6_test[-1]),])
usage_index_RPD_6_clusters_combinations2 <- list(combinations[which.min(cluster1_test2[-1]),],
                                                 combinations[which.min(cluster2_test2[-1]),],
                                                 combinations[which.min(cluster3_test2[-1]),],
                                                 combinations[which.min(cluster4_test2[-1]),],
                                                 combinations[which.min(cluster5_test2[-1]),],
                                                 combinations[which.min(cluster6_test2[-1]),])

