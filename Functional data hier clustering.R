
library(fda.usc)
library(dendextend)
## Different distances

# L2 norm
L2norm_gb <- norm.fd(gb_fda$fd)
L2norm_voice <- norm.fd(voice_fda$fd)
L2norm_usage_index_05 <- norm.fd(usage_index_05_fda$fd)
L2norm_usage_index_06 <- norm.fd(usage_index_06_fda$fd)

######################## GB Basis #######################################
dist_gb <- dist(L2norm_gb)
hclust_gb <- hclust(dist_gb)
dend_gb <- as.dendrogram(hclust_gb)

plot_gb_4 <- dend_gb %>% color_branches(k = 4) %>% set("branches_lwd", c(2,2,2,2,2)) %>%
  set("branches_lty", c(1,1,1,1,1))
plot(plot_gb_4)
plot_gb_5 <- dend_gb %>% color_branches(k = 5) %>% set("branches_lwd", c(2,2,2,2,2)) %>%
  set("branches_lty", c(1,1,1,1,1))
plot(plot_gb_5)

######################## Voice Basis #######################################
dist_voice <- dist(L2norm_voice)
hclust_voice <- hclust(dist_voice)
dend_voice <- as.dendrogram(hclust_voice)
plot_voice_5 <- dend_voice %>% color_branches(k = 5) %>% set("branches_lwd", c(2,2,2,2,2,2)) %>%
  set("branches_lty", c(1,1,1,1,1,1))
plot(plot_voice_5)
plot_voice_6 <- dend_voice %>% color_branches(k = 6) %>% set("branches_lwd", c(2,2,2,2,2,2)) %>%
  set("branches_lty", c(1,1,1,1,1,1))
plot(plot_voice_6)

######################## Usage Index Basis ####################################
dist_usage_index_05 <- dist(L2norm_usage_index_05)
hclust_usage_index_05 <- hclust(dist_usage_index_05)
dend_usage_index_05 <- as.dendrogram(hclust_usage_index_05)
plot_usage_index_05_5 <- dend_usage_index_05 %>% color_branches(k = 5) %>% set("branches_lwd", c(2,2,2,2,2,2)) %>%
  set("branches_lty", c(1,1,1,1,1,1))
plot(plot_usage_index_05_5)
plot_usage_index_05_6 <- dend_usage_index_05 %>% color_branches(k = 6) %>% set("branches_lwd", c(2,2,2,2,2,2)) %>%
  set("branches_lty", c(1,1,1,1,1,1))
plot(plot_usage_index_05_6)

dist_usage_index_06 <- dist(L2norm_usage_index_06)
hclust_usage_index_06 <- hclust(dist_usage_index_06)
dend_usage_index_06 <- as.dendrogram(hclust_usage_index_06)
plot_usage_index_06_5 <- dend_usage_index_06 %>% color_branches(k = 5) %>% set("branches_lwd", c(2,2,2,2,2,2)) %>%
  set("branches_lty", c(1,1,1,1,1,1))
plot(plot_usage_index_06_5)
plot_usage_index_06_6 <- dend_usage_index_06 %>% color_branches(k = 6) %>% set("branches_lwd", c(2,2,2,2,2,2)) %>%
  set("branches_lty", c(1,1,1,1,1,1))
plot(plot_usage_index_06_6)












