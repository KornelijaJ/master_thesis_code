

gb_outliers_prep <- data.frame(user_id = gb_outliers_boxplot,
                            GB_4_FM = 99999,
                            GB_4_mode = 99999,
                            GB_4_RPD = 99999)
voice_outliers_prep <- data.frame(user_id = voice_outliers_boxplot,
                               Voice_6_FM = 99999,
                               Voice_6_mode = 99999,
                               Voice_6_RPD = 99999)
outliers_prep <- merge(gb_outliers_prep, voice_outliers_prep, by = "user_id", all=TRUE)

# gb_offers_prep <- data.frame(user_id = gb_fda$fd$fdnames$reps,
#                                        GB_5_FM = gb_clustering_l2_5$cluster,
#                                        GB_5_mode = gb_clustering_l2_5$cluster,
#                                        GB_5_RPD = gb_clustering_l2_5$cluster)
gb_offers_prep <- data.frame(user_id = gb_fda$fd$fdnames$reps,
                             GB_4_FM = gb_clustering_4_DkBk$cls,
                             GB_4_mode = gb_clustering_4_DkBk$cls,
                             GB_4_RPD = gb_clustering_4_DkBk$cls)

# voice_offers_prep <- data.frame(user_id = voice_fda$fd$fdnames$reps,
#                              Voice_5_FM = voice_clustering_l2_5$cluster,
#                              Voice_5_mode = voice_clustering_l2_5$cluster,
#                              Voice_5_RPD = voice_clustering_l2_5$cluster)
voice_offers_prep <- data.frame(user_id = voice_fda$fd$fdnames$reps,
                                Voice_6_FM = voice_clustering_6_DkBk$cls,
                                Voice_6_mode = voice_clustering_6_DkBk$cls,
                                Voice_6_RPD = voice_clustering_6_DkBk$cls)

usage_offers_prep <- full_join(gb_offers_prep, voice_offers_prep, by = "user_id")
unlimited_voice_prep <- usage_offers_prep %>% filter(is.na(GB_4_FM)) %>% subset(select=-c(GB_4_FM,
                                                                                          GB_4_mode,
                                                                                          GB_4_RPD))
unlimited_gb_prep <- usage_offers_prep %>% filter(is.na(Voice_6_FM)) %>% subset(select=-c(Voice_6_FM,
                                                                                          Voice_6_mode,
                                                                                          Voice_6_RPD))
unlimited_gb <- inner_join(unlimited_voice_prep, gb_outliers_prep, by = "user_id")
unlimited_voice <- inner_join(unlimited_gb_prep, voice_outliers_prep, by = "user_id")

usage_offers <- union(usage_offers_prep, unlimited_gb) %>% union(unlimited_voice) %>%
  filter(!(is.na(GB_4_FM)) & !(is.na(Voice_6_FM)))
nrow(usage_offers)

usage_offers_values <- usage_offers %>%
  mutate(GB_4_FM = ifelse(GB_4_FM==1,18,ifelse(GB_4_FM==2,12,ifelse(GB_4_FM==3,18,ifelse(GB_4_FM==4,8,99999)))),
         GB_4_mode = ifelse(GB_4_mode==1,18,ifelse(GB_4_mode==2,12,ifelse(GB_4_mode==3,18,ifelse(GB_4_mode==4,8,99999)))),
         GB_4_RPD = ifelse(GB_4_RPD==1,18,ifelse(GB_4_RPD==2,10,ifelse(GB_4_RPD==3,18,ifelse(GB_4_RPD==4,6,99999)))),
         Voice_6_FM = ifelse(Voice_6_FM==1,1900,ifelse(Voice_6_FM==2,1400,ifelse(Voice_6_FM==3,800,ifelse(Voice_6_FM==4,500,ifelse(Voice_6_FM==5,1000,ifelse(Voice_6_FM==6,300,99999)))))),
         Voice_6_mode = ifelse(Voice_6_mode==1,1900,ifelse(Voice_6_mode==2,1400,ifelse(Voice_6_mode==3,800,ifelse(Voice_6_mode==4,500,ifelse(Voice_6_mode==5,1100,ifelse(Voice_6_mode==6,300,99999)))))),
         Voice_6_RPD = ifelse(Voice_6_RPD==1,1900,ifelse(Voice_6_RPD==2,1500,ifelse(Voice_6_RPD==3,700,ifelse(Voice_6_RPD==4,500,ifelse(Voice_6_RPD==5,1100,ifelse(Voice_6_RPD==6,400,99999)))))))

usage_offers_values %>% group_by(GB_4_FM, Voice_6_FM) %>% summarise(n=n()) %>% arrange(desc(n))
usage_offers_values %>% group_by(GB_4_mode, Voice_6_mode) %>% summarise(n=n()) %>% arrange(desc(n))
usage_offers_values %>% group_by(GB_4_RPD, Voice_6_RPD) %>% summarise(n=n()) %>% arrange(desc(n))


