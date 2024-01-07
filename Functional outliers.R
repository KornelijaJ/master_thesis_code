

# GB Outliers
gb_fts <- fts(x=0:35, y = gb_smooth, xname = "Month", yname = "GB")

gb_outliers_trim <- foutliers(gb_fts, method = "depth.trim")$outliers
gb_outliers_pond <- foutliers(gb_fts, method = "depth.pond")$outliers
gb_outliers_lrt <- foutliers(gb_fts, method = "lrt")$outliers

gb_outliers_boxplot <- gb_fda$fd$fdnames$reps[boxplot(gb_fda$fd)$outpoint]
boxplot(gb_fda$fd)

# outliers plot
gb_df <- data.frame(user_id = gb_fda$fd$fdnames$reps)
robMah_outliers_gb_df <- data.frame(user_id = as.character(robMah_outliers_gb),
                                    outlier = 1)
joined_robMah_outliers <- left_join(gb_df,
          robMah_outliers_gb_df,
          by = "user_id")
joined_robMah_outliers[is.na(joined_robMah_outliers)] = 2

plot(gb_fda$fd);
lines(gb_fda$fd,col=joined_robMah_outliers[,2],lwd=2,lty=1)

# Voice Outliers
voice_fts <- fts(x=0:35, y = voice_smooth, xname = "Month", yname = "GB")

voice_outliers_FM <- foutliers(voice_fts, method = "lrt", dfunc = depth.FM)$outliers
voice_outliers_mode <- foutliers(voice_fts, method = "lrt", dfunc = depth.mode)$outliers
voice_outliers_RPD <- foutliers(voice_fts, method = "lrt", dfunc = depth.RPD)$outliers
voice_outliers_RPD <- foutliers(voice_fts, method = "lrt", dfunc = depth.RP)$outliers

voice_outliers_boxplot <- voice_fda$fd$fdnames$reps[boxplot(voice_fda$fd)$outpoint]
boxplot(voice_fda$fd)

# Usage index Outliers
usage_index_05_fts <- fts(x=0:35, y = usage_index_05_smooth, xname = "Month", yname = "Usage Index")
usage_index_06_fts <- fts(x=0:35, y = usage_index_06_smooth, xname = "Month", yname = "Usage Index")

usage_index_05_outliers_boxplot <- usage_index_05_fda$fd$fdnames$reps[boxplot(usage_index_05_fda$fd)$outpoint]
usage_index_06_outliers_boxplot <- usage_index_06_fda$fd$fdnames$reps[boxplot(usage_index_06_fda$fd)$outpoint]
boxplot(usage_index_05_fda$fd)
boxplot(usage_index_06_fda$fd)





