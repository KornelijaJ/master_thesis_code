
# Monthly usage index: \alpha * GB + (1 - \alpha) * Voice
# choice of \alpha should be based on expertise evaluation
# maybe choosing \alpha can depend on that how much value GB and Voice has in monthly usage?

monthly_data <- data.frame(non_0_usage_per_month_df) %>%
  subset(select=c(month, user_id, data_gb_used,sms_outgoing_number_of_messages,voice_outgoing_duration_in_minutes))

# Let's normalize GB and Voice usage by using Standard scaling (scales data to be with mean 0 and variance 1)
standardize_monthly_data <- monthly_data %>%
  mutate(standardize_gb_used = normalize(data_gb_used, method = "range", range = c(0,1)),
         standardize_voice_min = normalize(voice_outgoing_duration_in_minutes, method = "range", range = c(0,1)),
         gb_rate = round(standardize_gb_used/(standardize_gb_used+standardize_voice_min),4))
summary(standardize_monthly_data$standardize_gb_used)
summary(standardize_monthly_data$standardize_gb_used)
summary(standardize_monthly_data$gb_rate)

standardize_monthly_data %>% group_by(gb_rate) %>% summarise(n=n()) %>% arrange(desc(n))
mean(standardize_monthly_data$gb_rate)
median(standardize_monthly_data$gb_rate)

round(sum(standardize_monthly_data$standardize_gb_used)/sum(standardize_monthly_data$standardize_gb_used+standardize_monthly_data$standardize_voice_min),4)

alpha05 = 0.5
monthly_data <- monthly_data %>%
  mutate(usage_index_05 = alpha05*data_gb_used + (1-alpha05)*voice_outgoing_duration_in_minutes)







