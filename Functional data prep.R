
library(readxl)
library(sqldf)
library(dplyr)
library(tidyverse)
library(data.table)
library(ggplot2)
library(reshape2)
library(tidyr)

library(fda)
library(fda.usc)

## FUNCTIONAL DATA PREPARATION
gb_func <- monthly_data %>% anti_join(data.frame(user_id = as.double(gb_outliers_boxplot)), by = "user_id") %>%
  subset(select=c(month, user_id, data_gb_used)) %>%
  pivot_wider(
    names_from = user_id, 
    values_from = data_gb_used, 
    names_expand = TRUE
  )

voice_func <- monthly_data %>% anti_join(data.frame(user_id = as.double(voice_outliers_boxplot)), by = "user_id") %>%
  subset(select=c(month, user_id, voice_outgoing_duration_in_minutes)) %>%
  pivot_wider(
    names_from = user_id, 
    values_from = voice_outgoing_duration_in_minutes, 
    names_expand = TRUE
  )

usage_index_05_func <- monthly_data %>% 
  anti_join(data.frame(user_id = as.double(usage_index_05_outliers_boxplot)), by = "user_id") %>%
  subset(select=c(month, user_id, usage_index_05)) %>%
  pivot_wider(
    names_from = user_id, 
    values_from = usage_index_05, 
    names_expand = TRUE
  )

#########################
gb_smooth <- as.matrix(gb_func[,-1])
voice_smooth <- as.matrix(voice_func[,-1])
usage_index_05_smooth <- as.matrix(usage_index_05_func[,-1])

######################## GB Basis #######################################
monthRng <- c(0,35)
lambda <- 1e2
gb_basis  <- create.fourier.basis(monthRng, nbasis=21)
dayfdPar <- fdPar(gb_basis, lambda=lambda)
gb_fda <- smooth.basis(0:35, gb_smooth, dayfdPar)
plot(gb_fda)

#### Smoothed data VS original (raw) data
par(mfrow=c(3,2))
fd_gb <- gb_fda$fd
for(i in 1:6){
  plot(gb_smooth[,i], xlab = 'month', ylab = 'value')
  lines(fd_gb[i],col=2)
  readline("Press <return to continue")
}

######################## Voice Basis #######################################
lambda_voice <- 1e3
voice_basis  <- create.fourier.basis(monthRng, nbasis=21)
dayfdPar_voice <- fdPar(voice_basis, lambda=lambda_voice)
voice_fda <- smooth.basis(0:35, voice_smooth, dayfdPar_voice)
plot(voice_fda)

layout(matrix(c(1,2), nrow=1, byrow=TRUE))
plot(voice_fda)
plot(gb_fda)

#### Smoothed data VS original (raw) data
par(mfrow=c(3,2))
fd_voice <- voice_fda$fd
for(i in 1:6){
  plot(voice_smooth[,i], xlab = 'month', ylab = 'value')
  lines(fd_voice[i],col=2)
  readline("Press <return to continue")
}

######################## Index Basis ####################################
lambda_usage_index_05 <- 1e3
usage_index_basis  <- create.fourier.basis(monthRng, nbasis=21)
dayfdPar_usage_index_05 <- fdPar(usage_index_basis, lambda=lambda_usage_index_05)
usage_index_05_fda <- smooth.basis(0:35, usage_index_05_smooth, dayfdPar_usage_index_05)
plot(usage_index_05_fda)

lambda_usage_index_06 <- 1e3
dayfdPar_usage_index_06 <- fdPar(usage_index_basis, lambda=lambda_usage_index_06)
usage_index_06_fda <- smooth.basis(0:35, usage_index_06_smooth, dayfdPar_usage_index_06)
plot(usage_index_06_fda)

#### Smoothed data VS original (raw) data
par(mfrow=c(3,2))
fd_usage_index_05 <- usage_index_05_fda$fd
for(i in 1:6){
  plot(usage_index_05_smooth[,i], xlab = 'month', ylab = 'value')
  lines(fd_usage_index_05[i],col=2)
  readline("Press <return to continue")
}

fd_usage_index_06 <- usage_index_06_fda$fd
for(i in 1:6){
  plot(usage_index_06_smooth[,i], xlab = 'month', ylab = 'value')
  lines(fd_usage_index_06[i],col=2)
  readline("Press <return to continue")
}

######################## Offers Basis ####################################

voice_offers_smooth <- as.matrix(voice_offers_prep)
gb_offers_smooth <- as.matrix(data_offers_prep)

voice_offers_wo_unlim_smooth <- as.matrix(voice_offers_prep_wo_unlim)
gb_offers_wo_unlim_smooth <- as.matrix(data_offers_prep_wo_unlim)

offers_basis  <- create.fourier.basis(monthRng, nbasis=35)
dayfdPar_offers <- fdPar(offers_basis)

voice_offers_fda <- smooth.basis(0:35, voice_offers_smooth, dayfdPar_offers)
plot(voice_offers_fda)

voice_offers_wo_unlim_fda <- smooth.basis(0:35, voice_offers_wo_unlim_smooth, dayfdPar_offers)
plot(voice_offers_wo_unlim_fda)

#### Smoothed data VS original (raw) data
par(mfrow=c(3,2))
fd_voice_offers <- voice_offers_fda$fd
for(i in 1:6){
  plot(voice_offers_smooth[,i], xlab = 'month', ylab = 'value')
  lines(fd_voice_offers[i],col=2)
  readline("Press <return to continue")
}

gb_offers_fda <- smooth.basis(0:35, gb_offers_smooth, dayfdPar_offers)
plot(gb_offers_fda)

gb_offers_wo_unlim_fda <- smooth.basis(0:35, gb_offers_wo_unlim_smooth, dayfdPar_offers)
plot(gb_offers_wo_unlim_fda)

#### Smoothed data VS original (raw) data
par(mfrow=c(3,2))
fd_gb_offers <- gb_offers_fda$fd
for(i in 1:6){
  plot(gb_offers_smooth[,i], xlab = 'month', ylab = 'value')
  lines(fd_gb_offers[i],col=2)
  readline("Press <return to continue")
}

# fiktyvūs voice oferiai
fic_voice_offers_wo_unlim_smooth <- as.matrix(fic_voice_offers_prep_wo_unlimited)
fic_voice_offers_wo_unlim_fda <- smooth.basis(0:35, fic_voice_offers_wo_unlim_smooth, dayfdPar_offers)
plot(fic_voice_offers_wo_unlim_fda)

