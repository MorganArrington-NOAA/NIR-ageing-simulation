# plotting for simulation model results
library(dplyr)
library(tidyr)
library(ggplot2)
library(mdatools)
library(Metrics)
library(MLmetrics)
# 
# library(plotly)
# library(stringr)
# 

# Load data
preds_df <- read.csv("C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Data/part 3 preds_indageerror_iter10_2013.csv") #simulated data

preds_df <- preds_df[,-1]

# Fit model to all data with original age as baseline


og_mod <- pls(all_dat[, c(7:506)], all_dat$Age, scale = TRUE, ncomp = 10, cv = 50)

# Now want to turn this data into long format for plotting.

# first need to turn jitter_age columns into one long format column
# need to join by row so that each specimen repeats 500 times
jitter_age <- preds_df[,c(8:17)] #change this depending on iter
jitter_age <- pivot_longer(jitter_age, cols=c(1:ncol(jitter_age)), names_to="iter_num", values_to = "jitter_age", names_prefix="jitter_age_")

# shorten preds_df without jitter_age
preds_df <- preds_df[,-c(8:17)] #change depeding on Iter

# pivot_longer pred data
preds_df_L <- pivot_longer(preds_df,  cols=c(8:ncol(preds_df)), names_to="iter_num", values_to="prediction", names_prefix="preds_")

# rejoin 
preds_df_L <- cbind(jitter_age$jitter_age, preds_df_L)

# Clean up data
preds_df_L <- preds_df_L %>%
  transmute(ID, Code, original_age = Age, resampled_age = `jitter_age$jitter_age`, iter_num, prediction)

# plot predictions relative to original age from og_mod
ggplot(all_dat)+
  geom_point(aes(Age, og_mod$cvres$y.pred[,10,1]), alpha = 0.3)+
  geom_abline(slope = 1, intercept = 0)+
  #geom_smooth(aes(x = Age, y = og_mod$cvres$y.pred[,10,1]),method = "lm", color = "red")+
  geom_text(x=0, y=13, label = "RMSE = 0.574", size = 3, hjust = 0)+
  geom_text(x=0, y=12, label = expression(paste(r^{2},"= 0.904")), size = 3, hjust = 0)+
  scale_x_continuous(lim = c(0,13), breaks=seq(0,13,1))+
  scale_y_continuous(lim = c(0,13), breaks=seq(0,13,1))+
  labs(x = "Original age (years)",
       y = "Predicted age (years)")+
  theme_classic()

og_mod$cvres$r2[1,10]
og_mod$cvres$rmse[1,10]

# plot original vs. resampled age
ggplot(preds_df_L)+
  geom_point(aes(original_age, resampled_age), alpha = 0.3)+
  geom_abline(slope = 1, intercept = 0)+
  scale_x_continuous(lim = c(0,18), breaks=seq(0,18,1))+
  scale_y_continuous(lim = c(0,18), breaks=seq(0,18,1))+
  labs(x = "Original age (years)",
       y = "Resampled age (years)")+
  theme_classic()

# frequency table EXTRA
library(FSA)

ab.ale <- ageBias(resampled_age~original_age,data=preds_df_L) #col.lab="Resampled Age",
                  #row.lab="Original Age")

summary(ab.ale,what="symmetry")

  
# plot predictions relative to resampled_age

ggplot(preds_df_L)+
  geom_point(aes(as.numeric(resampled_age), prediction), alpha = 0.3)+
  geom_abline(slope = 1, intercept = 0)+
  scale_x_continuous(lim = c(0,18), breaks=seq(0,18,1))+
  scale_y_continuous(lim = c(0,18), breaks=seq(0,18,1))+
  labs(x = "Resampled age (years)",
       y = "Predicted age (years)")+
  theme_classic()


# full prediction plot
# summarize for plotting
# what makes the most sense for summarizing.... I guess just prediction by resampled age? Yes, because the idea is if the sample was actually a different age, the resampled age might hit it at some point. Should it be summized by specimen # as well?
og_preds <- data.frame(all_dat$Age, og_mod$cvres$y.pred[,10,1])

pred_summ_og <- og_preds%>%
  transmute(Age = `all_dat.Age`, prediction = `og_mod.cvres.y.pred...10..1.`)%>%
  group_by(Age)%>%
  summarize(mean_pred = mean(prediction), se_pred = sd(prediction)) #for SE - /sqrt(n())

pred_summ <- preds_df_L %>%
  group_by(resampled_age) %>%
  summarize(mean_pred = mean(prediction), se_pred = sd(prediction)) #for SE - /sqrt(n())

ggplot(pred_summ)+
  # geom_point(aes(as.numeric(known_age), prediction, color = ref_type), alpha = 0.3)+
  geom_line(data = pred_summ, aes(x = as.numeric(resampled_age), y = mean_pred))+
  geom_ribbon(data = pred_summ, aes(x = as.numeric(resampled_age), ymin = mean_pred - se_pred, ymax = mean_pred + se_pred), alpha = 0.3)+
  geom_line(data = pred_summ_og, aes(x = as.numeric(Age), y = mean_pred), color = "red")+
  geom_ribbon(data = pred_summ_og, aes(x = as.numeric(Age), ymin = mean_pred - se_pred, ymax = mean_pred + se_pred), color = "red", fill = "red", alpha = 0.3)+
  geom_abline(slope = 1, intercept = 0, color = "grey50")+
  scale_x_continuous(lim = c(0,14), breaks=seq(1,14,1))+
  scale_y_continuous(lim = c(0,14), breaks=seq(1,14,1))+
  
  labs(x = "Reference Age (years)",
       y = "Predicted age (years)")+
  theme_classic()


# Calculate RMSE and R2 as side plots to show range
RMSE <- preds_df_L %>%
  group_by(iter_num) %>%
  summarise(rmse = rmse(resampled_age,prediction))

# Plot
ggplot(RMSE)+
  geom_boxplot(aes(x = "", y = rmse))+
  labs(x = " ",
    y = "RMSE (years)")+
  #scale_y_continuous(lim = c(.7,1), breaks=seq(.7,1,.05))+
  geom_hline(yintercept = og_mod$cvres$rmse[1,10], color = "red")+ #10 components selected
  theme_classic()


# Calculate R2 
R2 <- preds_df_L %>%
  group_by(iter_num)%>%
  summarise(R2 = R2_Score(resampled_age,prediction))

# Plot
ggplot(R2)+
  geom_boxplot(aes(x = "", y = R2))+
  labs(x = " ",
       y = "R2 (years)")+
  geom_hline(yintercept = og_mod$cvres$r2[1,10], color = "red")+ #10 components selected
  #scale_y_continuous(lim = c(.9,1), breaks=seq(.9,1,.01))+
  theme_classic()

