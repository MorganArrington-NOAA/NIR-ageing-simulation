##################################################
# Fit PLS model to simulated data

# Packages
library(pls)
library(mdatools)
library(tidyverse)

# Load in data
sim_dat <- read.csv("~/AFSC A&G Contract/Simulation Project/Data/simulated_full_data_0.01.csv") #simulated age data

sim_dat <- sim_dat[,-1]

# Run PLS model
m <- pls(sim_dat[,c(3:921)], sim_dat$known_age, 6, scale = TRUE, cv = 1, info = "Age prediction model")

summary(m$res$cv)

# Look at scores
plotScores(m$res$cal$xdecomp, show.labels = FALSE, cgroup = sim_dat$known_age)


# Plotting
plotXVariance(m)
plotXCumVariance(m)

plot(m)
plotPredictions(m)
plotPredictions(m$res$cv, show.stat = TRUE)

#Extract predictions
sim_dat_pred <- cbind(sim_dat, m$cvres$y.pred)
sim_dat_pred <- sim_dat_pred%>%
  rename(m_Comp_1 = `Comp 1.1`, m_Comp_2 = `Comp 2.1`, m_Comp_3 = `Comp 3.1`, m_Comp_4 = `Comp 4.1` , m_Comp_5 = `Comp 5.1`, m_Comp_6 = `Comp 6.1`)

#Plot for real

ggplot(sim_dat_pred)+
  geom_point(aes(x = known_age, y = round(m_Comp_6), color = as.factor(known_age)), alpha = .5)+
  scale_x_continuous(limits = c(0,10), breaks=seq(0,10,2))+
  scale_y_continuous(limits = c(0,10), breaks=seq(0,10,2))+
  geom_smooth(aes(x = known_age, y = round(m_Comp_6)),method = "lm")+
  geom_text(x=0, y=10, label="RMSE(CV) = 0.095", hjust = 0, size = 3)+
  geom_text(x=0, y=9, label = "RPD = 30.17", size = 3, hjust = 0)+
  geom_text(x=0, y=8, label = expression(paste(r^{2},"= 0.99")), size = 3, hjust = 0)+
  labs(x = "Known age",
       y = "Predicted age",
       subtitle = "Model fit on known ages")+
  theme_classic()

# Error_age
# Run PLS model
m2 <- pls(sim_dat[,c(3:921)], sim_dat$error_age, 10, scale = TRUE, cv = 1, info = "Age prediction model")

summary(m2$res$cv)

# Scores
plotScores(m2$res$cal$xdecomp, show.labels = FALSE, cgroup = sim_dat$known_age)

#Plotting
plotXVariance(m2)
plotXCumVariance(m2)

plot(m2)
plotPredictions(m2)
plotPredictions(m2$res$cv, show.stat = TRUE)

#Extract predictions
sim_dat_pred <- cbind(sim_dat_pred, m2$cvres$y.pred)
sim_dat_pred <- sim_dat_pred%>%
  rename(m2_Comp_1 = `Comp 1.1`, m2_Comp_2 = `Comp 2.1`, m2_Comp_3 = `Comp 3.1`, m2_Comp_4 = `Comp 4.1` , m2_Comp_5 = `Comp 5.1`, m2_Comp_6 = `Comp 6.1`, m2_Comp_7 = `Comp 7.1`, m2_Comp_8 = `Comp 8.1`, m2_Comp_9 = `Comp 9.1`, m2_Comp_10 = `Comp 10.1`)

#Plot for real
# sim_dat_pred$known_age <- as.factor(sim_dat_pred$known_age)
# sim_dat_pred$error_age <- as.factor(sim_dat_pred$error_age)

ggplot(sim_dat_pred)+
  geom_point(aes(x = error_age, y = round(m2_Comp_7), color = as.factor(error_age)), alpha = .5)+
  scale_x_continuous(limits = c(0,13), breaks=seq(0,14,2))+
  scale_y_continuous(limits = c(0,10), breaks=seq(0,10,2))+
  geom_smooth(aes(x = error_age, y = round(m2_Comp_7)),method = "lm")+
  geom_text(x=0, y=10, label="RMSE(CV) = 0.747", hjust = 0, size = 3)+
  geom_text(x=0, y=9, label = "RPD = 3.96", size = 3, hjust = 0)+
  geom_text(x=0, y=8, label = expression(paste(r^{2},"= 0.936")), size = 3, hjust = 0)+
  labs(x = "Simulated age",
       y = "Predicted age",
       subtitle = "Model fit on error ages")+
  theme_classic()

# plot preds against one another
ggplot(sim_dat_pred)+
  geom_point(aes(round(m_Comp_6), round(m2_Comp_7)))+
  labs(x = "Predicted age from known age",
       y = "Predicted age from simulated age estimate")+
  theme_classic()

# difference plot
pred_diff <- sim_dat_pred%>%
  summarise(difference = round(m_Comp_6) - round(m2_Comp_7), count = n())

###########################################
# Do this for error of 0.05
# Load in data
sim_dat <- read.csv("~/AFSC A&G Contract/Simulation Project/Data/simulated_full_data_0.05.csv") #simulated age data

sim_dat <- sim_dat[,-1]

# Run PLS model
m3 <- pls(sim_dat[,c(3:921)], sim_dat$known_age, 6, scale = TRUE, cv = 1, info = "Age prediction model")

summary(m3$res$cv)

#Plotting
plotXVariance(m3)
plotXCumVariance(m3)

plot(m3)
plotPredictions(m3)
plotPredictions(m3$res$cv, show.stat = TRUE)

#Extract predictions
sim_dat_pred <- cbind(sim_dat_pred, m3$cvres$y.pred)
sim_dat_pred <- sim_dat_pred%>%
  rename(m3_Comp_1 = `Comp 1.1`, m3_Comp_2 = `Comp 2.1`, m3_Comp_3 = `Comp 3.1`, m3_Comp_4 = `Comp 4.1` , m3_Comp_5 = `Comp 5.1`, m3_Comp_6 = `Comp 6.1`)

#Plot for real

ggplot(sim_dat_pred)+
  geom_point(aes(x = known_age, y = round(m3_Comp_6), color = as.factor(known_age)), alpha = .5)+
  scale_x_continuous(limits = c(0,10), breaks=seq(0,10,2))+
  scale_y_continuous(limits = c(0,12), breaks=seq(0,12,2))+
  geom_smooth(aes(x = known_age, y = round(m3_Comp_6)),method = "lm")+
  geom_text(x=0, y=10, label="RMSE(CV) = 0.291", hjust = 0, size = 3)+
  geom_text(x=0, y=9, label = "RPD = 9.87", size = 3, hjust = 0)+
  geom_text(x=0, y=8, label = expression(paste(r^{2},"= 0.99")), size = 3, hjust = 0)+
  labs(x = "Known age",
       y = "Predicted age",
       subtitle = "Model fit on known ages")+
  theme_classic()

# Error_age
# Run PLS model
m4 <- pls(sim_dat[,c(3:921)], sim_dat$error_age, 10, scale = TRUE, cv = 1, info = "Age prediction model")

summary(m4$res$cv)

#Plotting
plotXVariance(m4)
plotXCumVariance(m4)

plot(m4)
plotPredictions(m4)
plotPredictions(m4$res$cv, show.stat = TRUE)

#Extract predictions
sim_dat_pred <- cbind(sim_dat_pred, m4$cvres$y.pred)
sim_dat_pred <- sim_dat_pred%>%
  rename(m4Comp_1 = `Comp 1.1`, m4_Comp_2 = `Comp 2.1`, m4_Comp_3 = `Comp 3.1`, m4_Comp_4 = `Comp 4.1` , m4_Comp_5 = `Comp 5.1`, m4_Comp_6 = `Comp 6.1`, m4_Comp_7 = `Comp 7.1`, m4_Comp_8 = `Comp 8.1`, m4_Comp_9 = `Comp 9.1`, m4_Comp_10 = `Comp 10.1`)

#Export model predictions
write.csv(sim_dat_pred, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Data/model_predictions_doublereaddata.csv")

#Plot for real
# sim_dat_pred$known_age <- as.factor(sim_dat_pred$known_age)
# sim_dat_pred$error_age <- as.factor(sim_dat_pred$error_age)

ggplot(sim_dat_pred)+
  geom_point(aes(x = error_age, y = round(m4_Comp_10), color = as.factor(known_age)), alpha = .5)+
  scale_x_continuous(limits = c(0,13), breaks=seq(0,14,2))+
  scale_y_continuous(limits = c(0,13), breaks=seq(0,14,2))+
  geom_smooth(aes(x = error_age, y = round(m4_Comp_10)),method = "lm")+
  geom_text(x=0, y=10, label="RMSE(CV) = 0.757", hjust = 0, size = 3)+
  geom_text(x=0, y=9, label = "RPD = 3.90", size = 3, hjust = 0)+
  geom_text(x=0, y=8, label = expression(paste(r^{2},"= 0.935")), size = 3, hjust = 0)+
  labs(x = "Simulated age",
       y = "Predicted age",
       subtitle = "Model fit on error ages")+
  theme_classic()

# plot preds against one another
ggplot(sim_dat_pred)+
  geom_point(aes(round(m3_Comp_6), round(m4_Comp_10)), alpha = 0.05)+
  labs(x = "Predicted age from known age",
       y = "Predicted age from simulated age estimate")+
  scale_x_continuous(limits = c(0,13), breaks=seq(0,14,2))+
  scale_y_continuous(limits = c(0,13), breaks=seq(0,14,2))+
  theme_classic()

# difference plot
pred_diff <- sim_dat_pred%>%
  summarise(difference = round(m_Comp_6) - round(m2_Comp_7), count = n())

# make bar plot
## need to calculate different between preds and known age for each
pred_error1 <- sim_dat_pred%>%
  transmute(pred_error = known_age - round(m3_Comp_6))

pred_error1_counts <- pred_error1%>%
  group_by(pred_error)%>%
  summarize(count = n())

ggplot(pred_error1)+
  geom_bar(aes(x = pred_error))+
  scale_y_continuous(limits = c(0,2000), breaks = seq(0,2000,500))+
  theme_classic()

pred_error2 <- sim_dat_pred%>%
  transmute(pred_error = known_age - round(m4_Comp_10))

pred_error2_counts <- pred_error2%>%
  group_by(pred_error)%>%
  summarize(count = n())

ggplot(pred_error2)+
  geom_bar(aes(x = pred_error))+
  theme_classic()

# Explore model error
plotXResiduals(m, ncomp = 6)
plotYResiduals(m, ncomp = 6)

plotXResiduals(m2, ncomp = 7)
plotXResiduals(m3, ncomp = 6)
plotXResiduals(m4, ncomp = 10)

plot(m$coeffs$se)
plot(m2$coeffs$se)
plot(m3$coeffs$se)
plot(m4$coeffs$se)

plot(m$coeffs)
