# plotting for simulation model results
library(tidyverse)
library(plotly)
library(stringr)
library(mdatools)
library(Metrics)
library(MLmetrics)

# Load data
preds_df <- read.csv("~/AFSC A&G Contract/Simulation Project/Data/all_model_preds_0.05.csv") #simulated age data

# Now that we have data.frame of preds from each Iter, we can pivot longer to plot preds vs. "True Age"
preds_df_L <- pivot_longer(round(preds_df),  cols=c(3:ncol(preds_df)), names_to="ref_type", values_to="prediction", names_prefix="X")

# Clean up data

preds_df_L <- preds_df_L %>%
  transmute(known_age = `age_df$known_age`, error_age = `age_df$error_age`, ref_type = case_when(str_detect(ref_type, "T") ~ "T", str_detect(ref_type, "E") ~ "E"), prediction)

preds_df_L$known_age <- as.factor(preds_df_L$known_age)
preds_df_L$error_age <- as.factor(preds_df_L$error_age)
preds_df_L$ref_type <- as.factor(preds_df_L$ref_type)

# summarize for plotting

pred_summ <- preds_df_L %>% 
  group_by(known_age, ref_type) %>%
  summarize(mean_pred = mean(prediction), se_pred = sd(prediction)) #for SE - /sqrt(n())

# plot predictions relative to known age

ggplot(filter(preds_df_L, ref_type == "T"))+
  geom_point(aes(as.numeric(known_age), prediction, color = ref_type), alpha = 0.3)+
  geom_abline(slope = 1, intercept = 0)+
  scale_color_manual(values = "darkblue")+
  scale_fill_manual(values = "darkblue")+
  scale_x_continuous(lim = c(0,12), breaks=seq(0,12,1))+
  scale_y_continuous(lim = c(0,12), breaks=seq(0,12,1))+
  labs(x = "True age (years)",
       y = "Predicted age (years)")+
  theme_classic()

# plot predictions relative to error age

ggplot(filter(preds_df_L, ref_type == "E"))+
  geom_point(aes(as.numeric(error_age), prediction, color = ref_type), alpha = 0.3)+
  geom_abline(slope = 1, intercept = 0)+
  scale_color_manual(values =  "cornflowerblue")+
  scale_fill_manual(values =  "cornflowerblue")+
  scale_x_continuous(lim = c(0,12), breaks=seq(1,12,1))+
  scale_y_continuous(lim = c(0,12), breaks=seq(1,12,1))+
  labs(x = "Simulated age estimate (years)",
       y = "Predicted age (years)")+
  theme_classic()

# full prediction plot for error f 0.01

ggplot(preds_df_L)+
  geom_point(aes(as.numeric(known_age), prediction, color = ref_type), alpha = 0.3)+
  geom_line(data = pred_summ, aes(x = as.numeric(known_age), y = mean_pred, color = ref_type))+
  geom_ribbon(data = pred_summ, aes(x = as.numeric(known_age), ymin = mean_pred - se_pred, ymax = mean_pred + se_pred, fill = ref_type), alpha = 0.3)+
  geom_abline(slope = 1, intercept = 0, color = "grey50")+
  scale_color_manual(values = c("cornflowerblue", "darkblue"))+
  scale_fill_manual(values = c("cornflowerblue", "darkblue"))+
  scale_x_continuous(lim = c(0,12), breaks=seq(1,12,1))+
  scale_y_continuous(lim = c(0,12), breaks=seq(1,12,1))+
  labs(x = "True age (years)",
       y = "Predicted age (years)")+
  theme_classic()


# Calculate RMSE and R2 as side plots to show range
RMSE <- preds_df %>%
  summarise(rmse_t = across(3:1002, ~rmse(`age_df$known_age`,round(.))), rmse_e = across(1003:2002, ~rmse(`age_df$error_age`,round(.))))

RMSE <- t(RMSE)
RMSE <- data.frame(cbind(rep(c("T", "E"), each = 1000), RMSE))
colnames(RMSE) <- c("ref_type", "RMSE")
RMSE$ref_type <- as.factor(RMSE$ref_type)
RMSE$RMSE <- as.numeric(RMSE$RMSE)

# Plot
ggplot(RMSE)+
  geom_boxplot(aes(x = ref_type, y = RMSE, color = ref_type))+
  scale_color_manual(values = c("cornflowerblue", "darkblue"))+
  labs(x = "Reference age type used for model calibration",
       y = "RMSE (years)")+
  theme_classic()


# Calculate R2 
R2 <- preds_df %>%
  summarise(R2_t = across(3:1002, ~R2_Score(`age_df$known_age`,round(.))), R2_e = across(1003:2002, ~R2_Score(`age_df$error_age`,round(.))))

R2 <- t(R2)
R2 <- data.frame(cbind(rep(c("T", "E"), each = 1000), R2))
colnames(R2) <- c("ref_type", "R2")
R2$ref_type <- as.factor(R2$ref_type)
R2$R2 <- as.numeric(R2$R2)

# Plot
ggplot(R2)+
  geom_boxplot(aes(x = ref_type, y = R2, color = ref_type))+
  scale_color_manual(values = c("cornflowerblue", "darkblue"))+
  labs(x = "Reference age type used for model calibration",
       y = "R2 (years)")+
  theme_classic()

# Calcuate RMSE and R2 for both model types relative to true age
RMSE <- preds_df %>%
  summarise(rmse_t = across(3:1002, ~rmse(`age_df$known_age`,round(.))), rmse_e = across(1003:2002, ~rmse(`age_df$known_age`,round(.))))

RMSE <- t(RMSE)
RMSE <- data.frame(cbind(rep(c("T", "E"), each = 1000), RMSE))
colnames(RMSE) <- c("ref_type", "RMSE")
RMSE$ref_type <- as.factor(RMSE$ref_type)
RMSE$RMSE <- as.numeric(RMSE$RMSE)

# Plot
ggplot(RMSE)+
  geom_boxplot(aes(x = ref_type, y = RMSE, color = ref_type))+
  scale_color_manual(values = c("cornflowerblue", "darkblue"))+
  labs(x = "Reference age type used for model calibration",
       y = "RMSE (years)")+
  theme_classic()


# Calculate R2 
R2 <- preds_df %>%
  summarise(R2_t = across(3:1002, ~R2_Score(`age_df$known_age`,round(.))), R2_e = across(1003:2002, ~R2_Score(`age_df$known_age`,round(.))))

R2 <- t(R2)
R2 <- data.frame(cbind(rep(c("T", "E"), each = 1000), R2))
colnames(R2) <- c("ref_type", "R2")
R2$ref_type <- as.factor(R2$ref_type)
R2$R2 <- as.numeric(R2$R2)

# Plot
ggplot(R2)+
  geom_boxplot(aes(x = ref_type, y = R2, color = ref_type))+
  scale_color_manual(values = c("cornflowerblue", "darkblue"))+
  labs(x = "Reference age type used for model calibration",
       y = "R2 (years)")+
  theme_classic()