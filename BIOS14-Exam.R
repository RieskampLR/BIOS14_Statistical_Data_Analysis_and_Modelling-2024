# BIOS14 Exam

# Total body length: tbl
# Fore wing length: fwl
# Hind wing length: hwl
# Copulation success: cop
# Species: sp

# Imports ####

library(ggplot2)
library(dplyr)
library(MASS)

# Data handling and checks: ####

# Load data frames
CSf = read.csv("C:/Users/lea/OneDrive/Dokumente/UniLund/BIOS14/ExamBIOS14/female_CS.csv")
CSm = read.csv("C:/Users/lea/OneDrive/Dokumente/UniLund/BIOS14/ExamBIOS14/male_CS.csv")
CVf = read.csv("C:/Users/lea/OneDrive/Dokumente/UniLund/BIOS14/ExamBIOS14/female_CV.csv")
CVm = read.csv("C:/Users/lea/OneDrive/Dokumente/UniLund/BIOS14/ExamBIOS14/male_CV.csv")

# Combine into 1 data frame
all_data <- bind_rows(CSf, CSm, CVf, CVm)

# Remove irrelevant columns
all_data <- all_data[, !(colnames(all_data) %in% c("year", "id", "marked", "abl", "thorl", "thorw", "fpl", "fpw"))]

# Check for missing values
any(is.na(all_data))
# --> no NA values

# Summary statistics: ####

summary(all_data)
all_data %>%
  group_by(sp, sex) %>%
  summarise(count = n())
table(all_data$cop)

# Data exploration: ####

# Basic GLMs for tbl, fwl, and hwl
glm_tbl <- glm(cop ~ tbl, family = binomial, data = all_data)
glm_fwl <- glm(cop ~ fwl, family = binomial, data = all_data)
glm_hwl <- glm(cop ~ hwl, family = binomial, data = all_data)
summary(glm_tbl) # --> no significant effect of tbl on cop (p>0.05)
summary(glm_fwl) # --> significant effect of fwl on cop (p<0.05)
summary(glm_hwl) # --> significant effect of hwl on cop (p<0.05)

# GLMs with lifespan as a covariate
glm_tbl_lifespan <- glm(cop ~ tbl + lifespan, family = binomial, data = all_data)
glm_fwl_lifespan <- glm(cop ~ fwl + lifespan, family = binomial, data = all_data)
glm_hwl_lifespan <- glm(cop ~ hwl + lifespan, family = binomial, data = all_data)
summary(glm_tbl_lifespan) # --> no significant effect of tbl on cop (p>0.05)
summary(glm_fwl_lifespan) # --> significant effect of fwl on cop (p<0.05)
summary(glm_hwl_lifespan) # --> significant effect of hwl on cop (p<0.05)
# --> lifespan has no significant effect in any of the models (p>0.05)

# Model comparison with ANOVA
anova(glm_tbl, glm_tbl_lifespan, test = "Chisq")
anova(glm_fwl, glm_fwl_lifespan, test = "Chisq")
anova(glm_hwl, glm_hwl_lifespan, test = "Chisq")
# --> no significant difference between models in any case (p>0.05)

# GLMs with species as a factor
glm_tbl_sp <- glm(cop ~ tbl * sp, family = binomial, data = all_data)
glm_fwl_sp <- glm(cop ~ fwl * sp, family = binomial, data = all_data)
glm_hwl_sp <- glm(cop ~ hwl * sp, family = binomial, data = all_data)
summary(glm_tbl_sp) # --> significant effect of tbl on cop (p<0.05)
summary(glm_fwl_sp) # --> significant effect of fwl on cop (p<0.05)
summary(glm_hwl_sp) # --> significant effect of hwl on cop (p<0.05)
# sp has no significant effect in the tbl and fwl models (p>0.05)
# but has a significant effect and interaction in the hwl model (p<0.05)

# GLMs with sex as a factor
glm_tbl_sex <- glm(cop ~ tbl * sex, family = binomial, data = all_data)
glm_fwl_sex <- glm(cop ~ fwl * sex, family = binomial, data = all_data)
glm_hwl_sex <- glm(cop ~ hwl * sex, family = binomial, data = all_data)
summary(glm_tbl_sex) # --> no significant effect of tbl on cop (p>0.05)
summary(glm_fwl_sex) # --> significant effect of fwl on cop (p<0.05)
summary(glm_hwl_sex) # --> significant effect of hwl on cop (p<0.05)
# sex has no significant effect in any of the models (p>0.05)

# Extract values for selected models: ####

# Coefficients & p-values
coef_tbl <- summary(glm_tbl)$coef
exp_coef_tbl <- exp(coef_tbl["tbl", "Estimate"]) - 1
p_value_tbl <- coef_tbl["tbl", "Pr(>|z|)"]
# --> -0.03686729 & 0.074
coef_fwl <- summary(glm_fwl)$coef
exp_coef_fwl <- exp(coef_fwl["fwl", "Estimate"]) - 1
p_value_fwl <- coef_fwl["fwl", "Pr(>|z|)"]
# --> 0.3233487 & 2.5e-53
coef_hwl_sp <- summary(glm_hwl_sp)$coef
exp_coef_hwl_spCS <- exp(coef_hwl_sp["hwl", "Estimate"]) - 1
p_value_hwl_spCS <- coef_hwl_sp["hwl", "Pr(>|z|)"]
# --> 0.2723689 & 2.29e-28
exp_coef_hwl_spCV <- exp(coef_hwl_sp["hwl", "Estimate"] + coef_hwl_sp["hwl:spCV", "Estimate"]) - 1
# --> 0.4123115
exp_coef_hwl_sp_sp <- exp(coef_hwl_sp["spCV", "Estimate"]) - 1
p_value_hwl_sp_sp <- coef_hwl_sp["spCV", "Pr(>|z|)"]
# --> -0.9720979 & 0.038
exp_coef_hwl_sp_int <- exp(coef_hwl_sp["hwl:spCV", "Estimate"]) - 1
p_value_hwl_sp_int <- coef_hwl_sp["hwl:spCV", "Pr(>|z|)"]
# --> 0.1099858 & 0.049

# Pseudo-R²
pseudo_r2_tbl <- 1 - (glm_tbl$deviance / glm_tbl$null.deviance)
# --> 0.0008184671
pseudo_r2_fwl <- 1 - (glm_fwl$deviance / glm_fwl$null.deviance)
# --> 0.06680286
pseudo_r2_hwl_sp <- 1 - (glm_hwl_sp$deviance / glm_hwl_sp$null.deviance)
# --> 0.05604784


# Plots: ####

# tbl
ggplot(all_data, aes(x = tbl, y = cop)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) + 
  geom_abline(intercept = coef_tbl["(Intercept)", "Estimate"], slope = 0, linetype = "dashed", color = "black") + 
  labs(x = "Total Body Length (mm)", y = "Predicted Copulation Success") +
  theme_classic()

# fwl
ggplot(all_data, aes(x = fwl, y = cop)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) + 
  geom_abline(intercept = coef_fwl["(Intercept)", "Estimate"], slope = 0, linetype = "dashed", color = "black") + 
  labs(x = "Forewing Length (mm)", y = "Predicted Copulation Success") +
  theme_classic()

# hwl
ggplot(all_data, aes(x = hwl, y = cop, color = sp)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, aes(color = sp)) +
  scale_color_manual(values = c("CS" = "lightblue", "CV" = "blue")) +
  geom_abline(intercept = coef_hwl_sp["(Intercept)", "Estimate"], slope = 0, linetype = "dashed", color = "black") + 
  labs(x = "Hindwing Length (mm)", y = "Predicted Copulation Success") +
  guides(color = guide_legend(title = NULL)) +
  theme_classic()


