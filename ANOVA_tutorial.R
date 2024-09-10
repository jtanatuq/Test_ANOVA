# install.packages("tidyverse")
# install.packages("ggpubr")
# install.packages("rstatix")

library(tidyverse)
library(ggpubr)
library(rstatix)

data("PlantGrowth")
set.seed(1234) # random number generator
PlantGrowth %>% sample_n_by(group, size = 1)

levels(PlantGrowth$group)

PlantGrowth <- PlantGrowth %>%
  reorder_levels(group, order = c("ctrl", "trt1", "trt2"))

PlantGrowth %>%
  group_by(group) %>%
  get_summary_stats(weight, type = "mean_sd")

ggboxplot(PlantGrowth, x = "group", y = "weight")

PlantGrowth %>% 
  group_by(group) %>%
  identify_outliers(weight)

# Build the linear model
model  <- lm(weight ~ group, data = PlantGrowth)
# Create a QQ plot of residuals
ggqqplot(residuals(model))

# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))

PlantGrowth %>%
  group_by(group) %>%
  shapiro_test(weight)

ggqqplot(PlantGrowth, "weight", facet.by = "group")

plot(model, 1)

PlantGrowth %>% levene_test(weight ~ group)

res.aov <- PlantGrowth %>% anova_test(weight ~ group)
res.aov

# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "group")

ggboxplot(PlantGrowth, x = "group", y = "weight") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )
