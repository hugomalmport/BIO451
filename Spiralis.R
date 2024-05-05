# Uncomment install lines below if packages has not been installed
# install.packages("tidyverse")
# install.packages("ggpubr")
# install.packages("rstatix")
# install.packages("datarium")
# install.packages('emmeans')
library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)
library(readxl)
library(emmeans)

# Read data from excel file
spiralis_data <- read_excel("C:\\Users\\mats\\OneDrive - Crossaster\\MK\\Biologi\\Experimentell marinekologi\\Project Fucus zonation\\Dev\\Fucus Data.xlsx", sheet = "Data")

# Make the salinity treatment a factor
spiralis_data$salinity_treatment_psu <- as.factor(spiralis_data$salinity_treatment_psu)

# Make lifestage a factor
spiralis_data$lifestage <- as.factor(spiralis_data$lifestage)

# Check that the levels for the salinity treatment are correct
levels(spiralis_data$salinity_treatment_psu)

# Check that the levels lifestage are correct
levels(spiralis_data$lifestage)


### One-way ANOVAs for adults and germlings


# Get data for salinity treatments with no desiccation
data_no_desiccation <- filter(spiralis_data, desiccation == "N")

# Get data for salinity treatments and desiccation
data_with_desiccation <- filter(spiralis_data, desiccation == "Y")

# Summary statistics for all samples in the salinity treatment
summary_stats_all <- data_no_desiccation |> group_by(salinity_treatment_psu) |> get_summary_stats(chlorophyll, type = "mean_sd")

# Summary statistics for germling samples in the salinity treatment
summary_stats_germlings <- filter(data_no_desiccation, lifestage == "G") |> group_by(salinity_treatment_psu) |> get_summary_stats(chlorophyll, type = "mean_sd")

# Summary statistics for adult samples in the salinity treatment
summary_stats_adults <- filter(data_no_desiccation, lifestage == "A") |> group_by(salinity_treatment_psu) |> get_summary_stats(chlorophyll, type = "mean_sd")

# Box plot of all samples in the salinity treatment
ggplot(data_no_desiccation, aes(x = salinity_treatment_psu, y = chlorophyll)) +
  geom_boxplot() +
  ggtitle("All samples in the salinity treatment")

# Box plot of all samples by lifestage in the salinity treatment
ggplot(data_no_desiccation, aes(x = lifestage, y = chlorophyll)) +
  geom_boxplot() +
  facet_wrap(~salinity_treatment_psu) +
  ggtitle("All samples by lifestage in the salinity treatment")

# Get data for adults with no desiccation
adults_no_desiccation <- filter(data_no_desiccation, lifestage == "A")

# Check for outliers in the data for adults (filtering out relevant columns)
adults_no_desiccation[,c(2,5,11)] |> group_by(salinity_treatment_psu) |> identify_outliers(chlorophyll)

# Get data for germlings with no desiccation
germlings_no_desiccation <- filter(data_no_desiccation, lifestage == "G")

# Check for outliers in the data for germlings (filtering out relevant columns)
germlings_no_desiccation[,c(2,5,11)] |> group_by(salinity_treatment_psu) |> identify_outliers(chlorophyll)

# Check normality assumption by groups for adults. 
# Computing Shapiro-Wilk test for each group level. 
# If the data is normally distributed, the p-value should be greater than 0.05.
adults_no_desiccation |>
  group_by(salinity_treatment_psu) |>
  shapiro_test(chlorophyll)

# Check normality assumption by groups for germlings 
germlings_no_desiccation |>
  group_by(salinity_treatment_psu) |>
  shapiro_test(chlorophyll)

# QQ plot draws the correlation between a given data and the normal distribution. 

# QQ plots for each group level for adults
ggqqplot(adults_no_desiccation, "chlorophyll", facet.by = "salinity_treatment_psu")

# QQ plots for each group level for adults
ggqqplot(germlings_no_desiccation, "chlorophyll", facet.by = "salinity_treatment_psu")

# Build the linear model for adults
model_adults  <- lm(chlorophyll ~ salinity_treatment_psu, data = adults_no_desiccation)

# If there is no evident relationships between residuals and fitted values (the mean of each groups)
# then we can assume the homogeneity of variances.
# Residuals versus fits plot for adults
plot(model_adults, 1, main = "Adults")

# If the p-value is > 0.05 in the Levene test, then is not significant difference between variances across groups 
# and then we can assume the homogeneity of variances in the different treatment groups.
# Levene test for adults
adults_no_desiccation |> levene_test(chlorophyll ~ salinity_treatment_psu)

# Build the linear model for germlings
model_germlings  <- lm(chlorophyll ~ salinity_treatment_psu, data = germlings_no_desiccation)

# Residuals versus fits plot for germlings
plot(model_germlings, 1, main = "Germlings")

# Levene test for germlings
germlings_no_desiccation |> levene_test(chlorophyll ~ salinity_treatment_psu)

# One-way ANOVA for adults
res_adults.aov <- adults_no_desiccation |> anova_test(chlorophyll ~ salinity_treatment_psu)
res_adults.aov

# One-way ANOVA for germlings
res_germlings.aov <- germlings_no_desiccation |> anova_test(chlorophyll ~ salinity_treatment_psu)
res_germlings.aov

# Pairwise comparisons adults
pwc_adults <- adults_no_desiccation %>% tukey_hsd(chlorophyll ~ salinity_treatment_psu)
pwc_adults

# Pairwise comparisons germlings
pwc_germlings <- germlings_no_desiccation %>% tukey_hsd(chlorophyll ~ salinity_treatment_psu)
pwc_germlings

# Box plots with p-values for adults
pwc_adults <- pwc_adults %>% add_xy_position(x = "salinity_treatment_psu")
ggboxplot(adults_no_desiccation, x = "salinity_treatment_psu", y = "chlorophyll") +
  stat_pvalue_manual(pwc_adults, hide.ns = TRUE) +
  labs(
    title = "Adults",
    subtitle = get_test_label(res_adults.aov, detailed = TRUE),
    caption = get_pwc_label(pwc_adults)
  )

# Box plots with p-values for germlings
pwc_germlings <- pwc_germlings %>% add_xy_position(x = "salinity_treatment_psu")
ggboxplot(germlings_no_desiccation, x = "salinity_treatment_psu", y = "chlorophyll") +
  stat_pvalue_manual(pwc_germlings, hide.ns = TRUE) +
  labs(
    title = "Germlings",
    subtitle = get_test_label(res_germlings.aov, detailed = TRUE),
    caption = get_pwc_label(pwc_germlings)
  )

#####################


### Two-way ANOVA (salinity, lifestage, no desiccation)


# Summary statistics for all samples in the salinity treatment
# summary_stats_all <- data_no_desiccation |> group_by(lifestage, salinity_treatment_psu) |> get_summary_stats(chlorophyll, type = "mean_sd")
summary_stats_all <- data_no_desiccation |> group_by(salinity_treatment_psu, lifestage) |> get_summary_stats(chlorophyll, type = "mean_sd")

# Box plot of chlorophyll by salinity treatment, colored by lifestage:
bxp <- ggboxplot(
  data_no_desiccation, x = "lifestage", y = "chlorophyll",
  color = "salinity_treatment_psu", palette = "jco"
)
# bxp <- ggboxplot(
#   data_no_desiccation, x = "salinity_treatment_psu", y = "chlorophyll",
#   color = "lifestage", palette = "jco"
# )
bxp

# Check for outliers in the data (filtering out relevant columns)
data_no_desiccation[,c(2,4,5,11)] %>%
  group_by(salinity_treatment_psu, lifestage) %>%
  identify_outliers(chlorophyll)
# data_no_desiccation[,c(2,4,5,11)] %>%
#   group_by(lifestage, salinity_treatment_psu) %>%
#   identify_outliers(chlorophyll)

# Check normality assumption by groups. 
# Computing Shapiro-Wilk test for each group level. 
# If the data is normally distributed, the p-value should be greater than 0.05.
data_no_desiccation |>
  group_by(salinity_treatment_psu, lifestage) |>
  shapiro_test(chlorophyll)
# data_no_desiccation |>
#   group_by(lifestage, salinity_treatment_psu) |>
#   shapiro_test(chlorophyll)

# Build the linear model
model  <- lm(chlorophyll ~ salinity_treatment_psu*lifestage,
             data = data_no_desiccation)
# model  <- lm(chlorophyll ~ lifestage*salinity_treatment_psu,
#              data = data_no_desiccation)

# QQ plot draws the correlation between a given data and the normal distribution. 
# Create a QQ plot of residuals
ggqqplot(residuals(model))

# Compute Shapiro-Wilk test of normality
# If all the points fall approximately along the reference line, we can assume normality
shapiro_test(residuals(model))

# Check normality assumption by groups. Computing Shapiro-Wilk test for each combinations of factor levels
# If the p value > 0.05 for each combination, then we can assume normal distribution for each combination
data_no_desiccation %>%
  group_by(salinity_treatment_psu, lifestage) %>%
  shapiro_test(chlorophyll)
# data_no_desiccation %>%
#   group_by(lifestage, salinity_treatment_psu) %>%
#   shapiro_test(chlorophyll)

# QQ plot draws the correlation between a given data and the normal distribution. 
# QQ plots for each combination
ggqqplot(data_no_desiccation, "chlorophyll", ggtheme = theme_bw()) +
  facet_grid(salinity_treatment_psu ~ lifestage)
# ggqqplot(data_no_desiccation, "chlorophyll", ggtheme = theme_bw()) +
#   facet_grid(lifestage ~ salinity_treatment_psu)

# If the Levene’s test is not significant (p > 0.05), 
# then we can assume the homogeneity of variances in the different groups.
data_no_desiccation |> levene_test(chlorophyll ~ salinity_treatment_psu*lifestage)
# data_no_desiccation |> levene_test(chlorophyll ~ lifestage*salinity_treatment_psu)

# Two-way ANOVA (salinity, lifestage, no desiccation)
res.aov <- data_no_desiccation %>% anova_test(chlorophyll ~ salinity_treatment_psu * lifestage)
# res.aov <- data_no_desiccation %>% anova_test(chlorophyll ~ lifestage * salinity_treatment_psu)
res.aov


## Post-hoc tests depending on results

# Significant interaction
# Group the data by lifestage and fit  anova
model <- lm(chlorophyll ~ salinity_treatment_psu * lifestage, data = data_no_desiccation)
data_no_desiccation %>%
  group_by(salinity_treatment_psu) %>%
  anova_test(chlorophyll ~ lifestage, error = model)
# model <- lm(chlorophyll ~ lifestage * salinity_treatment_psu, data = data_no_desiccation)
# data_no_desiccation %>%
#   group_by(lifestage) %>%
#   anova_test(chlorophyll ~ salinity_treatment_psu, error = model)

# Pairwise comparisions (significant interaction)
pwc <- data_no_desiccation %>% 
  group_by(salinity_treatment_psu) %>%
  emmeans_test(chlorophyll ~ lifestage, p.adjust.method = "bonferroni") 
pwc
# pwc <- data_no_desiccation %>% 
#   group_by(lifestage) %>%
#   emmeans_test(chlorophyll ~ salinity_treatment_psu, p.adjust.method = "bonferroni") 
# pwc

# Pairwise t-test (non-significant interaction)
data_no_desiccation %>%
  pairwise_t_test(
    chlorophyll ~ lifestage, 
    p.adjust.method = "bonferroni"
  )


# Pairwise comparisions (non-significant interaction)
model <- lm(chlorophyll ~ lifestage * salinity_treatment_psu, data = data_no_desiccation)
data_no_desiccation %>% 
  emmeans_test(
    chlorophyll ~ salinity_treatment_psu, p.adjust.method = "bonferroni",
    model = model
  )

# Box plots with p-values
pwc <- pwc %>% add_xy_position(x = "salinity_treatment_psu")
# pwc <- pwc %>% add_xy_position(x = "lifestage")
bxp +
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

#####################


### Three-way ANOVA





headache %>%
  group_by(gender, risk, treatment) %>%
  get_summary_stats(pain_score, type = "mean_sd")

ggplot(data_no_desiccation, aes(x = lifestage, y = chlorophyll)) +
  geom_boxplot() +
  facet_wrap(~salinity_treatment_psu) +
  ggtitle("Site N1 before desiccation")

ggplot(filter(spiralis_data, site == "N1", desiccation == "Y"), aes(x = lifestage, y = chlorophyll)) +
  geom_boxplot() +
  facet_wrap(~salinity_treatment_psu) +
  ggtitle("Site N1 after desiccation")

ggplot(data_no_desiccation, aes(x = lifestage, y = chlorophyll)) +
  geom_boxplot() +
  facet_wrap(~salinity_treatment_psu) +
  ggtitle("All sites before desiccation")

ggplot(filter(spiralis_data, desiccation == "Y"), aes(x = lifestage, y = chlorophyll)) +
  geom_boxplot() +
  facet_wrap(~salinity_treatment_psu) +
  ggtitle("All sites after desiccation")

model  <- lm(chlorophyll ~ site, data = spiralis_data)
ggqqplot(residuals(model))
shapiro_test(residuals(model))
