# Uncomment install lines below if packages has not been installed
# install.packages("tidyverse")
# install.packages("ggpubr")
# install.packages("rstatix")
# install.packages("datarium")
# install.packages('emmeans')
rm(list = ls())
library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)
library(readxl)
library(emmeans)
# install.packages("lmerTest")
# library(lmerTest)
remove_extreme_outliers <- function(df, df2){
  # Check for outliers 
  outliers <- df |> group_by(treatment_psu) |> identify_outliers(all_of(pigment))
  # Filter out extreme outliers
  extreme_outliers <- filter(outliers, is.extreme == TRUE)
  
  # Remove extreme outliers
  for (extreme_outlier in extreme_outliers$sample_num) {
    df <- subset(df, sample_num != extreme_outlier)
    df2 <- subset(df2, sample_num != extreme_outlier)
  }
  return(list(df,df2))
}

# Load data frames prepared by the cleaning script
load("~/Downloads/cleaned_data.Rdata")

# Assign pigment(s) for analysis to the variable "pigment"
# The options are "chlorophyll_a", "chlorophyll_c" and "carotenoids"
pigment = "chlorophyll_c"


spiralis_data <- spiralis_data_clean

spiralis_data <- spiralis_data |> drop_na(all_of(pigment))
# Check that the levels for the salinity treatment are correct
levels(spiralis_data$treatment_psu)

# Check that the levels life_stage are correct
levels(spiralis_data$life_stage)



# Create data frames for the different experiment groups
adults_no_desiccation <- filter(spiralis_data, life_stage == "A", desiccation == "N")
germlings_no_desiccation <- filter(spiralis_data, life_stage == "G", desiccation == "N")
adults_with_desiccation <- filter(spiralis_data, life_stage == "A", desiccation == "Y")
germlings_with_desiccation <- filter(spiralis_data, life_stage == "G", desiccation == "Y")

# Remove all extreme outliers for the pigment in each experiment group and in the overall data (spiralis_data)
dfs <- remove_extreme_outliers(adults_no_desiccation, spiralis_data)
adults_no_desiccation <- dfs[[1]]
spiralis_data <- dfs[[2]]
dfs <- remove_extreme_outliers(germlings_no_desiccation, spiralis_data)
germlings_no_desiccation <- dfs[[1]]
spiralis_data <- dfs[[2]]
dfs <- remove_extreme_outliers(adults_with_desiccation, spiralis_data)
adults_with_desiccation <- dfs[[1]]
spiralis_data <- dfs[[2]]
dfs <- remove_extreme_outliers(germlings_with_desiccation, spiralis_data)
germlings_with_desiccation <- dfs[[1]]
spiralis_data <- dfs[[2]]

################################################################

### One-way ANOVAs for adults and germlings with no desiccation

################################################################

# Get data for salinity treatments with no desiccation
data_no_desiccation <- filter(spiralis_data, desiccation == "N")

# Line plot of data (no desiccation)
ggline(data_no_desiccation, x = "treatment_psu", y = pigment, color = "life_stage", title = paste(pigment,"- no desiccation"),
       #ggline(data_no_desiccation, x = "treatment_psu", y = pigment, color = "life_stage", title = paste(pigment, "nm - no desiccation", sep = ""),
       add = c("mean_se", "dotplot"),
       palette = c("#EF9600", "#00DEBB"))


# Get data for salinity treatments and desiccation
data_with_desiccation <- filter(spiralis_data, desiccation == "Y")
levels(data_with_desiccation$life_stage)
# Line plot of data (with desiccation)
ggline(data_with_desiccation, x = "treatment_psu", y = pigment, color = "life_stage", title = paste(pigment, "- with desiccation"),
       add = c("mean_se", "dotplot"),
       palette = c("#EF9600", "#00DEBB"))


## Statistics for salinity treatment

# Summary statistics for germling samples in the salinity treatment
summary_stats_germlings <- filter(data_no_desiccation, life_stage == "G") |> group_by(treatment_psu) |> get_summary_stats(.data[[pigment]], type = "mean_sd")
summary_stats_germlings

# Summary statistics for adult samples in the salinity treatment
summary_stats_adults <- filter(data_no_desiccation, life_stage == "A") |> group_by(treatment_psu) |> get_summary_stats(.data[[pigment]], type = "mean_sd")
summary_stats_adults


# Box plot of all samples by life_stage in the salinity treatment
ggplot(data_no_desiccation, aes(x = life_stage, y=.data[[pigment]])) +
  geom_boxplot() +
  facet_wrap(~treatment_psu) +
  ggtitle(paste(pigment,"response, no desiccation"))


# Build the linear model for adults
model_adults  <- lm(as.formula(paste(pigment, "~ treatment_psu")), data = adults_no_desiccation)

summary(model_adults)

# If there is no evident relationships between residuals and fitted values (the mean of each groups)
# then we can assume the homogeneity of variances.
# Residuals versus fits plot for adults
plot(model_adults, 1, main = "Adults")
# QQ plot draws the correlation between a given data and the normal distribution. 
# If points fall along the line we can assume normality.
plot(model_adults, 2, main = "Adults")

# Build the linear model for germlings
model_germlings  <- lm(as.formula(paste(pigment, "~ treatment_psu")), data = germlings_no_desiccation)

# If there is no evident relationships between residuals and fitted values (the mean of each groups)
# then we can assume the homogeneity of variances.
# Residuals versus fits plot for germlings
plot(model_germlings, 1, main = "Germlings")

# QQ plot draws the correlation between a given data and the normal distribution. 
# If points fall along the line we can assume normality.
plot(model_germlings, 2, main = "Germlings")


################################################################

# One-way ANOVA for adults with no desiccation

################################################################

res_adults.aov <- adults_no_desiccation |> anova_test(as.formula(paste(pigment, "~ treatment_psu")))
res_adults.aov

# Pairwise comparisons adults (Tukey's HSD)
pwc_adults <- adults_no_desiccation |> tukey_hsd(as.formula(paste(pigment, "~ treatment_psu")))
pwc_adults


# Box plots with p-values for adults
pwc_adults <- pwc_adults |> add_xy_position(x = "treatment_psu")
ggboxplot(adults_no_desiccation, x = "treatment_psu", y = pigment) +
  stat_pvalue_manual(pwc_adults, hide.ns = TRUE) +
  labs(
    title = "Adults",
    subtitle = get_test_label(res_adults.aov, detailed = TRUE),
    caption = get_pwc_label(pwc_adults)
  )


################################################################

# One-way ANOVA for germlings with no desiccation

################################################################

res_germlings.aov <- germlings_no_desiccation |> anova_test(as.formula(paste(pigment,"~ treatment_psu")))
res_germlings.aov

# Pairwise comparisons germlings
pwc_germlings <- germlings_no_desiccation %>% tukey_hsd(as.formula(paste(pigment,"~ treatment_psu")))
pwc_germlings


# Box plots with p-values for germlings
pwc_germlings <- pwc_germlings %>% add_xy_position(x = "treatment_psu")
ggboxplot(germlings_no_desiccation, x = "treatment_psu", y = pigment) +
  stat_pvalue_manual(pwc_germlings, hide.ns = TRUE) +
  labs(
    title = "Germlings",
    subtitle = get_test_label(res_germlings.aov, detailed = TRUE),
    caption = get_pwc_label(pwc_germlings)
  )

################################################################

### One-way ANOVAs for adults and germlings with desiccation

################################################################

# Summary statistics for germling samples in the salinity treatment
summary_stats_germlings <- filter(data_with_desiccation, life_stage == "G") |> group_by(treatment_psu) |> get_summary_stats(.data[[pigment]], type = "mean_sd")
summary_stats_germlings

# Summary statistics for adult samples in the salinity treatment
summary_stats_adults <- filter(data_with_desiccation, life_stage == "A") |> group_by(treatment_psu) |> get_summary_stats(.data[[pigment]], type = "mean_sd")
summary_stats_adults



ggplot(data_with_desiccation, aes(x = life_stage, y=.data[[pigment]])) +
  geom_boxplot() +
  facet_wrap(~treatment_psu) +
  ggtitle(paste(pigment,"response, after desiccation"))

# Build the linear model for adults
model_adults_desiccation  <- lm(as.formula(paste(pigment,"~ treatment_psu")), data = adults_with_desiccation)

# If there is no evident relationships between residuals and fitted values (the mean of each groups)
# then we can assume the homogeneity of variances.
# Residuals versus fits plot for adults
plot(model_adults_desiccation, 1, main = "Adults after desiccation")
# QQ plot draws the correlation between a given data and the normal distribution. 
# If points fall along the line we can assume normality.
plot(model_adults_desiccation, 2, main = "Adults after desiccation")

# Build the linear model for germlings
model_germlings_desiccation  <- lm(as.formula(paste(pigment,"~ treatment_psu")), data = germlings_with_desiccation)

# If there is no evident relationships between residuals and fitted values (the mean of each groups)
# then we can assume the homogeneity of variances.
# Residuals versus fits plot for germlings
plot(model_germlings_desiccation, 1, main = "Germlings")

# QQ plot draws the correlation between a given data and the normal distribution. 
# If points fall along the line we can assume normality.
plot(model_germlings_desiccation, 2, main = "Germlings")

################################################################

# One-way ANOVA for adults with desiccation

################################################################

res_adults_desiccation.aov <- adults_with_desiccation |> anova_test(as.formula(paste(pigment,"~ treatment_psu")))
res_adults_desiccation.aov

# Pairwise comparisons adults
pwc_adults_desiccation <- adults_with_desiccation |> tukey_hsd(as.formula(paste(pigment,"~ treatment_psu")))
pwc_adults_desiccation

# Box plots with p-values for adults
pwc_adults_desiccation <- pwc_adults_desiccation |> add_xy_position(x = "treatment_psu")
ggboxplot(adults_with_desiccation, x = "treatment_psu", y = pigment) +
  stat_pvalue_manual(pwc_adults_desiccation, hide.ns = TRUE) +
  labs(
    title = "Adults",
    subtitle = get_test_label(res_adults_desiccation.aov, detailed = TRUE),
    caption = get_pwc_label(pwc_adults_desiccation)
  )

################################################################

# One-way ANOVA for germlings with desiccation

################################################################

res_germlings_desiccation.aov <- germlings_with_desiccation |> anova_test(as.formula(paste(pigment,"~ treatment_psu")))
res_germlings_desiccation.aov

# Pairwise comparisons germlings
pwc_germlings_desiccation <- germlings_with_desiccation %>% tukey_hsd(as.formula(paste(pigment,"~ treatment_psu")))
pwc_germlings_desiccation


# Box plots with p-values for germlings
pwc_germlings_desiccation <- pwc_germlings_desiccation |> add_xy_position(x = "treatment_psu")
ggboxplot(germlings_with_desiccation, x = "treatment_psu", y = pigment) +
  stat_pvalue_manual(pwc_germlings_desiccation, hide.ns = TRUE) +
  labs(
    title = "Germlings",
    subtitle = get_test_label(res_germlings_desiccation.aov, detailed = TRUE),
    caption = get_pwc_label(pwc_germlings_desiccation)
  )

################################################################

### Two-way ANOVA (salinity, life_stage) - no desiccation

################################################################

# Summary statistics for all samples in the salinity treatment
data_no_desiccation |>
  group_by(treatment_psu, life_stage) |>
  get_summary_stats(.data[[pigment]], type = "mean_sd")

# Box plot of all samples in the salinity treatment
bxp <- ggboxplot(
  data_no_desiccation, x = "treatment_psu", y = pigment,
  color = "life_stage", palette = "jco"
)
bxp

# Build the linear model for experiment without desiccation
model_no_desiccation  <- lm(as.formula(paste(pigment, "~ treatment_psu*life_stage")),
                            data = data_no_desiccation)
# QQ plot draws the correlation between a given data and the normal distribution. 
# If points fall along the line we can assume normality.
# QQ plot of the models residuals
ggqqplot(residuals(model_no_desiccation))

# QQ plots for each combination in the experiment
# If points fall along the reference line for each combination, we can assume normality.
ggqqplot(data_no_desiccation, pigment, ggtheme = theme_bw()) +
  facet_grid(treatment_psu ~ life_stage)

# Check homogeneity of variance
plot(model_no_desiccation,1)

# Two-way ANOVA
res.aov <- data_no_desiccation %>% anova_test(as.formula(paste(pigment, "~ treatment_psu*life_stage")))
res.aov

################################################################

### Use these post-hoc tests if there is interaction

################################################################

# Compute simple main effects
data_no_desiccation |>
  group_by(life_stage) |>
  anova_test(as.formula(paste(pigment, "~ treatment_psu")), error = model_no_desiccation)

# Compute pairwise comparisons
pwc_interaction <- data_no_desiccation |>
  group_by(life_stage) |>
  emmeans_test(as.formula(paste(pigment, "~ treatment_psu")) , p.adjust.method = "bonferroni")
pwc_interaction

pwc <- pwc_interaction %>% add_xy_position(x = "treatment_psu")

################################################################

### Use these tests post-hoc if there is NO interaction

################################################################

# Compute pairwise comparisons
pwc_no_interaction <- data_no_desiccation |> 
  emmeans_test(
    as.formula(paste(pigment, "~ treatment_psu")), p.adjust.method = "bonferroni",
    model = model_no_desiccation
  )
pwc_no_interaction

pwc <- pwc_no_interaction %>% add_xy_position(x = "treatment_psu")

################################################################

### End of post-hoc tests

################################################################

# Box plot with p-values
bxp +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

################################################################

### Two-way ANOVA (salinity, life_stage) - with desiccation

################################################################

# Summary statistics for all samples in the salinity treatment
data_with_desiccation |>
  group_by(treatment_psu, life_stage) |>
  get_summary_stats(.data[[pigment]], type = "mean_sd")

# Box plot of all samples in the salinity treatment
bxp <- ggboxplot(
  data_with_desiccation, x = "treatment_psu", y = pigment,
  color = "life_stage", palette = "jco"
)
bxp

# Build the linear model for experiment with desiccation
model_with_desiccation  <- lm(as.formula(paste(pigment, "~ treatment_psu*life_stage")),
                              data = data_with_desiccation)

# QQ plot draws the correlation between a given data and the normal distribution. 
# If points fall along the line we can assume normality.
# QQ plot of the models residuals
ggqqplot(residuals(model_with_desiccation))

# QQ plots for each combination in the experiment
# If points fall along the reference line for each combination, we can assume normality.
ggqqplot(data_with_desiccation, pigment, ggtheme = theme_bw()) +
  facet_grid(treatment_psu ~ life_stage)

# Check homogeneity of variance
plot(model_with_desiccation,1)

# Two-way ANOVA
res.aov <- data_with_desiccation %>% anova_test(as.formula(paste(pigment, "~ treatment_psu*life_stage")))
res.aov

################################################################

### Use these post-hoc tests if there is interaction

################################################################

# Compute simple main effects
data_with_desiccation |>
  group_by(life_stage) |>
  anova_test(as.formula(paste(pigment, "~ treatment_psu")), error = model_with_desiccation)

# Compute pairwise comparisons
pwc_interaction <- data_with_desiccation |>
  group_by(life_stage) |>
  emmeans_test(as.formula(paste(pigment, "~ treatment_psu")), p.adjust.method = "bonferroni")
pwc_interaction

################################################################

### Use these tests post-hoc if there is NO interaction

################################################################

# Compute pairwise comparisons
pwc_no_interaction <- data_with_desiccation |> 
  emmeans_test(
    as.formula(paste(pigment, "~ treatment_psu")), p.adjust.method = "bonferroni",
    model = model_with_desiccation
  )
pwc_no_interaction

pwc <- pwc_no_interaction %>% add_xy_position(x = "treatment_psu")

################################################################

### End of post-hoc tests

################################################################

# Box plot with p-values
bxp +
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

###########################################

### One-way ANOVAs for the no-touch samples and the wild samples

###########################################

ggline(spiralis_data_no_touch, x = "treatment_psu", y = pigment, color = "life_stage", title = paste(pigment,"- no-touch"),
       add = c("mean_se", "dotplot"),
       palette = c("#EF9600", "#00DEBB"))


# Build the linear model for no-touch
model_no_touch  <- lm(as.formula(paste(pigment,"~ life_stage")), data = spiralis_data_no_touch)

# If there is no evident relationships between residuals and fitted values (the mean of each groups)
# then we can assume the homogeneity of variances.
# Residuals versus fits plot for no-touch
plot(model_no_touch, 1, main = "No-touch")

# QQ plot draws the correlation between a given data and the normal distribution. 
# If points fall along the line we can assume normality.
plot(model_no_touch, 2, main = "No-touch")

res_no_touch.aov <- spiralis_data_no_touch |> anova_test(as.formula(paste(pigment, "~ life_stage")))
res_no_touch.aov

# Build the linear model for wild
model_wild  <- lm(as.formula(paste(pigment,"~ life_stage")), data = spiralis_data_wild)

# If there is no evident relationships between residuals and fitted values (the mean of each groups)
# then we can assume the homogeneity of variances.
# Residuals versus fits plot for no-touch
plot(model_wild, 1, main = "Wild")

# QQ plot draws the correlation between a given data and the normal distribution. 
# If points fall along the line we can assume normality.
plot(model_wild, 2, main = "Wild")


ggline(spiralis_data_wild, x = "treatment_psu", y = pigment, color = "life_stage", title = paste(pigment,"- wild"),
       add = c("mean_se", "dotplot"),
       palette = c("#EF9600", "#00DEBB"))

res_wild.aov <- spiralis_data_wild |> anova_test(as.formula(paste(pigment, "~ life_stage")))
res_wild.aov

