
# Parameter estimation for a distribution with unknown shape parameters
# Example from:  Bury(1999) pp.225-226, parameter estimates as given by Bury are
# shape = 6.40 and scale=2.54.
data <- c(16, 11.6, 19.9, 18.6, 18, 13.1, 29.1, 10.3, 12.2, 15.6, 12.7, 13.1,
          19.2, 19.5, 23, 6.7, 7.1, 14.3, 20.6, 25.6, 8.2, 34.4, 16.1, 10.2, 12.3)


library(ggplot2)

rto <-read.csv("C:\\Users\\jeff\\OneDrive\\Books\\Prob and Stats\\Data\\resilience.csv")
summary(rto)

boxplot(rto,horizontal=TRUE, col = "skyblue", lwd=2, cex.axis=1.5)  

#Welch Two Sample t-test
with(rto, t.test(x=New_RTO, y=Legacy_RTO, conf.level = 0.99))

wilcox <- wilcox.test(rto$New_RTO, rto$Legacy_RTO, conf.level = 0.99)
names(wilcox)
library(tidyverse)

rto.t.test <- t.test(rto$New_RTO, rto$Legacy_RTO, conf.level = 0.99)

rto.diff <- round(rto.t.test$estimate[1] - rto.t.test$estimate[2], 1)

conf.level <- attr(rto.t.test$conf.int, "conf.level") * 100

names(rto.t.test)
rto.t.test$method # Name of the test
rto.t.test$data.name #variables being compared
rto.t.test$estimate  #sample means
rto.t.test$statistic #test statistic
rto.t.test$p.value   # p-value
rto.t.test$parameter # degrees of freedom
rto.t.test$stderr    # s.e.
rto.t.test$null.value #Null hypothesis
rto.t.test$alternative #Alternative hypothesis
rto.t.test$conf.int  # 95% CI

print(paste0("The test method is: ",rto.t.test$method))
print(paste0("The sample means are: ",round(rto.t.test$estimate,4)))
print(paste0("The difference in means is: ",rto.diff))
print(paste0("The test statistic is: ",rto.t.test$statistic))
print(paste0("The degrees of freedom are: ",rto.t.test$parameter))
print(paste0("The standard error is: ",rto.t.test$stderr))
print(paste0("Null hypothesis: The difference in means = ",rto.t.test$null.value))
print(paste0("Alternative hypothesis: x-bar is ",rto.t.test$alternative," y-bar"))
print(paste0("the p-value = ",rto.t.test$p.value))
print(paste0("the confidence level = ",conf.level,"%"))
print(paste0("the confidence interval = ",rto.t.test$conf.int))

rto.t.test$conf.int

# Confidence level as a %
conf.level <- attr(rto.t.test$conf.int, "conf.level") * 100

# Our study finds that birth weights are on average `r rto.diff`g higher in the non-smoking group compared to the smoking group (t-statistic `r round(rto.t.test$statistic,2)`, p=`r round(rto.t.test$p.value, 3)`, `r conf.level`% CI [`r round(rto.t.test$conf.int,1)`]g)

sd(rto$New_RTO)

#t.test(x, y, var.equal = TRUE)


mat_dat <-read.csv("C:\\Users\\jeff\\OneDrive\\Books\\Prob and Stats\\Data\\airframe_materials.csv")
summary(mat_dat)

boxplot(mat_dat,horizontal=TRUE, col = "skyblue", lwd=2, cex.axis=1.5)  



#########################################################################################
#  Load the required packages by running the following code in RStudio
#########################################################################################

library(tibble)
#library(readxl)
library(stats)
#library(openxlsx)
library(tidyr)
library(broom)
library(multcomp)
library(dplyr)

#########################################################################################
#PART 01: PREPARING DATA FOR ANOVA
#########################################################################################

# Read in the data from the Excel file by running the following code:
  
mat_dat <- as.data.frame(read.csv("C:\\Users\\jeff\\OneDrive\\Books\\Prob and Stats\\Data\\infrared.csv"))
significance_level <- 0.05
repetitions <- 10
# Combine the data for all groups into a single data frame by running the following code:

summary(mat_dat)

boxplot(mat_dat,horizontal=TRUE, col = "skyblue", lwd=2, cex.axis=1.5)  
plot(mat_dat)

mat_dat$index <- 1:nrow(mat_dat)
mat_dat

#Convert wide dataframe to long dataframe
mat_long <- mat_dat %>%
  pivot_longer(cols = -index, names_to = "material", values_to = "mean_score")

#Check the structure of the long dataframe
str(mat_long)
# View part the the long dataframe
head(mat_long,20)

#Impute missing values using the medians
mat_imputed <- data.frame(
  original = mat_long$mean_score,
  median_score = replace(mat_long$mean_score, is.na(mat_long$mean_score), median(mat_long$mean_score, na.rm = TRUE))
)
mat_imputed

#Put the imputed data into a dataframe
mat_imp <- as.data.frame(cbind(mat_long$material,mat_imputed$median_score))

# Rename the variables

colnames(mat_imp)[1] <- "Factor"
colnames(mat_imp)[2] <- "Response"


library(reshape2) 
library(ggplot2) 

# creating a dataframe 
data_frame <- mat_imp


# creating the modified dataframe 
data_mod <- melt(data_frame, id.vars='col1',  
                  measure.vars=c('col2', 'col3')) 

# creating a plot 
p < - ggplot(data_mod) + 
  geom_boxplot(aes(x=col1, y=value, color=variable)) 

# printing the plot 
print(p) 

# Perform the ANOVA
aov.out <- aov(Response~Factor, data = mat_imp)

# Print the ANOVA Table
summary(aov.out) 

#########################################################################################
#PART 02: EFFECT AND GENERAL RESULTS
#########################################################################################

anova_table <- as.data.frame(anova(aov.out))

# Extract the results from the ANOVA test
DF_labels = anova_table$Df[1]
DF_residuals = anova_table$Df[2]
SS_labels = anova_table$`Sum Sq`[1]
SS_residuals = anova_table$`Sum Sq`[2]
MS_labels = anova_table$`Mean Sq`[1]
MS_residuals = anova_table$`Mean Sq`[2]
F_statistic <- anova_table$F[1]
F_critical_value <- qf(0.95, length(unique(combined_data$Factor))-1, aov.out$df.resid)
p_value <- anova_table$"Pr(>F)"[1]

#Post-hoc: Bonferroni Correction
bonferroni_corr <- p.adjust(p_value, method = "bonferroni")

# Create a data frame with ANOVA results
anova_results <- data.frame(
  DF_Between = DF_labels,
  DF_Within = DF_residuals,
  SS_Between = SS_labels,
  SS_Within = SS_residuals,
  MS_Between = MS_labels,
  MS_Within = MS_residuals,
  F_statistic = F_statistic,
  F_critical_value = F_critical_value,
  p_value = p_value,
  Eta_squared = Eta_squared,
  bonferroni_corr = bonferroni_corr,
  #Omega_squared = omega_squared,
  alpha = significance_level)
anova_results

#########################################################################################
#PART 03: POST HOC TESTS
#########################################################################################

#Post-hoc: Bonferroni Correction
cat("\nBonferroni-Correction = ", round(bonferroni_corr, 3), "\n")

#Post-hoc: Tukey's HSD
tukey <- TukeyHSD(aov.out)
tukey_table <- tidy(tukey)
tukey_df <- as.data.frame(tukey_table)


#Post-hoc: Scheffe's test
#mc <- glht(aov.out, linfct = mcp(Factor = "Tukey"))
#summary(mc)

#mc_summary <- summary(mc)
#scheffe_sum <- tidy(mc_summary)

#head(scheffe_sum)

#Pair of groups are significantly different from each other if the adjusted p-value < 0.05. 

#########################################################################################
#PART 04: CONCLUSIONS
#########################################################################################


# Function to generate conclusions based on parameters and characteristics
generate_conclusions <- function(p_value, F_statistic,  bonferroni_corr) {
  conclusions <- data.frame(Variable = character(), Conclusions = character(), stringsAsFactors = FALSE)
  
  
  # p-value conclusion
  if (p_value < significance_level) {
    conclusions <- rbind(conclusions, data.frame(Variable = "p-value < alpha.Reject the null hypothesis.There is significant evidence to support the alternative hypothesis.Hence, at least one group mean is different from the others."))
  } else {
    conclusions <- rbind(conclusions, data.frame(Variable = "p-value > alpha.Fail to reject the null hypothesis.There is not enough evidence to support the alternative hypothesis.Hence, there is no significant evidence to determine that at least one group mean is different from the others."))
  }
  
  
  # F-statistic conclusion
  if (F_statistic < F_critical_value) {
    conclusions <- rbind(conclusions, data.frame(Variable = "F < Fcrit.Fail to reject the null hypothesis.There is not enough evidence to support the alternative hypothesis.Hence, there is no significant evidence to determine that at least one group mean is different from the others."))
  } else {
    conclusions <- rbind(conclusions, data.frame(Variable = "F > Fcrit.Reject the null hypothesis.There is significant evidence to support the alternative hypothesis.Hence, at least one group mean is different from the others.."))
  }
  
  return(conclusions)
}


# Generate conclusions based on parameters and characteristics
conclusions_df <- generate_conclusions(p_value, F_statistic, bonferroni_corr)

# Print the conclusions data frame
print(conclusions_df)


# Post-hoc: Bonferroni Correction conclusion
if (any(bonferroni_corr < significance_level)) {
  significant_comparisons <- which(bonferroni_corr < significance_level)
  paste("There are significant differences between group", significant_comparisons)
} else {
  "There are no significant differences between the groups"
}

if (bonferroni_corr < significance_level) {
  b_result_text <- "b < alpha.There are significant differences between group\n"
} else {
  b_result_text <- "b > alpha.There are no significant differences between the groups\n"
}
