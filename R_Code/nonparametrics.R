

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

df2 <- as.data.frame(read.csv("C:\\Users\\jeff\\OneDrive\\Books\\Prob and Stats\\Data\\aerogel_anova.csv"))

# Concatenate the data rows in df2 into a single vector r .
r = c(t(as.matrix(df2))) # response data 
r 

# Assign new variables for the treatment levels and number of control blocks.
f = c("Aerogel1", "Aerogel2", "Aerogel3", "Aerogel4")   # treatment levels 
k = 4                    # number of treatment levels 
n = 5                    # number of control blocks
#Create a vector of treatment factors that corresponds to the each element in r of step 3 with the gl function.
tm = gl(k, 1, n*k, factor(f))   # matching treatment 
tm 

#Similarly, create a vector of blocking factors for each element in the response data r.
blk = gl(n, k, k*n)             # blocking factor 
blk 

#Apply the function aov to a formula that describes the response r by both the treatment factor tm and the block control blk.
av = aov(r ~ tm + blk)
summary(av)

###############################################################
# Combine the data for all groups into a single data frame by running the following code:
my_data <- as.data.frame(read.csv("C:\\Users\\jeff\\OneDrive\\Books\\Prob and Stats\\Data\\aerogel_anova2.csv"))

#Convert wide dataframe to long dataframe
mat_long <- my_data %>%
  pivot_longer(cols = -Epoxy, names_to = "Aerogel_Type", values_to = "Mean_Score")

# creating a plot 
ggplot(mat_long) + 
  geom_boxplot(aes(x=Aerogel_Type, y=Mean_Score, color=Aerogel_Type), lwd=1.1) 

# Perform the ANOVA
aov.out <- aov(Mean_Score~Aerogel_Type+Epoxy, data = mat_long)

# Print the ANOVA Table
summary(aov.out) 

my_data
mat_long
summary(aov.out)

#perform Friedman Test
friedman.test(y=mat_long$Mean_Score, groups=mat_long$Aerogel_Type, blocks=mat_long$Epoxy)

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
  p_value = p_value,
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
