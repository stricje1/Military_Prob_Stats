## Data Presenytation

# Electrical 	9
# Mechanical 	24
# Misuse 	13

## Data for of Radar Breakdowns
# Create the data for the chart
A <- c(9, 24, 13)
B <- c("Electrical", "Mechanical", "Misuse")

# Bar Chart of Radar Breakdowns
barplot(A, names.arg = B, xlab ="Cause",
        ylab ="Articles", col ="dodgerblue",
        main ="Radar Breakdown Causes",
        cex.main=1.5, cex.lab=1.5, cex.axis=1)

## Pareto Chart of Radar Breakdowns
require(ggplot2)
require(ggQC)
# Load the data
x = c("Electrical", "Mechanical", "Misuse")
y = strtoi(c("9", "24", "13"))
# Form a dataframe
df <- as.data.frame(y,x)
# Create the plot
ggplot(df, aes(x=x, y=y)) +
  stat_pareto(point.color = "red",
              point.size = 5,
              line.color = "blue",
              linewidth = 1.5,
              bars.fill = c("dodgerblue", "lightgreen", "orange")
  ) +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20))



# install and Load qcc package
install.packages('qcc')

## Pareto using the qcc package
library(qcc)
# x axis numbers
defect <- c(9, 24, 13)
# x axis titles
names(defect) <- c("Electrical", "Mechanical", "Misuse")
# plot and frequency distribution
pareto.chart(defect, xlab = "", # x-axis label
             ylab="Frequency", # label y left
             col=heat.colors(length(defect)),
             cumperc = seq(0, 100, by = 20),
             ylab2 = "Cumulative Percentage",
             main = "Causes of Radar Breakdowns",
)

## Pie Chart
ggplot(df, aes(x="", y=y, fill=x)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme(
    plot.title = element_text(size = 30),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 20), 
    legend.text = element_text(size = 20)) +
  geom_text(aes(label = y),
            position = position_stack(vjust = 0.5), size=10) +
  ggtitle("Radar Breakdown Causes") 


## Generate a Histogram

chips <- read.csv("C:\\Users\\jeff\\OneDrive\\Books\\Prob and Stats\\Data\\computer_chips.csv")

battery <- read.csv("C:\\Users\\jeff\\OneDrive\\Books\\Prob and Stats\\Data\\battery_life.csv")

hist(battery$remaining_life)

run_times <- read.csv("C:\\Users\\jeff\\OneDrive\\Books\\Prob and Stats\\Data\\run_times.csv")
hist(run_times$times)

ggplot(chips, aes(x=Num_Defect)) +
  geom_histogram( binwidth=1, fill="aquamarine2", color="aquamarine4", alpha=.75) +
  ggtitle("Number of defective computer chips per batch with binwidth = 1") +
  theme(
    plot.title = element_text(size=20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
  )

ggplot(run_times, aes( y = times)) +
  geom_boxplot(fill = "grey90", color = "dodgerblue", lwd=2) +
  coord_flip() +
  scale_x_discrete() + 
  xlab("") +
  ylab("Army Physical Fitness 2-mile run times (seconds)") +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))

mean(run_times$times)
var(run_times$times)
range(run_times$times)
nrow(run_times)
max(run_times$times)
min(run_times$times)
median(run_times$times)
quantile(run_times$times, probs = c(0,0.25,0.5,0.75,1))

print(paste0("sample size = ",nrow(run_times)))
print(paste0("sample mean = ",mean(run_times$times)))
print(paste0("sample median = ",mean(run_times$times)))
print(paste0("sample variance = ",var(run_times$times)))
print(paste0("sample std dev = ",sd(run_times$times)))
print(paste0("sample minimum = ",min(run_times$times)))
print(paste0("sample maximum = ",max(run_times$times)))
print(paste0("sample range = ",range(run_times$times)))
print(paste0("first quartile = ",quantile(run_times$times, probs = c(0.25))))
print(paste0("third quartile = ",quantile(run_times$times, probs = c(0.75))))
#calculate CV
cv <- sd(run_times$times) / mean(run_times$times) * 100
cv

ggplot(battery, aes(x=remaining_life)) +
  geom_histogram( binwidth=.025, fill="purple3", color="gray90", alpha=0.9) +
  geom_density(color = "blue", fill = "orange2", alpha = 0.6) +
  ggtitle("Remaining Battery Life with Bin size = 0.25 mAh") +
  theme(
    plot.title = element_text(size=20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
  )

annotations <- data.frame(
  x = c(round(min(run_times$times), 2), round(mean(run_times$times), 2), round(max(run_times$times), 2)),
  y = c(4, 12, 3),
  label = c("Min:", "Mean:", "Max:")
) 
ggplot(run_times, aes(x=times)) +
  geom_histogram(binwidth=20, color = "#000000", fill = "#0099F8") +
  geom_text(data = annotations, aes(x = x, y = y, label = paste(label, x)), size = 5, fontface = "bold") +
  ggtitle("Army 2-mile Run Times with Bin size = 20 Seconds") +
  theme(
    plot.title = element_text(size=20),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15),
  )

ggplot(run_times, aes(x=times)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "blue2") +
  geom_density(color = "red3", size = 2, fill = "skyblue", alpha = 0.6) +
  ggtitle("Army 2-mile Run Times with Bin size = 20 Seconds") +
  theme(
    plot.title = element_text(size=20),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15),
  )

# Computer faults dataframe
faults <- read.csv("C:\\Users\\jeff\\OneDrive\\Books\\Prob and Stats\\Data\\comp_faults.csv")
faults %>%
  count(computer_number, fault, sort=T)

# Creating two-way table from data frame
Faults_Table <- addmargins(table("Fault"=faults$computer_number,
                                         "Frequency"=faults$fault))
# View table
Faults_Table

library(probs)

# Create Probabilty Space
Faults_Data <- probspace(faults)
Faults_Data

# Conditional Probabilities
Cumputer1_fault_prob <- Prob(Faults_Data, event=computer_number == "1", given=fault == "1")
Cumputer1_fault_prob
Cumputer1_nofault_prob <- 1-Cumputer1_fault_prob
Cumputer1_nofault_prob

Cumputer2_fault_prob <- Prob(Faults_Data, event=computer_number == "2", given=fault == "1")
Cumputer2_fault_prob
Cumputer2_nofault_prob <- 1-Cumputer1_fault_prob
Cumputer1_nofault_prob

Cumputer3_fault_prob <- Prob(Faults_Data, event=computer_number == "3", given=fault == "1")
Cumputer3_fault_prob
Cumputer3_nofault_prob <- 1-Cumputer1_fault_prob
Cumputer3_nofault_prob

# Independent Event Probabilties
Prob_111 <-Cumputer1_fault_prob*Cumputer2_fault_prob*Cumputer3_fault_prob
Prob_110 <-Cumputer1_fault_prob*Cumputer2_fault_prob*Cumputer3_nofault_prob
Prob_101 <-Cumputer1_fault_prob*Cumputer2_nofault_prob*Cumputer3_fault_prob
Prob_100 <-Cumputer1_fault_prob*Cumputer2_nofault_prob*Cumputer3_nofault_prob

Prob_011 <-Cumputer1_nofault_prob*Cumputer2_fault_prob*Cumputer3_fault_prob
Prob_010 <-Cumputer1_nofault_prob*Cumputer2_fault_prob*Cumputer3_nofault_prob
Prob_001 <-Cumputer1_nofault_prob*Cumputer2_nofault_prob*Cumputer3_fault_prob
Prob_000 <-Cumputer1_nofault_prob*Cumputer2_nofault_prob*Cumputer3_nofault_prob

# Probaility of computers in use
print(paste0("Computer1 fails, Computer2 fails, Computer3 fails =", round(Prob_111,8)))
print(paste0("Computer1 fails, Computer2 fails, Computer3 works =", round(Prob_110,8)))
print(paste0("Computer1 fails, Computer2 works, Computer3 fails =", round(Prob_101,8)))      
print(paste0("Computer1 fails, Computer2 works, Computer3 works =", round(Prob_100,8)))
print(paste0("Computer1 works, Computer2 fails, Computer3 fails =", round(Prob_011,8)))
print(paste0("Computer1 works, Computer2 fails, Computer3 works =", round(Prob_010,8)))
print(paste0("Computer1 works, Computer2 works, Computer3 fails =", round(Prob_001,8)))
print(paste0("Computer1 works, Computer2 works, Computer3 works =", round(Prob_000,8)))

# Probability of a system failure
print(paste0("Probability of a system failure =", round(Prob_111,8)))

library(ggplot2)
cylinders <- read.csv("C:\\Users\\jeff\\OneDrive\\Books\\Prob and Stats\\Data\\cylinders.csv")
ggplot(cylinders, aes(x=diameter)) +
  geom_histogram( binwidth=0.05, fill="skyblue", color="blue", alpha=.75) +
  annotate("text", label = "Outlier", x = 50.35, y = 2.5, size = 8, colour = "red") +
  ggtitle("Diameter of metal cylinders") +
  theme(
    plot.title = element_text(size=20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
  )

ggplot(cylinders, aes(x=diameter)) +
  geom_histogram( binwidth=0.075, fill="skyblue", color="blue", alpha=.75) +
  ggtitle("Diameter of metal cylinders") +
  theme(
    plot.title = element_text(size=20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
  )

ggplot(cylinders, aes( y = diameter)) +
  geom_boxplot(fill = "grey80", color = "blue", lwd=2) +
  coord_flip() +
  scale_x_discrete() + 
  xlab("") +
  ylab("Diameter of metal cylinders") +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))

boxplot(cylinders,horizontal=TRUE)  


sim_data <- mvrnorm(n = 500, mu = 10, Sigma = 3)

data = rnorm(n = 500, mean = 10, sd = 3)
hist(data)
mean(data)

library(MASS)
Sigma <- matrix(c(10,3,3,2),2,2)
Sigma
var(mvrnorm(n = 1000, rep(0, 2), Sigma))


set.seed(23)
#rnorm(500, mean = rep(c(10,10,10),each=50), sd = 3)

m1<-matrix(rnorm(15000,10,3),ncol=500)
df1<-as.data.frame(m1)

mean(df1$V2)
sd(df1$V2)
summary(df1$V2)

mns_m<-as.data.frame(colMeans(df1))
mns_m<-mns_m %>% 
  rename(
    means=`colMeans(df1)`
  )
head(mns_m)

result <- t.test(df1)

mns_v<-as.data.frame(colVars(df1))
mns_v<-mns_v %>% 
  rename(
    var=`colVars(df1)`
  )
head(mns_v)

ci_out <- cbind(CI_tdist(df1[,c(1:500)],97.5))

sim_data <- cbind(mns_m['means'], sqrt(mns_v['var']), CI_tdist(df1[,c(1:500)],97.5))

ggplot(mns, aes(x=means)) +
  geom_histogram(binwidth=.1, color = "#000000", fill = "#0099F8") 

mean(mns$means)
var(mns$means) 
min(mns$means)
max(mns$means)
quantile(mns$means, probs = c(0.25))
quantile(mns$means, probs = c(0.75))

##############

mns3<-as.data.frame(colVars(df1))
mns3<-mns3 %>% 
  rename(
    vars1=`colVars(df1)`
  )
head(mns3)

ggplot(mns3, aes(x=vars1)) +
  geom_histogram(binwidth=1, color = "#000000", fill = "#0099F8") 

summary(mns3)

mean(mns3$vars1)
sd(mns3$vars1)
var(mns3$vars1) 
min(mns3$vars1)
max(mns3$vars1)
quantile(mns3$vars1, probs = c(0.25))
quantile(mns3$vars1, probs = c(0.75))
###############


set.seed(1)
rchi<-as.data.frame(rchisq(100000, df = 7))
rchi<-rchi %>% 
  rename(
    means=`rchisq(1e+05, df = 7)`
  )
ggplot(rchi, aes(x=means)) +
  geom_histogram(binwidth=1, color = "#000000", fill = "#0099F8")

m2<-matrix(rchisq(100000,df = 7),ncol=500)
df2<-as.data.frame(m2)
mean(df2$V2)
sd(df2$V2)
summary(df2$V1)

mean(colMeans(df2[1:10], na.rm = TRUE))
mns2<-as.data.frame(colMeans(df2))

mns2<-mns2 %>% 
  rename(
    means=`colMeans(df2)`
  )
head(mns2)

ggplot(mns2, aes(x=means)) +
  geom_histogram(binwidth=.1, color = "#000000", fill = "#0099F8") 

summary(mns2)

mean(mns2$means)
var(mns2$means) 
min(mns2$means)
max(mns2$means)
quantile(mns2$means, probs = c(0.25))
quantile(mns2$means, probs = c(0.75))



######### CI
#USAGE: CI_normal(matrix[,c(x:y)],97.5) - multiple column from a normal distribution
#USAGE: CI_tdist(matrix[,c(x:y)],97.5) - multiple column from a t distribution
#USAGE: CI_normal(matrix[,x],97.5) - single column from a normal distribution
#USAGE: CI_tdist(matrix[,x],97.5) - single column from a t distribution
#Created and updated by Santhilal Subhash on 2016/06/07 
CI_normal <- function(li,stat)
{
  cat(paste0("CI","\t","column","\t","Lower.limit","\t","Upper.limit","\n"))
  if(length(colnames(li))>1)
  {
    for(i in 1:length(colnames(li)))
    {
      sample <- colnames(li)
      error <- qnorm(stat/100)*sd(li[,i])/sqrt(length(li[,i]))
      left <- mean(li[,i])-error
      right <- mean(li[,i])+error
      cat(paste0(stat,"% norm dist","\t",sample[i],"\t",left,"\t",right,"\n"))
      
    }
  }
  else{
    error <- qnorm(stat/100)*sd(li)/sqrt(length(li))
    left <- mean(li)-error
    right <- mean(li)+error
    cat(paste0(stat,"% norm dist","\tNA","\t",left,"\t",right,"\n"))
    
  }
  
}
CI_tdist <- function(li,stat)
{
  cat(paste0("CI       ","\t","column","\t","Mean","\t","SD","\t","Lower.limit","\t","Upper.limit","\n"))
  if(length(colnames(li))>1)
  {
    for(i in 1:length(colnames(li)))
    {
      sample <- colnames(li)
      error <- qt(stat/100,df=length(li[,i])-1)*sd(li[,i])/sqrt(length(li[,i]))
      means <- round(mean(li[,i]),4)
      sds <- round(sqrt(var(li[,i])),4)
      left <- round(mean(li[,i])-error,4)
      right <- round(mean(li[,i])+error,4)
      cat(paste0(stat,"% t-dist","\t",sample[i],"\t",means,"\t",sds,"\t",left,"\t",right,"\n"))
      
    }
  }
  else{
    
    error <- qt(stat/100,df=length(li)-1)*sd(li)/sqrt(length(li))
    left <- mean(li)-error
    right <- mean(li)+error
    cat(paste0(stat,"% t-dist","\tNA","\t",left,"\t",right,"\n"))
    
  }
  
}

cbind(CI_tdist(df1[,c(1:500)],97.5))
