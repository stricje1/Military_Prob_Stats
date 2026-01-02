library(readr)
petro <-read.csv("C:\\Users\\jeff\\OneDrive\\Books\\Prob and Stats\\Data\\petro2.csv")

head(petro)

summary(petro)


plot(petro, col = 'dodgerblue')

library("skimr")
s1 <- skim(petro)

p1<-print(paste0("sample variable name    = ",s1$skim_variable[1]))
p2<-print(paste0("sample mean value       = ",s1$numeric.mean[1]))
p3<-print(paste0("sample stand deviation  = ",s1$numeric.sd[1]))
p4<-print(paste0("sample minimum value    = ",s1$numeric.p0[1]))
p5<-print(paste0("sample lower percentile = ",s1$numeric.p25[1]))
p6<-print(paste0("sample median value     = ",s1$numeric.p50[1]))
p7<-print(paste0("sample upper percentile = ",s1$numeric.p75[1]))
p8<-print(paste0("sample maximum value    = ",s1$numeric.p100[1]))

q1<-print(paste0("sample variable name    = ",s1$skim_variable[2]))
q2<-print(paste0("sample mean value       = ",s1$numeric.mean[2]))
q3<-print(paste0("sample stand deviation  = ",s1$numeric.sd[2]))
q4<-print(paste0("sample minimum value    = ",s1$numeric.p0[2]))
q5<-print(paste0("sample lower percentile = ",s1$numeric.p25[2]))
q6<-print(paste0("sample median value     = ",s1$numeric.p50[2]))
q7<-print(paste0("sample upper percentile = ",s1$numeric.p75[2]))
q8<-print(paste0("sample maximum value    = ",s1$numeric.p100[2]))


p1;p2;p3;p4;p5;p6;p7;p8

q1;q2;q3;q4;q5;q6;q7;q8


KS <- ks.test(petro$X80_degrees, petro$X170_degrees)

KS$statistic
KS$p.value
KS$alternative
KS$method
KS$exact

data_mod <- melt(petro,
                  measure.vars=c('X80_degrees', 'X170_degrees')) 
# creating a plot 
p <- ggplot(data_mod) + 
  geom_boxplot(aes(x=value, y=variable, color=variable),
               fill = "white", size = 1.25,
               outlier.colour = "red", outlier.shape = 1)
  
p

ggplot(data_mod, aes(x = value, col = variable),log='x')+
  stat_ecdf(lwd = 1.2)+
  scale_x_log10()+
  labs(x="index",y="diameter")






library(ggplot2)
library("ggpubr")
theme_set(
  theme_bw() +
    theme(legend.position = "top")
)

p <- ggplot(petro, aes(x = index, y = X80_degrees)) + 
  geom_boxplot(aes(fill = supp), position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))
p

library(ggplot2)
library(plotly)
# Origrnal scale
p1 <- ggplot(petro, aes(x=index, y=X80_degrees))
p2 <- ggplot(petro, aes(x=index, y=X170_degrees))
p1 + geom_point(color = "dodgerblue", size = 3) 
p2 + geom_point(color = "dodgerblue", size = 3) 
#Log scale
#p2 <- ggplot(petro, aes(x=temp, y=log(diameter)))
#p2 + geom_point(color = "dodgerblue", size = 3) 
show(p1,p2)


# Scatter plots (sp)
sp <- ggscatter(petro, x = "index", y = "X80_degrees",
                add = "reg.line",               # Add regression line
                conf.int = TRUE,                # Add confidence interval
                color = "cyl", palette = "jco", # Color by groups "cyl"
                shape = "cyl"                   # Change point shape by groups "cyl"
)+
  stat_cor(aes(color = cyl), label.x = 3)       # Add correlation coefficient
sp



fit1 <- lm(diameter ~ temp, data = petro)
summary(fit1)

ggplot(petro, aes(x=temp, y=X80_degrees)) + 
  geom_point(shape=20, color="red3", size = 3)+
  geom_smooth(method=lm, se=FALSE, linetype="solid",
              color="blue", lwd = 1.25)

fit2 <- lm(log(diameter) ~ temp, data = petro)
summary(fit2)

ggplot(petro, aes(x=temp, y=log(diameter))) + 
  geom_point(shape=20, color="red3", size = 3)+
  geom_smooth(method=lm, se=FALSE, linetype="solid",
              color="blue", lwd = 1.25)

ggPredict(fit2$model, mode=1, pred=log(diameter), modx=temp, show.point = FALSE, se=TRUE, xpos=0.5)

fit3 <- lm(log(Planet_k) ~ Z3*Z6*Z12*Z15*Z18*Z21, data = geo_mag)
summary(fit2)


plot(fit1$residuals, col = 'red3', pch = 16, cex=1.25)


fit4 <- lm(log(diameter) ~ temp+I(temp^2), data = petro)
summary(fit4)

plot(fit4$residuals, col = 'dodgerblue', pch = 16, cex=1.25)

library(predict3d)
library(rgl)
predict3d(fit4, show.subtitle=FALSE, se=TRUE, radius = 0.15, xpos=0.5)
plot(fit4)
rglwidget(elementId = "1st")

library(ggplot2)
library(plotly)
# Create a basic 2D scatter plot with ggplot2

ggplot(petro, aes(temp, (diameter))) + geom_point(color = "dodgerblue", size = 3) + geom_smooth()


ggplot(fit4, aes(x = .fitted, y = .resid)) +
  geom_point(col = 'red4', size = 2) +
  geom_hline(yintercept = 0) +
  labs(title = "WD Yield at Various Temperatures",
       x = "Fitted Values",   y = "Residuals")


ks.test(fit4$fitted.values, "pexp", 0.0003240)

perf <-read.csv("C:\\Users\\jeff\\OneDrive\\Books\\Prob and Stats\\Data\\perf.csv")

library(patchwork)

df1 <- data.frame(perf$x, perf$lb)
df2 <- data.frame(perf$x, perf$F_x)
df3 <- data.frame(perf$x, perf$ub)

p1 <- df1 %>%
  ## Plotting measurements before treatment
  ggplot(aes(x = `x`, y = `perf.lb`))+
  geom_line()

p2 <- df2 %>%
  ## Plotting measurements after treatment
  ggplot(aes(x = `x`, y = `perf.F_x`))+
  geom_line()

p3 <- df3 %>%
  ## Plotting measurements after treatment
  ggplot(aes(x = `x`, y = `perf.ub`))+
  geom_line()

p1+p2+p3

ggplot() +
  geom_line(data = df1, aes(x = x1, y = y1), color = "black") +
  geom_line(data = df2, aes(x = x2, y= y2), color = "red")

data_mod1 <- melt(perf, measure.vars=c('x', 'F_x'))
data_mod2 <- melt(perf, measure.vars=c('x', 'lb'))
data_mod3 <- melt(perf, measure.vars=c('x', 'ub'))



# creating a plot 
ggplot(perf, aes(x = x, y = F_x))+
  stat_ecdf(lwd = 1.2, col = 'dodgerblue')+
  labs(x="x",y="F(x)")

ggplot(data_mod3, aes(x = value, y = variable))+
stat_ecdf(lwd = 1.2, col = 'dodgerblue')+
  labs(x="x",y="F(x)")

ggplot(perf, aes(x=x, y=F_x)) + 
  geom_point(aes(color=factor(lb))) +
  geom_point(aes(color=factor(F_x))) +
  geom_point(aes(color=factor(ub)))

ggplot(perf, aes(x = x, y = F_x)) + 
  stat_ecdf(lwd = 1.2, col = 'dodgerblue')+
  geom_point(shape = 20, color = "blue", size = 3) +
  geom_smooth(method = lm,  linetype = "solid",
              color = "red3", fill = "dodgerblue") 

fm1 <- lm(F_x ~ I(1-exp(-0.1*(x-5))), data = perf)

library(glmnet)
train <- perf[c(1,2)] 
Y <- perf[c(2)]
X <- model.matrix(Y ~x, train)
lasso_reg <- glmnet(x, y, alpha = 1,
                    lambda = 0.025)
lambda <- 10^seq(10, -2, length = 100)
cv.ridge_reg <- cv.glmnet(X_train, Y_train, alpha = 0,
                          nfolds = 5, type.measure = "deviance", 
                          trace.it = 1,
                          relax = FALSE)
plot(cv.ridge_reg)
fm1.prd.int <- predict(fm1, interval = "pred")
perf.dat.prd <- cbind(perf, fm1.prd.int)
head(perf.dat.prd, 10)

ggplot(data = perf.dat.prd, aes(x = x, y = F_x)) + 
  stat_ecdf(lwd = 1.2, col = 'blue4')+
  geom_ribbon(aes(ymin = lwr, ymax = upr), 
              fill = "yellow", alpha = 0.2) + 
  geom_ribbon(aes(ymin = perf.dat.prd$lwr, ymax = perf.dat.prd$upr), 
              fill = "yellow", alpha = 0.6) + 
  geom_line(aes(y = fit), color = "red", size = 1.5) +
  ggtitle("Regression Line, 95% Confidence and Prediction
	 Bands")

train <-read.csv("C:\\Users\\jeff\\OneDrive\\Books\\Prob and Stats\\Data\\perf.csv")
train$RSO_Weight[is.na(train$RSO_Weight)] <-mean(train$RSO_Weight, na.rm = TRUE) 





train <- train[c(-1)] 
Y <- train[c(2)]
X <- model.matrix(F_x ~., train)
lambda <- 10^seq(10, -2, length = 100)

set.seed(567)
part <- sample(2, nrow(X), replace = TRUE, prob = c(0.8, 0.2))
X_train <- X[part == 1,]
X_cv <- X[part == 2,]
Y_train <- Y[part == 1,]
Y_cv <- Y[part == 2,]

ridge_reg <- glmnet(X[X_train], Y[X_train], alpha = 0,
                    lambda = lambda)


plot(ridge_reg, lwd = 2)    

cv.ridge_reg <- cv.glmnet(X_train, Y_train, alpha = 0,
                          nfolds = 5, type.measure = "deviance", trace.it = 1, relax = FALSE)
plot(cv.ridge_reg)

cv.ridge_reg.coef <- predict(ridge_reg, type = "coefficients",
                             lambda = bestlam)[1:9]
print(paste("Variable: Intercept =", cv.ridge_reg.coef[1]))
print(paste("Variable:", names(train), "=",
            cv.ridge_reg.coef[2:9]))

par(mar = c(11,4,2,1))
plotlabels <- names(train[1:8])
barplot(cv.ridge_reg.coef[2:9], main = "Model 2 Coefficients",
        ylab = "Coefficients", las = 2, cex = 1, cex.lab = 1,
        cex.main = 1.25, cex.sub = 1, cex.axis = 1, las = 2,
        col = "green3", names = plotlabels)


lasso_reg <- glmnet(X_train, Y_train, alpha = 1,
                    lambda = lambda)
bestlam <- lasso_reg$lambda.min

par(mfrow = c(1, 2))
plot(lasso_reg, lwd = 2)
plot(lasso_reg, xlim =c(-5,0), xvar = "lambda", label = TRUE,
     lwd = 2)
lasso_reg_cv = cv.glmnet(X_train, Y_train, alpha = 1)
coef(lasso_reg_cv)
par(mfrow = c(1,1))
plot(lasso_reg_cv)

pred <- predict(lasso_reg_cv, X_train, s = "lambda.min") 
plot(pred, col = 'dodgerblue', pch = 20)

ggplot(perf, aes(x = x, y = F_x))+
  stat_ecdf(lwd = 1.2, col = 'dodgerblue')+
  labs(x="x",y="F(x)")


data_mod <- melt(perf,
                 measure.vars=c('lb', 'F_x', 'ub')) 
# creating a plot 
ggplot(data_mod, aes(x = value, col = variable),log='x')+
  stat_ecdf(lwd = 1.2)+
  scale_x_log10()+
  labs(x="index",y="diameter")


visual1 = data.frame(perf$x, perf$lb)
visual2 = data.frame(perf$x, perf$F_x)
visual3 = data.frame(perf$x, perf$ub)

visuals = cbind(visual1,visual2)
visuals$vis=c(rep("visual1",19),rep("visual2",19))

visuals = cbind(visual1,visual2)
visuals = visuals[c(-1)]

  
  ggplot(visuals, aes(perf.x,perf.F_x,group=perf.lb,col=perf$lb)) + 
  geom_point() + geom_smooth()
  
  f<-function(x)+1-exp(-0.1*(x-5))
  
  colors <- c("ub" = "green", "F_x" = "dodgerblue", 
              "lb" = "red", "f" = "gray")
  
  
  p <- ggplot(perf, aes(x = x, y = F_x)) +
    # blue plot
    geom_line(data=visual1, aes(x=perf.x, y=perf.lb), color="red", lwd = 1.25) +
    geom_point(data=visual1, aes(x=perf.x, y=perf.lb), color = "red") + 

    stat_ecdf(lwd = 1.25 , col = 'dodgerblue') +

    geom_line(data=visual3, aes(x=perf.x, y=perf.ub), color="green", lwd = 1.25) +
    geom_point(data=visual3, aes(x=perf.x, y=perf.ub), color = "green")  +

    geom_function(fun=f, aes(x=x, y=F_x), lwd = 1.25, color = "gray50") +
    labs(x="index",y="diameter", color = "Legend")

  p 

  
  
  
calc <-read.csv("C:\\Users\\jeff\\OneDrive\\Books\\Prob and Stats\\Data\\calc.csv")
  
  head(calc)
  
  summary(calc)
  
  
  plot(calc, col = 'dodgerblue')
  
  library("skimr")
  s1 <- skim(calc)
  
  p1<-print(paste0("sample variable name    = ",s1$skim_variable[1]))
  p2<-print(paste0("sample mean value       = ",s1$numeric.mean[1]))
  p3<-print(paste0("sample stand deviation  = ",s1$numeric.sd[1]))
  p4<-print(paste0("sample minimum value    = ",s1$numeric.p0[1]))
  p5<-print(paste0("sample lower percentile = ",s1$numeric.p25[1]))
  p6<-print(paste0("sample median value     = ",s1$numeric.p50[1]))
  p7<-print(paste0("sample upper percentile = ",s1$numeric.p75[1]))
  p8<-print(paste0("sample maximum value    = ",s1$numeric.p100[1]))
  
  q1<-print(paste0("sample variable name    = ",s1$skim_variable[2]))
  q2<-print(paste0("sample mean value       = ",s1$numeric.mean[2]))
  q3<-print(paste0("sample stand deviation  = ",s1$numeric.sd[2]))
  q4<-print(paste0("sample minimum value    = ",s1$numeric.p0[2]))
  q5<-print(paste0("sample lower percentile = ",s1$numeric.p25[2]))
  q6<-print(paste0("sample median value     = ",s1$numeric.p50[2]))
  q7<-print(paste0("sample upper percentile = ",s1$numeric.p75[2]))
  q8<-print(paste0("sample maximum value    = ",s1$numeric.p100[2]))
  
  
  p1;p2;p3;p4;p5;p6;p7;p8
  
  q1;q2;q3;q4;q5;q6;q7;q8
  
  
  KS <- ks.test(petro$X80_degrees, petro$X170_degrees)
  
  KS$statistic
  KS$p.value
  KS$alternative
  KS$method
  KS$exact
  
  data_mod <- melt(calc, measure.vars=c('CalcinerA', 'CalcinerB')) 
  # creating a plot 
  p <- ggplot(data_mod) + 
    geom_boxplot(aes(x=value, y=variable, color=variable),
                 fill = "white", size = 1.25,
                 outlier.colour = "red", outlier.shape = 1)
  
  p
  
  ggplot(data_mod, aes(x = value, col = variable),log='x')+
    stat_ecdf(lwd = 1.2)+
    scale_x_log10()+
    labs(x="index",y="diameter")
  
  # Data in two numeric vectors
  CalcinerA <- c(88.4,93.2,87.4,94.3,93.0,94.3,89.9,90.5,90.8,93.1,92.8,91.9)
  CalcinerB <- c(92.6,93.2,89.2,94.8,93.3,94.0,93.2,91.7,91.5,92.0,90.7,93.8) 
  # Create a data frame
  my_data <- data.frame( 
    group = rep(c("Calciner A", "Calciner B"), each = 12),
    Brightness = c(CalcinerA,  CalcinerB)
  )
  
  library(dplyr)
  group_by(my_data, group) %>%
    summarise(
      count = n(),
      median = median(Brightness, na.rm = TRUE),
      IQR = IQR(Brightness, na.rm = TRUE)
    )

  res <- wilcox.test(Brightness ~ group, data = my_data,
                     exact = FALSE)
  res

  # Print the p-value only
  res$p.value  
  