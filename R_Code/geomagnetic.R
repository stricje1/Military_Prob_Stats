library(readr)
radar <-read.csv("C:\\Users\\jeff\\OneDrive\\Books\\Prob and Stats\\Data\\radar_maint.csv")

head(radar)

summary(radar)

plot(radar, col = 'dodgerblue')

library(ggplot2)
library(plotly)
# Origrnal scale
p2 <- ggplot(radar, aes(x=Time, y=R_t))
p2 + geom_point(color = "dodgerblue", size = 3) 
#Log scale
#p2 <- ggplot(radar, aes(x=Time, y=log(R_t)))
#p2 + geom_point(color = "dodgerblue", size = 3) 
p2

fit1 <- lm(R_t ~ Time, data = radar)
summary(fit1)

ggplot(radar, aes(x=Time, y=R_t)) + 
  geom_point(shape=20, color="red3", size = 3)+
  geom_smooth(method=lm, se=FALSE, linetype="solid",
              color="blue", lwd = 1.25)

fit2 <- lm(log(R_t) ~ Time, data = radar)
summary(fit2)

ggplot(radar, aes(x=Time, y=log(R_t))) + 
  geom_point(shape=20, color="red3", size = 3)+
  geom_smooth(method=lm, se=FALSE, linetype="solid",
              color="blue", lwd = 1.25)

ggPredict(fit2$model, mode=1, pred=log(R_t), modx=Time, show.point = FALSE, se=TRUE, xpos=0.5)

fit3 <- lm(log(Planet_k) ~ Z3*Z6*Z12*Z15*Z18*Z21, data = geo_mag)
summary(fit2)


plot(fit1$residuals, col = 'red3', pch = 16, cex=1.25)


fit4 <- lm(log(R_t) ~ Time+I(Time^2), data = radar)
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

ggplot(radar, aes(Time, (R_t))) + geom_point(color = "dodgerblue", size = 3) + geom_smooth()


ggplot(fit4, aes(x = .fitted, y = .resid)) +
  geom_point(col = 'red4', size = 2) +
  geom_hline(yintercept = 0) +
  labs(title = "WD Yield at Various Temperatures",
       x = "Fitted Values",   y = "Residuals")





