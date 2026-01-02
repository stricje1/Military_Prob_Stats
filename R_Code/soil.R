soil <- as.data.frame(read.csv("C:\\Users\\jeff\\OneDrive\\Books\\Prob and Stats\\Data\\soil_aggr.csv"))
soil
library(ggplot2)
ggplot(soil, aes(x = Stress, y = Modulus)) +
  geom_point(size=3, shape=16, col='dodgerblue') +
  labs(title = "Rundway Base Aggregates",
       x = "Stress",
       y = "Resilient Modulus") +
  theme(axis.line = element_line(arrow = arrow(angle = 30,
                                             length = unit(0.15, "inches"),
                                             ends = "last", 
                                             type = "closed")))
library(MASS)
mod1 <- lm(Modulus~Stress, data=soil)
anova(mod1)


ggplot(soil, aes(x = Stress, y = Modulus)) + 
  geom_point(alpha=0.7, size = 2.5) +   
  geom_abline(slope = -3.2544,
              intercept = 1078.30,
              color='red', lwd=2,  alpha=0.5)

ggplot(soil, aes(x = Stress, y = Modulus)) +
  geom_point(size=3, shape=16, col='dodgerblue') +
  labs(title = "Rundway Base Aggregates",
       x = "Stress",
       y = "Resilient Modulus") +
  theme(axis.line = element_line(arrow = arrow(angle = 30,
                                               length = unit(0.15, "inches"),
                                               ends = "last", 
                                               type = "closed")))
ggplot(soil, aes(x = Stress, y = Modulus)) + 
  geom_point(shape=18, color="blue", size=3)+
  geom_smooth(method=lm,  linetype="solid", lwd=2,
              color="red3") # fill="dodgerblue"


soil <- as.data.frame(read.csv("C:\\Users\\jeff\\OneDrive\\Books\\Prob and Stats\\Data\\soil_aggr.csv"))
soil
soil.mod <- lm(Modulus ~ Stress, data = soil)
soil.mod.prd <- predict(soil.mod, interval = "conf")
soil.prd <- cbind(soil, soil.mod.prd)
head(soil.prd,10)

fm1.prd.int <- predict(soil.mod, interval = "pred")
soil.dat.prd <- cbind(soil, fm1.prd.int)
head(soil.dat.prd,10)

soil.stdres = rstandard(soil.mod)

ggplot(mapping = aes(sample = soil.stdres)) +
  stat_qq_point(size = 4,color = "red2") +
  stat_qq_line(color="dodgerblue", size = 2) + 
  ggtitle("Runway Base Aggregates - Normal Probability Plot") + 
  xlab("Stress") + ylab("Resilient Modulus")

ggplot(data = soil.dat.prd, aes(x = Stress, y = Modulus)) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "deepskyblue4", alpha = 0.2) + 
  geom_ribbon(aes(ymin = soil.prd$lwr, ymax = soil.prd$upr), fill = "deepskyblue", alpha = 0.6) + 
  geom_line(aes(y = fit), color = "white", size = 1.5) +
  geom_point() + 
  ggtitle("Regression Line, 95% Confidence and Prediction Bands")

ggplot(mapping = aes(sample = ln.soil.stdres)) +
  stat_qq_point(size = 4,color = "red2") +
  stat_qq_line(color="dodgerblue", size = 2) + 
  ggtitle("soil - Normal Probability Plot") + 
  xlab("Stress") + ylab("2-mile Modulus")

soil <- as.data.frame(read.csv("C:\\Users\\jeff\\OneDrive\\Books\\Prob and Stats\\Data\\soil_aggr.csv"))
ln.soil <- soil[3:4]
ln.soil

ln.soil.mod <- lm(LN.Modulus ~ LN.Stress, data = ln.soil)
ln.soil.mod.prd <- predict(ln.soil.mod, interval = "conf")
ln.soil.prd <- cbind(ln.soil, ln.soil.mod.prd)
head(ln.soil.prd,10)

summary(ln.soil.mod)

ln.soil.stdres = rstandard(ln.soil.mod)

ggplot(ln.soil, aes(x = LN.Stress, y = LN.Modulus)) +
  geom_point(size=3, shape=16, col='dodgerblue') +
  labs(title = "Rundway Base Aggregates Transformation",
       x = "ln(Stress)",
       y = "ln(Resilient Modulus)") +
  theme(axis.line = element_line(arrow = arrow(angle = 30,
                                               length = unit(0.15, "inches"),
                                               ends = "last", 
                                               type = "closed")))

ggplot(mapping = aes(sample = ln.soil.stdres)) +
  stat_qq_point(size = 2,color = "red2") +
  stat_qq_line(color="dodgerblue", size = 1.5) + 
  ggtitle("Runway Base Aggregates - Normal Probability Plot") + 
  xlab("Stress") + ylab("Resilient Modulus")

ggplot(ln.soil, aes(x = LN.Stress, y = LN.Modulus)) + 
  geom_point(alpha=0.7, size = 2.5) +   
  geom_abline(slope = -3.2544,
              intercept = 1078.30,
              color='red', lwd=2,  alpha=0.5)

ggplot(ln.soil, aes(x = LN.Stress, y = LN.Modulus)) + 
  geom_point(shape=18, color="blue", size=3)+
  geom_smooth(method=lm,  linetype="solid", lwd=2,
              color="red3", fill="dodgerblue")


ggplot(data = ln.soil, aes(x = LN.Stress, y = LN.Modulus)) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "deepskyblue4", alpha = 0.2) + 
  geom_ribbon(aes(ymin = ln.soil.prd$lwr, ymax = ln.soil.prd$upr), fill = "deepskyblue", alpha = 0.6) + 
  geom_line(aes(y = fit), color = "white", size = 1.5) +
  geom_point() + 
  ggtitle("Regression Line, 95% Confidence and Prediction Bands")

ggplot(ln.soil, aes(x = LN.Stress, y = LN.Modulus)) +
  geom_point(size=3, shape=16, col='dodgerblue') +
  geom_smooth(method=lm,  linetype="solid", lwd=2,
              color="red3") +
  labs(title = "Rundway Base Aggregates",
       x = "Stress",
       y = "Resilient Modulus") +
  theme(axis.line = element_line(arrow = arrow(angle = 30,
                                               length = unit(0.15, "inches"),
                                               ends = "last", 
                                               type = "closed")))



ggplot(mapping = aes(sample = soil.stdres)) +
  stat_qq_point(size = 4,color = "red2") +
  stat_qq_line(color="dodgerblue", size = 2) + 
  ggtitle("Runway Base Aggregates - Normal Probability Plot") + 
  xlab("Stress") + ylab("Resilient Modulus")

ggplot(mapping = aes(sample = ln.soil.stdres)) +
  stat_qq_point(size = 4,color = "red2") +
  stat_qq_line(color="dodgerblue", size = 2) + 
  ggtitle("Transformed Runway Base Aggregates - Normal Probability Plot") + 
  xlab("Stress") + ylab("Resilient Modulus")

ggplot(soil, aes(x = Stress, y = Modulus)) +
  geom_point(size=3, shape=16, col='dodgerblue') +
  labs(title = "Rundway Base Aggregates",
       x = "Stress",
       y = "Resilient Modulus") +
  theme(axis.line = element_line(arrow = arrow(angle = 30,
                                               length = unit(0.15, "inches"),
                                               ends = "last", 
                                               type = "closed")))

M=rbind(6380*4^(0.2494),6380*6^(0.2494),6380*11^(0.2494), 
        6380*16^(0.2494),6380*20^(0.2494),6380*24^(0.2494),
        6380*29^(0.2494),6380*34^(0.2494),6380*38^(0.2494),
        6380*41^(0.2494),6380*46^(0.2494),6380*51^(0.2494),
        6380*53^(0.2494),6380*59^(0.2494),6380*68^(0.2494),
        6380*75^(0.2494))


base <-
  ggplot() +
  xlim(4, 75)
base + geom_function(fun = function(x) 6380*x^(0.2494), lwd=2, col='dodgerblue') +
  geom_point(data=soil, aes(x=Stress, y=Modulus), size=3, col='red') +
  ggtitle("Runway Base Aggregates - Original Data with Curve") + 
  xlab("Stress") + ylab("Resilient Modulus")
