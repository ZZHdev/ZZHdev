kicker<-read.table("D:\\FieldGoals2003to2006.txt",header=TRUE,sep=',')
View(kicker)
attach(kicker)
corF<-cor.test(FGt,FGtM1)
corF
fit.1 <- lm(FGt ~ FGtM1 + factor(Name), data = kicker)
summary(fit.1)
fit.2 <- lm(FGt ~ FGtM1 + factor(Name)-1, data = kicker)
summary(fit.2)
library(lme4)
fit.3 <- lmer(formula=FGt ~ FGtM1 +( 1|Name))
summary(fit.3)
fit.4 <- lmer(FGt ~ FGtM1 + (FGtM1|Name), data = kicker)
summary(fit.4)
coef(fit.4)$Name
library(ggplot2)
ggplot(kicker, aes(x =FGt , y =FGtM1 ,colour =Name )) +
  geom_line() + 
  geom_point() + 
  labs(title = "Field Goal Percentage in Year t-1",
       x = "FGt",
       y = "FGtM1") +
  theme_minimal() 
