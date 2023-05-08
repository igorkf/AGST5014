##################
# Question 3
##################
q3 <- read.csv("final_exam/Final_Q3.csv")

# creating response
q3 <- transform(q3, breed = factor(breed), gain = final_weight - initial_weight) 

with(q3, table(breed))  
# every treatment occurs 10 times

boxplot(gain ~ breed, data = q3) 
# the distribution of gain is very different between breed levels

mod1 <- aov(gain ~ breed, data = q3)
summary(mod1)  
# as we suspected, the breed is significant using a significance level of 5%

plot(mod1, which = 1)
# residuals vs fitted shows an horizontal pattern

plot(mod1, which = 2)  
# the residuals seems normal

shapiro.test(mod1$residuals)  
# we don't reject the null hypothesis that residuals are normally distributed

plot(mod1, which = 5)
# the residual variances are similar across the different breed levels, so we have homogeneity of variances

plot(mod1$residuals ~ rownames(q3), data = q3)
# the residuals vs exp. unit plot shows an horizontal pattern, which means we had a proper randomization

emmeans::emmeans(mod1, pairwise ~ breed)  # post-hoc analysis on breed
# all the pairwise differences are significant and the highest predicted mean gain was the breed B, so the breed B was the best


########################
# Question 7
########################
q7 <- read.csv("final_exam/Final_Q7.csv")
q7 <- transform(q7, 
                block = factor(block),
                irrigation = factor(irrigation),
                spacing = factor(spacing))
# row spacing: whole-plot
# irrigation: sub-plot
library(nlme)
mod2 <- lme(
  Y ~ spacing * irrigation,
  random = ~1|block/spacing, 
  data = q7
)
car::Anova(mod2, type = 'II')

# The spacing factor (whole-plot) is not significant, so we don't reject the null hypothesis that
# all the spacing mean levels are equal, using a significance level of alpha = 0.05.

