data <- read.csv("Fatalities.csv")

library(dplyr)
head(data)
str(data)
summary(data)

# QUESTION 8

data$jail <- factor(data$jail,
                    levels = c("no","yes"),
                    labels = c(0,1))

data$service <- factor(data$service,
                    levels = c("no","yes"),
                    labels = c(0,1))

table(data$jail, data$service)

two_aov <- aov(data$fatal ~ data$jail + data$service + data$jail:data$service, data= data)

summary(two_aov)

plot(two_aov,1)
plot(two_aov,2)

# QUESTION 9

new_york_fatal_pop <- subset(data, state== "ny", select = c(fatal1517, fatal2124, pop1517, pop2124))


p1 <- mean(new_york_fatal_pop$fatal1517 / new_york_fatal_pop$pop1517)

p2 <- mean(new_york_fatal_pop$fatal2124 / new_york_fatal_pop$pop2124)

p_bar <- (mean(new_york_fatal_pop$fatal1517) + mean(new_york_fatal_pop$fatal2124)) /  (mean(new_york_fatal_pop$pop1517) + mean(new_york_fatal_pop$pop2124))
q_bar <- 1 - p_bar

z_score_new_york <- (p1 - p2) / sqrt(p_bar*q_bar*( (1 / new_york_fatal_pop$pop1517[1]) + (1 / new_york_fatal_pop$pop2124[1])))
# -3.872306 < -1.96    ----> Reject H0   (New York - Between 1982-1988)


prop.test(c(new_york_fatal_pop$fatal1517[1] , new_york_fatal_pop$fatal2124[1] ), c(new_york_fatal_pop$pop1517[1], new_york_fatal_pop$pop2124[1]))
# p-value = 1.8e-0.7 (New York - 1982)


# QUESTION 7

nevada_prop <- subset(data, state== "nv", select = c(fatal, pop))

p_hat <- mean((nevada_prop$fatal) / (nevada_prop$pop))

p_zero <- 0.0001
q_zero <- 1 - p_zero

z_score_nevada <- (p_hat - p_zero) / sqrt((p_zero*q_zero) / mean(nevada_prop$pop)) 
# 17.01922 > 1.96 ----> Reject H0   (Nevada - Between 1982-1988)

mean(data$fatal / data$pop) #observed proportion of all data