install.packages("sjPlot")
library(sjPlot)

fatal <- read.csv("Fatalities.csv")

head(fatal)

leveneTest(fatal~service*jail, data = fatal)


fatal$jail <- factor(fatal$jail,
                    levels = c("no","yes"),
                    labels = c(0,1))

fatal$service <-factor(fatal$service,
                      levels = c("no","yes"),
                      labels = c(0,1))
fatal$fatal <- log(fatal$fatal)
table(fatal$jail, fatal$service)



two_aov <- aov(fatal ~ jail + service + jail:service, data= fatal)



summary(two_aov)

plot(two_aov,1)
plot(two_aov,2)


#decription
summary(fatal)

#multp regression between total fatality vs youngdriver and drinkage
fatalmlm <- lm(fatal ~ drinkage + youngdrivers, data = fatal)

summary(fatalmlm)


#multlp regression between alcholol fatalities vs beer tax
fatalmlm2 <- lm(afatal ~ income + beertax, data = fatal)

leveneTest(number~type, data = b2)
#for the anova, i created a new dataframe. In the type column;
#fatalyoung = fatal1517, fatalmid = fatal1820, fatalold = fatal2124
#types should be factors, fatalities should be numeric

b2 <- read.csv("Book5.csv")
b2$number <- log(b2$number)
b2$type <- as.factor(b2$type)
b2$number <- as.numeric(b2$number)

summary(b2)
str(b2)
#one-way ANOVA test and results
fatal.anova <- aov(number~type, data = b2)
summary(fatal.anova)
plot(fatal.anova,2)
plot(fatal.anova,1)

library(dplyr)
library(ggplot2)

a1 <- subset(b2, type == "fatalyoung")
a2 <- subset(b2, type == "fatalmid")
a3 <- subset(b2, type == "fatalold")

df <- rbind(a1, a2, a3)

df%>%ggplot(aes(x=number, fill=type))+geom_density(alpha=0.5)+labs(title = "Density Graph of Driver Types") + theme_classic()

model1 <- lm(number ~ type, data = df)

anova(model1)
library(tidyverse)
library(multcomp)

summary(glht(model1, mcp(type = "Tukey")))
t.test(df%>%filter(type=="fatalold")%>%pull(), df%>%filter(type=="fatalmid")%>%pull())
plot(TukeyHSD(fatal.anova, conf.level=.95), las = 2)
#models

fatal$frate <- with(fatal, fatal/pop * 10000)

fmodel <- lm(fatal~pop1517, data = fatal)

summary(fmodel)

fplot <- ggplot(fatal, aes(x = pop1517, y = fatal)) + geom_point(colour = "purple", alpha = 0.5) +
  geom_smooth(colour = "black") + theme_classic()
fplot

cor(fatal$pop1517, fatal$fatal)

plot(fatal$pop1517, fatal$fatal)
abline(fmodel)


newsub <- subset(fatal, year == 1985, select=c(fatal, state, year))
rownames(newsub) = NULL
dataf <- data.frame(age_15_17 = sum(newsub$fatal1517), age_18_20 = sum(newsub$fatal1820), age_21_24 = sum(newsub$fatal2124))

totalnums <- c(dataf$age_15_17, dataf$age_18_20, dataf$age_21_24)

library(ggplot2)

d <- read.csv("Book1.csv")
ggplot(newsub, aes(x = fatal, y = state)) + geom_bar(stat = "identity", fill = "purple", colour = "purple", alpha = 0.5) + theme_classic() +
  geom_text(aes(label=fatal), size = 3, vjust = 0.4, hjust = -0.04) +
  labs(title = "Fatality Numbers in USA (1986)")

library(usmap)
library(ggplot2)

plot_usmap(data = newsub, values = "fatal", regions = "states", colour = "black") + 
  labs(title = "Fatality numbers in US",
       subtitle = "This map shows the fatalities in 1986.") +
  scale_fill_continuous(
    low = "white", high = "purple", name = "Fatality (1986)", label = scales::comma) +
  theme(panel.background = element_rect(color = "black", fill = "white"), legend.box.margin = margin(6, 6, 6, 6))
