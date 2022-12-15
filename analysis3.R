library(magrittr)
library(ggplot2)
library(readr)
Fatalities <- read.csv("Fatalities.csv")
Fatalities=na.omit(Fatalities)
#simple lr
#plot
model=lm(nfatal2124~afatal,data=Fatalities)

plot1=Fatalities %>% 
  ggplot(aes(x = afatal, y = nfatal)) +
  geom_point(color="#000099")+coord_cartesian(xlim=c(min(Fatalities$afatal),max(Fatalities$afatal)),ylim=c(min(Fatalities$nfatal),max(Fatalities$nfatal))) + geom_smooth(method = "lm", col = "#FF3366" )
plot1+theme_classic()
#summary
print(summary(model))

model=lm(spirits~afatal,data=Fatalities)
plot1=Fatalities %>% ggplot(aes(x = afatal, y = spirits)) +geom_point(color="#000099") + geom_smooth(method = "lm", col = "#FF3366")
plot1+theme_classic()
#summary
print(summary(model))

qqplot(Fatalities$spirits, Fatalities$afatal)

#Two sample test:
#H0: the mean of afatal which breath is yes is equal to the mean of afatal which breath test is no

#H1: the mean of afatal which breath is yes is not  equal to the mean of afatal which breath test is no

breathy <- filter(Fatalities, Fatalities$breath == 'yes') 
breathno <- filter(Fatalities, Fatalities$breath == 'no')
ggplot(Fatalities, aes(Fatalities$afatal)) + 
  geom_density(aes(data = Fatalities$afatal, fill = Fatalities$breath),   position = 'identity', alpha = 0.5) +
  labs(x = 'death w/alchol', y = 'Density') + scale_fill_discrete(name = 'Breath') + theme_classic()



breathy <- filter(Fatalities, Fatalities$breath == 'yes') 
breathno <- filter(Fatalities, Fatalities$breath == 'no')
test <- t.test(breathy[,"afatal"], breathno[,"afatal"])
test


#From the output of the test we can see the p-value is equal to 0.001903,
#which is much less than 0.05. Therefore, we reject H0 and conclude
#there is a difference between the mean death wtih alchol of the breath test and no breath test.

#One sample test:
#H0:mean of drinkage=21
#H1:mean of drinkage !=21

test2=t.test(Fatalities$drinkage, mu=21)
test2


#From the output of the test we can see the p-value is less than  2.2e-16,
#which is much less than 0.05. Therefore, we reject H0 and conclude
#the mean of drinkage is not equal to 21.
