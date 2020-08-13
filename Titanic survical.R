install.packages("tidyverse")
install.packages("titanic")
install.packages("dplyr")
install.packages("dslabs")
library(tidyverse)
library(titanic)
library(dplyr)
library(dslabs)
head(titanic_train)
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))


str(titanic_train)

 titanic_train %>% filter(!is.na(Age))  %>% group_by(Sex) %>% summarize(maxAge = max(Age))

##QQPLOT AGE DISTRIBUTION
 params <- titanic %>%
   filter(!is.na(Age)) %>%
   summarize(mean = mean(Age), sd = sd(Age)) 

 titanic %>% ggplot(aes(sample = Age)) + geom_qq(dparams = params) +    geom_abline()

##
t1 <- titanic %>% select(Age)
x1 <- titanic %>% filter(titanic$Survived == 0)
x1
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age, y= ..count.., fill= Survived)) + geom_density(alpha = 0.2)

titanic %>%
  ggplot(aes(x = Survived, fill = Sex)) +
  geom_bar(position = position_dodge())


  #BOXPLOT
head(titanic)
titanic %>% filter(Fare > 0) %>%
    ggplot(aes(Survived,Fare)) +
    scale_y_continuous(trans= "log2") + 
    geom_boxplot(alpha =0.2) 

titanic %>% filter(Fare > 0) %>%
  ggplot(aes(Survived,Fare)) +
  geom_boxplot(alpha =0.2) 

titanic %>% filter(Fare > 0 & Survived == 0) %>% summarize(sum=sum(Fare)) 
titanic %>% filter(Fare > 0 & Survived == 1) %>% summarize(sum=sum(Fare)) 

titanic %>%  ggplot(aes(Fare, Survived) +
  geom_bar()

titanic %>%  ggplot(aes(Pclass, fill=Survived)) +
  geom_bar(position = position_fill())

titanic %>%  ggplot(aes(Survived, fill=Pclass)) +
  geom_bar(position = position_fill())

titanic %>%  ggplot(aes(Age, ..count..,color=Pclass)) +
  geom_density()

titanic %>%  ggplot(aes(Pclass)) +
  geom_bar()

titanic %>%  ggplot(aes(Age,..count.., fill=Survived)) +
  geom_density(alpha=0.2) + facet_grid(Pclass~Sex)


# barplot of passenger class filled by survival
titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar() +
  ylab("Count")
# barplot of passenger class filled by survival with position_fill
titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar(position = position_fill()) +
  ylab("Proportion")
# Barplot of survival filled by passenger class with position_fill
titanic %>%
  ggplot(aes(Survived, fill = Pclass)) +
  geom_bar(position = position_fill()) +
  ylab("Proportion")

titanic %>%
  filter(Fare > 0) %>%
  ggplot(aes(Survived, Fare)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log2") +
  geom_jitter(alpha = 0.2)
