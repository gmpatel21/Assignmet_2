#import Library

library(tidyverse)

#Assigning Dataframe
dft <- read.csv("titanic.csv")

glimpse(dft)

# giving factor to different column



dft$sex <- as.factor(dft$sex)
dft$who <- as.factor(dft$who)
dft$class <- as.factor(dft$class)
dft$alive <- as.factor(dft$alive)
dft$alone <- as.factor(dft$alone)



View(dft)

# making age 0 where age is below 1  

dft$age[dft$age < 1] <- 0

# replace null with average

  age <- dft %>% 
    select(age)

#drop nulls
age <- drop_na(age)

# making age 0 where age is below 1  

age$age[age$age < 1] <- 0
  mean(age$age)
  
# Mean- 29.691
  
dft <- dft %>% 
    mutate(age = replace_na(
    age,29.691))
# dropping some column

dft <- dft %>% 
  select(1:11,13:15)
View(dft)

# looking for all nulls and empty char with only space


view(is.na(dft) | dft == "") # which will give you TRUE/FALSE with sort in
                             # view window we can check all nulls or space char

# replace with unkown

dft<- dft %>%
  replace(is.na(dft),"unkown") %>% 
  replace(dft == "","unkown")
  
#Visualize the data

#bar chart

dft %>% 
  ggplot(mapping = aes(x = class))+
  geom_bar()+
  xlab("CLASS")+
  ggtitle("Count of each gender with class")

# another bar chart

dft %>% 
  drop_na(class,age) %>%
  ggplot(mapping = aes(class,fill=class)) +
  geom_bar(alpha=0.8)+
  facet_wrap(~who)+
  xlab("CLASS")+
  ggtitle("Count of each gender with category in each class")


#histogram

dfx <- read.csv("titanic.csv")
dft %>% 
  ggplot(aes(age))+
  geom_histogram()

# as it gives incorrect representation
#load data again only for visulization purpose

dfx <- read.csv("titanic.csv")

dfx %>% 
  drop_na(age) %>% 
  ggplot(aes(age))+
  geom_histogram()+
  xlab("AGE")+
  ggtitle("histogram of age over count")
  
#scatter plot

dft %>% 
  ggplot(mapping = aes(fare,age,color=who))+
  geom_point()+
  xlab("FARE")+
  ylab("AGE")+
  ggtitle("Age vs fare with Categorical class")


#box plot.

dft %>% 
  drop_na(embark_town,age) %>%
  ggplot(mapping = aes(y=age,x=embark_town,fill=embark_town)) +
  geom_boxplot()+
  xlab("Embark Town")+
  theme(axis.text.x = element_text(angle = 45))+
  ggtitle("Genral age from different Town")
