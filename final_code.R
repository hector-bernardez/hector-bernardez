library(broom)
library(car)
library(dplyr)
library(ggplot2)
library(infer)
library(magrittr)
library(readr)
library(readxl)
library(tseries)
library(tidyr)
############################################################################################

cookie_cats <- read_csv("cookie_cats.csv")

head(cookie_cats)

summary(cookie_cats)

#Overall 7 day retention rate
cookie_cats %>% summarise(retention_7_rate=mean(retention_7))

#Overall 1 day retention rate
cookie_cats %>% summarise(retention_1_rate=mean(retention_1))

#Players who never played the game post installation
cookie_cats %>% filter(sum_gamerounds==0) %>% count()

#Difficulty vs level
difficulty <- cookie_cats %>% group_by(sum_gamerounds) %>% count %>% filter(sum_gamerounds<=150)
ggplot(data=difficulty, mapping = aes(x=sum_gamerounds, y=n)) + geom_line()

################################################################################################################

#Cross tabulation

#Tabulating 7-day retention
table_7 <- table(cookie_cats$version, cookie_cats$retention_7)
prop.table(x=table_7, margin=1)

#Tabulating 1-day retention
table_1 <- table(cookie_cats$version, cookie_cats$retention_1)
prop.table(x=table_1, margin=1)

################################################################################################################

#Testing significance of differences in proportions/rates

#For retention_7, prop.test between versions
prop_test(x=cookie_cats,
          response=retention_7,
          explanatory=version,
          success="TRUE",
          alternative="less",
          order=c("gate_40", "gate_30"))

#For retention_1, prop.test between versions
prop_test(x=cookie_cats,
          response=retention_1,
          explanatory=version,
          success="TRUE",
          alternative="less",
          order=c("gate_40", "gate_30")) #Necessary, otherwise it becomes gate_30-gate_40
          
###############################################################################################################

#Testing sum_gamerounds between versions

#Tests of normality

#Shapiro Wilk test
shapiro.test(x=cookie_cats$sum_gamerounds)
#sample size > 5000 cannot be used

#Jarque-Bera Test
jarque.bera.test(x=cookie_cats$sum_gamerounds) #tseries package

#Using the Q-Q plot
qqnorm(cookie_cats$sum_gamerounds)
qqline(cookie_cats$sum_gamerounds)

#Removal of outlier
max <- max(cookie_cats$sum_gamerounds)
outlier <- which(cookie_cats$sum_gamerounds==max)
cookie_cats_no_outlier <- cookie_cats[-outlier,]

#qqplots without outlier
qqnorm(cookie_cats_no_outlier$sum_gamerounds)
qqline(cookie_cats_no_outlier$sum_gamerounds)

#Transforming the data with square root
cookie_cats_modified <- cookie_cats_no_outlier %>% mutate(sqrt_sum=sqrt(sum_gamerounds))

qqnorm(cookie_cats_modified$sqrt_sum)
qqline(cookie_cats_modified$sqrt_sum)

#Testing the homogeneity of variances
leveneTest(y=sum_gamerounds~version, data=cookie_cats)

#Mann-Whitney U-Test
version_40 <- cookie_cats %>% 
              filter(version=="gate_40") %>% 
              select(sum_gamerounds)

version_30 <- cookie_cats %>% 
              filter(version=="gate_30") %>% 
              select(sum_gamerounds)

wilcox.test(x=version_30$sum_gamerounds, y=version_40$sum_gamerounds)

############################################################################################

#Bootstrap estimates

#Bootstrapping the retention rate 1
bootstrap_function_1 <- function(cookie_cats){
  cookie_cats %>% 
    slice_sample(prop=1, replace=TRUE) %>%
    group_by(version) %>%
    summarise(retention_1_rate=mean(retention_1))
}

bootstrap_mean_1_spread <- data.frame()
bootstrap_mean_1 <- data.frame() 
for(i in 1:500){
  output <- bootstrap_function_1(cookie_cats)
  output_spread <- bootstrap_function_1(cookie_cats) %>% spread(key=version, value=retention_1_rate)
  bootstrap_mean_1 <- rbind(bootstrap_mean_1, output)
  bootstrap_mean_1_spread <- rbind(bootstrap_mean_1_spread, output_spread)
}

bootstrap_mean_1_spread_mutate <- bootstrap_mean_1_spread %>% mutate(diff=gate_30-gate_40)


#Plotting 1-day retention rate
ggplot(data=bootstrap_mean_1,
       mapping=aes(x=retention_1_rate,
                   colour=version)) + geom_density()

#Plotting 1-day retention difference
ggplot(data=bootstrap_mean_1_spread_mutate,
       mapping=aes(x=diff)) + geom_density()

############################################################################################

#Bootstrapping the retention rate 7
bootstrap_function_7 <- function(cookie_cats){
  cookie_cats %>% 
    slice_sample(prop=1, replace=TRUE) %>%
    group_by(version) %>%
    summarise(retention_7_rate=mean(retention_7))
}

bootstrap_mean_7_spread <- data.frame()
bootstrap_mean_7 <- data.frame() 
for(i in 1:500){
  output <- bootstrap_function_7(cookie_cats)
  output_spread <- bootstrap_function_7(cookie_cats) %>% spread(key=version, value=retention_7_rate)
  bootstrap_mean_7 <- rbind(bootstrap_mean_7, output)
  bootstrap_mean_7_spread <- rbind(bootstrap_mean_7_spread, output_spread)
}

bootstrap_mean_7_spread_mutate <- bootstrap_mean_7_spread %>% mutate(diff=gate_30-gate_40)


#Plotting 7-day retention rate
ggplot(data=bootstrap_mean_7,
       mapping=aes(x=retention_7_rate,
                   colour=version)) + geom_density()

#Plotting 1-day retention difference
ggplot(data=bootstrap_mean_7_spread_mutate,
       mapping=aes(x=diff)) + geom_density()
