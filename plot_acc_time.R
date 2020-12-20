library(tidyverse)
library(ggplot2)
library(RColorBrewer)

data_time <- read.table("finalTables/fullFinalTable_time.txt")
train$DIQ010<-as.factor(train$DIQ010)
levels(train$DIQ010)<-c("diabetes", "no_diabetes")

#plot diabetes by year
data_time$SDDSRVYR <- as.factor(data_time$SDDSRVYR)
levels(data_time$SDDSRVYR) <- c("2007-2008", "2009-2010","2011-2012","2013-2014", "2015-2016", "2017-2018")


diabetes_by_year <- data_time %>% group_by(SDDSRVYR) %>% 
  summarise(sum(DIQ010 == 1)/(sum(DIQ010 == 1)+ sum(DIQ010 == 2)))

ggplot(diabetes_by_year, aes(x = SDDSRVYR, y = `sum(DIQ010 == 1)/(sum(DIQ010 == 1) + sum(DIQ010 == 2))`))+ 
  geom_line(group = 1)+ geom_point(color = "red")+ xlab("Year")+ ylab("Proportion of diabetes")+
  ggtitle("Proportion of diabetes in participants")

#plot health condition by year
data_time$HSD010<-as.factor(data_time$HSD010)
levels(data_time$HSD010)<-c("excellent", "very_good", "good", "fair", "poor")

health_con_prop_by_year <- data_time %>% group_by(SDDSRVYR) %>% 
  summarise(excellent = mean(HSD010 == 'excellent'), very_good = mean(HSD010 == 'very_good'),
            good = mean(HSD010 == 'good'), fair = mean(HSD010 == 'fair'), 
            poor = mean(HSD010 == 'poor')) %>% gather(Health_condition, proportion, -c(SDDSRVYR))

health_con_prop_by_year$Health_condition<- factor(health_con_prop_by_year$Health_condition, levels = c("excellent", "very_good", "good", "fair", "poor"))

ggplot(health_con_prop_by_year, aes(x = SDDSRVYR, y = proportion))+ geom_bar(aes(fill = Health_condition), stat = "identity")+
  xlab("Year")+ ylab("Proportions of participants")+
  ggtitle("Health condition over 10 years")+ scale_fill_brewer(palette = "YlGn", labels = c("Excellent", "Very good", "Good", "Fair", "Poor"))+
  labs(fill = "Health condition") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()


#plot health diet
data_time$DBQ700<-as.factor(data_time$DBQ700)
levels(data_time$DBQ700)<-c("excellent", "very_good", "good", "fair", "poor")

health_diet_prop_by_year <- data_time %>% group_by(SDDSRVYR) %>% 
  summarise(excellent = mean(DBQ700 == 'excellent'), very_good = mean(DBQ700 == 'very_good'),
            good = mean(DBQ700 == 'good'), fair = mean(DBQ700 == 'fair'), 
            poor = mean(DBQ700 == 'poor')) %>% gather(Diet_health, proportion, -c(SDDSRVYR))

health_diet_prop_by_year$Diet_health <- factor(health_diet_prop_by_year$Diet_health, levels = c("excellent", "very_good", "good", "fair", "poor"))

ggplot(health_diet_prop_by_year, aes(x = SDDSRVYR, y = proportion))+ geom_bar(aes(fill = as.factor(Diet_health)), stat = "identity")+
  xlab("Year")+ ylab("Proportions of participants")+
  ggtitle("Diet health over 10 years")+  
  scale_fill_brewer(palette = "YlOrRd", labels = c("Excellent", "Very good", "Good", "Fair", "Poor"))+
  labs(fill = "Diet health") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  coord_flip()


