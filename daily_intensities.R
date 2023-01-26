library(tidyverse)
library(ggplot2)
library(readr)
library(skimr)

#read the csv file
daily_intensities <- read.csv("dailyIntensities_merged.csv")

#review the summary of data
skim_without_charts(daily_intensities)
head(daily_intensities)


#Create a summary table for average daily intensities
daily_intensities_changes <- daily_intensities %>% 
  group_by(ActivityDay) %>% 
    summarise(Daily_Sedentary = mean(SedentaryMinutes),Daily_LightlyActive = mean(LightlyActiveMinutes),
              Daily_FairlyActive = mean(FairlyActiveMinutes),Daily_VeryActive = mean(VeryActiveMinutes))
 

#change text to date
daily_intensities_changes$ActivityDay <- as.Date(daily_intensities_changes$ActivityDay, format='%m/%d')
head(daily_intensities_changes)

#stack the dataset
daily_intensities_changes_stacked <- stack(daily_intensities_changes)


#Changed the col name
daily_intensities_changes_stacked <- daily_intensities_changes_stacked %>% 
  rename(Level_of_Intensities = ind) %>% 
  rename(Minutes = values)

#check outliers
daily_intensities_changes_stacked %>%  
  ggplot() + 
  geom_boxplot(mapping = aes(x = Level_of_Intensities, y = Minutes, fill = Level_of_Intensities )) + 
  facet_wrap(~Level_of_Intensities, scales = 'free') +
  ggtitle("Average of Each Daily Intensities")

#filter out outliers (daily_intensities_changes)
s_q1 <- quantile(daily_intensities_changes$Daily_Sedentary , 0.25)
s_q3 <- quantile(daily_intensities_changes$Daily_Sedentary, 0.75)
s_iqr <- IQR(daily_intensities_changes$Daily_Sedentary)

l_q1 <- quantile(daily_intensities_changes$Daily_LightlyActive , 0.25)
l_q3 <- quantile(daily_intensities_changes$Daily_LightlyActive, 0.75)
l_iqr <- IQR(daily_intensities_changes$Daily_LightlyActive)

f_q1 <- quantile(daily_intensities_changes$Daily_FairlyActive , 0.25)
f_q3 <- quantile(daily_intensities_changes$Daily_FairlyActive , 0.75)
f_iqr <- IQR(daily_intensities_changes$Daily_FairlyActive )

v_q1 <- quantile(daily_intensities_changes$Daily_VeryActive, 0.25)
v_q3 <- quantile(daily_intensities_changes$Daily_VeryActive, 0.75)
v_iqr <- IQR(daily_intensities_changes$Daily_VeryActive)

daily_intensities_changes <- daily_intensities_changes %>% 
  filter((daily_intensities_changes$Daily_Sedentary > s_q1 -1.5*s_iqr & daily_intensities_changes$Daily_Sedentary < s_q3 +1.5*s_iqr)|
           (daily_intensities_changes$Daily_LightlyActive > l_q1 -1.5*l_iqr & daily_intensities_changes$Daily_LightlyActive < l_q3 +1.5*l_iqr)|
           (daily_intensities_changes$Daily_FairlyActive > f_q1 -1.5*f_iqr & daily_intensities_changes$Daily_FairlyActive < f_q3 +1.5*f_iqr)|
           (daily_intensities_changes$Daily_VeryActive > v_q1 -1.5*v_iqr & daily_intensities_changes$Daily_VeryActive < v_q3 +1.5*v_iqr))

#Create line graph
daily_intensities_changes %>%  
  ggplot() + 
  geom_smooth(aes(x = ActivityDay , y = Daily_Sedentary, color = 'Daily_Sedentary'),se = FALSE) +
  geom_smooth(aes(x = ActivityDay , y = Daily_LightlyActive, color = 'Daily_LightlyActive'),se = FALSE) +
  geom_smooth(aes(x = ActivityDay , y = Daily_FairlyActive, color = 'Daily_FairlyActive'),se = FALSE) +
  geom_smooth(aes(x = ActivityDay , y = Daily_VeryActive, color = 'Daily_VeryActive'),se = FALSE) +  
  labs( title = "Daily Intensities Trend", x = "Date", y = "Minutes") +
  scale_color_discrete(name = "Levels of intensities") 
 