library(tidyverse) 
library(reshape2)
library(scales)
library(ggplot2)

dailyactivity_merged <- read.csv("D:/R Studio Files/Case Study 32/archive/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")

dailycalories_merged <- read.csv("D:/R Studio Files/Case Study 32/archive/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")

dailyintensities_merged <- read.csv("D:/R Studio Files/Case Study 32/archive/Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")

dailysteps_merged <- read.csv("D:/R Studio Files/Case Study 32/archive/Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")

sleepday_merged <- read.csv("D:/R Studio Files/Case Study 32/archive/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv") 

weightlog_merged <- read.csv("D:/R Studio Files/Case Study 32/archive/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")

Merge1 <- merge(dailyactivity_merged, dailycalories_merged, by = c("Id", "Calories"))

Merge2 <- merge(dailyintensities_merged, dailyintensities_merged,
  by = c("Id","ActivityDay","SedentaryMinutes", "LightlyActiveMinutes","FairlyActiveMinutes","VeryActiveMinutes", "SedentaryActiveDistance", "LightActiveDistance", "ModeratelyActiveDistance", "VeryActiveDistance"))

merge_daily <- merge(Merge1, Merge2, by = c("Id","ActivityDay","SedentaryMinutes", "LightlyActiveMinutes","FairlyActiveMinutes","VeryActiveMinutes", "SedentaryActiveDistance", "LightActiveDistance", "ModeratelyActiveDistance", "VeryActiveDistance")) %>%
  select(-ActivityDay) %>% rename(Date = ActivityDate)


colnames(sleepday_merged)[colnames(sleepday_merged) == "SleepDay"] <- "Date"

sleepday_merged$Date <- as.Date(sleepday_merged$Date, format = "%m/%d/%Y %I:%M:%S %p")

sleepday_merged$Date <- format(sleepday_merged$Date, "%m/%d/%Y")


merge_daily$Date <- as.Date(merge_daily$Date, format = "%m/%d/%Y")

merge_daily$Date <- format(merge_daily$Date, "%m/%d/%Y")

daily_data <- merge(merge_daily, sleepday_merged, by = c("Id","Date")) %>% drop_na() %>% select(-TrackerDistance)

summary(daily_data)

daily_data$Date <- as.Date(daily_data$Date, format = "%m/%d/%Y")
sum(duplicated(daily_data))

duplicated_rows <- daily_data[duplicated(daily_data), ]

daily_data <- unique(daily_data)

## Daily_data has 24 unqiue users, i got this by looking at the amount of unique Id's.




usertype_data <- daily_data %>%
  mutate(
    user_type = factor(case_when(
      SedentaryMinutes > mean(SedentaryMinutes) & LightlyActiveMinutes < mean(LightlyActiveMinutes) & FairlyActiveMinutes < mean(FairlyActiveMinutes) &
        VeryActiveMinutes < mean(VeryActiveMinutes) ~ "Sedentary",
      SedentaryMinutes < mean(SedentaryMinutes) & LightlyActiveMinutes > mean(LightlyActiveMinutes) & FairlyActiveMinutes < mean(FairlyActiveMinutes) &
        VeryActiveMinutes < mean(VeryActiveMinutes) ~ "Lightly Active",
      SedentaryMinutes < mean(SedentaryMinutes) & LightlyActiveMinutes < mean(LightlyActiveMinutes) & FairlyActiveMinutes > mean(FairlyActiveMinutes) &
        VeryActiveMinutes < mean(VeryActiveMinutes) ~ "Fairly Active",
      SedentaryMinutes < mean(SedentaryMinutes) & LightlyActiveMinutes < mean(LightlyActiveMinutes) & FairlyActiveMinutes < mean(FairlyActiveMinutes) &
        VeryActiveMinutes > mean(VeryActiveMinutes) ~ "Very Active"
    ), levels = c("Sedentary", "Lightly Active", "Fairly Active", "Very Active"))
  ) %>%
  drop_na()

#Usertype_data has 19 unique users after filtering and cleaning the data.

usertype_data %>%
  group_by(user_type) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(user_type) %>%
  summarise(total_percent = total / totals) %>%
  ggplot(aes(user_type,y=total_percent, fill=user_type)) +
  geom_col()+
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="none") +
  labs(title="User type distridution", x=NULL) +
  theme(legend.position="none", text = element_text(size = 20),plot.title = element_text(hjust = 0.5))

ggplot(usertype_data, aes(user_type, Calories, fill=user_type)) +
  geom_boxplot() +
  theme(legend.position="none") +
  labs(title="Calories burned by User type", x=NULL) +
  theme(legend.position="none", text = element_text(size = 20),plot.title = element_text(hjust = 0.5))






daily_data %>%
  summarise(
    distance = factor(case_when(
      TotalDistance < 4.5 ~ "< 4.5 mi",
      TotalDistance >= 4.5 & TotalDistance <= 7 ~ "4.5 > & < 7 mi",
      TotalDistance > 7 ~ "> 7 mi",
    ),levels = c("> 7 mi","4.5 > & < 7 mi","< 4.5 mi")),
    steps = factor(case_when(
      TotalSteps < 6000 ~ "< 6k steps",
      TotalSteps >= 6000 & TotalSteps <= 10000 ~ "6k > & < 10k Steps",
      TotalSteps > 10000 ~ "> 10k Steps",
    ),levels = c("> 10k Steps","6k > & < 10k Steps","< 6k steps")),
    Calories) %>%
  ggplot(aes(steps,Calories,fill=steps)) +
  geom_boxplot() +
  facet_wrap(~distance)+
  labs(title="Calories burned by Steps and Distance",x=NULL) +
  theme(legend.position="none", text = element_text(size = 20),plot.title = element_text(hjust = 0.5))



sleepType_by_userType <- daily_data %>%
  group_by(Id) %>%
  reframe(
    user_type = factor(case_when(
      SedentaryMinutes > mean(SedentaryMinutes) & LightlyActiveMinutes < mean(LightlyActiveMinutes) & FairlyActiveMinutes < mean(FairlyActiveMinutes) & VeryActiveMinutes < mean(VeryActiveMinutes) ~ "Sedentary",
      SedentaryMinutes < mean(SedentaryMinutes) & LightlyActiveMinutes > mean(LightlyActiveMinutes) & FairlyActiveMinutes < mean(FairlyActiveMinutes) & VeryActiveMinutes < mean(VeryActiveMinutes) ~ "Lightly Active",
      SedentaryMinutes < mean(SedentaryMinutes) & LightlyActiveMinutes < mean(LightlyActiveMinutes) & Fairly ActiveMinutes > mean(FairlyActiveMinutes) & VeryActiveMinutes < mean(VeryActiveMinutes) ~ "Fairly Active",
      SedentaryMinutes < mean(SedentaryMinutes) & LightlyActiveMinutes < mean(LightlyActiveMinutes) & FairlyActiveMinutes < mean(FairlyActiveMinutes) & VeryActiveMinutes > mean(VeryActiveMinutes) ~ "Very Active"),
      levels = c("Sedentary", "Lightly Active", "Fairly Active", "Very Active")
    ),
    sleep_type = factor(case_when(
      mean(TotalMinutesAsleep) < 360 ~ "Bad Sleep",
      mean(TotalMinutesAsleep) > 360 & mean(TotalMinutesAsleep) <= 480 ~ "Normal Sleep",
      mean(TotalMinutesAsleep) > 480 ~ "Over Sleep"),
      levels = c("Bad Sleep", "Normal Sleep", "Over Sleep")
    ),
    total_sleep = sum(TotalMinutesAsleep)
  ) %>%
  drop_na() %>%
  group_by(user_type) %>%
  reframe(
    bad_sleepers = sum(sleep_type == "Bad Sleep"),
    normal_sleepers = sum(sleep_type == "Normal Sleep"),
    over_sleepers = sum(sleep_type == "Over Sleep"),
    total = n()
  ) %>%
  group_by(user_type) %>%
  reframe(
    bad_sleepers = bad_sleepers / total, 
    normal_sleepers = normal_sleepers / total, 
    over_sleepers = over_sleepers / total
  )



sleepType_by_userType_melted<- melt(sleepType_by_userType, id.vars = "user_type")



ggplot(sleepType_by_userType_melted, aes(user_type, value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x=NULL, fill="Sleep type") + 
  theme(legend.position="bottom",text = element_text(size = 20),plot.title = element_text(hjust = 0.5))