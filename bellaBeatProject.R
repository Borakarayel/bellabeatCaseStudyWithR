install.packages("tidyverse")
install.packages("dplyr")
install.packages("janitor")
install.packages("lubridate")
install.packages("ggpubr")
install.packages("here")
install.packages("skimr")
install.packages("ggrepel")
install.packages("ggplot2")

library(tidyverse)
library(dplyr)
library(janitor)
library(lubridate)
library(ggpubr)
library(here)
library(skimr)
library(ggrepel)
library(ggplot2)

daily_activity <- read.csv(file="~/Data Analysis/Projects/bellabeatCaseStudy/bellaBeatProject_/dataFiles/dailyActivity_merged.csv")
heartrate <- read.csv(file="~/Data Analysis/Projects/bellabeatCaseStudy/bellaBeatProject_/dataFiles/heartrate_seconds_merged.csv")
hourly_cal <- read.csv(file="~/Data Analysis/Projects/bellabeatCaseStudy/bellaBeatProject_/dataFiles/hourlyCalories_merged.csv")
hourly_intens <- read.csv(file="~/Data Analysis/Projects/bellabeatCaseStudy/bellaBeatProject_/dataFiles/hourlyIntensities_merged.csv")
hourly_steps <- read.csv(file="~/Data Analysis/Projects/bellabeatCaseStudy/bellaBeatProject_/dataFiles/hourlySteps_merged.csv") 
min_cal<- read.csv(file="~/Data Analysis/Projects/bellabeatCaseStudy/bellaBeatProject_/dataFiles/minuteCaloriesNarrow_merged.csv")
min_intens <- read.csv(file="~/Data Analysis/Projects/bellabeatCaseStudy/bellaBeatProject_/dataFiles/minuteIntensitiesNarrow_merged.csv")
min_met <- read.csv(file="~/Data Analysis/Projects/bellabeatCaseStudy/bellaBeatProject_/dataFiles/minuteMETsNarrow_merged.csv")
min_sleep <- read.csv(file="~/Data Analysis/Projects/bellabeatCaseStudy/bellaBeatProject_/dataFiles/minuteSleep_merged.csv")
min_steps_nar <- read.csv(file="~/Data Analysis/Projects/bellabeatCaseStudy/bellaBeatProject_/dataFiles/minuteStepsNarrow_merged.csv")
weight_log <- read.csv(file="~/Data Analysis/Projects/bellabeatCaseStudy/bellaBeatProject_/dataFiles/weightLogInfo_merged.csv")

### <span style="color:#FA8072"> 3.3 Previewing the Data Frames  </span> <a class="anchor" id="previewing_data"></a>
The data frames previously created will be previewed, and a search will be conducted to identify key details and common points across each data frame.


# head() Preview the first few rows of the data frame
# str() Check the structure of the data frame
head(daily_activity)
str(daily_activity)

head(heartrate)
str(heartrate)

head(hourly_cal)
str(hourly_cal)

head(hourly_intens)
str(hourly_intens)

head(hourly_steps)
str(hourly_steps)

head(min_cal)
str(min_cal)

head(min_intens)
str(min_intens)

head(min_met)
str(min_met)

head(min_sleep)
str(min_sleep)

head(min_steps_nar)
str(min_steps_nar)

head(weight_log)
str(weight_log)

As a result of the examination, it was found that each data frame contains a unique column named "ID." The depth of the dataset can be assessed by analyzing the values within these ID columns.


# Using n_distinct from dplyr to count unique IDs in each CSV file.
n_distinct(daily_activity$Id)
n_distinct(heartrate$Id)
n_distinct(hourly_cal$Id)
n_distinct(hourly_steps$Id)
n_distinct(min_cal$Id)
n_distinct(min_sleep$Id)
n_distinct(min_steps_nar$Id)
n_distinct(weight_log$Id)

As observed, the number of unique IDs varies across the data frames. Therefore, data frames with missing IDs (such as heart rate and weight log) will be excluded from the analysis. Only the data frames with complete ID information and relevant to the research scope, will be utilized in the analysis. These dare frames are:
* daily_activity
* hourly_steps
* hourly_cal
* min_sleep

### <span style="color:#FA8072"> 3.4 Cleaning and Formating </span> <a class="anchor" id="cleaning_and_formating"></a>
Before proceeding, it is essential to ensure that the data frames we will use are properly cleaned and formatted according to the required specifications. This step is crucial for accurate analysis and reliable results.

#### <span style="color:#FA8072"> 3.4.1 Data Cleaning </span> <a class="anchor" id="cleaning"></a>
Data cleaning is conducted to handle any duplicate entries or NA values present in the dataset.



# Removing duplicate rows and rows with NA values from the daily_activity data frame
daily_activity <- daily_activity %>%
  distinct() %>%  # Remove duplicate rows based on all columns
  drop_na()       # Remove rows with any NA values

min_sleep <- min_sleep %>%
  distinct() %>%
  drop_na()

hourly_steps <- hourly_steps %>%
  distinct() %>%
  drop_na()

hourly_cal <- hourly_cal %>%
  distinct() %>%
  drop_na()

Finally, a thorough check is performed to ensure no remaining duplicates or missing values in the dataset.


# Checking for duplicates in each dataset.
sum(duplicated(daily_activity))
sum(duplicated(hourly_steps))
sum(duplicated(hourly_cal))
sum(duplicated(min_sleep))

# Checking for missing values in each dataset.
sum(is.na(daily_activity))
sum(is.na(min_sleep))
sum(is.na(hourly_steps))
sum(is.na(hourly_cal))


#### <span style="color:#FA8072"> 3.4.2 Data Formating </span> <a class="anchor" id="formating"></a>

We are standardizing column names by converting them to lowercase to ensure consistency across datasets before merging. The clean_names() function from the janitor package is an excellent tool for this purpose, as it automatically converts column names to a consistent, clean format (e.g., lowercase, removes special characters, and replaces spaces with underscores).


# Clean column names to ensure consistent formatting
daily_activity <- clean_names(daily_activity)
min_sleep <- clean_names(min_sleep)
hourly_steps <- clean_names(hourly_steps)
hourly_cal <- clean_names(hourly_cal)


# Check the column names of the datasets
colnames(daily_activity) 
colnames(hourly_cal)
colnames(hourly_steps)
colnames(min_sleep)

#### <span style="color:#FA8072"> 3.4.3 Standardizing the Format of Date and Time Columns </span> <a class="anchor" id="standardizing"></a>

We are standardizing date columns across datasets to ensure consistency for analysis and merging. Columns are renamed for uniformity and converted to either date-only or datetime formats based on the data's granularity. This process ensures consistent naming and formatting, facilitating seamless comparison, merging, and analysis of datasets.
                                                  
                                                  
                                                  # Standardizing date columns across datasets for consistency before analysis and merging.
                                                  # In the daily_activity dataset, the "activity_date" column is renamed to "date", and the values are converted to Date format.
                                                  daily_activity_date <- daily_activity %>%
                                                    rename(date = activity_date) %>%
                                                    mutate(date = as_date(date, format = "%m/%d/%Y"))
                                                  
                                                  # In the min_sleep dataset, the "date" column is converted to a Datetime format, including time information, with the system's timezone.
                                                  min_sleep_date <- min_sleep %>%
                                                    mutate(date = as_datetime(date, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone()))
                                                  
                                                  # In the hourly_cal and hourly_steps datasets, the "activity_hour" column is renamed to "date", 
                                                  # and the values are converted to Datetime format, including time information, with the system's timezone.
                                                  hourly_cal_date <- hourly_cal %>%
                                                    rename(date = activity_hour) %>%
                                                    mutate(date = as_datetime(date, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone()))
                                                  
                                                  hourly_steps_date <- hourly_steps %>%
                                                    rename(date = activity_hour) %>%
                                                    mutate(date = as_datetime(date, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone()))
                                                  
                                                  ### <span style="color:#FA8072"> 3.5 Merging Datasets </span> <a class="anchor" id="merging"></a>
                                                  
                                                  To explore potential correlations between variables, we will merge the daily_activity and daily min_sleep datasets. The merge will use id and date as primary keys to ensure accurate alignment of records. This approach allows us to analyze relationships between activity levels and sleep patterns effectively.
                                                  
                                                  # Merging the daily_activity and min_sleep datasets using "id" and "date" as primary keys.
                                                  daily_activity_sleep <- merge(daily_activity_date, min_sleep_date, by=c("id", "date"))
                                                  
                                                  # The glimpse() function provides a quick summary of the merged dataset
                                                  glimpse(daily_activity_sleep)
                                                  
                                                  ## <span style="color:#FA8072"> 4. Analyze and Share Phase </span> <a class="anchor" id="analyze_phase_4"></a>
                                                  
                                                  We will analyze Fitbit user trends to assess how these insights can inform BellaBeat's marketing strategy.

### <span style="color:#FA8072"> 4.1 User Types per activity level  </span> <a class="anchor" id="user_types"></a>
Due to the absence of activity density distinctions in our dataset, we will categorize users based on their step count. This classification will be guided by the guidelines outlined in the article available at <a href="https://www.medicinenet.com/how_many_steps_a_day_is_considered_active/article.htm">medicinenet.com</a>

<em>"Sedentary is less than 5,000 steps per day<br> 
Somewhat active is 7,500 to 9,999 steps per day<br>
Active is more than 10,000 steps per day<br>
Highly active is more than 12,500"<br> </em>

First we will calculate the daily steps average by user.


#Calculates mean steps, calories, and sleep per user and sorts by steps.
daily_average <- daily_activity_sleep %>%
  group_by(id)%>%
  summarise(mean_daily_steps = mean(total_steps),
            mean_daily_calories = mean(calories),
            mean_daily_sleep = mean(value)) %>%
  arrange(desc(mean_daily_steps))

#check the created daily_average data frame with head function 
head(daily_average)

We will now focus on analyzing the daily_average data frame to classify users based on their daily average steps. This classification will help identify patterns in user activity levels and provide valuable insights for further analysis.


#Classifies users based on average daily steps.
user_type_step <- daily_average %>%
  mutate(usage = case_when(
    mean_daily_steps < 5000 ~ "sedentary",
      mean_daily_steps >= 5000 & mean_daily_steps < 7499 ~ "somewhat active",
      mean_daily_steps >= 7500 & mean_daily_steps < 9999 ~ "fairly active",
    mean_daily_steps >= 10000 ~ "very active",
    TRUE ~ "unknown" #catch all conditions
  ))

The data was grouped by user type, with counts and percentages calculated for each group. The percentages were formatted for clarity, and the usage column was converted to a factor with specified levels.


# Groups data, calculates counts, percentages, and formats labels.
user_type_percent <- user_type_step %>%
  group_by(usage) %>%
  summarise(total = n()) %>%
  mutate(total_num_user = sum(total)) %>%
  group_by(usage ) %>% 
  summarise(total_percent = total / total_num_user) %>%
  mutate(labels = scales::percent(total_percent))


# Converts 'usage' column to a factor with specified levels.
user_type_percent$usage <- factor(user_type_percent$usage ,
                                      levels = c("very active", 
                                                 "fairly active",
                                                 "somewhat active", 
                                                 "sedentary"))

The analysis reveals that users are fairly distributed across different activity levels based on their daily step count. Based on this information, we can create a graph to visually depict the distribution for better clarity.

#Creates a pie chart visualizing the distribution of user types
user_type_percent %>%
  ggplot(aes(x="", y=total_percent, fill=usage))+
  geom_bar(stat='identity', width = 1, color="white")+
  coord_polar("y", start=0)+
  theme_minimal()+
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold"))+
  scale_fill_manual(values = c("#FA8072","#F29F7F", "#F1B278", "#F1C58A"))+
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  labs(title="User Type Distribution")

### <span style="color:#FA8072"> 4.2 Correlations  </span> <a class="anchor" id="correlations"></a>

To explore potential correlations between the following variables:

<strong><em>mean_daily_steps vs. mean_daily_calories<br>
mean_daily_steps vs. mean_daily_sleep</strong> </em>

We will begin by visualizing the relationship between mean_daily_steps and mean_daily_steps_calories through a plot.


#Creates a scatter plot to analyze the relationship between daily steps and calories.
ggplot(user_type_step, aes(mean_daily_steps, mean_daily_calories))+
  geom_jitter(alpha=.5)+
  geom_rug(position="jitter", size=0.08)+
  geom_smooth(method = "lm",  formula = y ~ x, size =.6, color= "#FA8072")+
  stat_cor(method = "pearson", label.x = 10000, label.y = 1300, size=6)+
  labs(title= "Mean Daily Steps vs. Mean Daily Calories", x= "Steps", y="Calories")+
  theme_minimal()

The Pearson correlation analysis and visualization reveal an R-value of 0.26 for the relationship between Daily Steps and Calories, indicating a weak positive correlation. <a href="https://medium.com/@anthony.demeusy/pearson-correlation-methodology-limitations-alternatives-part-1-methodology-42abe8f1ba90">The scale of correlation can be seen here.</a>

Next, we will proceed with the visualization of the relationship between Mean Daily Steps and Mean Daily Sleep.


#Creates a scatter plot to analyze the relationship between daily steps and daily sleep
ggplot(user_type_step, aes(mean_daily_steps, mean_daily_sleep*1000))+
  geom_jitter(alpha=.5)+
  geom_rug(position="jitter", size=.08)+
  geom_smooth(method = "lm",  formula = y ~ x, linewidth =.6, color= "#FA8072")+
  stat_cor(method = "pearson", label.x = 9000, label.y = 950)+
  labs(title= "Mean Daily Steps vs. Mean Daily Sleep Score", x= "Steps", y="Sleep Score")+
  theme_minimal()

The Pearson correlation analysis shows a negative R-value, indicating an very weak inverse correlation between Mean Daily Steps and Mean Daily Sleep Score in this dataset.

### <span style="color:#FA8072"> 4.3 Activity Level During the Week </span> <a class="anchor" id="activity_level"></a>

To better understand and analyze user mobility, we will examine the min_steps dataframe by grouping data based on hours and days. This analysis will help identify the times and days when users are most active. To do this, we will start by splitting the date column in the min_steps data frame into two (as date and time). We then format these cells in a way that we can use later.


# Splits 'date' into 'date' and 'time', converts 'date' to Date format and 'time' to hms format, defaulting to "00:00:00" if NA.
hourly_steps_updtd <- hourly_steps_date %>%
    separate(date, into = c("date", "time"), sep = " ", ) %>%
    mutate(date = ymd(date)) %>%
    mutate(time = if_else(is.na(time), hms::as_hms("00:00:00"), hms::as_hms(time)))

# Creates a dataframe with average steps grouped by 'weekday' and 'time'
step_weekday <- hourly_steps_updtd %>%
  mutate(weekday = weekdays(date)) %>%
  group_by(weekday, time) %>%
  summarise(mean_steps = mean(step_total), .groups = "drop")



step_weekday$weekday <- ordered(step_weekday$weekday, 
                                  levels=c("Monday", "Tuesday", "Wednesday", 
                                           "Thursday", "Friday", "Saturday",
                                           "Sunday"))

Now we create a detailed line chart using the step_week data frame we created.

# Line chart of mean steps by weekday over time, with points, minimal theme,
# and rotated time-based x-axis labels.
ggplot(step_weekday, aes(x = time, y = mean_steps, group = weekday, color = weekday)) +
  geom_line() + 
  geom_point() +
  labs(title = "Mean Steps by Weekday and Time", x = "Time", y = "Mean Steps") +
  theme_minimal() + 
  scale_x_time(labels = scales::time_format("%H:%M"), breaks = scales::breaks_width("1 hour")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("Saturday" = "#FA8072", "Sunday" = "#FA8072", 
                                "Monday" = "#F1C58A","Tuesday" = "#F1C58A",
                                "Wednesday" = "#F1C58A", "Thursday" = "#F1C58A",
                                "Friday" = "#F1C58A"))

The graph clearly illustrates differences between weekends and weekdays. On weekends, the most intense activity occurs between 14:00 and 16:00, while on weekdays, it peaks between 17:00 and 19:00. Notably, activity begins earlier on weekdays. Additionally, a significant decrease in activity is observed on weekdays, particularly between 14:00 and 16:00.

### <span style="color:#FA8072"> 4.4 Use of smart device </span> <a class="anchor" id="usage"></a>
To assess the intensity of smart device usage, we calculated the usage frequency based on the number of days each user engaged with the device. This categorization allows us to classify users into three distinct usage types: low, moderate, and high usage.


# Groups data by 'id', counts days used, and categorizes usage
daily_use <- daily_activity %>%
  group_by(id) %>%
  summarize(days_used=sum(n())) %>%
  mutate(usage = case_when(
    days_used >= 1 & days_used <= 10 ~ "low use",
    days_used >= 11 & days_used <= 20 ~ "moderate use", 
    days_used >= 21 & days_used <= 100 ~ "high use" ))

# Groups data by 'usage' and calculates percentage
daily_use_percent <- daily_use %>%
  group_by(usage) %>%
  summarise(user_count= n()) %>%
  mutate(total_percent= scales::percent(user_count/sum(user_count)))%>% 
  mutate(total_percent = user_count / sum(user_count) * 100)

We visualized the distribution of these usage categories through a donut chart, which provides a clear representation of the proportions of users in each category.


# Creates a donut chart of user proportions by usage category
ggplot(daily_use_percent, 
       aes(x=2, y=user_count, fill=usage))+
  geom_bar(stat="identity", width=1, color="darkgray")+
  coord_polar("y", start=0)+
  xlim(1.0, 2.5)+
  labs(title = "Proportion of Users by Usage",
       fill = "Usage Category")+
  theme_void()+
  theme(plot.title = element_text(hjust=0.5))+
  geom_text(aes(label = paste0(round(total_percent, 1), "%")), 
            position = position_stack(vjust =0.5),
            color = "white",
            size = 4.5)+
  scale_fill_manual(values = c("#FA8072",
                               "#F29F7F",
                               "#F1C58A"))

## <span style="color:#FA8072"> 5. Act Phase </span> <a class="anchor" id="act_phase_5"></a>

As a result of the data examination and analysis, the following recommendations have been developed:

User Activity Categorization
The app can provide personalized insights by categorizing users into activity levels such as "Very Active," "Fairly Active," "Somewhat Active," or "Sedentary," based on their step counts. This feature will enable users to see where they stand and encourage them to move to a higher activity group for improved health and fitness.

Targeting Low-Activity Users
To motivate users with lower activity levels, the app can create specialized in-app sections tailored to their needs. These sections could include personalized tips, challenges, and motivational content designed to foster a more active lifestyle.

Diversifying Data Collection Options
To appeal to a wider audience, the app could expand its data collection capabilities to include activities such as yoga, pilates, rowing, cycling, and general fitness exercises. This would ensure inclusivity for users with different fitness interests.

Tailored Interfaces for Different Days
Since user behavior differs between weekdays and weekends, the app could introduce dynamic interfaces or features that adapt to these variations, offering a more user-centric experience.

Addressing Inactivity During Weekdays
Data reveals significant drops in user mobility at specific times during weekdays. By sending gentle reminders during these periods, the app can prompt users to take short activity breaks, reducing prolonged sedentary intervals.

Promoting Regular Product Usage
Many users are categorized as low or average in terms of usage. Educating them on the benefits of consistent app engagement—such as better health monitoring, tailored recommendations, and improved product development—could drive higher adoption rates. Additionally, designing diverse product variations to suit different user preferences could further enhance engagement.



