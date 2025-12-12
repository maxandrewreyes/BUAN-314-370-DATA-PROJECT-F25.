library(tidyverse)
library(dplyr)
library(ggplot2)
library(sqldf)
install.packages("ggridges")
library(ggridges)
library(viridisLite)

mh <- read.csv("https://raw.githubusercontent.com/audreyallen2324/BUAN-314-370-DATA-PROJECT-F25./refs/heads/main/Mental_Health_and_Social_Media_Balance_Dataset.csv", header = TRUE)
dim(mh)
str(mh)
table(mh$Gender)
view(mh)



# User_ID, Age, Gender, Daily_Screen_time.hrs., Sleep_Quality.1.10, Stress_Level.1.10, Days_Without_Social_Media,
#Exercise_Frequency.week., Social_Media_Platform, Happiness_Index.1.10

cor(mh$Daily_Screen_Time.hrs., mh$Sleep_Quality.1.10.)
#-0.7589098 Strong negative inverse

# RENAME HEADERS
mh <- mh %>%
  rename(
    Daily_Screen_Time = Daily_Screen_Time.hrs.,
    Sleep_Quality = Sleep_Quality.1.10.,
    Stress_Level = Stress_Level.1.10.,
    Days_With_Exercise = Exercise_Frequency.week.,
    Happiness_Index = Happiness_Index.1.10.
  )

# FIND ANY NULL OR MISSING VALUES AND CONVERT DAILY SCREEN TIME TO MINUTES
sum(is.na(mh))

mh <- mh %>%
  mutate(Daily_Screen_Time_Minutes = Daily_Screen_Time * 60)

glimpse(mh)

library(ggplot2)

QUERY8<-"SELECT Social_Media_Platform,
          MIN(Stress_Level) AS Min_Stress,
          AVG(Stress_Level) AS Avg_Stress,
          MAX(Stress_Level) AS Max_Stress
          FROM mh
          GROUP BY Social_Media_Platform
          ORDER BY Social_Media_Platform;"
sqldf(QUERY8)


ggplot(mh, aes(x = Social_Media_Platform, y = Stress_Level)) +
  geom_boxplot(fill = "skyblue") +
  labs(
    title = "Stress Level by Social Media Platform",
    x = "Social Media Platform",
    y = "Stress Level (1–10)"
  )

QUERY9 <- "SELECT 
           CASE
            WHEN Daily_Screen_Time BETWEEN 0 AND 3.67 THEN 'Low'
            WHEN Daily_Screen_Time BETWEEN 3.68 AND 7.34 THEN 'Medium'
            WHEN Daily_Screen_Time BETWEEN 7.35 AND 11 THEN 'High'
           END AS Screen_Time_Level,
           Gender,
           COUNT(*) AS Num_Observations,
           ROUND(COUNT(*) * 100.00 / (SELECT COUNT(*) 
                                      FROM mh),2) AS Percent_of_Total
           FROM mh
           GROUP BY Screen_Time_Level, Gender
           ORDER BY 
           CASE Screen_Time_Level
            WHEN 'Low' THEN 1
            WHEN 'Medium' THEN 2
            WHEN 'High' THEN 3
           END;"

sqldf(QUERY9)


ggplot(mh, aes(Daily_Screen_Time_Minutes, fill = Gender)) +
  geom_histogram(binwidth = 30, position = "dodge") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(mh$Daily_Screen_Time_Minutes), 60)) +
  labs(
    title = "Average Screen Time",
    x = "Daily Screen Time (Minutes)",
    y = "Quantity of Observations"
  )

## TEST CODE NOT IN THE PRESENTATION OR DOC ###################################
Plot <- "SELECT Social_Media_Platform, 
         AVG(Happiness_Index) AS Avg_Happiness
         FROM mh
         GROUP BY Social_Media_Platform
          "
mh_1 <- sqldf(Plot)

ggplot(mh, aes(x = Social_Media_Platform, y= Avg_Happiness)) +    
  geom_bar(stat="identity", fill=alpha("blue", 0.3)) +
  coord_cartesian(ylim = c(6.5, 9)) +
  theme_bw() +
  theme(
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar(start = 0) 

##################################

glimpse(mh)



ggplot(mh, aes(x = Days_With_Exercise , y = Social_Media_Platform, fill = Social_Media_Platform)) +
  geom_density_ridges(scale = 3, alpha = 0.9, color = "black") +
  theme_ridges() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(0, max(mh$Days_With_Exercise), 1)) +
  labs(
    title = "Days of Exercise per Week per Social Media Platform ",
    x = "Rating (0 → 10)",
    y = "Social Media Platform"
  )
###############################################################################

QUERY6<-"SELECT 
        ROUND(Daily_Screen_Time_Minutes / 60) AS hour,
        ROUND(AVG(Sleep_Quality), 2) AS avg_sleep,
        ROUND(AVG(Happiness_Index), 2) AS avg_happiness
        FROM mh
        GROUP BY hour
        ORDER BY hour;"
sqldf(QUERY6)
mh_2 <- sqldf(QUERY6)
  
ggplot(mh_2, aes(x = hour)) +
  geom_line(aes(y = avg_sleep, color = "Average Sleep"),linewidth = 2) +
  geom_line(aes(y = avg_happiness, color = "Average Happiness"), linewidth = 2) +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, max(mh_2$hour), 1)) +
  labs(
    x = "Screen Time (Hours)",
    y = "Scale (1-10)",
    title = "Sleep & Happiness as Screen Time Increases")


