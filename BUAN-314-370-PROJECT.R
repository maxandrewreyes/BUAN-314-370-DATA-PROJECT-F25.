##CLEANING DATA 

library(tidyverse)
library(sqldf)
library(tidyr)
library(knitr)
library(stringr)

mh <- read.csv('https://raw.githubusercontent.com/audreyallen2324/BUAN-314-370-DATA-PROJECT-F25./refs/heads/main/Mental_Health_and_Social_Media_Balance_Dataset.csv')
view(mh)

#RENAME HEADERS
mh <- mh %>%
  rename(Daily_Screen_Time = Daily_Screen_Time.hrs.,
         Sleep_Quality = Sleep_Quality.1.10.,
         Stress_Level = Stress_Level.1.10.,
         Days_With_Exercise = Exercise_Frequency.week.,
         Happiness_Index = Happiness_Index.1.10.)

#FIND ANY NULL OR MISSING VALUES AND CONVERT DAILY SCREEN TIME TO MINUTES
sum(is.na(mh))

mh <- mh %>%
  mutate(Daily_Screen_Time_Minutes = Daily_Screen_Time * 60)
glimpse(mh)

#QUERY 1 Analyzing Well-being Across Different Levels of Screen Time 

QUERY1 <- "SELECT
  CASE
    WHEN Daily_Screen_Time_Minutes < 120 THEN 'Low'
    WHEN Daily_Screen_Time_Minutes BETWEEN 120 AND 300 THEN 'Medium'
    ELSE 'High'
  END AS usage_group,
  ROUND(AVG(Happiness_Index), 2) AS mean_mental,
  ROUND(AVG(Days_With_Exercise), 2) AS mean_physical,
  COUNT(*) AS n
FROM mh
GROUP BY usage_group"
sqldf(QUERY1)


#ANALYSIS OF STRESS VS. SCREEN TIME VS. SLEEP QUALITY 
mh <- mh %>%
  mutate(ScreenTimeCat = cut(Daily_Screen_Time_Minutes,
                             breaks = c(-Inf, 120, 300, Inf),
                             labels = c("Low", "Medium", "High")))

# 1. Bar plot: Mean Stress Level by Screen Time Category

mh %>%
  group_by(ScreenTimeCat) %>%
  summarize(mean_stress = mean(Stress_Level, na.rm = TRUE)) %>%
  ggplot(aes(x = ScreenTimeCat, y = mean_stress, fill = ScreenTimeCat)) +
  geom_col(alpha = 0.8) +
  labs(
    title = "Mean Stress Level by Daily Screen Time",
    x = "Screen Time Category",
    y = "Mean Stress Level"
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(limits = c(0, 10), breaks = 0:10) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

QUERY2 <- "SELECT
  CASE
    WHEN Daily_Screen_Time_Minutes <= 120 THEN 'Low'
    WHEN Daily_Screen_Time_Minutes <= 300 THEN 'Medium'
    ELSE 'High'
  END AS ScreenTimeCat,
  ROUND(AVG(Stress_Level), 2) AS mean_stress
FROM mh
WHERE Daily_Screen_Time_Minutes IS NOT NULL
GROUP BY ScreenTimeCat
ORDER BY 
  CASE 
    WHEN ScreenTimeCat = 'Low' THEN 1
    WHEN ScreenTimeCat = 'Medium' THEN 2
    ELSE 3
  END;"
sqldf(QUERY2)

# 2. Scatter plot: Screen Time vs. Sleep Quality (with trend line) and Query 3
ggplot(mh, aes(x = Daily_Screen_Time_Minutes, y = Sleep_Quality)) +
  geom_jitter(width = 0.5, alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Sleep Quality vs. Daily Screen Time (minutes)",
    x = "Daily Screen Time (minutes)",
    y = "Sleep Quality (1-10)"
  ) +
  theme_minimal(base_size = 14)

QUERY3 <- "SELECT
  FLOOR(daily_screen_time_minutes / 30) * 30 AS screen_time_bin,
  ROUND(AVG(sleep_quality)) AS avg_sleep_quality,
  COUNT(*) AS people
FROM mh
WHERE daily_screen_time_minutes IS NOT NULL
  AND sleep_quality BETWEEN 1 AND 10
GROUP BY screen_time_bin
ORDER BY screen_time_bin;"
sqldf(QUERY3)


# 3. VIOLIN PLOT TO SHOW HAPPINESS COMPARED TO EXERCISE AND QUERY 4
mh <- mh %>%
  mutate(Exercise_Group = cut(Days_With_Exercise,
                              breaks = c(-Inf, 2, 5, Inf),
                              labels = c("Low", "Medium", "High")))

ggplot(mh, aes(x = Exercise_Group, y = Happiness_Index, fill = Exercise_Group)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.15, outlier.shape = NA, color = "black", alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, color = "red", fill = "yellow") +
  labs(
    title = "Happiness Scores Across Exercise Frequency Groups",
    x = "Exercise Frequency (Days per Week)",
    y = "Happiness Index (1-10)") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") 

QUERY4 <- "SELECT
  Exercise_Group,
  COUNT(*) AS num_people,
  ROUND(AVG(Happiness_Index),2) AS avg_happiness,
  MIN(Happiness_Index) AS min_happiness,
  MAX(Happiness_Index) AS max_happiness,
  ROUND(STDEV(Happiness_Index), 2) AS stddev_happiness
FROM (
  SELECT
    CASE
      WHEN Days_With_Exercise <= 2 THEN 'Low'
      WHEN Days_With_Exercise <= 5 THEN 'Medium'
      ELSE 'High'
    END AS Exercise_Group,
    Happiness_Index
  FROM mh
  WHERE Happiness_Index BETWEEN 1 AND 10
    AND Days_With_Exercise IS NOT NULL
) AS grouped
GROUP BY Exercise_Group
ORDER BY
  CASE Exercise_Group
    WHEN 'Low' THEN 1
    WHEN 'Medium' THEN 2
    WHEN 'High' THEN 3
    ELSE 4
  END;"
sqldf(QUERY4)


###LINEAR REGRESSION MODEL
pairs(mh[,4:8])
cor(mh[,4:8])

plot(mh$Daily_Screen_Time, mh$Sleep_Quality)
plot(mh$Daily_Screen_Time~mh$Sleep_Quality)

LM<-lm(Daily_Screen_Time~Sleep_Quality, mh)
summary(LM) 
confint(LM)

plot(LM$fitted.values~mh$Sleep_Quality, add=TRUE)
abline(LM$coefficients[1], LM$coefficients[2], col='blue', lwd=2)



#RESIDUALS
plot(LM$residuals)
abline(0,0,col='black')
hist(LM$residuals)
summary(LM$residuals)





