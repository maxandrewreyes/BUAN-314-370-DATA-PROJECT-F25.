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
#Query 7 --> Analysis of how the average happiness index fluctuates as screen time hours increases

QUERY7 <- sqldf("
SELECT
  ROUND(Daily_Screen_Time) AS screen_time_hour,
  AVG(Happiness_Index) AS avg_happiness,
  COUNT(*) AS n
FROM mh
WHERE Daily_Screen_Time IS NOT NULL
  AND Happiness_Index IS NOT NULL
GROUP BY ROUND(Daily_Screen_Time)
ORDER BY screen_time_hour
")
str(QUERY7)

#Visulaization for Query 7
library(ggplot2)

ggplot(QUERY5, aes(x = screen_time_hour, y = avg_happiness)) +
  geom_segment(
    aes(xend = screen_time_hour, y = 0, yend = avg_happiness),
    color = "gray70",
    linewidth = 1
  ) +
  geom_point(size = 4, color = "steelblue") +
  scale_y_continuous(limits = c(0, 10), breaks = 0:10) +
  labs(
    title = "Average Happiness by Daily Screen Time (Hours)",
    x = "Daily Screen Time (Hours)",
    y = "Average Happiness Index"
  ) +
  theme_minimal(base_size = 14)

#Query 8 --> Stress x Sleep x Screen Time 
sqldf("SELECT
  CASE
    WHEN Daily_Screen_Time < 3 THEN 'Low Screen'
    WHEN Daily_Screen_Time < 6 THEN 'Medium Screen'
    ELSE 'High Screen'
  END AS ScreenGroup,
  ROUND(AVG(CASE WHEN Days_With_Exercise >= 4 THEN Stress_Level END), 2) AS stress_high_exercise,
  ROUND(AVG(CASE WHEN Days_With_Exercise < 4 THEN Stress_Level END), 2) AS stress_low_exercise
FROM mh
WHERE Stress_Level BETWEEN 1 AND 10
GROUP BY ScreenGroup
ORDER BY
  CASE ScreenGroup
    WHEN 'Low Screen' THEN 1
    WHEN 'Medium Screen' THEN 2
    ELSE 3
  END;
")
#Visualization for Query 8
library(ggplot2)

ggplot(QUERY8, 
       aes(x = Sleep_Quality, y = Screen_Group, fill = avg_stress)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient(
    low = "#E3F2FD",
    high = "#B71C1C",
    name = "Avg Stress"
  ) +
  labs(
    title = "Stress Levels Across Sleep Quality and Screen Time",
    x = "Sleep Quality (1–10)",
    y = "Screen Time Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

#QUERY 9 --> Average Stress by Exercise Frequency
QUERY9 <- sqldf("
SELECT
  Days_With_Exercise AS exercise_days,
  ROUND(AVG(Stress_Level), 2) AS avg_stress,
  COUNT(*) AS n
FROM mh
WHERE Stress_Level BETWEEN 1 AND 10
GROUP BY Days_With_Exercise
ORDER BY Days_With_Exercise;
")
QUERY9

#Visualization for Query 9
ggplot(QUERY10, aes(x = exercise_days, y = avg_stress)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(size = 3, color = "darkred") +
  labs(
    title = "Average Stress Level by Days of Exercise",
    x = "Days Exercised per Week",
    y = "Average Stress Level"
  ) +
  theme_minimal(base_size = 14)
