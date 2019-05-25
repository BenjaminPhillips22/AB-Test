library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

set.seed(101)

# dataframe with binary conversion information

# There are around 1000-2000 unique visits a day
start_date <- dmy_hms('01-06-2019 00:00:01')
times <- start_date + seconds(cumsum(floor(runif(n = 10000, min = 1, max = 300))))

df <- data_frame(
    'time' = times,
    'dates' = dmy(format(`times`, "%d-%m-%Y")),
    'id' = 1:length(dates),
    'conversion' = rbinom(n = length(times), size = 1, prob = 0.6)
    )
View(df)

# how many entries per day?
df %>% group_by(`dates`) %>% summarise(`visits_per_day` = n()) %>% View()

# plot daily conversion
daily_conversion_df <- df %>%
    group_by(`dates`) %>% 
    summarise(conversion_rate = mean(`conversion`))
View(daily_conversion_df)

g1 <- daily_conversion_df %>% 
    ggplot(mapping = aes(x = `dates`, y = `conversion_rate`)) + 
    geom_point() + 
    geom_line() +
    labs(title = 'Daily Conversion Rate', y = 'Conversion Rate', x = 'Date') +
    ylim(0, 1)
g1

# plot Weekly conversion
weekly_conversion_df <- df %>%
    mutate(`week` = strftime(`dates`, format = "%V")) %>% 
    group_by(`week`) %>% 
    mutate(`max_date` = max(`dates`)) %>% 
    ungroup() %>% 
    group_by(`max_date`) %>% 
    summarise(conversion_rate = mean(`conversion`))
View(daily_conversion_df)

g2 <- weekly_conversion_df %>% 
    ggplot(mapping = aes(x = `max_date`, y = `conversion_rate`)) + 
    geom_point() + 
    geom_line() +
    labs(title = 'Weekly Conversion Rate', y = 'Conversion Rate', x = 'Date') +
    ylim(0, 1)
g2

