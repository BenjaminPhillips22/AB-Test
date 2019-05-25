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

# AA test
start_date <- dmy_hms('01-06-2019 00:00:01')
times <- start_date + seconds(cumsum(floor(runif(n = 10000, min = 1, max = 300))))

df2 <- data_frame(
    'time' = times,
    'dates' = dmy(format(`times`, "%d-%m-%Y")),
    'id' = 1:length(dates),
    'conversion' = rbinom(n = length(times), size = 1, prob = 0.6)
)
View(df2)

df_AA <- rbind.data.frame(cbind.data.frame(df, treatment = rep('A', nrow(df))),
                          cbind.data.frame(df2, treatment = rep('B', nrow(df2)))
)
View(df_AA)


# plot daily conversion
daily_conversion_df <- df_AA %>%
    group_by(`dates`, `treatment`) %>% 
    summarise(conversion_rate = mean(`conversion`))
View(daily_conversion_df)

g3 <- daily_conversion_df %>% 
    ggplot(mapping = aes(x = `dates`, y = `conversion_rate`, colour = `treatment`)) + 
    geom_point() + 
    geom_line() +
    labs(title = 'Daily Conversion Rate', y = 'Conversion Rate', x = 'Date') +
    ylim(0, 1)
g3


# plot Weekly conversion
weekly_conversion_df <- df_AA %>%
    mutate(`week` = strftime(`dates`, format = "%V")) %>% 
    group_by(`week`, `treatment`) %>% 
    mutate(`max_date` = max(`dates`)) %>% 
    ungroup() %>% 
    group_by(`max_date`, `treatment`) %>% 
    summarise(conversion_rate = mean(`conversion`))
View(weekly_conversion_df)

g4 <- weekly_conversion_df %>% 
    ggplot(mapping = aes(x = `max_date`, y = `conversion_rate`, colour = `treatment`)) + 
    geom_point() + 
    geom_line() +
    labs(title = 'Weekly Conversion Rate', y = 'Conversion Rate', x = 'Date') +
    ylim(0, 1)
g4


# 
get_pvalue <- function(df){
    # returns chi-squared p-value
    m1 <- df %>% 
        group_by(`treatment`, `conversion`) %>% 
        summarise(n = n())
    
    m2<-matrix(as.numeric(c(m1[1,3], m1[3,3], m1[2,3], m1[4,3])), 2,2)
    rownames(m2)<-c("A", "B")
    colnames(m2)<-c("No (0)", "Yes (1)")
    
    # chi-square test
    return(chisq.test(m2)$p.value)
}


get_pvalue(df_AA)

start_date
class(times)
max(times)
#?seq(times[1], times[10], by = 'minutes')
minutes <- ymd_hms(unique(format(df_AA$time, "%Y-%m-%d %H:%M:00")))
minutes[10:13]
df_AA$time[1]
df_AA %>% filter(`time` < minutes[10])


p.values = c(1)
new_mins <- as.POSIXct(minutes[1])
for(i in seq(10, length(minutes), 10)){
    p.values <- c(p.values, get_pvalue(df = df_AA %>% filter(`time` < minutes[i])))
    new_mins <- c(new_mins, minutes[i])
}

df_p.values <- data.frame(
    'p.values' = p.values,
    'time' = new_mins
)

g5 <- df_p.values %>% 
    ggplot(mapping = aes(x = `time`, y = `p.values`)) + 
    geom_point() + 
    geom_line() +
    labs(title = 'p-value over time', y = 'p-value', x = 'Date') +
    ylim(0, 1)
g5


# run this 1000 times. How often does the p-value get smaller than 0.05?

too_many_p.values <- function(seed, make_plot=FALSE){
    
    set.seed(seed)
    
    start_date <- dmy_hms('01-06-2019 00:00:01')
    times <- start_date + seconds(cumsum(floor(runif(n = 10000, min = 1, max = 300))))
    
    df <- data_frame(
        'time' = times,
        'dates' = dmy(format(`times`, "%d-%m-%Y")),
        'id' = 1:length(dates),
        'conversion' = rbinom(n = length(times), size = 1, prob = 0.6)
    )
    
    # AA test
    start_date <- dmy_hms('01-06-2019 00:00:01')
    times <- start_date + seconds(cumsum(floor(runif(n = 10000, min = 1, max = 300))))
    
    df2 <- data_frame(
        'time' = times,
        'dates' = dmy(format(`times`, "%d-%m-%Y")),
        'id' = 1:length(dates),
        'conversion' = rbinom(n = length(times), size = 1, prob = 0.6)
    )
    
    df_AA <- rbind.data.frame(cbind.data.frame(df, treatment = rep('A', nrow(df))),
                              cbind.data.frame(df2, treatment = rep('B', nrow(df2)))
    )
    
    p.values = c(1)
    minutes <- ymd_hms(sort(unique(format(df_AA$time, "%Y-%m-%d %H:%M:00"))))
    new_mins <- as.POSIXct(minutes[1])
    for(i in seq(30, length(minutes), 10)){
        p.values <- c(p.values, get_pvalue(df = df_AA %>% filter(`time` < minutes[i])))
        new_mins <- c(new_mins, minutes[i])
    }
    
    
    if(make_plot){
        
        df_p.values <- data.frame(
            'p.values' = p.values,
            'time' = new_mins
        )
        
        g5 <- df_p.values %>% 
            ggplot(mapping = aes(x = `time`, y = `p.values`)) + 
            geom_point() + 
            geom_line() +
            labs(title = 'p-value over time', y = 'p-value', x = 'Date') +
            ylim(0, 1)
        print(g5)
        
    }
    
    # return
    return(sum(p.values < 0.05) > 0)
    
    
}

seeds <- 1:1000
below_0.05 <- c()
for(s in seeds){
    below_0.05 <- c(below_0.05, too_many_p.values(s))
}

sum(below_0.05)/length(below_0.05)

head(seeds[below_0.05], 10)

t <- Sys.time()
too_many_p.values(6, TRUE)
print(Sys.time() - t)
