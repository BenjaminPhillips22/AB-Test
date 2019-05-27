# script with functions

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

get_bayes_pvalue <- function(df){
    # information for prior
    p <- 0.6
    sig <- 0.01
    alpha <- ( (1-p)/(sig^2) - 1/p )*(p^2)
    beta <- alpha*(1/p - 1)
    
    print(alpha)
    print(beta)
    
    A_binom <- df %>% filter(`treatment` == 'A') %>% select(`conversion`) %>% as.matrix()
    B_binom <- df %>% filter(`treatment` == 'B') %>% select(`conversion`) %>% as.matrix()
    
    AB1 <- bayesTest(A_binom,
                     B_binom,
                     priors = c('alpha' = alpha,'beta' = beta),
                     n_samples = 1e5,
                     distribution = 'bernoulli')
    
    return(summary(AB1)$probability)
}
get_bayes_pvalue(df_AA)

AB1

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


