# library imports ----------------------------------------------------------

sapply( c("magrittr","dplyr","reshape2","ggplot2","jsonlite"),
        function(x){
            if(!x %in% rownames(installed.packages())){
                install.packages(x)
            }
            suppressPackageStartupMessages(library(x, character.only = TRUE))

            x
        },USE.NAMES = FALSE)

#  -------------------------------------------------------- -----
# RAW DATA IMPORT -------------------------------------------------------------
#  -------------------------------------------------------------

data_state <- read_json("https://api.covid19india.org/states_daily.json",simplifyVector = TRUE)$states_daily %>%
    melt(id.vars = c("status","date")) %>%
    transmute(state = variable,
              date = as.Date(date,"%d-%b-%y"),
              status,
              count = as.integer(value)) %>%
    filter(state == "tn", status == "Confirmed") %>% select(-state)

data_state$count[is.na(data_state$count)] <- 0

data_state %<>% group_by(status) %>%
    mutate(tally = cumsum(count))

#  -------------------------------------------------------------
#  Function to simulate  -------------------------------------------------------------
#  -------------------------------------------------------------

simulate_cases <- function(data_cumulative, date_predict, n_days,
                           estimation_window = 3,iters = 1000, simplify = TRUE ){

    # estimation of K = Rt/serial_interval --------------------------------------------------------

    K <- (log(data_cumulative$tally[data_cumulative$date == (date_predict - 1)]) -
              log(data_cumulative$tally[data_cumulative$date == (date_predict - estimation_window)]) ) /
        (estimation_window - 1)

    # Running simulations --------------------------------------------------------
    bootstrap <- matrix(0,nrow = (n_days + 1), ncol = iters) -> bootstrap_new_cases
    bootstrap[1,] <- data_cumulative$tally[data_cumulative$date == (date_predict - 1)]

    for(i in 2:(n_days+1)){
        expected <- bootstrap[i-1,] %>% sapply(function(j){
            j*exp(K) - j
        })

        bootstrap_new_cases[i,] <- expected %>%
            sapply(function(j){
                rpois(1,j)
            })
        bootstrap[i,] <- bootstrap[i-1,] + bootstrap_new_cases[i,]
    }

    if(n_days == 1){
        predicted <- data.frame(day = 1,
                   quantile(bootstrap_new_cases[-1,],probs= c(0.025,0.5,0.975)) %>%  t) %>%
            transmute(date = date_predict + day -1,
                      lower = X2.5. ,
                      mean = X50.,
                      upper = X97.5.)
    }else{
        predicted <- data.frame(day = 1:n_days,
                                apply(bootstrap_new_cases[-1,],1,quantile,probs= c(0.025,0.5,0.975)) %>%  t) %>%
            transmute(date = date_predict + day -1,
                      lower = X2.5. ,
                      mean = X50.,
                      upper = X97.5.)
    }


    if(simplify){
        return(predicted)
    }else{
        return(bootstrap_new_cases)
    }

}

#  -------------------------------------------------------------
#  Simulations -------------------------------------------------------------
#  -------------------------------------------------------------

predicted <- lapply((Sys.Date() - 1:7),function(d){
    simulate_cases(data_cumulative = data_state,
                          date_predict = d,
                          n_days = 1)

}) %>%
    Reduce(f = bind_rows , x = .) %>%
    mutate(mean = NA) %>%
    bind_rows(simulate_cases(data_cumulative = data_state,
                             date_predict = Sys.Date(),
                             n_days = 7))
#  -------------------------------------------------------------
#  Plotting -------------------------------------------------------------
#  -------------------------------------------------------------

data_state %>%  filter(status == "Confirmed") %>%
    ggplot(aes(x= date)) +
    geom_histogram(aes(y = count),stat = "identity", fill = "#999999")+
    geom_histogram(data = predicted,aes(x = date, y = mean), fill = "#CCCCCC", stat = "identity")+
    geom_errorbar(data = predicted, aes( ymin = lower, ymax = upper))+
    # geom_ribbon(data = predicted, aes( ymin = lower, ymax = upper), alpha = 0.3)+
    # geom_line(data = predicted,aes(x = date, y = mean), linetype = 2) +
    theme_bw()


