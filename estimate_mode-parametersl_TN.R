# library imports ----------------------------------------------------------

sapply( c("magrittr","dplyr","readr","reshape2","skimr","ggplot2","gghighlight","jsonlite","deSolve","XML","EpiEstim"),
        function(x){
            if(!x %in% rownames(installed.packages())){
                install.packages(x)
            }
            suppressPackageStartupMessages(library(x, character.only = TRUE))

            x
        },USE.NAMES = FALSE)

#  -------------------------------------------------------------
# RAW DATA IMPORT -------------------------------------------------------------
#  -------------------------------------------------------------

# population data ---------------------------------------------------------

population_data <-readHTMLTable(readLines("https://en.wikipedia.org/wiki/List_of_states_and_union_territories_of_India_by_population"))[[2]] %>%
    {
        population_data <- data.frame(.[-1,])
        colnames(population_data) <- .[1,]
        population_data %>%
            transmute(state = `State or union territory` %>% gsub("\\[c\\]","",x = .),
                      pop = gsub("[^0-9]|\\(.*\\)","",population_data$`Population(%)`) %>%   as.numeric())

    }

# TN case data ------------------------------------------------------------

data_state <- read_json("https://api.covid19india.org/states_daily.json",simplifyVector = TRUE)$states_daily %>%
    melt(id.vars = c("status","date")) %>%
    transmute(state = variable,
              date = as.Date(date,"%d-%b-%y"),
              status,
              count = as.integer(value)) %>%
    filter(state == "tn") %>% select(-state)

data_state$count[is.na(data_state$count)] <- 0

data_state_cumulative <- data_state %>% group_by(status) %>%
    mutate(tally = cumsum(count)) %>%
    filter(date > "2020-04-10")

#  -------------------------------------------------------------
#  DATA DRIVEN PARAMETER ESTIMATION -------------------------------------------------------------
#  -------------------------------------------------------------

# estimation of Re --------------------------------------------------------

re_state <- data_state %>%
    filter(status == "Confirmed") %>%
    transmute(dates = date,
              I = count) %>%
    estimate_R(incid = .,
               method = "parametric_si",
               config = make_config(list(mean_si = 4.7,std_si = 2.9)))
plot(re_state)
Re <- re_state$R$`Mean(R)` %>% { mean(.[(length(.)-6):length(.)])}

# parameter values --------------------------------------------------------


parameters_table <- tribble(
    ~parameter,   ~value, ~symbol,
    "R0", 1.8 , "R0",
    "incubation time", 5.1 , "inc_t",
    "infectious time", 5.0 , "inf_t",
    "hospitalization time", 16.0 , "hosp_t",
    "Infection mortality rate", 0.01 , "IFR"
)

parameters <- parameters_table$value
names(parameters) <- parameters_table$symbol
parameters <- c(parameters, beta = as.numeric(parameters["R0"]/parameters["inf_t"]))
parameters

pop_size <- population_data$pop[population_data$state == "Tamil Nadu"]
social_dist <- 1

state <- c(S = pop_size,
           E = 1,
           I = 0,
           H = 0,
           R = 0,
           D = 0)

seird <- function(t, state , parameters){
    with(as.list(c(state,parameters)),{
        # if(t > (shift + 20)){
        #     social_dist <- 0.1
        # }

        dS <- - S * I * beta * social_dist / pop_size

        dE <- (S * I * beta * social_dist/ pop_size) -
            E/ inc_t

        dI <- (E/ inc_t)-
            (I / inf_t)

        dH <- (I / inf_t) -
            (H/hosp_t)

        dR <- (1-IFR) * H / hosp_t

        dD <- IFR * H / hosp_t

        list(c(dS,dE,dI,dH,dR,dD))
    })
}

times <- seq(0, 500, by = 1)

out <- ode(y = state, times = times, func = seird, parms = parameters) %>% as.data.frame()

n_days <- data_state_cumulative %>% filter(status == "Deceased") %>%  nrow()

shift <- 1:(nrow(out)- n_days) %>%
    sapply(function(i){
        data.frame(actual = data_state_cumulative$tally[data_state_cumulative$status == "Deceased"],
                   predicted = out$D[i:(i+n_days-1)]) %>%
            transmute(RMSE = (actual - predicted)^2) %>% sum(.)

    }) %>% which.min()

which.min((out$D - min(data_state_cumulative$count[data_state_cumulative$status == "Deceased"]))^2)

fitted_data <- out %>% filter(time >= shift) %>%
    mutate(date = data_state_cumulative$date[1] + time - shift) %>%
    transmute(date,
              Deceased = D,
              Confirmed = I+ H + R) %>%
    melt(id.vars = "date") %>% rename(predicted = value, status = variable) %>%
    left_join(data_state_cumulative, by = c("date" = "date", "status" = "status"))

fitted_data %>%
    filter(date < Sys.Date() + 7) %>%
    ggplot(aes(x = date , color = status))+
    geom_line(aes(y=tally, linetype = "actual")) +
    scale_y_log10()+
    geom_line(aes(y = predicted, linetype = "predicted")) +
    theme_bw()

out %>% transmute(I/H, E/H) %>% apply(2,median,na.rm = TRUE) %>% {
    data.frame(parameter = names(.),
               value = .)
} %>%
    bind_rows(data.frame(parameter = "shift",
                         value = shift))

fitted_data %>% filter(status == "Confirmed") %>%  na.omit() %>%
    transmute(detection_ratio = tally/predicted) %$% mean(.$detection_ratio)
