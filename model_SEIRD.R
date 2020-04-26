library(deSolve)
library(ggplot2)
library(dplyr)


parameters_table <- tribble(
    ~parameter,   ~value, ~symbol,
    "R0", 3.0 , "R0",
    "incubation time", 6.0 , "inc_t",
    "hospitalization time", 21 , "hosp_t",
    "Infection mortality rate", 0.01 , "IFR"
)

parameters <- parameters_table$value
names(parameters) <- parameters_table$symbol
parameters <- c(parameters, beta = as.numeric(parameters["R0"]/parameters["hosp_t"]))
parameters

pop_size <- 7.1E6
social_dist <- 1.0


state <- c(S = pop_size,
           E = 1,
           I = 0,
           R = 0,
           D = 0)

seird <- function(t, state , parameters){
    with(as.list(c(state,parameters)),{

        dS <- - S * I * beta * social_dist / pop_size

        dE <- (S * I * beta * social_dist / pop_size) -
            E/ inc_t

        dI <- (E/ inc_t)-
            I / hosp_t

        dR <- (1-IFR) * I / hosp_t

        dD <- IFR * I / hosp_t

        list(c(dS,dE,dI,dR,dD))
    })
}

times <- seq(0, 400, by = 0.1)

out <- ode(y = state, times = times, func = seird, parms = parameters) %>% as.data.frame()

out %>%
    ggplot(aes(x = time))+
    geom_line(aes(y=R, color = "Recovered")) +
    geom_line(aes(y = D, color = "Fatality"), )+
   geom_line(aes(y=I, color = "Confirmed")) +
    geom_line(aes(y=S, color = "Susceptible")) +
    ylab("count")+xlab("time(days)")+
    # scale_y_log10() +
    theme_bw()


cat("%recovered:",out$R[nrow(out)]*100/pop_size)
cat("%fatality:",out$D[nrow(out)]*100/pop_size)
