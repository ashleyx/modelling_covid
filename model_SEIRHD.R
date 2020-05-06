library(deSolve)
library(ggplot2)
library(dplyr)


parameters_table <- tribble(
    ~parameter,   ~value, ~symbol,
    "R0", 4.0 , "R0",
    "incubation time", 6.3 , "inc_t",
    "infectious time", 5.0 , "inf_t",
    "hospitalization time", 16.0 , "hosp_t",
    "Infection mortality rate", 0.01 , "IFR"
)

parameters <- parameters_table$value
names(parameters) <- parameters_table$symbol
parameters <- c(parameters, beta = as.numeric(parameters["R0"]/parameters["inf_t"]))
parameters

pop_size <- 1.4E9
social_dist <- 1

state <- c(S = pop_size,
           E = 1,
           I = 0,
           H = 0,
           R = 0,
           D = 0)

seird <- function(t, state , parameters){
    with(as.list(c(state,parameters)),{
        if(t > (shift + 20)){
            social_dist <- 0.1
        }

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

plot <- out %>% filter( D > 10) %>%
    ggplot(aes(x = time))+
    # geom_line(aes(y=R, color = "Recovered")) +
    geom_line(aes(y = D, color = "Fatality"), )+
    geom_line(aes(y=H, color = "Hospitalized")) +
    # geom_line(aes(y=S, color = "Susceptible")) +
    ylab("count")+xlab("time(days)")+
    # scale_y_log10() +
    theme_bw()

# print(plot)

cat("\nRe:", social_dist*parameters["R0"])
cat("\n%infected:",out$R[nrow(out)]*100/pop_size)
cat("\n%fatality:",out$D[nrow(out)]*100/pop_size)

cat("\nfatalities:", scales::scientific(out$D[nrow(out)]))
