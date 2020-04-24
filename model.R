library(deSolve)
library(ggplot2)
library(dplyr)

source("parameters.R")

parameters <- parameters_table$value
names(parameters) <- parameters_table$symbol

pop_size <- 7.1E6
social_dist <- 1.0


state <- c(S = pop_size,
           E = 1,
           Ia = 0,
           Im = 0,
           Is = 0,
           Ic = 0,
           R = 0,
           Fat = 0)

seir <- function(t, state , parameters){
    with(as.list(c(state,parameters)),{
        if(t > 15)
            social_dist <- 0.5

        dS <- (-S*social_dist*beta_a*Ia/pop_size -
                   S*social_dist*beta_s*Im/pop_size)*t

        dE <- (S*social_dist*beta_a*Ia/pop_size +
                   S*social_dist*beta_s*Im/pop_size -
                   E * 1/inc_p)*t

        dIa <- (E*1/inc_p -
                    Ia*as_f/as_m_t -
                    Ia * (1-as_f)/as_m_t)*t

        dIm <- (Ia * (1-as_f)/as_m_t -
                    Im * m_sev_r/m_sev_t  -
                    Im * ft_m/ft_t-
                    Im * rec_m_r/rec_m_t)*t

        dIs <- (Im * m_sev_r/m_sev_t  -
                    Is * sev_c_r/sev_c_t -
                    Is * ft_s/ft_t-
                    Is * rec_sev_r/rec_sev_t)*t

        dIc <- (Is * sev_c_r/sev_c_t -
                    Ic * ft_c/ft_t -
                    Ic * rec_c_r/rec_c_t)*t

        dR <- (Im * rec_m_r/rec_m_t +
                   Is * rec_sev_r/rec_sev_t +
                   Ic * rec_c_r/rec_c_t +
                   Ia*as_f/as_m_t)*t

        dFat <- (Im * ft_m/ft_t +
                     Is * ft_s/ft_t +
                     Ic * ft_c/ft_t)*t
        list(c(dS,dE,dIa,dIm,dIs,dIc,dR,dFat))
    })
}

times <- seq(0, 40, by = 0.1)


out <- ode(y = state, times = times, func = seir, parms = parameters)

out %>%  as.data.frame() %>% mutate(hospitalized = Im+Is+Ic,
                                    infected = E+hospitalized) %>%
    filter(time > 5) %>%
    ggplot(aes(x = time))+
    geom_line(aes(y = Fat, color = "Fatality"), )+
    geom_line(aes(y= hospitalized, color = "Hospitalized")) +
    # geom_line(aes(y=R, color = "Recovered")) +
    # geom_line(aes(y=S, color = "Susceptible")) +
    geom_line(aes(y=infected, color = "Infected")) +
    ylab("count")+xlab("time(days)")+
    # scale_y_log10() +
    theme_bw()

tail(out)
