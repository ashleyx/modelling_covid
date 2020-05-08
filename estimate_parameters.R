library(readr)
library(magrittr)
library(dplyr)
library(ggplot2)

parameters_values <- read_csv("https://github.com/midas-network/COVID-19/raw/master/parameter_estimates/2019_novel_coronavirus/estimates.csv") %>%
    mutate(value = as.numeric(value),
           upper_bound = as.numeric(upper_bound),
           lower_bound = as.numeric(lower_bound))

colnames(parameters_values)
parameters_values %>% select(name) %>%  unique() %>% unlist() %>%  unname()

parameters <- c("serial interval" ,
  "latent period",
  "infection fatality ratio" ,
  "incubation period",
  "time from symptom onset to hospitalization",
  "time from symptom onset to death",
  "time from symptom onset to recovery") %>% sapply(function(i) mean(parameters_values$value[parameters_values$name == i]) ) %>%
    {
        x <- data.frame(abbrv = c("si","lat_p","ifr","inc_p","t_s2h","t_s2d","t_s2r"),
                   value = .,
                   name = names(.))
        rownames(x) <- NULL
        x
    }

parameters$value[parameters$abbrv == "ifr"] <- 0.01
parameters$value[parameters$abbrv == "t_s2d"] <- 28 # observation from trajectories in india

print(parameters)
write_csv(parameters,"parameter_values.csv")
# trash -------------------------------------------------------------------
#
# c("serial interval" ,
#   "latent period",
#   "infection fatality ratio" ,
#   "incubation period",
#   "time from symptom onset to hospitalization",
#   "time from symptom onset to death",
#   "time from symptom onset to recovery") %>% lapply(function(i){
#       paste0(i," = ", mean(parameters_values$value[parameters_values$name == i])) %>% cat("\n")
#
#       parameters_values %>% filter(name == i) %>%
#           ggplot(aes(x=id))+ geom_pointrange(aes(y=value,ymin = lower_bound , ymax = upper_bound)) +
#           geom_hline(yintercept = mean(parameters_values$value[parameters_values$name == i])) +
#           ggtitle(i)+
#           theme_bw()+
#           theme(axis.title.x = element_blank())
#   })

