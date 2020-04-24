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


parameters_values %>% filter(name == "incubation period") %>%
    ggplot(aes(x=id))+ geom_pointrange(aes(y=value,ymin = lower_bound , ymax = upper_bound)) +
    geom_hline(yintercept = mean(parameters_values$value[parameters_values$name == "incubation period"]))
paste0(" average incubation period = ", mean(parameters_values$value[parameters_values$name == "incubation period"]))

parameters_values %>% filter(name == "time from symptom onset to hospitalization") %>%
    ggplot(aes(x=id))+ geom_pointrange(aes(y=value,ymin = lower_bound , ymax = upper_bound)) +
    geom_hline(yintercept = mean(parameters_values$value[parameters_values$name == "incubation period"]))
paste0("time from symptom onset to hospitalization  = ", mean(parameters_values$value[parameters_values$name == "time from symptom onset to hospitalization"]))


parameters_values %>% filter(name == "time from hospitalization to recovery") %>%
    ggplot(aes(x=id))+ geom_pointrange(aes(y=value,ymin = lower_bound , ymax = upper_bound)) +
    geom_hline(yintercept = mean(parameters_values$value[parameters_values$name == "incubation period"]))
paste0("time from hospitalization to recovery= ", mean(parameters_values$value[parameters_values$name == "time from hospitalization to recovery"]))
