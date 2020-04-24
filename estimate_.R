# https://www.medrxiv.org/content/10.1101/2020.03.17.20037481v1.full.pdf - what is servial interval?
# library imports ----------------------------------------------------------

sapply( c("magrittr","dplyr","readr","ggplot2","jsonlite","tidyr","EpiEstim"),
        function(x){
            if(x %in% installed.packages()){
                suppressPackageStartupMessages(library(x, character.only = TRUE))
            }else{
                install.packages(x)
                suppressPackageStartupMessages(library(x, character.only = TRUE))
            }
            x
        },USE.NAMES = FALSE)


#  -------------------------------------------------------------
# RAW DATA IMPORT -------------------------------------------------------------
#  -------------------------------------------------------------

data_india_raw <- read_json("https://api.covid19india.org/raw_data.json",simplifyVector = TRUE)$raw_data %>%
    mutate(dateannounced = as.Date(dateannounced, "%d/%m/%y")) %>%
    na.omit()

parameters_values <- read_csv("https://github.com/midas-network/COVID-19/raw/master/parameter_estimates/2019_novel_coronavirus/estimates.csv") %>%
    mutate(value = as.numeric(value),
           upper_bound = as.numeric(upper_bound),
           lower_bound = as.numeric(lower_bound))

#  -------------------------------------------------------------
# Serial Intervals in publicatinos -------------------------------------------------------------
#  -------------------------------------------------------------

colnames(parameters_values)
parameters_values %>% select(name) %>% unlist() %>% unique() %>% unname()

parameters_values %>% filter(name == "serial interval") %>%
    ggplot(aes(x=id))+ geom_pointrange(aes(y=value,ymin = lower_bound , ymax = upper_bound)) +
    theme_bw() + ggtitle("Distribution of Serial Intervals from Publication")
paste0("mean of means:serial interval = ", mean(parameters_values$value[parameters_values$name == "incubation period"]))

#  -------------------------------------------------------------
# NATIONAL Re -------------------------------------------------------------
#  -------------------------------------------------------------

data_incidence_national <- data_india_raw %>% filter(detectedstate != "") %>%
    group_by(dateannounced) %>%
    summarize(I = length(dateannounced)) %>%
    transmute(dates = dateannounced, I) %>%
    complete(dates = seq.Date(min(dates), max(dates), by="day")) %>%
    mutate(I = ifelse(is.na(I),0,I)) %>%
    filter(dates > "2020-03-20")

result_national <- estimate_R(incid = as.data.frame(data_incidence_national),
                  method = "parametric_si",
                  config = make_config(list(mean_si = 7,std_si = 3)))

plot(result_national)

#  -------------------------------------------------------------
# DISTRICT/STATE -------------------------------------------------------------
#  -------------------------------------------------------------

data_incidence_state <- data_india_raw %>% filter(detecteddistrict == "Chennai") %>%
    group_by(dateannounced) %>%
    summarize(I = length(dateannounced)) %>%
    transmute(dates = dateannounced, I) %>%
    complete(dates = seq.Date(min(dates), max(dates), by="day")) %>%
    mutate(I = ifelse(is.na(I),0,I)) %>%
    filter(dates > "2020-03-29")

result_state <- estimate_R(incid = as.data.frame(data_incidence_state),
                  method = "parametric_si",
                  config = make_config(list(mean_si = 7,std_si = 3)))
plot(result_state)
