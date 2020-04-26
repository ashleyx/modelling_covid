sapply( c("magrittr","dplyr","ggplot2","jsonlite","reshape2"),
        function(x){
            suppressPackageStartupMessages(library(x, character.only = TRUE))
            x
        },USE.NAMES = FALSE)
options(stringsAsFactors = FALSE)

data_india <- read_json("https://api.covid19india.org/states_daily.json",simplifyVector = TRUE)$states_daily %>%
    melt(id.vars = c("status","date")) %>%
    transmute(state = variable,
              date = as.Date(date,"%d-%b-%y"),
              status,
              count = as.integer(value)) %>%
    group_by(state,status) %>%
    mutate(count = cumsum(count)) %>%
    filter(state == "tt")


shift <- which.min((out$D - min(data_india$count[data_india$status == "Deceased"]))^2)
print(shift)

out_fitted_old <- out %>% filter(time >= shift) %>%
    mutate(date = data_india$date[1] + time - shift) %>%
    transmute(date,
              Deceased = D,
              Confirmed = I+ H) %>%
    melt(id.vars = "date") %>% rename(predicted = value, status = variable) %>%
    left_join(data_india, by = c("date" = "date", "status" = "status")) %>% na.omit()
head(out_fitted)

out_fitted_old %>% filter(status == "Deceased") %>%
ggplot(aes(x = date , color = status))+
    geom_line(aes(y=count, linetype = "actual")) +
    geom_line(aes(y = predicted, linetype = "predicted")) +
    scale_y_log10()

