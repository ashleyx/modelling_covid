
# library imports ----------------------------------------------------------

sapply( c("magrittr","dplyr","ggplot2","jsonlite","EpiEstim","purrr",
          "cowplot","future","fable","bsts","EpiSoon","reshape2"),
        function(x){
            if(!x %in% rownames(installed.packages())){
                install.packages(x)
            }
            suppressPackageStartupMessages(library(x, character.only = TRUE))

            x
        },USE.NAMES = FALSE)

# TN case data ------------------------------------------------------------

data_state <- read_json("https://api.covid19india.org/states_daily.json",simplifyVector = TRUE)$states_daily %>%
    melt(id.vars = c("status","date")) %>%
    transmute(state = variable,
              date = as.Date(date,"%d-%b-%y"),
              status,
              count = as.integer(value)) %>%
    filter(state == "tn") %>% dplyr::select(-state)

data_state$count[is.na(data_state$count)] <- 0

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

obs_rts <- data.frame(rt = re_state$R$`Mean(R)`,
           date = re_state$dates[-1:-7],
           timeseries = "Tamil Nadu")

obs_cases <- data.frame(cases = data_state$count[data_state$status == "Confirmed"],
                        date = data_state$date[data_state$status == "Confirmed"],
                        timeseries = "Tamil Nadu")
models <- list("AR 3" =
                   function(...) {EpiSoon::bsts_model(model =
                                                          function(ss, y){bsts::AddAr(ss, y = y, lags = 3)}, ...)},
               "Semi-local linear trend" =
                   function(...) {EpiSoon::bsts_model(model =
                                                          function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)}, ...)},
               "ARIMA" =


                    function(...){EpiSoon::fable_model(model = fable::ARIMA(y ~ time), ...)})

future::plan("sequential")

## Compare models
forecasts <- EpiSoon::compare_timeseries(obs_rts, obs_cases, models,
                                         horizon = 7, samples = 10,
                                         serial_interval = EpiSoon::example_serial_interval)

forecasts

EpiSoon::plot_forecast_evaluation(forecasts$forecast_rts, obs_rts, c(7)) +
    ggplot2::facet_grid(model ~ timeseries) +
    cowplot::panel_border()

EpiSoon::plot_forecast_evaluation(forecasts$forecast_cases, obs_cases, c(7)) +
    ggplot2::facet_grid(model ~ timeseries, scales = "free") +
    cowplot::panel_border()

EpiSoon::summarise_scores(forecasts$case_scores)


EpiSoon::summarise_scores(forecasts$case_scores)

