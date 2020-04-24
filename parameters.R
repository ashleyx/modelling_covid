library(tibble)

R0 <- 2.8
generation_time <- 12


parameters_table <- tribble(
    ~parameter,   ~value, ~symbol,
    "effective_contact_rate_symptomatic",    R0/9,    "beta_s",
    "effective_contact_rate_asymptomatic",    R0/4.3,    "beta_a",
    "incubation_period" ,  6.3,    "inc_p",
    "asymptomatic_fraction",   0.2,    "as_f",
    "asymptomatic_to_mild",  4.3,    "as_m_t",
    "recovery_mild_ratio",   0.8,    "rec_m_r",
    "recovery_mild_time", 9,    "rec_m_t",
    "recovery_severe_ratio", 0.6,    "rec_sev_r",
    "recovery_severe_time", 14,    "rec_sev_t",
    "recovery_critical_ratio",   0.5,    "rec_c_r",
    "recovery_critical_time" ,21,    "rec_c_t",
    "fatality_mild" ,0.01,    "ft_m",
    "fatality_severe", 0.10,    "ft_s",
    "fatality_critical", 0.5,    "ft_c",
    "fatality_time", 7 , "ft_t",
    "mild_to_severe_time", 8,    "m_sev_t",
    "mild_to_severe_ratio", 0.19,    "m_sev_r",
    "severe_to_critical_time", 8,    "sev_c_t",
    "severe_to_critical_ratio", 0.30,    "sev_c_r"
)


cat("Sanity intact?\n")
sum(parameters_table$value[parameters_table$symbol %in% c("rec_m_r","m_sev_r","ft_m")]) == 1
sum(parameters_table$value[parameters_table$symbol %in% c("rec_sev_r","sev_c_r","ft_s")]) == 1
sum(parameters_table$value[parameters_table$symbol %in% c("rec_c_r","ft_c")]) == 1
