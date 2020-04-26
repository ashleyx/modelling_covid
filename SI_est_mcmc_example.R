data("MockRotavirus")


## estimate the reproduction number (method "si_from_data")
MCMC_seed <- 1
overall_seed <- 2
R_si_from_data <- estimate_R(MockRotavirus$incidence,
                             method = "si_from_data",
                             si_data = MockRotavirus$si_data,
                             config = make_config(list(si_parametric_distr = "G",
                                                       mcmc_control = make_mcmc_control(list(burnin = 1000,
                                                                                             thin = 10, seed = MCMC_seed),
                                                                                        n1 = 500, n2 = 50,
                                                                                        seed = overall_seed))))

## compare with version with no uncertainty
R_Parametric <- estimate_R(MockRotavirus$incidence,
                           method = "parametric_si",
                           config = make_config(list(
                               mean_si = mean(R_si_from_data$SI.Moments$Mean),
                               std_si = mean(R_si_from_data$SI.Moments$Std))))
## generate plots
p_uncertainty <- plot(R_si_from_data, "R", options_R=list(ylim=c(0, 1.5)))
p_no_uncertainty <- plot(R_Parametric, "R", options_R=list(ylim=c(0, 1.5)))
gridExtra::grid.arrange(p_uncertainty, p_no_uncertainty,ncol=2)

## the left hand side graph is with uncertainty in the SI distribution, the
## right hand side without.
## The credible intervals are wider when accounting for uncertainty in the SI
## distribution.

## estimate the reproduction number (method "si_from_sample")
MCMC_seed <- 1
overall_seed <- 2
SI.fit <- coarseDataTools::dic.fit.mcmc(dat = MockRotavirus$si_data,
                                        dist = "G",
                                        init.pars = init_mcmc_params(MockRotavirus$si_data, "G"),
                                        burnin = 1000,
                                        n.samples = 5000,
                                        seed = MCMC_seed)
si_sample <- coarse2estim(SI.fit, thin = 10)$si_sample
R_si_from_sample <- estimate_R(MockRotavirus$incidence,
                               method = "si_from_sample",
                               si_sample = si_sample,
                               config = make_config(list(n2 = 50,
                                                         seed = overall_seed)))
plot(R_si_from_sample)

## check that R_si_from_sample is the same as R_si_from_data
## since they were generated using the same MCMC algorithm to generate the SI
## sample (either internally to EpiEstim or externally)
all(R_si_from_sample$R$`Mean(R)` == R_si_from_data$R$`Mean(R)`)

## End(Not run)
