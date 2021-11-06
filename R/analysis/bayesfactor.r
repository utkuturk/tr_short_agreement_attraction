

fname_responses_null <- "./R/fits/responses_null"
fname_responses_null_stan <- "./R/models/responses_null.stan"
m_responses_null <- brm(ResponseYes ~ cGrammatical * cAttractorPlural +
                                (cGrammatical * cAttractorPlural + 1| subject) +
                                (cGrammatical * cAttractorPlural + 1| item),
                    data = df_merged_nofillers,
                    family = bernoulli("probit"), 
                    prior = c(
                            set_prior("student_t(3,0,2.5)", class = "Intercept"),
                            set_prior("normal(0,1)", class = "b", coef = "cAttractorPlural"),
                            set_prior("normal(0,1)", class = "b", coef = "cGrammatical"),
                            set_prior("normal(0,1)", class = "b", coef = "cGrammatical:cAttractorPlural"),
                            set_prior("normal(0,1)", class = "sd"),
                            set_prior("lkj(2)", class = "cor")
                            ),
                    chains = n_chains, cores = n_cores, iter = n_iter, warmup = n_warmup, init_r = .1,
                    file = fname_responses_null, save_model = fname_responses_null_stan,
                    save_all_pars = TRUE)

lml_null <- bridgesampling::bridge_sampler(m_responses_null, silent = TRUE)

brm_fit_multiple <- function(prior_sd, lower_boundary, fname) {

    fit <- brm(
        ResponseYes ~ cEndsInConsonant * cGrammatical * cAttractorPlural +
                    (cGrammatical * cAttractorPlural + 1 | subject) +
                    (cGrammatical * cAttractorPlural + 1 | item),
        data = df_merged_nofillers,
        prior = c(
            prior(normal(0,1), class = Intercept),
            set_prior(paste0("normal(0,", prior_sd, ")"), class = "b", lb = lower_boundary),
            prior(normal(0,1), class = "sd"),
            prior(lkj(2), class = "cor")
        ),
        family = bernoulli("probit"),
        chains = 4, cores = 6,
        iter = 20000, warmup = 2000, init_r = .1,
        save_all_pars = TRUE,
        file = file.path("./R/fits", fname),
        save_model = file.path("./R/models", paste0(fname, ".stan")),
    )

    fit

}

m_responses_normal0_01 <- brm_fit_multiple(.01, NA, "m_responses_normal0_01")
m_responses_normal0_02 <- brm_fit_multiple(.02, NA, "m_responses_normal0_02")
m_responses_normal0_04 <- brm_fit_multiple(.04, NA, "m_responses_normal0_04")
m_responses_normal0_06 <- brm_fit_multiple(.06, NA, "m_responses_normal0_06")
m_responses_normal0_08 <- brm_fit_multiple(.08, NA, "m_responses_normal0_08")
m_responses_normal0_1 <- brm_fit_multiple(.1, NA, "m_responses_normal0_1")
m_responses_normal01 <- brm_fit_multiple(1, NA, "m_responses_normal01")

m_responses_normal_plus0_01 <- brm_fit_multiple(.01, 0, "m_responses_normal_plus0_01")
m_responses_normal_plus0_02 <- brm_fit_multiple(.02, 0, "m_responses_normal_plus0_02")
m_responses_normal_plus0_04 <- brm_fit_multiple(.04, 0, "m_responses_normal_plus0_04")
m_responses_normal_plus0_06 <- brm_fit_multiple(.06, 0, "m_responses_normal_plus0_06")
m_responses_normal_plus0_08 <- brm_fit_multiple(.08, 0, "m_responses_normal_plus0_08")
m_responses_normal_plus0_1 <- brm_fit_multiple(.1, 0, "m_responses_normal_plus0_1")
m_responses_normal_plus01 <- brm_fit_multiple(1, 0, "m_responses_normal_plus01")


models_priors <- c(
    m_responses_normal0_01,
    m_responses_normal0_02,
    m_responses_normal0_04,
    m_responses_normal0_06,
    m_responses_normal0_08,
    m_responses_normal0_1,
    m_responses_normal01,
    m_responses_normal_plus0_01,
    m_responses_normal_plus0_02,
    m_responses_normal_plus0_04,
    m_responses_normal_plus0_06,
    m_responses_normal_plus0_08,
    m_responses_normal_plus0_1,
    m_responses_normal_plus01
)


df_bf_priors_info <- data.frame(
    model_name = models_priors,
    beta_prior_sd = rep(c(0.01, seq(0.02, 0.1, 0.02), 1), 2),
    beta_lb = c(rep(c(NA,0), each = 7)),
)

df_bf_priors <- data.frame()

for (m in models_priors) {
    lml_fit <- bridgesampling::bridge_sampler(m, silent = TRUE)

    s <- posterior_samples(m)

    diff <- exp(s$b_Intercept + s$b_cGrammatical + s$b_cAttractorPlural + s$b_cGrammatical:cAttractorPlural) - 
            exp(s$b_Intercept + s$b_cGrammatical + s$b_cAttractorPlural - s$b_cGrammatical:cAttractorPlural)

    temp_df <- data.frame(
        BF = bridgesampling::bayes_factor(lml_fit, lml_null)$bf,
        beta_posterior_mean = posterior_summary(m, pars = "b_cGrammatical:cAttractorPlural")[, "Estimate"],
        beta_posterior_lq = posterior_summary(m, pars = "b_cGrammatical:cAttractorPlural")[, "Q2.5"],
        beta_posterior_hq = posterior_summary(m, pars = "b_cGrammatical:cAttractorPlural")[, "Q97.5"],
        diff_mean = mean(diff),
        diff_lq = quantile(diff, 0.025),
        diff_hq = quantile(diff, 0.975)
    )

    df_bf_priors <- rbind(df_bf_priors, temp_df)

}

df_bf_priors <- cbind(df_bf_priors_info, df_bf_priors)