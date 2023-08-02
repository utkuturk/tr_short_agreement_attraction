

df_merged %<>% within(., {
  cGrammatical <- ifelse(grammatical == "grammatical", .5, -.5)
  cUngrammatical <- ifelse(grammatical == "ungrammatical", .5, -.5)
  cAttractorPlural <- ifelse(attractor_num == "plural", .5, -.5)
  cEndsInConsonant <- ifelse(experiment != "Experiment 1", .5, -.5)
  #cFreqlog_n1n2 <- scale(freqlog_n1n2)
  #cFreqlog_n1 <- scale(freqlog_n1)
  #cFreqlog_n2 <- scale(freqlog_n2)
})

priors <- c(set_prior("student_t(3,0,2.5)", class = "Intercept"),
            set_prior("normal(0,1)", class = "b"),
            set_prior("cauchy(0,1)", class = "sd"),
            set_prior("lkj(2)", class = "cor"))


df_merged_nofillers <- df_merged %>% subset(is.na(source) | source != "filler")

# RUN WITH PRIORS!


models <- c()

n_chains <- 4
n_cores <- 4
n_iter <- 2000
n_warmup <- 1000

fname_responses <- "./R/fits/responses_priors"
fname_responses_stan <- "./R/models/responses_priors.stan"
m_responses <- brm(ResponseYes ~ cEndsInConsonant * cGrammatical * cAttractorPlural + 
                                 (cGrammatical * cAttractorPlural + 1| subject) + 
                                 (cGrammatical * cAttractorPlural + 1| item),
                   data = df_merged_nofillers,
                   prior = priors,
                   family = bernoulli("probit"), 
                   chains = n_chains, cores = n_cores, iter = n_iter, warmup = n_warmup, init_r = .1,
                   file = fname_responses, save_model = fname_responses_stan)

models[['responses']] <- m_responses

fname_ungram_responses <- "./R/fits/ungram_responses_priors"
fname_ungram_responses_stan <- "./R/models/ungram_responses_priors.stan"
m_ungram_responses <- brm(ResponseYes ~ cEndsInConsonant * cAttractorPlural + 
                            (cAttractorPlural + 1| subject) + 
                            (cAttractorPlural + 1| item),
                          data = df_merged_nofillers %>% subset(grammatical != "grammatical"),
                          family = bernoulli("probit"), 
                          prior = priors,
                          chains = n_chains, cores = n_cores, iter = n_iter, warmup = n_warmup, init_r = .1,
                          file = fname_ungram_responses, save_model = fname_ungram_responses_stan)

models[['ungram_responses']] <- m_ungram_responses



tables <- vector('list', length(models))
for (i in seq_along(models)) {
  fname <- paste0('results_table_', names(models[i]))
  tables[[fname]] <- fixef(models[[i]], summary = T, robust = F) %>%
    as.data.frame() #%>% tibble::rownames_to_column("variables")
  
}


{
  if (exp1only == T) {
    priors <- c(set_prior("student_t(3,0,2.5)", class = "Intercept"),
                set_prior("normal(0,1)", class = "b", coef = "freqlog_n1n2"),
                set_prior("normal(0,1)", class = "b", coef = "cGrammatical"),
                set_prior("normal(0,1)", class = "b", coef = "cAttractorPlural"),
                set_prior("normal(0,1)", class = "b", coef = "cGrammatical:freqlog_n1n2"),
                set_prior("normal(0,1)", class = "b", coef = "cAttractorPlural:freqlog_n1n2"),
                set_prior("normal(0,1)", class = "b", coef = "cGrammatical:cAttractorPlural"),
                set_prior("normal(0,1)", class = "b", coef = "cGrammatical:cAttractorPlural:freqlog_n1n2"),
                set_prior("cauchy(0,1)", class = "sd"),
                set_prior("lkj(2)", class = "cor"))
    fname_responses_exp1only <-  "./R/fits/responses_exp1only_priors"
    fname_responses_exp1only_stan <- "./R/models/responses_exp1only_priors.stan"
    m_responses_exp1only <- 
      brm(ResponseYes ~ cGrammatical * cAttractorPlural * freqlog_n1n2 + 
                       (cGrammatical * cAttractorPlural * freqlog_n1n2 + 1| subject) + 
                       (cGrammatical * cAttractorPlural + 1| item),
          data = df_merged_nofillers %>% subset(experiment == "Experiment 1"),
          family = bernoulli("probit"), 
          prior = priors,
          chains = n_chains, cores = n_cores, iter = n_iter, warmup = n_warmup, init_r = .1,
          file = fname_responses_exp1only, save_model = fname_responses_exp1only_stan
    )
    models[['responses_exp1']] <- m_responses_exp1only
  }
}

{
  if (lagoonly == T) {
    priors <- c(set_prior("student_t(3,0,2.5)", class = "Intercept"),
                set_prior("normal(0,1)", class = "b", coef = "freqlog_n1n2"),
                set_prior("normal(0,1)", class = "b", coef = "cGrammatical"),
                set_prior("normal(0,1)", class = "b", coef = "cAttractorPlural"),
                set_prior("normal(0,1)", class = "b", coef = "cGrammatical:freqlog_n1n2"),
                set_prior("normal(0,1)", class = "b", coef = "cAttractorPlural:freqlog_n1n2"),
                set_prior("normal(0,1)", class = "b", coef = "cGrammatical:cAttractorPlural"),
                set_prior("normal(0,1)", class = "b", coef = "cGrammatical:cAttractorPlural:freqlog_n1n2"),
                set_prior("cauchy(0,1)", class = "sd"),
                set_prior("lkj(2)", class = "cor"))
    fname_responses_Lagoonly <-  "./R/fits/responses_Lagoonly_priors"
    fname_responses_Lagoonly_stan <- "./R/models/responses_Lagoonly_priors.stan"
    m_responses_Lagoonly <- 
      brm(ResponseYes ~ cGrammatical * cAttractorPlural * freqlog_n1n2 +
                       (cGrammatical * cAttractorPlural * freqlog_n1n2 + 1| subject) + 
                       (cGrammatical * cAttractorPlural + 1| item),
          data = df_merged_nofillers %>% subset(experiment != "Experiment 1"),
          family = bernoulli("probit"), 
          prior = priors,
          chains = n_chains, cores = n_cores, iter = n_iter, warmup = n_warmup, init_r = .1,
          file = fname_responses_Lagoonly, save_model = fname_responses_Lagoonly_stan)
    models[['responses_lago']] <- m_responses_Lagoonly
    
  }
}


{
  if (rt == T) {
    priors <- c(set_prior("student_t(3,0,2.5)", class = "Intercept"),
                set_prior("normal(0,1)", class = "b"),
                set_prior("cauchy(0,1)", class = "sd"),
                set_prior("lkj(2)", class = "cor"))
    fname_rt <-  "./R/fits/rt_priors"
    fname_rt_stan <- "./R/models/rt_priors.stan"
    m_rts <- brm(RT ~ cEndsInConsonant * cGrammatical * cAttractorPlural * freqlog_n1n2 +
                     (cGrammatical * cAttractorPlural * freqlog_n1n2 + 1| subject) +
                     (cGrammatical * cAttractorPlural + 1| item),
                 data = df_merged_nofillers,
                 family = lognormal(),
                 prior = priors,
                 chains = n_chains, cores = n_cores, iter = n_iter, warmup = n_warmup,
                 file = fname_rt, save_model = fname_rt_stan)
    models[['rt']] <- m_rts
  }
}


