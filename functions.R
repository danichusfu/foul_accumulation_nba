fit_gamma_cens <- function(censored, X){
  ll_gamma <- function(logshape, logscale){
    -sum(ifelse(censored,
                pgamma(X, exp(logshape), scale = exp(logscale),
                       lower.tail = FALSE, log.p = TRUE),
                dgamma(X, exp(logshape), scale = exp(logscale), log = TRUE)
    ))
  }
  
  coefs <- stats4::mle(ll_gamma, start = list(logshape = log(1), logscale = log(480)))
  
  
  return(tibble(shape = exp(coefs@coef[1]), rate = 1/exp(coefs@coef[2]), ll_gamma = coefs@min))
  
}


ll_exponential <- function(data, rate){
  - sum(ifelse(data$censored,
               pexp(data$X, rate = rate,
                    lower.tail = FALSE, log.p = TRUE),
               dexp(data$X, rate = rate, log = TRUE)
  ))
}


prep_stan_data <- function(data, players){
  data %>% 
    group_by(FOULER, FOUL_FROM) %>%
    mutate(i = row_number(GAME_ID),
           censored = if_else(TRANSITION_OBSERVED == 1, 0, 1)) %>%
    select(FOULER, FOUL_FROM, i, X = PT_UNTIL_FOUL, censored) %>%
    ungroup() %>%
    complete(FOULER, FOUL_FROM, censored, fill = list(i = 1, X = NA)) %>%
    group_by(FOULER, censored) %>%
    mutate(X = replace_na(X, mean(X, na.rm = T))) %>%
    ungroup() %>%
    inner_join(players, by = "FOULER") %>%
    arrange(FOUL_FROM, FOULER, censored) %>%
    unite(foul_from_position, c("FOUL_FROM", "position"), remove = F) %>%
    return()
}

get_stan_data <- function(data_to_model, priors, prior_var){
  
  data_cens <-
    data_to_model %>%
    filter(censored == 1)
  
  data_obs <-
    data_to_model %>%
    filter(censored == 0)
  
  seq <-
    data_obs %>%
    group_by(FOUL_FROM, FOULER) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    mutate(seq = cumsum(n)) %>%
    pull(seq)
  
  seq <- c(0, seq)
  
  
  seq_cens <-
    data_cens %>%
    group_by(FOUL_FROM, FOULER) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    mutate(seq = cumsum(n)) %>%
    pull(seq)
  
  seq_cens <- c(0, seq_cens)
  
  prior_seq <- 
    data_to_model %>%
    group_by(FOUL_FROM, FOULER, {{prior_var}}) %>%
    summarise(n = n()) %>%
    group_by({{prior_var}}) %>%
    summarise(n_foul = n()) %>%
    ungroup() %>%
    mutate(prior_seq = cumsum(n_foul)) %>%
    pull(prior_seq)
  
  prior_seq <- c(1, prior_seq)
  
  y <- 
    data_obs %>%
    pull(X)
  
  y_cens <-
    data_cens %>%
    pull(X)
  
  num_prior_levels <-
    data_to_model %>%
    count({{prior_var}}) %>%
    nrow()
  
  a_b_mu <- 
    data_to_model %>%
    group_by({{prior_var}}) %>%
    summarise() %>%
    left_join(priors) %>% 
    pull(mean)
  
  a_b_sigma <- 
    data_to_model %>%
    group_by({{prior_var}}) %>%
    summarise() %>%
    left_join(priors) %>% 
    pull(Sigma)

  
  stan_data <- 
    list(N         = length(y), 
         N_cens    = length(y_cens),
         J         = length(seq) - 1, 
         K         = num_prior_levels,
         P         = (length(seq) - 1)/6,
         y         = y, 
         y_cens    = y_cens,
         K_J       = prior_seq,
         seq       = seq,
         seq_cens  = seq_cens,
         a_b_mu    = a_b_mu,
         a_b_sigma = a_b_sigma)
  
  return(stan_data)
}


get_priors <- function(prior_data, prior_players, prior_var){
  
  mles <- 
    prior_data %>%
    filter(FOULER %in% prior_players$FOULER) %>%
    group_by(FOULER, FOUL_FROM) %>%
    mutate(i = row_number(GAME_ID),
           censored = if_else(TRANSITION_OBSERVED == 1, FALSE, TRUE)) %>%
    select(FOULER, FOUL_FROM, i, X = PT_UNTIL_FOUL, censored) %>%
    ungroup() %>%
    mutate(censored = as.numeric(censored)) %>%
    select(FOULER, FOUL_FROM, X, censored) %>%
    group_by(FOULER, FOUL_FROM) %>%
    mutate(num_observed = sum(censored == 0)) %>%
    filter(num_observed > 5) %>%
    nest(data = - c(FOULER, FOUL_FROM)) %>%
    mutate(mle_gamma = map(data, possibly(~ fit_gamma_cens(.$censored, .$X), NA))) %>%
    unnest(mle_gamma) %>%
    select(-data)
  
  mles_swap <-
    mles %>%
    left_join(id_position_map, by = "FOULER") %>%
    filter(FOUL_FROM %in% c(4, 5)) %>%
    ungroup() %>%
    mutate(FOUL_FROM = if_else(FOUL_FROM == 4, 5, 4))
  
  
 priors <-
    mles %>%
    left_join(id_position_map, by = "FOULER") %>%
    bind_rows(mles_swap) %>%
    unite(foul_from_position, c(FOUL_FROM, position), remove = F) %>%
    ungroup() %>%
    select({{prior_var}}, shape, rate) %>%
    nest(data = - {{prior_var}}) %>%
    mutate(Sigma = map(data, cov),
           mean = map(data, ~ c(mean(.$shape), mean(.$rate)))) %>%
    select(-data)
  
  return(priors)
}


fit_stan_model <- function(data_to_model, prior_data, prior_players, prior_var, file_name){
  priors <- get_priors(prior_data, prior_players, {{prior_var}})
  
  stan_data <- get_stan_data(data_to_model, priors, {{prior_var}})
  
  #stan_data$a_b_sigma <- c(0.008 , 0.0003)
  #stan_data$a_b_sigma <- c(0.453 , 0.00156)
  
  init_fn <- function() {
    list("alpha_beta" = matrix(rep(c(1, 1, 1, 1, 1, 1, 
                                     1/485, 1/484, 1/483, 1/482, 1/481, 1/480), 
                                   each = stan_data$P), ncol = 2))
  }
  
  fit <- stan(file = "gamma_trunc_norm_model_cens.stan", 
              data = stan_data,
              chains = 3,
              init = init_fn,
              control = list(adapt_delta = 0.9))
  
  write_rds(fit, paste0("player_specific_17_18/fit_", file_name, ".rds"))
  
}



tidy_stan <- function(data_to_model, top_100, fit){
  
  fouler_by_foul_level <- 
    data_to_model %>%
    group_by(FOUL_FROM, FOULER) %>%
    summarise(n = n())
  
  variates <- rstan::extract(fit)
  alpha_beta <- variates$alpha_beta
  
  alphas <- 
    alpha_beta[,, 1] %>% 
    as_tibble() %>%
    pivot_longer(cols = everything(), names_to = "param", values_to = "alpha") %>%
    nest(alpha = -param)
  
  betas <- 
    alpha_beta[,, 2] %>% 
    as_tibble() %>%
    pivot_longer(cols = everything(), names_to = "param", values_to = "beta") %>%
    nest(beta = -param)
  
  y_new <-
    variates$y_new %>%
    as_tibble() %>%
    pivot_longer(cols = everything(), names_to = "param", values_to = "y_new") %>%
    nest(y_new = -param)
  
  fouler_by_foul_level <- 
    fouler_by_foul_level %>%
    left_join(top_100, by = c("FOULER")) %>% 
    bind_cols(alphas) %>%
    bind_cols(betas) %>%
    bind_cols(y_new) 
  
  return(fouler_by_foul_level)
}


get_mle_info_for_paper <- function(mles){
  
  nba_colours <- c("#1D428A", "#C8102E", "#FE4917", "#27A2CB", "#F0D900", "#333E48")
  
  mles %>%
    ungroup() %>%
    count(FOUL_FROM)
  
  print( mles %>%
    ungroup() %>%
    mutate(mean = shape/rate/60,
           FOUL_FROM = factor(FOUL_FROM + 1)) %>%
    ggplot(aes(x = FOUL_FROM, y = mean, group = FOUL_FROM)) +
    geom_point() +
    geom_boxplot(aes(fill = FOUL_FROM), alpha = 0.3) +
    labs(x = "Foul Level n",
         y = "Estimate of Mean Fouling Time in Minutes") +
    scale_fill_manual(values = nba_colours) +
    theme(legend.position = "none",
          text = element_text(size = 14)))
  
  
  #ggsave("prior_foul_levels.png", height = 5, width = 7, units = "in")
  
  print( mles %>%
    ungroup() %>%
    mutate(mean = shape/rate/60,
           FOUL_FROM = factor(FOUL_FROM + 1)) %>%
    left_join(id_position_map, by = c("FOULER")) %>%  
    mutate(position = case_when(position == "F" ~ "Forward",
                                position == "G" ~ "Guard",
                                position == "C" ~ "Big")) %>%
    ggplot(aes(x = position, y = mean, group = position)) +
    geom_point() +
    geom_boxplot(aes(fill = position), alpha = 0.3) +
    labs(x = "Position",
         y = "Estimate of Mean Fouling Time in Minutes") +
    scale_fill_manual(values = nba_colours) +  
    theme(legend.position = "none",
          text = element_text(size = 14)) )
  
  
  #ggsave("prior_positions.png", height = 5, width = 7, units = "in")
}


fit_ind_stan_model <- function(player){
  data_to_model %>%
    filter(FOULER == player) %>%
    fit_stan_model(prior_data, prior_players, foul_from_position, paste("foul_from_position_constrained", player, sep = "_"))
}

