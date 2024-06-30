## =========================================================
## Euler–Maruyama method 
##
## approximate numerical solution of SDEs
## (boundary correction has been made)
## =========================================================
## install packages
source("code/libraries.R")


euler_stochastic2 <- function (deterministic_rate, stochastic_rate, initial_condition, 
                               Amat, parameters1 = NULL, t_start = 0, deltaT = 1, timelength = 1, 
                               D1 = 1, shock = F, parameters2 = NULL, t_shock = NULL, duration = NULL, seed = NULL) 
{
  set.seed(seed)
  curr_vec <- c(initial_condition, t = t_start)
  vec_names <- names(curr_vec)
  n_vars <- length(vec_names)
  time_eq <- c(dt ~ 1)
  new_rate_eq <- c(deterministic_rate, time_eq) %>% formula.tools::rhs()
  time_eq_stoc <- c(dt ~ 0)
  new_stochastic_rate <- c(stochastic_rate, time_eq_stoc) %>% 
    formula.tools::rhs()
  n_steps = as.integer(timelength / deltaT)
  out_list <- vector("list", length = n_steps)
  out_list[[1]] <- curr_vec
  for (i in 2:n_steps) {
    if(shock){
      if(i < t_shock / deltaT | i > (t_shock + duration)/deltaT){
        in_list <- c(parameters1, curr_vec) %>% as.list() %>% append(list(A = Amat))
      } else {
        in_list <- c(parameters2, curr_vec) %>% as.list() %>% append(list(A = Amat))
      }
    } else {
      in_list <- c(parameters1, curr_vec) %>% as.list()
    }
    curr_rate <- sapply(new_rate_eq, FUN = eval, envir = in_list) %>% 
      purrr::set_names(nm = vec_names)
    curr_stoch_rate <- sapply(new_stochastic_rate, FUN = eval, 
                              envir = in_list) %>% purrr::set_names(nm = vec_names)
    
    v3 <- c(curr_vec, curr_rate * deltaT, curr_stoch_rate *
              sqrt(2 * D1 * deltaT) * rnorm(n_vars))
    
    curr_vec <- tapply(v3, names(v3), sum) 
    # correction for violating boundaries[0,1]
    curr_vec[-length(curr_vec)] %<>% ifelse(. > 1, abs(. - 2), .) # this would not be over 2 with optimal noise magnitutde
    curr_vec[-length(curr_vec)] %<>% ifelse(. < 0, abs(.), .) # this would not be lower than -2 with optimal noise magnitutde
    out_list[[i]] <- curr_vec
  }
  out_results <- out_list %>% dplyr::bind_rows() %>% dplyr::relocate(t)
  return(out_results)
}

