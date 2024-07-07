## =========================================================
## Model specification
##
## specify parameter values given the chosen scenario
## define differential equations
## 
## return: list of relevant parameters and equations
## =========================================================
## install packages
source("code/libraries.R")

mod_spec <- function(scenario = "base", init_val = 0, mat, ...){
  
  ## weigthed adjacency matrix
  # A <- matrix(c( .30, 0, 0, 0, 0, 0, 0, 0, 0,
  #                .33, .30, .14, .15, 0, .13, 0, 0, .15,
  #                .13, .14, .30, .22, .23, 0, 0, 0, 0,
  #                .21, .15, .22, .30, 0, 0, .12, 0, 0,
  #                0, 0, 0, .17, .30, 0, 0, 0, 0,
  #                0, .13, 0, 0, .15, .30, .2, .15, .22,
  #                0, 0, 0, 0, 0, 0, .30, .17, 0,
  #                0, 0, 0, 0, 0, 0, 0, .30, 0,
  #                0, 0, 0, 0, 0, 0, 0, .3, 0.30), 9, 9, byrow = T)
  A <- mat
  rownames(A) <- colnames(A) <- c("anh", "sad", "slp", "ene", "app", "glt", "con", "mot", "sui")
  
  ## beta
  Beta_bistable <- (-colSums(A) * (1.20)) |> set_names(paste0("Beta_", colnames(A))) # -omega * (1 + 0.415)
  Beta_sick <- (-colSums(A) * (0.80))  |> set_names(paste0("Beta_", colnames(A))) 
  
  ## delta 
  delta <- rep(9, 9) |> set_names(paste0("delta_", colnames(A)))
  
  ## high resilience case
  if (scenario == "high") {
    diag(A) <- 0.25
    delta <- rep(10, 9) |> set_names(paste0("delta_", colnames(A)))
  } 
  
  ## low resilience case
  if (scenario == "low") {
    diag(A) <- 0.35
    delta <- rep(8, 9) |> set_names(paste0("delta_", colnames(A)))
  } 
  
  
  # define deterministic part:
  # "dS[i] ~ S[i]*(1-S[i])*(Beta[i] + A_i[i]*S[i] + SUM(A_[j][i]*S_[j])*(1+ delta[i]*f(S[i])))"
  
  dif_eq <- 1:9 |> map(function(x){
    lhs <- paste0("dS", paste0("_",colnames(A)[x]))
    sumAj <- paste0("S", paste0("_",colnames(A)[-x]), collapse = ",") 
    rhs <- stringr::str_replace_all(
      "Sk * (1-Sk) * (Betak + (A[q,q] * Sk) + ((1+ deltak * f(Sk)) * A[-q,q] %*% ", c(k = paste0("_",colnames(A)[x]), q = x)) |> paste0(paste0("c(", sumAj, ")))"))
    form <- as.formula(paste(lhs,"~", rhs))
  })
  
  
  # define stochastic part:
  sto_eq <-  1:9 |> map(function(x){
    lhs <- paste0("dS", paste0("_",colnames(A)[x]))
    rhs <- 1  # change as you need
    form <- as.formula(paste(lhs,"~", rhs))
  })
  
  init <- c(S_anh = init_val, 
            S_sad = init_val, 
            S_slp = init_val, 
            S_ene = init_val, 
            S_app = init_val, 
            S_glt = init_val, 
            S_con = init_val, 
            S_mot = init_val, 
            S_sui = init_val)
  
  return(model = list(delta = delta, Beta_bistable = Beta_bistable, Beta_sick = Beta_sick, A = A, initial_values = init, dif_eq = dif_eq, sto_eq = sto_eq))
}


