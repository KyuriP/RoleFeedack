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

mod_spec <- function(scenario = "base", init_val = 0, ...){
  
  ## weigthed adjacency matrix
  A <- matrix(c( .30, 0, 0, 0, 0, 0, 0, 0, 0,
                 .33, .30, .14, .15, 0, .13, 0, 0, .15,
                 .13, .14, .30, .22, .23, 0, 0, 0, 0,
                 .21, .15, .22, .30, 0, 0, .12, 0, 0,
                 0, 0, 0, .17, .30, 0, 0, 0, 0,
                 0, .13, 0, 0, .15, .30, .2, .15, .22,
                 0, 0, 0, 0, 0, 0, .30, .17, 0,
                 0, 0, 0, 0, 0, 0, 0, .30, 0,
                 0, 0, 0, 0, 0, 0, 0, .3, 0.30), 9, 9, byrow = T)
  rownames(A) <- colnames(A) <- c("anh", "sad", "slp", "ene", "app", "glt", "con", "mot", "sui")
  
  ## beta
  Beta_bistable <- c(-1.373, -1.019, -0.934, -1.189, -0.962, -0.608, -0.877, -1.302, -2.30) |> set_names(paste0("Beta_", colnames(A))) # -omega * (1 + 0.41)
  # Beta_bistable <- c(-1.4067, -1.0440, -0.9570, -1.2180, -0.9860, -0.6235, -0.8990, -1.334, -2.30) |> set_names(paste0("Beta_", colnames(A))) # -omega * (1 + 0.45)
  # Beta_bistable <- c(-1.355, -0.95, -0.905, -1.165, -0.935, -0.575, -0.845, -1.285, -2.30) |> set_names(paste0("Beta_", colnames(A))) # -omega * (1 + appx 0.375)
  # Beta_bistable <- c(-1.309, -0.972, -0.891, -1.134, -0.918, -0.580, -0.837, -1.242, -2.30) |> set_names(paste0("Beta_", colnames(A))) # -omega * (1 + 0.35)
  
  Beta_sick <- c(-0.91, -0.66, -0.60, -0.78, -0.62, -0.37, -0.56, -0.86, -1.30) |> set_names(paste0("Beta_", colnames(A))) #- A_j + dist*5
  
  ## delta 
  delta <- rep(9, 9) |> set_names(paste0("delta_", colnames(A)))
  
  ## high resilience case
  if (scenario == "high") {
    diag(A) <- 0.25
    delta <- rep(8, 9) |> set_names(paste0("delta_", colnames(A)))
  } 
  
  ## low resilience case
  if (scenario == "low") {
    diag(A) <- 0.35
    delta <- rep(10, 9) |> set_names(paste0("delta_", colnames(A)))
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


