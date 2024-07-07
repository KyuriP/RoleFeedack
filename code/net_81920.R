## =========================================================
## Main simulation 
##
## run simulation 500 times for each of the different 
## resilience scenarios (low / baseline / high)
## =========================================================
## install packages
library(plyr)
library(doParallel)
library(furrr)
source("code/libraries.R")


## source necessary functions
source("code/euler_stochastic2.R")
source("code/mod_specification.R")
source("code/gen_network.R")


# get all networks
# Main execution
## weigthed adjacency matrix
A <- matrix(c( .30, 0, 0, 0, 0, 0, 0, 0, 0,
               .33, .30, .14, .15, 0, .13, 0, 0, .15,
               0,  0, .30, .22, .23, 0, 0, 0, 0,
               .21, 0, 0, .30, 0, 0, 0.12, 0, 0,
               0, 0, 0, .17, .30, 0, 0, 0, 0,
               0, .13, 0, 0, .15, .30, .2, .15, .22,
               0, 0, 0, 0, 0, 0, .30, .17, 0,
               0, 0, 0, 0, 0, 0, 0, .30, 0,
               0, 0, 0, 0, 0, 0, 0, .3, 0.30), 9, 9, byrow = T)
rownames(A) <- colnames(A) <- c("anh", "sad", "slp", "ene", "app", "glt", "con", "mot", "sui")

# modifiable_edges <- list(c(2,1), c(2,9), c(3,1), c(3,5), c(4,1), c(4,7), c(5,4), c(6,5), c(6,7), c(6,8), c(6,9), c(7,8), c(9, 8))

modifiable_edges <- list(c(2,1), c(2,3), c(2,4), c(2,6), c(2,9), c(3,4), c(3,5), c(4,1), c(4,7), c(5,4), c(6,2), c(6,5), c(6,7), c(6,8), c(6,9), c(7,8), c(9,8))


all_networks <- generate_configurations(A, modifiable_edges)[65537:81920]


## define model specifics: choose the scenario and initial value for symptoms
mod <- mod_spec(scenario = "base", init_val = 0.01, mat = A)


## define "f"
f <- function(x) x^2


## original params
parms1 <- c(mod$Beta_bistable, mod$delta)
## params given shock (beta: increases)  
parms2 <- c(mod$Beta_sick, mod$delta)

## define dt and the number of time steps:
deltaT <- .1 # dt 
timelength <- 2000 # length of simulation
n_steps <- as.integer(timelength / deltaT) # must be a number greater than 1

## specify the magnitude of noise and specifics of shock 
D_stoeq1 <- 0.01  # before shock
# t_shock <- 1000 # time that shock begins
t_shock <- 50 # time that shock begins

shock_duration <- 300 # shock duration time points

# shock period 
shock_period <- data.frame(time = 0:timelength) |>
  mutate(shock = ifelse(time >= t_shock & time <= t_shock + shock_duration, TRUE, FALSE))


## Explicitly specifying number of workers
## (default is parallelly::availableCores())
plan(multicore)
message("Number of parallel workers: ", nbrOfWorkers())


## aggregate symptom level
n_sims <- 50

# run simulation n_sims times

aggregated <- furrr::future_map(1:n_sims, function(i) {
  furrr::future_map(1:length(all_networks), function(j) { euler_stochastic2(
    Amat = all_networks[[j]], 
    deterministic_rate = mod$dif_eq,
    stochastic_rate = mod$sto_eq,
    initial_condition = mod$initial_values,
    parameters1 = parms1,
    parameters2 = parms2, 
    deltaT = deltaT,
    timelength = timelength,
    D1 = D_stoeq1,
    shock = TRUE,
    t_shock = t_shock, 
    duration = shock_duration) |>
      # due to tiny differences in the underlying floating point representation of the numbers in R
      dplyr::filter(round(t,1) == 400.0 |round(t,1) == 800.0 |round(t,1) == 1200.0 |round(t,1) == 1600.0 | round(t,1) == 1999.9) |>
      dplyr::mutate(total = rowSums(pick(S_anh:S_sui)), .keep = "none")
  }, .options =furrr_options(seed = TRUE)) |> 
    purrr::list_rbind() 
}, .options = furrr_options(seed = TRUE)) |> 
  purrr::list_cbind() |>
  dplyr::mutate(mat = paste0(rep(1:length(all_networks), each = 5),"-", c(400, 800, 1200, 1600, 2000)))


saveRDS(aggregated, "res_81920.rds")