
## =========================================================
## Main Simulation Script
##
## Description:
## - Generate all unique network configurations based on the provided matrix A.
## - Simulate system dynamics 50 times for each network configuration.
## - Compute and save the average results across simulations.
##
## Note:
## - The original matrix A is processed here.
## - Additional configurations were executed separately on Snellius.
##   Corresponding scripts can be found in the "snellius" folder.
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

## set the seed
# set.seed(123)

# get all networks
# Main execution
## weighted adjacency matrix
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

## define modifiable edges
modifiable_edges <- list(c(2,1), c(2,3), c(2,4), c(2,6), c(2,9), c(3,4), c(3,5), c(4,1), c(4,7), c(5,4), c(6,2), c(6,5), c(6,7), c(6,8), c(6,9), c(7,8), c(9,8))

## define dyadic loop edges
# dyadic_loops <- list(c(2,6), c(6,2))

## generate all unique network configurations
all_networks <- generate_configurations(A, modifiable_edges)
## generate all unique network configs considering dyadic_loops
# all_networks <- generate_configurations2(A, modifiable_edges, dyadic_loops)


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

# number of iteration 
n_sims <- 50

# simulate given the original A matrix
original_res2 <- purrr::map(1:n_sims, ~ euler_stochastic2(
  Amat = mod$A, 
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
  duration = shock_duration)  |>
    # due to tiny differences in the underlying floating point representation of the numbers in R
    dplyr::filter(round(t,1) == 400.0 |round(t,1) == 800.0 |round(t,1) == 1200.0 |round(t,1) == 1600.0 | round(t,1) == 1999.9) |>
    dplyr::mutate(total = rowSums(pick(S_anh:S_sui)), .keep = "none")
) |> 
  purrr::list_cbind() |> 
  dplyr::mutate(mat = paste0(rep(0, each = 5),"-", c(400, 800, 1200, 1600, 2000)))

# get the average for the original A
ori_avg_res <- original_res2 |>
  rowwise() |>
  dplyr::mutate(avg = mean(c_across(starts_with("total...")))) |>
  select(avg, mat) |>
  ungroup() # cancel rowwise


# Simulate the (rest of) unique network configurations

## first start with all network configurations
# extract all networks
# all_networks <- generate_configurations(A, modifiable_edges)
# append the original A matrix to the list
all_networks <- append(list(A), all_networks)

# =======================================
# function to convert matrix to a character string
matrix_to_string <- function(mat) {
  paste(as.vector(mat), collapse = ",")
}
# convert matrices to string representations
matrix_strings <- sapply(all_networks, matrix_to_string)

# check for duplicates
duplicate_indices <- which(duplicated(matrix_strings))

# adjust indices to exclude the original network configuration
dup_ind <- duplicate_indices -1

# remove redundant configurations from bidirectional edges
real_all_networks <- all_networks[-c(duplicate_indices)]

# =======================================
# load data (run from snellius)
res16384 <- readRDS("data/res_16384.rds")
# "mat order" has to be manually edited 
res32768 <- readRDS("data/res_32768.rds") |> dplyr::mutate(mat = paste0(rep(16385:32768, each = 5),"-", c(400, 800, 1200, 1600, 2000)))
res49152 <- readRDS("data/res_49152.rds") |> dplyr::mutate(mat = paste0(rep(32769:49152, each = 5),"-", c(400, 800, 1200, 1600, 2000)))
res65536 <- readRDS("data/res_65536.rds") |> dplyr::mutate(mat = paste0(rep(49153:65536, each = 5),"-", c(400, 800, 1200, 1600, 2000)))
res81920 <- readRDS("data/res_81920.rds") |> dplyr::mutate(mat = paste0(rep(65537:81920, each = 5),"-", c(400, 800, 1200, 1600, 2000)))
res98304 <- readRDS("data/res_98304.rds") |> dplyr::mutate(mat = paste0(rep(81921:98304, each = 5),"-", c(400, 800, 1200, 1600, 2000)))
res114688 <- readRDS("data/res_114688.rds") |> dplyr::mutate(mat = paste0(rep(98305:114688, each = 5),"-", c(400, 800, 1200, 1600, 2000)))
res131071 <- readRDS("data/res_131071.rds") |> dplyr::mutate(mat = paste0(rep(114689:131071, each = 5),"-", c(400, 800, 1200, 1600, 2000)))

# bind'em together
res2 <- res16384 |> bind_rows(res32768, res49152, res65536, res81920, res98304, res114688, res131071) 
rm(res16384, res32768, res49152, res65536, res81920, res98304, res114688, res131071)


# file_paths <- list.files("data", full.names = TRUE, pattern = "res_.*\\.rds")
# res_list <- purrr::map(file_paths, readRDS)
# res2 <- purrr::map2_dfr(res_list, seq_along(res_list), ~ {
#   n_rows <- nrow(.x)
#   start_index <- (.y - 1) * 16384 + 1
#   end_index <- .y * 16384
#   .x |> 
#     dplyr::mutate(mat = paste0(rep(seq(start_index, end_index), each = 5)[1:n_rows], "-", c(400, 800, 1200, 1600, 2000)))
# })
# 
# # Remove the list to free up memory after combining the results
# rm(res_list)

# filter the duplicated ones
new_res2 <- res2 |> filter(!stringr::str_detect(mat, paste0("^((", paste(dup_ind, collapse = "|"), ")-)")))

# get the averages
avg_res <- res2 |> 
  rowwise() |>
  dplyr::mutate(avg = mean(c_across(starts_with("total...")))) |>
  dplyr::select(avg, mat) |>
  ungroup() # cancel rowwise()



## Explicitly specifying number of workers
## (default is parallelly::availableCores())
# plan(multicore)
# message("Number of parallel workers: ", nbrOfWorkers())
# 
# run simulation on all the rest network configs (these run on SNELLIUS)
# 
# aggregated <- furrr::future_map(1:n_sims, function(i) {
#   furrr::future_map(1:length(all_networks), function(j) { euler_stochastic2(
#     Amat = all_networks[[j]],
#     deterministic_rate = mod$dif_eq,
#     stochastic_rate = mod$sto_eq,
#     initial_condition = mod$initial_values,
#     parameters1 = parms1,
#     parameters2 = parms2,
#     deltaT = deltaT,
#     timelength = timelength,
#     D1 = D_stoeq1,
#     shock = TRUE,
#     t_shock = t_shock,
#     duration = shock_duration) |>
#       # due to tiny differences in the underlying floating point representation of the numbers in R
#       dplyr::filter(round(t,1) == 400.0 |round(t,1) == 800.0 |round(t,1) == 1200.0 |round(t,1) == 1600.0 | round(t,1) == 1999.9) |>
#       dplyr::mutate(total = rowSums(pick(S_anh:S_sui)), .keep = "none")
#   }, .options =furrr_options(seed = TRUE)) |>
#     purrr::list_rbind()
# }, .options = furrr_options(seed = TRUE)) |>
#   purrr::list_cbind() |>
#   dplyr::mutate(mat = paste0(rep(1:length(all_networks), each = 5),"-", c(400, 800, 1200, 1600, 2000)))
## saveRDS(aggregated, "res2.rds")


