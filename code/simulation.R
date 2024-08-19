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

## set the seed
# set.seed(123)

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

all_networks <- generate_configurations(A, modifiable_edges)


# get the number of loops 
# loops <- list()
# for (i in 1:length(all_networks)){
#   loops[[i]] <- find_loops(create_adjacency_list(all_networks2[[i]]), all_networks2[[i]]) 
# }
# 
# loop_numbers |> as.data.frame() |>
# ggplot(aes(x  = loop_numbers)) +
#   geom_histogram(bins = 60)
# 
# loop_numbers3 |> as.data.frame() |>
#   ggplot(aes(x  = loop_numbers3)) +
#   geom_histogram(bins = 60)

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

### ==========
# check the trajectory
# n_sims <- 50
# 
# # run simulation n_sims times
# aggregated <- purrr::map(1:n_sims, ~ euler_stochastic2(
#   Amat = mod$A, 
#   deterministic_rate = mod$dif_eq,
#   stochastic_rate = mod$sto_eq,
#   initial_condition = mod$initial_values,
#   parameters1 = parms1,
#   parameters2 = parms2, 
#   deltaT = deltaT,
#   timelength = timelength,
#   D1 = D_stoeq1,
#   shock = TRUE,
#   t_shock = t_shock, 
#   duration = shock_duration) |>
#     dplyr::mutate(totalsymptom = rowSums(dplyr::pick(S_anh:S_sui)))
# ) |> list_rbind(names_to = "sim")
# 
# ## IQR function
# inter_quantile <- function(x, probs = c(0.25, 0.5, 0.75)) {
#   tibble(
#     val = quantile(x, probs, na.rm = TRUE),
#     quant = probs
#   )
# }
# 
# ## Each symptom plot
# each_summ <- aggregated |> 
#   select(-c(totalsymptom, sim)) |>
#   tidyr::pivot_longer(!t, names_to = "symptoms") |>
#   reframe(inter_quantile(value), .by = c(t, symptoms)) |>
#   pivot_wider(names_from = "quant", values_from = "val", names_glue = "q{quant}") 
# 
# eachsym_quant <- ggplot(data = each_summ) +
#   geom_area(data = shock_period, aes(x = time, y = shock*max(sde_out[,-1])
#   ), inherit.aes = FALSE, fill = "darkgray", alpha = 0.2) +
#   geom_line(aes(x = t, y = q0.5, color = symptoms), alpha = 0.6, lwd = 0.2) +
#   geom_ribbon(aes(x=t,ymin=q0.25,ymax=q0.75, fill = symptoms), alpha=0.3) +
#   scale_color_discrete(name = "Symptoms", labels = c("anhedonia", "sad", "sleep", "energy", "appetite", "guilty", "concentration", "motor", "suicidal")) +
#   labs(x = "time", y= "") +
#   theme_classic(base_size=15) +
#   theme(legend.position="none") +
#   facet_wrap(~symptoms,
#              labeller = labeller(symptoms = labelllername) 
#   ) 
# 
# 
# ## Total symptom level plot
# summarized <- aggregated |>
#   reframe(inter_quantile(totalsymptom), .by = t) |>
#   pivot_wider(names_from = "quant", values_from = "val", names_glue = "q{quant}")
# 
# totalsym <- ggplot(data = summarized) +
#   geom_area(data = shock_period, aes(x = time, y = shock*max(sde_out[,-1])*ncol(sde_out[,-1])
#   ), inherit.aes = FALSE, fill = "darkgray", alpha = 0.2) +
#   geom_line(aes(x = t, y = q0.5), col = "red4", lwd = 0.2) +
#   geom_ribbon(aes(x=t,ymin=q0.25,ymax=q0.75),fill = "tomato1", alpha=0.3) +
#   labs(x = "time", y= "") +
#   geom_hline(yintercept = 5/3, linetype = 2, color = "azure4", lwd = 0.1) +
#   geom_hline(yintercept = 10/3, linetype = 2, color = "azure4", lwd = 0.1) +
#   theme_classic(base_size = 15)

#### ====================

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


saveRDS(aggregated, "res2.rds")


