library(dplyr)
library(ggplot2)
library(purrr)

# Select a single configuration
set.seed(123)
A_conf <- configs[[1]]
mod <- mod_spec("base", init_val = 0.01, mat = A_conf)

# Simulation parameters
rep_sizes <- c(10, 30, 50, 100)
deltaT <- 0.5
timelength <- 2000

# Run simulations for each repetition group
results <- map_dfr(rep_sizes, function(n_reps) {
  sims <- replicate(n_reps, {
    out <- euler_stochastic2(
      Amat = mod$A,
      deterministic_rate = mod$dif_eq,
      stochastic_rate = mod$sto_eq,
      initial_condition = mod$initial_values,
      parameters1 = c(mod$Beta_bistable, mod$delta),
      parameters2 = c(mod$Beta_sick, mod$delta),
      deltaT = deltaT,
      timelength = timelength,
      D1 = 0.01,
      shock = TRUE,
      t_shock = 50,
      duration = 300
    ) |> as.data.frame()
    
    out |> 
      mutate(time = out$t,
             total = rowSums(across(starts_with("S_")))) |> 
      select(time, total)
  }, simplify = FALSE)
  
  bind_rows(sims, .id = "rep") |> 
    mutate(rep = as.integer(rep), 
           group = paste0(n_reps, " reps"))
})

# Compute group mean trajectories
mean_df <- results |> 
  group_by(group, time) |> 
  summarise(mean_symptom = mean(total), .groups = "drop")

# Plot
ggplot(results, aes(x = time, y = total, group = interaction(rep, group))) +
  geom_line(alpha = 0.2, color = "gray60") +
  geom_line(data = mean_df, 
            aes(x = time, y = mean_symptom, color = group, group = group), 
            linewidth = 1.2) +
  theme_minimal(base_size = 14) +
  labs(x = "Time", y = "Aggregated symptom level",
       title = "Stability of symptom trajectories across repetition sizes",
       color = "Repetition count") +
  theme(legend.position = "bottom")

