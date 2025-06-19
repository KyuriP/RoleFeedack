library(dplyr)
library(ggplot2)
library(purrr)
library(patchwork)

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

# Compute mean and SD per time point for each repetition group
df_summary <- results %>%
  group_by(group, time) %>%
  summarise(
    mean_symptom = mean(total),
    sd_symptom = sd(total),
    .groups = "drop"
  )


df_summary <- df_summary |>
  mutate(group = factor(group, levels = c("10 reps", "30 reps", "50 reps", "100 reps")))

facet_plot <- ggplot(df_summary, aes(x = time, y = mean_symptom)) +
  geom_ribbon(aes(ymin = mean_symptom - sd_symptom,
                  ymax = mean_symptom + sd_symptom),
              fill = "gray70", alpha = 0.5) +
  geom_line(color = "steelblue", linewidth = 0.7) +
  facet_wrap(~group, ncol = 2) +
  labs(x = "Time", y = "Aggregated symptom level",
       title = "Symptom trajectories with SD (by repetition count)") +
  theme_minimal(base_size = 14)

mean_plot <- ggplot(df_summary, aes(x = time, y = mean_symptom, color = group)) +
  geom_line(linewidth = 0.7, alpha = 0.5) +
  labs(x = "Time", y = "Aggregated symptom level",
       title = "Comparison of mean trajectories across repetition counts",
       color = "Repetition") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")


# Combine with (a), (b) labels
# Combine plots with subplot labels
combined_plot <- (facet_plot / mean_plot) + patchwork::plot_annotation(tag_levels = 'a')

# Save to file
ggsave("figure/repetition_stability_combined.png", combined_plot,
       width = 10, height = 8, units = "in")
