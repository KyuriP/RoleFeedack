library(furrr); plan(multisession)

plan(multisession, workers = 10)  # or try 2

source("code/mod_specification.R")
source("code/euler_stochastic2.R")
source("code/helper_func.R")


# Define f(S) = S^2 for mod_spec()
f <- function(x) x^2

# Network structure
A_orig <- matrix(c(
  .30, 0, 0, 0, 0, 0, 0, 0, 0,
  .33, .30, .14, .15, 0, .13, 0, 0, .15,
  0,  0, .30, .22, .23, 0, 0, 0, 0,
  .21, 0, 0, .30, 0, 0, 0.12, 0, 0,
  0, 0, 0, .17, .30, 0, 0, 0, 0,
  0, .13, 0, 0, .15, .30, .2, .15, .22,
  0, 0, 0, 0, 0, 0, .30, .17, 0,
  0, 0, 0, 0, 0, 0, 0, .30, 0,
  0, 0, 0, 0, 0, 0, 0, .3, .30
), 9, 9, byrow = TRUE)
rownames(A_orig) <- colnames(A_orig) <- c("anh", "sad", "slp", "ene", "app", "glt", "con", "mot", "sui")

# Setup
edge_indices <- which(A_orig > 0 & row(A_orig) != col(A_orig), arr.ind = TRUE)
modifiable_edges <- lapply(seq_len(nrow(edge_indices)), function(i) edge_indices[i, ])
target_loops <- 0:20
max_per_group <- 100
sim_per_net <- 30
deltaT <- 0.2
timelength <- 2000

# Create random weighted network
set.seed(1)
A_rand <- matrix(0, 9, 9)
diag(A_rand) <- 0.30
rand_weights <- runif(nrow(edge_indices), 0.1, 0.4)
for (j in seq_len(nrow(edge_indices))) {
  A_rand[edge_indices[j, 1], edge_indices[j, 2]] <- rand_weights[j]
}

# Generate configs and loop counts
all_configs <- generate_configurations(A_rand, modifiable_edges)

# Count number of feedback loops (excluding self-loops)
loop_numbers <- purrr::map_dbl(all_configs, \(network) {
  find_loops(create_adjacency_list(network), network) |> length() - 9 # subtract self-loops
})


loop_counts <- loop_numbers
# Optional: print loop count distribution to debug
print(table(loop_numbers))

# Sample equal number of configurations per loop group (0, 5, 10, 15)
selected_configs <- unlist(lapply(target_loops, function(k) {
  idx <- which(loop_counts == k)
  if (length(idx) == 0) return(integer(0))
  sample(idx, min(max_per_group, length(idx)))
}), use.names = FALSE)

configs <- all_configs[selected_configs]
loop_counts <- loop_counts[selected_configs]

# Define the exact time points you want
target_times <- c(400, 800, 1200, 1600, 1999.8)
time_indices <- as.integer(target_times / deltaT + 1)  # Index positions

# Run simulation
# Run simulation and directly summarize by config and time
df_summary <- future_map_dfr(seq_along(configs), function(i) {
  A_conf <- configs[[i]]
  loop_count <- loop_counts[[i]]
  
  mod <- mod_spec("base", init_val = 0.01, mat = A_conf)
  
  sim_outputs <- replicate(sim_per_net, {
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
    
    out[time_indices, ] |>
      dplyr::mutate(total = rowSums(dplyr::across(dplyr::starts_with("S_")))) |>
      dplyr::select(t, total)
  }, simplify = FALSE)
  
  # Summarize across reps per time point
  bind_rows(sim_outputs, .id = "rep") |>
    dplyr::mutate(rep = as.integer(rep)) |>
    group_by(t) |>
    dplyr::summarise(
      mean_symptom = mean(total),
      loops = loop_count,
      sigma = cal_sd(A_conf)$sumsdStr,
      config_id = i,
      .groups = "keep"
    )
}, .progress = TRUE, .options = furrr_options(seed = TRUE))

# Save results
saveRDS(df_summary, "results/sensitivity_res_100_0-20loops.rds")



df_summary |>
  group_by(loops) |>
  dplyr::summarize(mean = mean(mean_symptom))

## recreate result 
df_summary |> 
  ggplot(aes(x = factor(loops), y = mean_symptom, fill= factor(t), color = factor(t))) +
  geom_boxplot(width = .7,
               outlier.alpha = 0.1,
               outlier.size = 0.6,
               outlier.shape = 21,
               outlier.color = NA,
               outlier.fill = NULL,
               alpha = 0.1,
               position = position_dodge(width = 0.8))  +
  scale_fill_manual(values = c("cyan3", "#E7B000", "salmon", "palegreen3", "slateblue3")) +
  scale_colour_manual(values = c("cyan4", "#E7A809", "salmon2", "palegreen4", "slateblue4"), labels = c("t = 400", "t = 800", "t = 1200", "t = 1600", "t = 2000")) +
  labs(x = "Number of feedback loop", y = "Average aggregated symptom level", col = "")  +
  theme_pubr() +
  guides(fill = "none") + 
  theme(legend.position = "bottom",
        # space between legend and plot
        legend.box.spacing = unit(1.3, "cm"),
        text = element_text(size = 23, family="Palatino"),
        legend.text=element_text(size=rel(0.9)),
        axis.title.y = element_text(vjust = +3),
        axis.title.x = element_text(vjust = -0.75),
        plot.margin = margin(t = 3, r = 4, b = 1, l = 1, "cm"))

