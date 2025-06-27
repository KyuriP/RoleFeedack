# ===================================================
# Sensitivity Analysis: Structural Robustness via Random Weights (Original Skeleton)
# ===================================================
# Simulates symptom dynamics using weight variants of a fixed empirical network structure.
# Evaluates how the number of feedback loops and structural variability influence symptom severity.
# Load model, solver, and utilities
source("code/mod_specification.R")
source("code/euler_stochastic2.R")
source("code/helper_func.R")
source("code/libraries.R")

plan(multisession, workers = 10)  # Parallel processing

# ---- Define original network structure ----
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

# ---- Define simulation parameters ----
edge_indices <- which(A_orig > 0 & row(A_orig) != col(A_orig), arr.ind = TRUE)
modifiable_edges <- lapply(seq_len(nrow(edge_indices)), function(i) edge_indices[i, ])
target_loops <- 0:20            # Desired feedback loop groups
max_per_group <- 100            # Max configs sampled per loop group
sim_per_net <- 30               # Number of simulations per config
deltaT <- 0.2                   # Time resolution
timelength <- 2000              # Total simulation duration


# ---- Randomize edge weights while preserving skeleton ----
set.seed(1)
A_rand <- matrix(0, 9, 9)
diag(A_rand) <- 0.30
rand_weights <- runif(nrow(edge_indices), 0.1, 0.4)
for (j in seq_len(nrow(edge_indices))) {
  A_rand[edge_indices[j, 1], edge_indices[j, 2]] <- rand_weights[j]
}

# ---- Generate directional configurations ----
modifiable_edges <- lapply(seq_len(nrow(edge_indices)), function(i) edge_indices[i, ])
all_configs <- generate_configurations(A_rand, modifiable_edges)

# Count loops (subtract 9 for self-loops)
loop_numbers <- purrr::map_dbl(all_configs, \(net) {
  find_loops(create_adjacency_list(net), net) |> length() - 9
})
print(table(loop_numbers))  # Loop distribution


# ---- Sample configurations by loop count ----
selected_configs <- unlist(lapply(target_loops, function(k) {
  idx <- which(loop_numbers == k)
  if (length(idx) == 0) return(integer(0))
  sample(idx, min(max_per_group, length(idx)))
}), use.names = FALSE)

configs <- all_configs[selected_configs]
loop_counts <- loop_numbers[selected_configs]

# Define the exact time points you want
target_times <- c(400, 800, 1200, 1600, 1999.8)
time_indices <- as.integer(target_times / deltaT + 1)  # Index positions


# ---- Run simulations and summarize ----
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
# saveRDS(df_summary, "results/sensitivity_res_100_0-20loops.rds")


df_summary |>
  group_by(loops) |>
  dplyr::summarize(mean = mean(mean_symptom))


# ---- Recreate main result  ----
## Fig1
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



## Fig2
# Compute nos1 and nos2 for each configuration in configs
nos_scores <- purrr::map(configs, function(mat) {
  graph <- create_adjacency_list(mat)
  loops <- find_loops(graph, mat)
  loops <- purrr::discard(loops, ~ .x$loop_length == 1)
  
  if (length(loops) == 0) return(tibble::tibble(nos1 = 0, nos2 = 0))
  
  node_counts <- str_extract_all(names(loops), "\\d", simplify = TRUE)[, -1]
  node_table <- table(node_counts)
  
  common_score <- sum(node_table^2)
  n_loop <- length(loops)
  nos1 <- common_score / (n_loop^2)
  nos2 <- sum(as.numeric(node_table) - 1) / n_loop
  
  tibble::tibble(nos1 = nos1, nos2 = nos2)
})

nos_df <- purrr::list_rbind(nos_scores) 

# Add config_id column to nos_df
nos_df <- nos_df |> 
  dplyr::mutate(config_id = seq_len(nrow(nos_df)))

# Join to df_summary by config_id
df_summary2 <- df_summary |>
  dplyr::left_join(nos_df, by = "config_id")


# install.packages("devtools")
# library(devtools)
# devtools::install_github("johannesbjork/LaCroixColoR")
# 
# # gradient color
# # pal <- wes_palette("Zissou1", 100, type = "continuous")
# pal <- LaCroixColoR::lacroix_palette("PeachPear", n = 10, type = "continuous") |> rev()

df_summary2 |> filter(near(t, 1600), loops != 0) |> # decide time points later 
  ggplot(aes(x = sigma , #(1/relstr) * 3,
             y = mean_symptom)) +
  geom_point(aes(col = nos1),#nos1), #jaccard*50), size = 1,
             alpha = 0.7, size = 1.3, shape=20) +
  geom_smooth(method = "loess", linewidth = 0.5, col = alpha("hotpink4", 0.7), se = F, span = 1) +
  
  # scale_color_gradientn(colours = pal) +
  # scale_color_gradientn(colours = alpha(rainbow(10), 0.5)) +
  scale_color_gradientn(colors = c("#172869", "#0A5396", "#037DB9", "#12A0B3", "#48C0AD", "#48C0AD", "#48C0AD", "#BBD9A8", "#E9CD98", "#E9A880", "#F26F44", "#FF3200")) +
  # scale_color_gradient(low = "navy", high = "green") +
  facet_wrap(~factor(loops)) +
  labs(x = expression("Weighted degree variability ("~sigma[tot]~")"), y = "Average aggregated symptom level", color = "Feedback loop\noverlap level") +
  theme_bw() +
  guides(colour=guide_colourbar(barwidth=30,label.position="bottom"))+
  theme(legend.position = "bottom",
        # space between legend and plot
        legend.box.spacing = unit(1.3, "cm"),
        text = element_text(size = 23, family="Palatino"),
        legend.text = element_text(size=rel(0.9)),
        axis.title.y = element_text(vjust = +3),
        axis.title.x = element_text(vjust = -0.75),
        plot.margin = margin(1, 2, 1, 1, "cm")) 

