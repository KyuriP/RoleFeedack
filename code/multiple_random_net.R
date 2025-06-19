library(furrr)
library(purrr)
library(dplyr)
library(stringr)
library(tibble)
library(ggplot2)

plan(multisession, workers = 12)

source("code/mod_specification.R")
source("code/euler_stochastic2.R")
source("code/helper_func.R")

# Parameters
n_random_networks <- 5
n_nodes <- 9
n_edges <- 16
max_per_group <- 500
sim_per_net <- 30
deltaT <- 0.5
timelength <- 2000
target_loops <- c(0, 3, 6, 9, 12, 15, 18)
target_times <- c(400, 800, 1200, 1600, 1999.5)
time_indices <- as.integer(target_times / deltaT + 1)

# Function to generate random weighted network
generate_random_weighted_network <- function(n_nodes, n_edges, weight_range, self_loop_value, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  A <- matrix(0, nrow = n_nodes, ncol = n_nodes)
  diag(A) <- self_loop_value
  off_diag_indices <- which(row(A) != col(A), arr.ind = TRUE)
  selected_edges <- off_diag_indices[sample(nrow(off_diag_indices), n_edges, replace = FALSE), ]
  rand_weights <- runif(n_edges, weight_range[1], weight_range[2])
  for (k in seq_len(n_edges)) {
    i <- selected_edges[k, "row"]
    j <- selected_edges[k, "col"]
    A[i, j] <- rand_weights[k]
  }
  var_names <- c("anh", "sad", "slp", "ene", "app", "glt", "con", "mot", "sui")
  rownames(A) <- colnames(A) <- var_names
  return(A)
}

# Main wrapper for 1 random base network
run_one_random_network_analysis <- function(seed_id) {
  A_random <- generate_random_weighted_network(n_nodes, n_edges, c(0.1, 0.4), self_loop_value = 0.3, seed = seed_id)
  modifiable_edges <- which(A_random > 0 & row(A_random) != col(A_random), arr.ind = TRUE)
  modifiable_edges <- lapply(seq_len(nrow(modifiable_edges)), function(i) modifiable_edges[i, ])
  
  all_configs <- generate_configurations(A_random, modifiable_edges)
  loop_numbers <- map_dbl(all_configs, \(net) find_loops(create_adjacency_list(net), net) |> length() - 9)
  
  selected_configs <- withr::with_seed(seed_id, {
    unlist(lapply(target_loops, function(k) {
      idx <- which(loop_numbers == k)
      if (length(idx) == 0) return(integer(0))
      sample(idx, min(max_per_group, length(idx)))
    }), use.names = FALSE)
  })
  
  configs <- all_configs[selected_configs]
  loop_counts <- loop_numbers[selected_configs]
  
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
        mutate(total = rowSums(across(starts_with("S_")))) |>
        select(t, total)
    }, simplify = FALSE)
    
    bind_rows(sim_outputs, .id = "rep") |>
      mutate(rep = as.integer(rep)) |>
      group_by(t) |>
      summarise(
        mean_symptom = mean(total),
        loops = loop_count,
        sigma = cal_sd(A_conf)$sumsdStr,
        config_id = i,
        seed = seed_id,
        .groups = "keep"
      )
  }, .progress = TRUE, .options = furrr_options(seed = TRUE))
  
  # Calculate loop overlap scores
  nos_scores <- map(configs, function(mat) {
    graph <- create_adjacency_list(mat)
    loops <- find_loops(graph, mat)
    loops <- discard(loops, ~ .x$loop_length == 1)
    
    if (length(loops) == 0) return(tibble(nos1 = 0, nos2 = 0))
    
    node_counts <- str_extract_all(names(loops), "\\d", simplify = TRUE)[, -1]
    node_table <- table(node_counts)
    
    common_score <- sum(node_table^2)
    n_loop <- length(loops)
    tibble(nos1 = common_score / (n_loop^2), nos2 = sum(as.numeric(node_table) - 1) / n_loop)
  })
  
  nos_df <- list_rbind(nos_scores)
  nos_df <- dplyr::mutate(nos_df, config_id = seq_len(nrow(nos_df)))
}

# Run across multiple random networks
set.seed(123)  # top-level seed for reproducibility
seeds <- sample(1:1e6, n_random_networks)  # fixed list of unique seeds

df_summary_all <- map_dfr(seeds, run_one_random_network_analysis)

# Save combined results
saveRDS(df_summary_all, "results/sensitivity_multiple_random_networks.rds")
