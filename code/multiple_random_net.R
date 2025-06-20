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

# Parameters ## make sure to set the correct settings
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
  nos_df <- mutate(nos_df, config_id = seq_len(nrow(nos_df)))
  
  # Return joined result
  df_out <- left_join(df_summary, nos_df, by = "config_id")
  return(df_out)
}


# Run across multiple random networks
set.seed(123)  # top-level seed for reproducibility
seeds <- sample(1:1e6, n_random_networks)  # fixed list of unique seeds

df_summary_all <- map_dfr(seeds, run_one_random_network_analysis)

# Save combined results
# saveRDS(df_summary_all, "results/sensitivity_multiple_random_networks.rds")



ggplot(df_summary_all, aes(x = factor(loops), y = mean_symptom, fill = factor(t), color = factor(t))) +
  geom_boxplot(outlier.alpha = 0.1, width = 0.7, alpha = 0.2, position = position_dodge(width = 0.8)) +
  labs(x = "Number of feedback loops", y = "Mean aggregated symptom level", color = "Time", fill = "Time") +
  theme_minimal()


df_summary_all |> filter(near(t, 1200), loops != 0) |> # decide time points later 
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
