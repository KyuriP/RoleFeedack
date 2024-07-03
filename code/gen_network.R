library(matrixStats)
library(igraph)
library(purrr)

# Create adjacency list
create_adjacency_list <- function(matrix) {
  A <- t(matrix)
  graph_list <- vector("list", nrow(A))
  for (i in seq_len(nrow(A))) {
    graph_list[[i]] <- list()
  }
  for (i in seq_len(nrow(A))) {
    for (j in seq_len(ncol(A))) {
      if (A[j, i] > 0) {
        graph_list[[j]] <- c(graph_list[[j]], i)
      }
    }
  }
  return(graph_list)
}

# Find loops
find_loops <- function(graph, matrix) {
  dfs <- function(node, path) {
    if (node %in% path) {
      loop <- c(path[which(path == node):length(path)], node)
      loops <<- union(loops, list(loop))
      return()
    }
    path <- c(path, node)
    next_nodes <- graph[[node]]
    if (!is.null(next_nodes)) {
      for (next_node in next_nodes) {
        dfs(next_node, path)
      }
    }
  }
  
  calculate_weighted_loop_length <- function(loop) {
    weight <- 0
    for (i in seq_len(length(loop) - 1)) {
      a <- loop[i]
      b <- loop[i + 1]
      weight <- weight + 1 / matrix[b, a]
    }
    return(weight)
  }
  
  loops <- list()
  for (start_node in seq_along(graph)) {
    dfs(start_node, integer(0))
  }
  
  unique_loops_with_lengths <- list()
  for (loop in loops) {
    if (loop[1] == min(loop)) {
      loop_length <- length(loop) - 1
      weighted_length <- calculate_weighted_loop_length(loop)
      unique_loops_with_lengths[[paste(loop, collapse = "-")]] <- list(loop_length, weighted_length)
    }
  }
  
  return(unique_loops_with_lengths)
}

# Calculate variance row sum
calculate_var_row_sum <- function(matrix) {
  in_degree <- colSums(matrix > 0)
  min_in_degree <- min(in_degree)
  var_in_degree <- var(in_degree)
  
  row_sum <- rowSums(matrix)
  var_row_sum <- var(row_sum)
  return(list(var_row_sum, min_in_degree, var_in_degree))
}

binary_str <- intToBits(8122)[1:length(modifiable_edges)]
binary_str[2] == 1

# Generate configurations
generate_configurations <- function(A, modifiable_edges) {
  configs <- list()
  for (config in 1:(2^length(modifiable_edges) - 1)) {
    new_A <- A
    binary_str <- intToBits(config)[1:length(modifiable_edges)]
    for (idx in seq_along(modifiable_edges)) {
      edge <- modifiable_edges[[idx]]
      i <- edge[1] 
      j <- edge[2] 
      if (binary_str[idx] == 1) {
        new_A[j, i] <- new_A[j, i] + new_A[i, j]
        new_A[i, j] <- 0
      }
    }
    configs <- c(configs, list(new_A))
  }
  return(configs)
}

# Analyze and save networks
analyze_and_save_networks <- function(configs, save_data = TRUE) {
  networks_by_loops <- vector("list", 62 - 15 + 1)
  names(networks_by_loops) <- 15:61
  
  for (config in configs) {
    graph_list <- create_adjacency_list(config)
    unique_loops_with_lengths <- find_loops(graph_list, config)
    loop_count <- length(unique_loops_with_lengths)
    var_row_sum_results <- calculate_var_row_sum(config)
    var_row_sum <- var_row_sum_results[[1]]
    min_in_degree <- var_row_sum_results[[2]]
    var_in_degree <- var_row_sum_results[[3]]
    
    if (!is.null(networks_by_loops[[as.character(loop_count)]])) {
      networks_by_loops[[as.character(loop_count)]] <- c(networks_by_loops[[as.character(loop_count)]], 
                                                         list(list(config, unique_loops_with_lengths, var_row_sum, min_in_degree, var_in_degree)))
    } else {
      message(sprintf("Unexpected number of loops: %d", loop_count))
    }
  }
  
  if (save_data) {
    saveRDS(networks_by_loops, file = "networks_data_K2.rds")
  }
  
  return(networks_by_loops)
}




