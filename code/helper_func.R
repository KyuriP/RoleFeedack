## =========================================================
## Helper Functions for Simulation and Analysis
##
## This script contains functions for generating network configurations,
## detecting feedback loops, and calculating metrics such as loop length 
## and degree variability, all essential for the subsequent data analysis.
## =========================================================

library(matrixStats)
library(igraph)
library(purrr)


# Create adjacency list
create_adjacency_list <- function(matrix) {
  A <- matrix
  graph_list <- vector("list", nrow(A))
  for (i in seq_len(nrow(A))) {
    graph_list[[i]] <- list()
  }
  for (i in seq_len(nrow(A))) {
    for (j in seq_len(ncol(A))) {
      if (A[i, j] > 0) {  # Correct the adjacency list creation
        graph_list[[i]] <- c(graph_list[[i]], j)
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
      loops <<- base::union(loops, list(loop))
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
      weight <- weight + (1 / matrix[b, a])
      # weight <- weight + (matrix[b, a])
      
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
      rel_weighted_length <- calculate_weighted_loop_length(loop) / loop_length
      unique_loops_with_lengths[[paste(loop, collapse = "-")]] <- 
        data.frame(loop_length = loop_length, 
                   rel_weighted_length = rel_weighted_length)
    }
  } 
  
  return(unique_loops_with_lengths)
}

# Calculate variance in-degree/strength
cal_sd <- function(matrix) {
  in_degree <- colSums(matrix > 0)
  in_strength <- colSums(matrix)
  out_degree <- rowSums(matrix > 0)
  out_strength <- rowSums(matrix)
  
  sd_in_degree <- sd(in_degree)
  sd_in_strength <- sd(in_strength)
  sd_out_degree <- sd(out_degree)
  sd_out_strength <- sd(out_strength)
  return(data.frame(sdInStr = sd_in_strength, sdInDg= sd_in_degree, sdOutStr = sd_out_strength, sdOutDg= sd_out_degree, sumsdStr = sd_in_strength + sd_out_strength, sumsdDg = sd_in_degree + sd_out_degree))
}


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


#########################################################################
# Function to generate all unique configurations considering dyadic loops
generate_configurations2 <- function(A, modifiable_edges, dyadic_loops) {
  configs <- list()
  
  # Total number of configurations
  total_configs <- 2^length(modifiable_edges)
  
  for (config in 0:(total_configs - 1)) {
    new_A <- A
    binary_str <- intToBits(config)[1:length(modifiable_edges)]
    binary_str <- as.integer(rev(binary_str))
    
    is_redundant <- FALSE
    
    # Check for dyadic loop redundancy
    for (dyadic_loop in dyadic_loops) {
      i <- which(sapply(modifiable_edges, function(x) all(x == dyadic_loop[1])))
      j <- which(sapply(modifiable_edges, function(x) all(x == dyadic_loop[2])))
      if (length(i) > 0 && length(j) > 0) {
        if (binary_str[i] == 1 && binary_str[j] == 1) {
          is_redundant <- TRUE
          break
        }
      }
    }
    
    if (is_redundant) next
    
    for (idx in seq_along(modifiable_edges)) {
      edge <- modifiable_edges[[idx]]
      if (binary_str[idx] == 1) {
        new_A[edge[2], edge[1]] <- new_A[edge[1], edge[2]]
        new_A[edge[1], edge[2]] <- 0
      }
    }
    
    configs <- append(configs, list(new_A))
  }
  
  return(configs)
}
