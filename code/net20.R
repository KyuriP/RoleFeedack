

# check nloops == 20
comb_avg_res |> filter(t == 2000 & nloop == 18, avg <=0.8 | avg >= 8, length ==2)
#low 35656
#high 68021
loop_info[[35656]]


highnet <-comb_avg_res|> filter(t == 2000) |> 
  # slice the top 100
  slice_max(order_by = avg, n = 100)

# for low net, we exclude the acyclic ones as they are not informative 
lownet <-comb_avg_res|> filter(t == 2000, nloop!=0) |>
  # slice the bottom 100 
  slice_min(order_by = avg, n = 100)

high_idx <- highnet$matr |> as.numeric() + 1
low_idx <- lownet$matr |> as.numeric() + 1

high_net <- loop_info[high_idx]
low_net <- loop_info[low_idx]

high_node <- high_net |>  purrr::map(\(x) str_extract_all(x$id, "\\d", simplify = T)[,-1] |> 
                          table(exclude = "") |> as.data.frame()) |> list_rbind() |>
  summarize(total = sum(Freq), .by = Var1) |>
  mutate(total = total / sum(total))

low_node <- low_net |>  purrr::map(\(x) str_extract_all(x$id, "\\d", simplify = T)[,-1] |> 
                                       table(exclude = "") |> as.data.frame()) |> list_rbind() |>
  summarize(total = sum(Freq), .by = Var1) |>
  mutate(total = total/sum(total))
  
# check the node frequency 
bind_rows(high_node, low_node, .id = "id") |>
  ggplot(aes(x = factor(Var1, level = c(1:9)), y = total, col = id, group = id)) +
  geom_point()+ geom_line()


# check the edges
# Sample vector of cycles

# Function to extract edges from a cycle
extract_edges <- function(cycle) {
  edges <- list()
  for (i in 1:length(cycle)){
    node <- unlist(strsplit(cycle[i], "-"))
    edge <- paste(node, c(node[-1], node[1]), sep = "-")
    # Exclude the last element to avoid repeating the first edge
    edges[[i]] <- edge[-length(edge)]}  
  return(unlist(edges))
}


# get the edge freq proportion
high_edges <- high_net |>  purrr::map(\(x) x$id |> extract_edges()) |> unlist() |> table() |> prop.table() |> as.data.frame()
low_edges <- low_net |>  purrr::map(\(x) x$id |> extract_edges()) |> unlist() |> table()  |> prop.table() |> as.data.frame()

high_edges |>
  arrange(Freq)

low_edges |>
  arrange(Freq)

bind_cols(high_edges, low_edges)|>
  summarize(diff = Freq...2 - Freq...4, .by = Var1...1)  |>
  arrange(diff)

    ggplot(aes(x = Var1...1, y = diff, group = 1)) +
  geom_line()

# plot the edge freq prop.
bind_rows(high_edges, low_edges, .id = "id") |>
  ggplot(aes(x = Var1, y = Freq, col = id, group = id)) +
  geom_point()+ geom_line()

# avg mat        nloop  nos1  nos2 jaccard specrad matr  t     group
# <dbl> <chr>      <dbl> <dbl> <dbl>   <dbl>   <dbl> <chr> <fct> <chr>
#   1 8.74  35746-2000    19 0.402 0.538   0.502   0.624 35746 2000  0.20+
#   2 0.451 42319-2000    19 0.437 0.573   0.528   0.648 42319 2000  0.20+
#   3 8.59  69298-2000    19 0.491 0.614   0.564   0.642 69298 2000  0.20+
#   4 8.58  69306-2000    19 0.491 0.614   0.564   0.642 69306 2000  0.20+

high19 <- loop_info[[35253]]
low19 <- loop_info[[92345]]

high19$loop_length |> hist()
add(low19$loop_length |> hist())
high19$rel_weighted_length |> hist()
low19$rel_weighted_length |> hist()

qgraph(all_networks[[52517]])
qgraph(all_networks[[92345]])

all_networks[[4078]]
loop_numbers[35]
loop_info[[42320]]
