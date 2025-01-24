## install packages
source("code/libraries.R")

# devtools::install_github("kyurip/RCIT", force = TRUE)
library(RCIT)        # Kernel-based conditional independence tests

# load helius data
helius <- read_sav("data/helius.sav")

# clean data
helius_dep <- helius |> dplyr::select(
  # Depression symptoms / sum scores
  H1_WlbvRecent1, H1_WlbvRecent2, H1_WlbvRecent3, H1_WlbvRecent4, H1_WlbvRecent5, H1_WlbvRecent6, H1_WlbvRecent7, H1_WlbvRecent_89, H1_WlbvRecent10, H1_PHQ9_sumscore) |>
  # Remove NAs 
  na.omit() |>
  dplyr::rename(anh = H1_WlbvRecent1, 
                sad = H1_WlbvRecent2, 
                slp = H1_WlbvRecent3, 
                ene = H1_WlbvRecent4, 
                app = H1_WlbvRecent5, 
                glt = H1_WlbvRecent6, 
                con = H1_WlbvRecent7, 
                mot = H1_WlbvRecent_89,
                sui = H1_WlbvRecent10) |>
  # PHQ9 score diagnosis: 0 – 4	None-minimal / 5 – 9	Mild	/ 10 – 14	Moderate	/ 15 – 19	Moderately Severe	/ 20 – 27	Severe
  dplyr::mutate(dep_level = case_when(
    H1_PHQ9_sumscore < 5 ~ "none",
    H1_PHQ9_sumscore < 10 ~ "mild",
    H1_PHQ9_sumscore < 15 ~ "moderate",
    H1_PHQ9_sumscore < 20 ~ "mod_severe",
    H1_PHQ9_sumscore >= 20 ~ "severe"),
    severity_group = ifelse(dep_level %in% c("none"), "no", "yes")
)

## ===================================================
## just exploring: plot and see whether there are diffs...
## ===================================================
helius_dep %>%
  mutate(severity_group = ifelse(dep_level == "severe", "Severe", "Rest")) %>%
  ggplot(aes(x = factor(sad), fill = factor(glt))) +
  geom_bar(position = "dodge") +
  facet_wrap(~severity_group) +
  labs(
    title = "Comparison of Conditional Distribution of 'dep' and 'glt'",
    x = "'dep' (Feeling Down, Depressed, or Hopeless)",
    y = "Count",
    fill = "'glt' (Feeling Bad About Oneself)"
  ) +
  theme_minimal()


helius_dep %>%
  mutate(severity_group = ifelse(dep_level == "severe", "Severe", "Rest")) %>%
  ggplot(aes(x = glt, fill = factor(sad))) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  facet_wrap(~severity_group) +
  labs(
    title = "Histogram of 'glt' by 'dep' and Severity",
    x = "'glt' (Feeling Bad About Oneself)",
    y = "Frequency",
    fill = "'dep' (Feeling Down, Depressed, or Hopeless)"
  ) +
  theme_minimal()

helius_dep %>%
  mutate(severity_group = ifelse(dep_level == "severe", "Severe", "Rest")) %>%
  ggplot(aes(x = sad, fill = severity_group)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~glt) +
  labs(
    title = "Conditional Density of 'dep' Given 'glt'",
    x = 'dep',
    y = "Density",
    fill = "Group"
  ) +
  theme_minimal()

## networks per group
helius_dep |>
  filter(dep_level == "none") |>
  select(1:9) |> cor_auto() |>
  qgraph(layout = "spring")

helius_dep |>
  filter(dep_level == "mild") |>
  select(1:9) |> cor_auto() |>
  qgraph(layout = "spring")

helius_dep |>
  filter(dep_level == "moderate") |>
  select(1:9) |> cor_auto() |>
  qgraph(layout = "spring")

helius_dep |>
  filter(dep_level == "mod_severe") |>
  select(1:9) |> cor_auto() |>
  qgraph(layout = "spring")

helius_dep |>
  filter(dep_level == "severe") |>
  select(1:9) |> cor_auto() |>
  qgraph(layout = "spring")


helius_dep |>
  filter(severity_group == "no") |>
  dplyr::select(1:9) |> cor_auto() |> 
  qgraph(layout = "spring")

helius_dep |>
  filter(severity_group == "yes") |>
  dplyr::select(1:9) |> cor_auto() |>
  qgraph(layout = "spring")


## ===========================
## find the PAG using FCI algorithm
## ===========================
## use GaussCItest
suffStat <- list(C = helius_dep |>
                   filter(severity_group == "yes") |>
                   dplyr::select(1:9) |> cor_auto(), n = nrow(helius_dep))
res <- fci(suffStat, indepTest=gaussCItest,
           alpha = 0.01, labels = colnames(helius_dep[1:9]), selectionBias = FALSE)
res@amat |> plotAG()

## use RCoT
suffStat <- list(data = as.matrix(helius_dep[1:9]), num_f = 100, num_f2 = 10)
res <- fci(suffStat, indepTest=RCoT, alpha = 0.01, p = 9, labels = colnames(helius_dep[1:9]), selectionBias = FALSE, skel.method = "stable.fast", numCores = 5)
res@amat |> plotAG()
#saveRDS(res, "empirical_fci_rcot_res.rds")



## ===========================
## PCMCI result figure
## ===========================

# load schumacher et al. data
lea <- read.csv2("data/data_lea.csv")[,1:12]
  
## ===========================
## result from PCMCI+
## ===========================
# anh o-o ene: 12 occurrences, 8.16%
# ene o-o anh: 12 occurrences, 8.16%
# 
# anh o-o sad: 8 occurrences, 5.44%
# sad o-o anh: 8 occurrences, 5.44%
# 
# con o-o ene: 7 occurrences, 4.76%
# ene o-o con: 6 occurrences, 4.08%
# 
# ene --> sad: 7 occurrences, 4.76%
# sad --> anh: 7 occurrences, 4.76%
# 
# ene o-o glt: 7 occurrences, 4.76%
# glt o-o ene: 7 occurrences, 4.76%
# 
# glt --> sad: 7 occurrences, 4.76%
# 
# glt o-o sad: 7 occurrences, 4.76%
# sad o-o glt: 7 occurrences, 4.76%
# 
# sad --> glt: 6 occurrences, 4.08%
# 
# anh --> sad: 6 occurrences, 4.08%


# Data preparation
edge_data <- data.frame(
  Edge = c("anh o-o ene", "anh o-o sad", "con o-o ene", "ene --> sad", "sad --> anh",
           "ene o-o glt","glt --> sad", "glt o-o sad", "sad --> glt", "anh --> sad"),
  Percentage = c(24, 16, 13, 7, 7, 14, 7, 14, 6, 6) / 147 * 100 #total non-empty network
)

edge_data <- data.frame(
  Edge = c("anh o-o ene", "anh o-o sad", "glt o-o sad", "ene o-o glt", "con o-o ene", "sad --> anh","glt --> sad", "ene --> sad",  "sad --> glt", "anh --> sad"),
  Percentage = c(24, 16, 14, 14, 13, 7, 7, 7, 6, 6) / 147 * 100 #total non-empty network
)

# edge_data <- data.frame(
#   Edge = c("anh -> ene", "sad -> con", "glt -> ene", "ene -> sad", "sad -> anh",
#            "anh -> sad", "glt -> sad", "con -> ene", "sad -> ene", "slp -> sad"),
#   Percentage = c(32, 32, 31, 31, 27, 26, 26, 25, 23, 22) / 203 * 100 #total network
# )

# Create the bar plot
edgefreq <- ggplot(edge_data, aes(x = reorder(Edge, Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = alpha("steelblue4", 0.9), color = "white", width = 0.7) +
  coord_flip() +
  labs(
    title = "Top 10 Edge Frequencies",
    x = "",
    y = "Percentage (%)"
  ) +
  theme_classic(base_size = 12.5) +
  scale_x_discrete(labels = rev(c("anh o-o ene", "anh o-o sad", "glt o-o sad", "ene o-o glt", "con o-o ene", expression("sad" %->% "anh"), expression("glt" %->% "sad"), expression("ene" %->% "sad"), expression("sad" %->% "glt"), expression("anh" %->% "sad")))) +
  theme(
    legend.position = "none",
    text = element_text(family = "Palatino"),
    plot.title = element_text(size = 13, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank()
  )

# ggsave("figure/data_edgefreq_pcmci+.png", plot = edgefreq, width = 15, height = 10, units = "cm", dpi = 300)

## plot the networks
dat_A <- matrix(c(.30, 0.1, 0, 0.1, 0, 0, 0, 0, 0,
                   0.1, .30, 0, 0.1, 0, 0, 0.1, 0, 0,
                   0,  0.1, .30, 0, 0, 0, 0, 0, 0,
                   0, 0.1, 0, .30, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, .30, 0, 0, 0, 0,
                   0, 0.1, 0, 0.1, 0, .30, 0, 0, 0,
                   0, 0, 0, 0.1, 0, 0, .30, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, .30, 0,
                   0, 0, 0, 0, 0, 0, 0, 0, .30), 9, 9, byrow = T)

rownames(dat_A) <- colnames(dat_A) <- c("anh", "sad", "slp", "ene", "app", "glt", "con", "mot", "sui")

manual_layout <- matrix(c( -1.00000000,-0.3697451,
                           -0.25362943,-0.4206165,
                           -0.86442347, 0.4941126,
                           -0.08080896, 0.3245454,
                           0.49177041, 0.9000000,
                           0.55942364,-0.1656119,
                           0.99799343, 0.2837986,
                           1.00000000,-0.7135021,
                           0.41570786,-1.0000000), 9, 2, byrow=T)


png(file = "figure/datanet.png", width=2000, height=2000, bg = 'transparent', family="Palatino")

qgraph(dat_A, layout = manual_layout, edge.color = "steelblue4", label.color = "black", asize= 5, fade =F, vsize=10, esize = 3)
title(main="Top 10 Edge Network", cex.main=8, family = "Palatino", line=-0.5)

dev.off()
