## =========================================================
## Empirical Analysis
##
## This script generates the bar plot and network visualization 
## based on the results obtained from the PCMCI+ algorithm applied to patient data 
## (corresponding to Figure 9 in the paper).
## Note: The actual analysis code is located in "empirical_analysis_pcmci.py".
## =========================================================



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
## PCMCI+ both directed&undirected
# edge_data <- data.frame(
#   Edge = c("anh o-o ene", "anh o-o sad", "glt o-o sad", "ene o-o glt", "con o-o ene", "sad --> anh","glt --> sad", "ene --> sad",  "sad --> glt", "anh --> sad"),
#   Percentage = c(24, 16, 14, 14, 13, 7, 7, 7, 6, 6) / 147 * 100 #total non-empty network
# )
# # Create the bar plot (for circle-circle edge involved)
# edgefreq <- ggplot(edge_data, aes(x = reorder(Edge, Percentage), y = Percentage)) +
#   geom_bar(stat = "identity", fill = alpha("steelblue4", 0.9), color = "white", width = 0.7) +
#   coord_flip() +
#   labs(
#     title = "Top 10 Edge Frequencies",
#     x = "",
#     y = "Percentage (%)"
#   ) +
#   theme_classic(base_size = 12.5) +
#   scale_x_discrete(labels = rev(c("anh o-o ene", "anh o-o sad", "glt o-o sad", "ene o-o glt", "con o-o ene", expression("sad" %->% "anh"), expression("glt" %->% "sad"), expression("ene" %->% "sad"), expression("sad" %->% "glt"), expression("anh" %->% "sad")))) +
#   theme(
#     legend.position = "none",
#     text = element_text(family = "Palatino"),
#     plot.title = element_text(size = 13, hjust = 0.5, face = "bold"),
#     axis.text.y = element_text(size = 14),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     strip.background = element_blank()
#   )


## PCMCI+ only directed 
edge_data <- data.frame(
  Edge = c("glt -> sad", "ene -> sad", "sad -> anh", "anh -> sad", "sad -> glt",
           "glt -> app", "con -> slp", "ene -> slp", "con -> mot", "sad -> con"),
  Percentage = c(7, 7, 7, 6, 6, 5, 5, 5, 5, 4) / 139 * 100 #total network
)

# Modify edges to create expressions for mathematical notation
edge_data$Edge <- gsub("->", "%->%", edge_data$Edge)  # Replace -> with %->% for parse
edge_data$Edge <- factor(edge_data$Edge, levels = edge_data$Edge[order(edge_data$Percentage)])

## PCMCI directed&undirected aggregated version
# edge_data <- data.frame(
#   Edge = c("anh -> ene", "sad -> con", "glt -> ene", "ene -> sad", "sad -> anh",
#            "anh -> sad", "glt -> sad", "con -> ene", "sad -> ene", "slp -> sad"),
#   Percentage = c(32, 32, 31, 31, 27, 26, 26, 25, 23, 22) / 203 * 100 #total network
# )


edgefreq <- ggplot(edge_data, aes(x = reorder(Edge, Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = alpha("steelblue4", 0.9), color = "white", width = 0.7) +
  coord_flip() +
  labs(
    title = "Top 10 Edge Frequencies",
    x = "",
    y = "Percentage (%)"
  ) +
  theme_classic(base_size = 12.5) +
  scale_x_discrete(labels = function(labels) {
    sapply(labels, function(x) parse(text = x))
  })+
  theme(legend.position = "none",
        text = element_text(family="Palatino"),
        plot.title = element_text(size = 13, hjust = 0.5, face="bold"),
        axis.text.y = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())

# ggsave("figure/data_edgefreq_pcmci+.png", plot = edgefreq, width = 15, height = 10, units = "cm", dpi = 300)

## plot the networks
dat_A <- matrix(c(.30, 0.1, 0, 0, 0, 0, 0, 0, 0,
                   0.1, .30, 0, 0, 0, 0.1, 0.1, 0, 0,
                   0,  0, .30, 0, 0, 0, 0, 0, 0,
                   0, 0.1, 0.1, .30, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, .30, 0, 0, 0, 0,
                   0, 0.1, 0, 0, 0.1, .30, 0, 0, 0,
                   0, 0, 0.1, 0, 0, 0, .30, 0.1, 0,
                   0, 0, 0, 0, 0, 0, 0, .30, 0,
                   0, 0, 0, 0, 0, 0, 0, 0, .30), 9, 9, byrow = T)

rownames(dat_A) <- colnames(dat_A) <- c("anh", "sad", "slp", "ene", "app", "glt", "con", "mot", "sui")

# # original manual layout
# manual_layout <- matrix(c( -1.00000000,-0.3697451,
#                            -0.25362943,-0.4206165,
#                            -0.86442347, 0.4941126,
#                            -0.08080896, 0.3245454,
#                            0.49177041, 0.9000000,
#                            0.43942364,-0.1656119,
#                            0.99799343, 0.2837986,
#                            1.00000000,-0.7135021,
#                            0.41570786,-1.0000000), 9, 2, byrow=T)
# adjusted for datanet
manual_layout <- matrix(c( -1.00000000,-0.3697451,
                           -0.25362943,-0.4206165,
                           -0.86442347, 0.4941126,
                           -0.08080896, 0.2245454,
                           0.49177041, 0.9000000,
                           0.43942364,-0.2656119,
                           0.99799343, 0.2837986,
                           1.00000000,-0.7135021,
                           0.41570786,-1.0000000), 9, 2, byrow=T)


# png(file = "figure/datanet.png", width=2000, height=2000, bg = 'transparent', family="Palatino")

qgraph(dat_A, layout = manual_layout, edge.color = "steelblue4", label.color = "black", asize= 5, fade =F, vsize=10, esize = 3)
title(main="Top 10 Edge Network", cex.main=8, family = "Palatino", line=-0.5)

# dev.off()
