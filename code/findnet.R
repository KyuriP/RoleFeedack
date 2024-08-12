

# check nloops == 21

a <- comb_avg_res |>
  filter(t == 1600, loop_numbers == 21,  avg <=0.7 | avg >= 8) |>
  select(1:3, 7,9) |>
  group_by(loop_numbers) |>
  arrange(desc(avg)) |>
    print(n=50)

# loop number == 21
#low "54598"
#high "35751"
par(mfrow=c(1,1))
# pdf(file = "figure/highnet_loop21.pdf", width=5, height=5, bg = 'transparent', family="Palatino")
qgraph(all_networks[[35751]], layout = manual_layout, edge.color ="coral", esize = 2, fade =F, asize =5, vsize=10)
# dev.off()
# pdf(file = "figure/lownet_loop21.pdf", width=5, height=5, bg = 'transparent', family="Palatino")
qgraph(all_networks[[54598]], layout = manual_layout, edge.color = "darkseagreen",  esize = 2, fade =F, asize =5, vsize=10)
# dev.off()

# check the loop info
loop_info[[35751]]; loop_info[[54598]]


# dev.off()
# par(mfrow=c(5,10))
# p <- sapply(idx, function(x) qgraph(all_networks[[x]], layout = manual_layout, asize = 10, esize = 10, title = paste(x)))

