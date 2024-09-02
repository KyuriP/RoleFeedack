## =========================================================
## Identifying Example Networks for Illustrative Purposes
##
## This script identifies example networks that are used to illustrate
## the fundamental differences in network structure between high and low 
## symptom level networks.
## =========================================================

a <- comb_avg_res |>
  filter(t == 1600, loop_numbers == 15,  avg <=0.8 | avg >= 8) |>
  select(1:3, 7,9) |>
  group_by(loop_numbers) |>
  arrange(desc(avg)) |>
    print(n=50)

par(mfrow=c(5,10))
as.numeric(a$matr) |> map(~qgraph(all_networks[[.x + 1]], layout = manual_layout, asize = 10))

# loop number == 21
#low "54598"
#high "35751"

# loop number == 15
#high "10419"
#low "45291"

par(mfrow=c(1,1))
# pdf(file = "figure/highnet_loop15.pdf", width=5, height=5, bg = 'transparent', family="Palatino")
qgraph(all_networks[[10419]], layout = manual_layout, edge.color ="coral", esize = 2, fade =F, asize =5, vsize=10)
# dev.off()
# pdf(file = "figure/lownet_loop15.pdf", width=5, height=5, bg = 'transparent', family="Palatino")
qgraph(all_networks[[45291]], layout = manual_layout, edge.color = "darkseagreen",  esize = 2, fade =F, asize =5, vsize=10)
# dev.off()

# check the loop info
loop_info[[35751]]; loop_info[[54598]]
loop_info[[10419]]; loop_info[[45291]]


# dev.off()
# par(mfrow=c(5,10))
# p <- sapply(idx, function(x) qgraph(all_networks[[x]], layout = manual_layout, asize = 10, esize = 10, title = paste(x)))

