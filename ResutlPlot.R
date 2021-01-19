datas <- function(x) {
  # mean, median, sd
  val_mean <- apply(x,1,mean)
  val_median <- apply(x,1,median)
  val_sd <- apply(x,1,sd)
  
  # make polygon position
  num <- 1:nrow(x)
  poly_x <- c(num,rev(num))
  poly_y <- c(val_mean+val_sd,rev(val_mean-val_sd))
  
  return(list(val_mean, val_median, val_sd, poly_x, poly_y))
}

alg_name = c("dqn", "dqn_softupdate", "ddqn")
path = c("[path]",
         "[path]",
         "[path]")
poly_color = c(rgb(1,0.71,0.77), rgb(0.6,1,0.6), rgb(0.2,0.6,0.8))
line_color = c(rgb(1,0.08,0.58), rgb(0,0.8,0), rgb(0,0.2,1))

plot(0, 0, type = "n", xlim=c(0, 2000), ylim=c(0, 200), xlab = "episode", ylab = "reward")
abline(h=c(0,50,100,150,200),lty="dotted",lwd=0.5)
abline(v=c(0,500,1000,1500,2000),lty="dotted",lwd=0.5)
for (i in 1:length(path)) {
  # import csv
  table = read.csv(path[i], header=TRUE)
  table <- table[, 2:21]
  
  result <- datas(table)
  val_mean <- result[[1]]
  val_median <- result[[2]]
  val_sd <- result[[3]]
  poly_x <- result[[4]]
  poly_y <- result[[5]]
  
  # plot
  # polygon(poly_x,poly.y,col=poly_color[i],lty=0)
  lines(val_mean,lwd=1, col=line_color[i])
}

legend("topleft", legend = alg_name, col = line_color, lty=1)
