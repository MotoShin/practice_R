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

moving_average <- function(x, n) {
  filter(x, rep(1,n)) / n
}

epi_num = 2000
sim_num = 20
moving_average_term = 100
alg_name = c("dqn", "dqn_softupdate", "ddqn", "ddqn_dueling_net")
path = c("[path]",
         "[path]",
         "[path]",
         "[path]")
poly_color = c(rgb(1,0.71,0.77), rgb(0.6,1,0.6), rgb(0.2,1,1), rgb(1,0.8,0.4))
line_color = c(rgb(1,0.08,0.58), rgb(0,0.8,0), rgb(0,0.2,1), rgb(1,0.6,0))

val_means <- matrix(, nrow=epi_num, ncol=length(path))
val_medians <- matrix(, nrow=epi_num, ncol=length(path))
val_sds <- matrix(, nrow=epi_num, ncol=length(path))
poly_xs <- matrix(, nrow=epi_num*2, ncol=length(path))
poly_ys <- matrix(, nrow=epi_num*2, ncol=length(path))

for (i in 1:length(path)) {
  # import csv
  table = read.csv(path[i], header=TRUE)
  table <- table[, 2:sim_num+1]
  # calculate
  result <- datas(table)
  val_means[, i] <- unlist(result[[1]])
  val_medians[, i] <- unlist(result[[2]])
  val_sds[, i] <- unlist(result[[3]])
  poly_xs[, i] <- unlist(result[[4]])
  poly_ys[, i] <- unlist(result[[5]])
}

# average plot
png("all.png", width = 800, height = 600)
plot(0, 0, type = "n", xlim=c(0, epi_num), ylim=c(0, max(val_means) + 10), xlab = "episode", ylab = "reward")
abline(h=seq(0, max(val_means) + 10, 50),lty="dotted",lwd=0.5)
abline(v=seq(0, epi_num, 500),lty="dotted",lwd=0.5)
for (i in 1:length(path)) {
  lines(val_means[, i], lwd=1, col = line_color[i])
}
legend("topleft", legend = alg_name, col = line_color, lty=1, bg = "white")
dev.off()

# moving average plot
png("moving_average.png", width = 800, height = 600)
plot(0, 0, type = "n", xlim = c(0, epi_num), ylim=c(0, max(val_means) + 10), xlab = "episode", ylab = "reward")
abline(h=seq(0, max(val_means) + 10, 50),lty="dotted",lwd=0.5)
abline(v=seq(0, epi_num, 500),lty="dotted",lwd=0.5)
for (i in 1:length(path)) {
  lines(moving_average(val_means[, i], moving_average_term), lwd=2, col = line_color[i])
}
legend("topleft", legend = alg_name, col = line_color, lty=1, bg = "white")
dev.off()

# unit plot
for (i in 1:length(path)) {
  png(paste(alg_name[i], ".png", sep = ""), width = 800, height = 600)
  temp = max(val_means[, i] + val_sds[, i])
  plot(0, 0, type = "n", xlim = c(0, epi_num), ylim = c(0, temp), xlab = "episode", ylab = "reward")
  abline(h=seq(0, temp, 50),lty="dotted",lwd=0.5)
  abline(v=seq(0, epi_num, 500),lty="dotted",lwd=0.5)
  polygon(poly_xs[, i], poly_ys[, i], col = poly_color[i], lty=0)
  lines(val_means[, i], lwd=1, col = line_color[i])
  legend("topleft", legend = alg_name[i], col = line_color[i], lty=1, bg = "white")
  dev.off()
}
