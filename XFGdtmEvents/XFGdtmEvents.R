## Results from running event detection on 10, 30 and 50 topic models respectively
nevents10 = c(2, 0, 1, 14, 0, 1, 0, 0, 0, 1,  rep(0, 40))
nevents30 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
              22, 0, 0, 0, 0, 0, 0, 1,  rep(0, 20))
nevents50 = c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 1, 0, 
              2, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 19, 0, 0, 0, 0, 0, 0, 
              0, 0, 0, 0, 0, 0)

mat           = cbind(nevents10, nevents30, nevents50)
mat           = t(mat)
colnames(mat) = c(1:50)
mat           = mat[, colSums(mat) != 0]

dev.new(width = 8, height = 4)
par(mar = c(2, 3.1, 0.5, 2.1), mfrow = c(1, 1))

barplot(mat, beside = TRUE, col = 1, lwd = 1:2, angle = c(10), 
        density = c(0, 20, 50), xaxt = "n", yaxt = "n", yaxs = "i")

box(lwd = 1.5, bty  = "l", col = "gray22")

axis(1, at = seq(2, 44, by=4) + 0.5,
labels     = colnames(mat),
cex.axis   = 1,
las        = 1,
col.axis   = "gray22",
tick       = FALSE,
line       = -0.5)

axis(side     = 2, 
     tick     = FALSE, 
     cex.lab  = 1, 
     line     = -0.8, 
     col.axis = "gray22",
     las      = 1)

mtext("Number of Events", 
      side = 2,
      line = 1.5, 
      cex  = 1, 
      col  = "gray22",
      las  = 3)
