options(stringsAsFactors = FALSE)
setwd("")

data.df      = read.csv("term_evolution.csv")
data.df$date = sapply(strsplit(data.df$date, " "), "[", 1)
data.df$date = as.Date(data.df$date, "%Y-%m-%d")

dev.new(width = 8, height = 6)
par(mar = c(2, 4.1, 2, 2.1), mfrow = c(2, 1))

# Gox23
plot(data.df$gox23,
     main = "Topic 23",
     lwd  = 2, 
     col  = "black",
     bty  = 'l',
     xaxt = "n",
     yaxt = "n",
     xlab = "",
     ylab = "",
     type = "l",
     xlim = c(min(data.df$date), max(data.df$date)),
     ylim = c(0, max(data.df$gox23)))

lines(data.df$date, data.df$gox23, lwd = 1.5)

box(lwd = 1.5, bty  = 'l', col = "gray22")

mtext("p(w = mtgox | k = 23)", 
      side = 2,
      line = 2.5, 
      cex  = 1, 
      col  = "gray22",
      las  = 3)

axis(side     = 2, 
     round(data.df$gox23, 2),
     tick     = FALSE, 
     cex.lab  = 1, 
     line     = -0.8, 
     col.axis = "gray22",
     las      = 1)

axis(side     = 1, 
     data.df$date, format(data.df$date, "   %m-%Y    "),
     tick     = FALSE, 
     cex.lab  = 0.1, 
     line     = -0.8, 
     col.axis = "gray22",
     las      = 1)

# Gox38
plot(data.df$gox38,
     main = "Topic 38",
     lwd  = 2, 
     col  = "black",
     bty  = 'l',
     xaxt = "n",
     yaxt = "n",
     xlab = "",
     ylab = "",
     type = "l",
     xlim = c(min(data.df$date), max(data.df$date)),
     ylim = c(0, max(data.df$gox38)))

lines(data.df$date, data.df$gox38, lwd = 1.5)

box(lwd = 1.5, bty  = 'l', col = "gray22")

mtext("p(w = mtgox | k = 38)", 
      side = 2,
      line = 2.5, 
      cex  = 1, 
      col  = "gray22",
      las  = 3)

axis(side     = 2, 
     round(data.df$gox38, 1),
     tick     = FALSE, 
     cex.lab  = 1, 
     line     = -0.8, 
     col.axis = "gray22",
     las      = 1)

axis(side     = 1, 
     data.df$date, format(data.df$date, "   %m-%Y    "),
     tick     = FALSE, 
     cex.lab  = 0.1, 
     line     = -0.8, 
     col.axis = "gray22",
     las      = 1)
