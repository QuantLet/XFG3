rm(list = ls(all = TRUE))
graphics.off()

# load data
PL = read.table("XFGPL.dat")
MMPL = read.table("XFGMMPL.dat")

# main computation
m = nrow(PL)
t = 1
Exc = 0
while (t <= m) {
    q = quantile(t(PL[t, ]), p = 0.99, type = 1)  # 99% quantile
    if (q < MMPL[t, 1]) {
        # counts exceeding values of q in MMPL
        Exc = Exc + 1
    }
    t = t + 1
}
Exc = Exc/m
Exc 
