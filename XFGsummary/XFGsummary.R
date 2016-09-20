# ----------------------------------------------------------------------
#  Book          XFG2        
# ----------------------------------------------------------------------
#  See_also      summarize
# ----------------------------------------------------------------------
#  Macro         XFGsummary
# ----------------------------------------------------------------------
#  Description   Descriptive Statistics
# ----------------------------------------------------------------------
#  Usage         -
# ----------------------------------------------------------------------
#  Input         none
# ----------------------------------------------------------------------
#  Output        
# ----------------------------------------------------------------------
#  Author        Xplore: Germar Knoechlein, 20010106;
#                R: Awdesch Melzer, 20120614
# ----------------------------------------------------------------------

# clear variables and close windows
rm(list=ls(all=TRUE))
graphics.off()

# define summarize function
summarize = function(x){
    Minimum   = apply(x,2,min)
    Maximum   = apply(x,2,max)
    Mean      = apply(x,2,mean)
    Median    = apply(x,2,median)
    Std.Error = apply(x,2,sd)

    table = cbind(Minimum, Maximum, Mean, Median, Std.Error)
    print(table)
}

# load data
INAAA = read.table("XFGINAAA.dat")

# specify columns
INAAA = INAAA[,4:12]

# rename columns
colnames(INAAA) = c("3M", "6M", "1Y", "2Y", "3Y", "4Y", "5Y", "7Y", "10Y")

summarize(INAAA)
