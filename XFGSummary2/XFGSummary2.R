# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()


# set working directory to source file location and read the data
# setwd("C:/...")
x = read.table("Returns.txt", header = TRUE)


# define the function
sum.stat = function(X) {

    # Computes the mean, standard deviation, median, quantiles (10% and 90 %),
    # skewness and kurtosis of the input data. If the the input data is a
    # matrix, the calculations are done on the columns. 
    #
    # Args:
    #   x: Input Data

    m    = apply(x,2,mean)  # mean
    sd   = apply(x, 2, sd)  # standard deviation
    med  = apply(x, 2, median)  # median
    q    = apply(x, 2, quantile, probs = c(0.1, 0.9))  # quantiles

    z    = scale(x)  # normalize columns of x
    skew = colSums(z^3)/(dim(x)[1])  # skewness
    kurt = colSums(z^4)/(dim(x)[1])  # kurtosis

    tab            = data.frame(cbind(m, sd, med, t(q), skew, kurt))  # create table with results
    names(tab)     = c("Mean", "SD", "Median", "0.1-q", "0.9-q", "Skewness","Kurtosis")  # name table columns
    row.names(tab) = names(x)  # name table rows

    return(tab)
}

# call function
sum.stat(x)