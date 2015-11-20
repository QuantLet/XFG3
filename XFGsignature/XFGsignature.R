ibm <- read.table("ibmSignature.dat",header=F,col.names=c("trades","rv"))

pdf(file="signature.pdf",paper="special",width=6,height=3)


par(mgp=c(1.75,0.75,0))
par(mar=c(3,3,0.7,0.7))


plot(ibm$trades,ibm$rv,type="l",xlab="sampling frequency in ticks",ylab="RV",lwd=1,col="blue",log="x")
dev.off()
