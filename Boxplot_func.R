## boxplot function


boxplot.fn = function(cur.est = biaglo.list[[j]],ymax=max(tph))
{
    par(las=1,mgp=c(2.25,0.5,0))
    boxplot(data.frame(cur.est),ylim=c(0,ymax),xlab="True tph",ylab="Estimated tph",names=tph,main=names(cur.est))
    points(1:10,seq(50,500,50),pch=16,col="blue",cex=2)
    legend(x="topleft",legend="True tph",pch=16,col="blue")
}
