## Clumped data
## run Fit_cal_loop.R first



lineplot.fn<-function(cur.list, cur.main, plot.cols)
{
  all.cols<-c("black","blue","forestgreen","red","orange","purple")
  par(mfrow=c(1,3), las = 1, mgp = c(2.75, 0.5, 0), mar = c(14, 4, 7, 2), oma=c(0,0,7,0))
  count<- c("all", "10", "15")
  for(i in 1:3)
  {
    plot(seq(50,500,50), cur.list[[i]][,plot.cols[1]], axes = FALSE, type = "l",
         ylim=c(min(cur.list[[i]][,plot.cols]),
                max(cur.list[[i]][,plot.cols])),
         main = count[i], xlab='tph', ylab='')
    axis(2,labels= )
    axis(1,at=seq(50,500,50),labels=seq(50,500,50), cex.axis = 0.5)
    for(k in 2:length(plot.cols))
    {
      lines(seq(50,500,50),cur.list[[i]][,plot.cols[k]],col=all.cols[k])
      box();
    }
    if(i==1)
      legend(x="topleft",legend=names(cur.list[[1]])[plot.cols],pch=c(1,16,16),col=all.cols[1:length(plot.cols)],cex=0.6)
  }
  mtext(paste(cur.main,"for Different tree diameters"), las=1, side=3, adj=0.5, col="red", outer=TRUE)
  mtext("True Tph", las = 1, side = 1, adj = 0.5, cex = 0.7, outer = TRUE)
}



## need to modify par() to fit each list
## currently only for MSE
MSElineplot.fn<-function(cur.list, cur.main)
{
  
  ## round up to a nice number everytime, for huge number
  roundUpNice <- function(x, nice=seq(1,10,0.1)) {
    ## if(length(x) != 1) stop("'x' must be of length 1")
    10^floor(log10(x)) * nice[[which(x < 10^floor(log10(x)) * nice)[[1]]]]
  }
  
  
  par(mfrow = c(3,2), las = 1, adj=0, mgp = c(1, 0.5, 0), mar = c(1.5, 0, 1.25, 0), oma=c(1.5,3.4,2,1))
  all.cols = c("black","blue","forestgreen","red", "orange", "purple")
  count<- c("all", "10", "15")
  for(i in 1:3)
  {
    minNum <- min(cur.list[[i]][,1:10])
    maxNum <- roundUpNice(max(cur.list[[i]][,1:10]))
    ## for BIA
    plot(seq(50,500,50), cur.list[[i]][,1], axes = FALSE, type = "l",
         ylim=c(minNum,maxNum),
         main = paste("BIA-",count[i]), xlab='', ylab='')
    
    axis(2, at = c(seq(0,maxNum,roundUpNice((maxNum-minNum)/6)),maxNum)) 
    
    if(i==3)
      axis(1,at=seq(50,500,50),labels=seq(50,500,50), cex.axis = 0.7)
    
    ## add more lines
    for(j in 2:4) {
      lines(seq(50,500,50), cur.list[[i]][,j], col=all.cols[j])
      box()
    }
    ## add legend for BIA plots
    if(i==1)
      legend(x="topleft",legend=c("BIASec","BIAqq","BIAq", "BIAt"),pch=c(1,16,16),col=c("black","blue","forestgreen","red"),cex=0.5)
    
    ## for GLO
    plot(seq(50,500,50), cur.list[[i]][,5], axes = FALSE, type = "l",
         ylim=c(minNum,maxNum),
         main = paste("GLO-",count[i]), xlab='', ylab='')
    if(i==3)
      axis(1,at=seq(50,500,50),labels=seq(50,500,50), cex.axis = 0.7)
    
    ## add more lines
    for(jj in 6:10) {
      lines(seq(50,500,50), cur.list[[i]][,jj], col=all.cols[jj-4])
      box()
    }
    ## add legends for GLO plots
    if(i==1)
      legend(x="topleft",legend=names(MSE_Fit_cal.list[5:10]),pch=c(1,16,16),col=c("black","blue","forestgreen","red","orange", "purple"),cex=0.4)
  }
  mtext("BIA&GLO -MSE for Different tree diameters", las=1, side=3, adj=0.5, col="red", outer=TRUE)
  mtext("True Tph", las = 1, side = 1, adj = 0.5, cex = 0.7, outer = TRUE)
}




## cur.list can be MSE_Fitall_cal.list or RME_Fitall_cal.list or RMAE_Fitall_cal.list
## cur.main, it's basically adding things before true_tph, if you want to specify Clumped or CSR, just type "Clumped MSE" or "CSR MSE"
true_tph.fn<-function(cur.list, cur.main) {
  par(mfrow=c(3,3),las=2, xpd = TRUE, mar = c(5,3,1.5,2),mgp=c(2.25,0.5,0), oma=c(0,0,2,0))
  for(i in 1:9)
  {
    plot(1:10, cur.list[[1]][i,1:10],axes=FALSE,
         ylim=c(min(cur.list[[1]][i,1:10], cur.list[[2]][i,1:10], cur.list[[3]][i,1:10]),
                max(cur.list[[1]][i,1:10], cur.list[[2]][i,1:10], cur.list[[3]][i,1:10])),
         main=paste("true tph=",tph[i]),ylab="",xlab="")
    axis(2)
    axis(1,at=1:10,labels=names(cur.list[[1]][1:10]))#,las=2)
    points(1:10,cur.list[[2]][i,1:10],pch=16,col="blue")
    points(1:10,cur.list[[3]][i,1:10],pch=16,col="forestgreen")
    if(i==1)
      legend(x="topleft",legend=c("All",expression(paste(dbh>=1,"0 cm")),expression(paste(dbh>=1,"5 cm"))),pch=c(1,16,16),col=c("black","blue","forestgreen"),cex=0.6)#,title="DBH threshold")      
  }
  mtext(paste(cur.main,"true_tph"), las=1, side=3, adj=0.5, col="red", outer=TRUE)
  
}





