## Big plot function

## run Fit_cal_loop.R first
#############################
## 1 - true_tph
## 2 - BOX_tph
## 3 - BIA_GLO_MSE
## 4 - BIA_GLO_RME
## 5 - BIA_GLO_RMAE

Plot.fn = function(control)
{
  ## round up to a nice number everytime, for huge number
  roundUpNice <- function(x, nice=seq(1,10,0.1)) {
    ## if(length(x) != 1) stop("'x' must be of length 1")
    10^floor(log10(x)) * nice[[which(x < 10^floor(log10(x)) * nice)[[1]]]]
  }
  
  ## for really small number
  roundUpNiceSmall <- function(x, nice=seq(1,10,1)) {
    ## if(length(x) != 1) stop("'x' must be of length 1")
    10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
  }

  if (control == 'true_tph') 
  {
    par(mfrow=c(3,3),las=2, xpd = TRUE, mar = c(5,3,1.5,2),mgp=c(2.25,0.5,0), oma=c(0,0,2,0))
    for(i in 1:9)
    {
      plot(1:10,MSE_Fit_cal.list[i,1:10],axes=FALSE,
           ylim=c(min(MSE_Fit_cal.list[i,1:10],MSE_Fit10_cal.list[i,1:10],MSE_Fit15_cal.list[i,1:10]),
                  max(MSE_Fit_cal.list[i,1:10],MSE_Fit10_cal.list[i,1:10],MSE_Fit15_cal.list[i,1:10])),
           main=paste("true tph=",tph[i]),ylab="",xlab="")
      axis(2)
      axis(1,at=1:10,labels=names(MSE_Fit_cal.list[1:10]))#,las=2)
      points(1:10,MSE_Fit10_cal.list[i,1:10],pch=16,col="blue")
      points(1:10,MSE_Fit15_cal.list[i,1:10],pch=16,col="forestgreen")
      if(i==1)
        legend(x="topleft",legend=c("All",expression(paste(dbh>=1,"0 cm")),expression(paste(dbh>=1,"5 cm"))),pch=c(1,16,16),col=c("black","blue","forestgreen"),cex=0.6)#,title="DBH threshold")      
    }
    mtext("MSE true_tph", las=1, side=3, adj=0.5, col="red", outer=TRUE)
    
    
    par(mfrow=c(3,3),las=2, xpd = TRUE, mar = c(5,3,1.5,2),mgp=c(2.25,0.5,0), oma=c(0,0,2,0))
    for(i in 1:9)
    {  
      plot(1:10,RME_Fit_cal.list[i,1:10],axes=FALSE,
           ylim=c(min(RME_Fit_cal.list[i,1:10],RME_Fit10_cal.list[i,1:10],RME_Fit15_cal.list[i,1:10]),
                  max(RME_Fit_cal.list[i,1:10],RME_Fit10_cal.list[i,1:10],RME_Fit15_cal.list[i,1:10])),
           main=paste("true tph=",tph[i]),ylab="",xlab="")
      axis(2)
      axis(1,at=1:10,labels=names(RME_Fit_cal.list[1:10]))#,las=2)
      points(1:10,RME_Fit10_cal.list[i,1:10],pch=16,col="blue")
      points(1:10,RME_Fit15_cal.list[i,1:10],pch=16,col="forestgreen")
      if(i==1)
        legend(x="topleft",legend=c("All",expression(paste(dbh>=1,"0 cm")),expression(paste(dbh>=1,"5 cm"))),pch=c(1,16,16),col=c("black","blue","forestgreen"),cex=0.6)#,title="DBH threshold")
    }
    mtext("RME true_tph", las=1, side=3, adj=0.5, col="red", outer=TRUE)
    
    par(mfrow=c(3,3),las=2, xpd = TRUE, mar = c(5,3,1.5,2),mgp=c(2.25,0.5,0), oma=c(0,0,2,0))
    for(i in 1:9)
    {  
      plot(1:10,RMAE_Fit_cal.list[i,1:10],axes=FALSE,
           ylim=c(min(RMAE_Fit_cal.list[i,1:10],RMAE_Fit10_cal.list[i,1:10],RMAE_Fit15_cal.list[i,1:10]),
                  max(RMAE_Fit_cal.list[i,1:10],RMAE_Fit10_cal.list[i,1:10],RMAE_Fit15_cal.list[i,1:10])),
           main=paste("true tph=",tph[i]),ylab="",xlab="")
      axis(2)
      axis(1,at=1:10,labels=names(RMAE_Fit_cal.list[1:10]))#,las=2)
      points(1:10,RMAE_Fit10_cal.list[i,1:10],pch=16,col="blue")
      points(1:10,RMAE_Fit15_cal.list[i,1:10],pch=16,col="forestgreen")
      if(i==1)
        legend(x="topleft",legend=c("All",expression(paste(dbh>=1,"0 cm")),expression(paste(dbh>=1,"5 cm"))),pch=c(1,16,16),col=c("black","blue","forestgreen"),cex=0.6)#,title="DBH threshold")
    }
    mtext("RMAE true_tph", las=1, side=3, adj=0.5, col="red", outer=TRUE)
    
  } else if (control == 'BOX_tph') {
      par(mfrow = c(4,3), las=2,xpd = NA, mgp=c(2.25,0.5,0), mar = c(4,3,1.5, 0.5), oma=c(0,0,2,0))
      for (tph_num in 1:10) 
      {
        sep_tph.df = data.frame(matrix(NA, ncol = 10, nrow = 5000))
        for (ii in 1:10) 
        {
          for (jj in 1:5000)
          {
            sep_tph.df[jj,ii] = biaglo.list[[ii]][jj,tph_num]
          }
        }
        boxplot(sep_tph.df, main=paste("true tph=",tph[tph_num]), axes = FALSE)
        axis(2)
        axis(1,at = 1:10,labels=names(biaglo.list[1:10]))
        # lines(x=tph[tph_num], col = "red", lty = 2)
        segments(0,tph[tph_num],10,tph[tph_num], col = "red", lty=2)
      }
      mtext("BOX -tph estimator", las=1, side=3, adj=0.5, col="red", outer=TRUE)
      
      
    } else if (control == 'BIA_GLO_MSE') {
      ## adj =0 for left align, las =1 for label horizantal, mgp for title and label distance, mar for plots area, oma for outer area
      par(mfrow = c(3,2), las = 1, adj=0, mgp = c(1, 0.5, 0), mar = c(1.5, 0, 1.25, 0), oma=c(1.5,3.4,2,1))
      all.cols = c("black","blue","forestgreen","red", "orange", "purple")
      count<- c("all", "10", "15")
      for(i in 1:3)
      {
        minNum <- min(MSE_Fitall_cal.list[[i]][,1:10])
        maxNum <- roundUpNice(max(MSE_Fitall_cal.list[[i]][,1:10]))
        ## for BIA
        plot(seq(50,500,50), MSE_Fitall_cal.list[[i]][,1], axes = FALSE, type = "l",
             ylim=c(minNum,maxNum),
             main = paste("BIA-",count[i]), xlab='', ylab='')
      
        axis(2, at = c(seq(0,maxNum,roundUpNice((maxNum-minNum)/6)),maxNum)) 
          
        if(i==3)
          axis(1,at=seq(50,500,50),labels=seq(50,500,50), cex.axis = 0.7)
        
        ## add more lines
        for(j in 2:4) {
          lines(seq(50,500,50), MSE_Fitall_cal.list[[i]][,j], col=all.cols[j])
          box()
        }
        ## add legend for BIA plots
        if(i==1)
          legend(x="topleft",legend=c("BIASec","BIAqq","BIAq", "BIAt"),pch=c(1,16,16),col=c("black","blue","forestgreen","red"),cex=0.5)
        
        ## for GLO
        plot(seq(50,500,50), MSE_Fitall_cal.list[[i]][,5], axes = FALSE, type = "l",
             ylim=c(minNum,maxNum),
             main = paste("GLO-",count[i]), xlab='', ylab='')
        if(i==3)
          axis(1,at=seq(50,500,50),labels=seq(50,500,50), cex.axis = 0.7)
        
        ## add more lines
        for(jj in 6:10) {
          lines(seq(50,500,50), MSE_Fitall_cal.list[[i]][,jj], col=all.cols[jj-4])
          box()
        }
        ## add legends for GLO plots
        if(i==1)
          legend(x="topleft",legend=names(MSE_Fit_cal.list[5:10]),pch=c(1,16,16),col=c("black","blue","forestgreen","red","orange", "purple"),cex=0.4)
      }
      mtext("BIA&GLO -MSE for Different tree diameters", las=1, side=3, adj=0.5, col="red", outer=TRUE)
      mtext("True Tph", las = 1, side = 1, adj = 0.5, cex = 0.7, outer = TRUE)
      
      
      
    } else if (control == 'BIA_GLO_RME') {
      par(mfrow = c(3,2), las = 1, adj=0, mgp = c(2.5, 0.5, 0), mar =c(1.5, 0, 1.25, 0), oma=c(1.5,3,2,1))
      all.cols = c("black","blue","forestgreen","red", "orange", "purple")
      count<- c("all", "10", "15")
      for(i in 1:3)
      {
        minNum <- round(min(RME_Fitall_cal.list[[i]][,1:10]),2)
        maxNum <- round(max(RME_Fitall_cal.list[[i]][,1:10]),2)
        ## for BIA
        plot(seq(50,500,50), RME_Fitall_cal.list[[i]][,1], axes = FALSE, type = "l",
             ylim=c(minNum,maxNum),
             main = paste("BIA-",count[i]), xlab='', ylab='')
        
        axis(2, at = c(seq(minNum,maxNum,roundUpNiceSmall((abs(maxNum-minNum))/6)),maxNum))
        
        # axis(2)
        if(i==3)
          axis(1,at=seq(50,500,50),labels=seq(50,500,50), cex.axis = 0.7)
        
        for (j in 2:4) {
          lines(seq(50,500,50), RME_Fitall_cal.list[[i]][,j], col=all.cols[j])
          box()
        }
        
        if(i==1)
          legend(x="topleft",legend=c("BIASec","BIAqq","BIAq", "BIAt"),pch=c(1,16,16),col=all.cols[1:4],cex=0.5)
        
        ## for GLO
        plot(seq(50,500,50), RME_Fitall_cal.list[[i]][,5], axes = FALSE, type = "l",
             ylim=c(minNum,maxNum),
             main = paste("GLO-",count[i]), xlab='', ylab='')
        if(i==3)
          axis(1,at=seq(50,500,50),labels=seq(50,500,50), cex.axis = 0.7)
        
        for(jj in 6:10){
          lines(seq(50,500,50), RME_Fitall_cal.list[[i]][,jj], col=all.cols[jj-4])
          box()
        }
        
        if(i==1)
          legend(x="topleft",legend=names(MSE_Fit_cal.list[5:10]),pch=c(1,16,16),col=all.cols,cex=0.4)
      }
      mtext("BIA&GLO -RME for Different tree diameters", las=1, side=3, adj=0.5, col="red", outer=TRUE)
      mtext("True Tph", las = 1, side = 1, adj = 0.5, cex = 0.7, outer = TRUE)
      box()
      
      
    } else if (control == 'BIA_GLO_RMAE') {
      par(mfrow = c(3,2), las = 1, adj=0, mgp = c(1, 0.5, 0), mar = c(2, 0, 3, 0), oma=c(1.5,2.5,2,1))
      all.cols = c("black","blue","forestgreen","red", "orange", "purple")
      count<- c("all", "10", "15")
      for(i in 1:3)
      {
        minNum <- min(RMAE_Fitall_cal.list[[i]][,1:10])
        maxNum <- roundUpNiceSmall(max(RMAE_Fitall_cal.list[[i]][,1:10]))
        ## for BIA
        plot(seq(50,500,50), RMAE_Fitall_cal.list[[i]][,1], axes = FALSE, type = "l",
             ylim=c(minNum,maxNum),
             main = paste("BIA-",count[i]), xlab='', ylab='')
        
        axis(2, at = c(seq(0,maxNum,roundUpNiceSmall((maxNum-0)/6)), maxNum))
    
        if(i==3)
          axis(1,at=seq(50,500,50),labels=seq(50,500,50), cex.axis = 0.7)
        
        for(j in 2:4) {
          lines(seq(50,500,50), RMAE_Fitall_cal.list[[i]][,j], col=all.cols[j])
          box()
        }
       
        if(i==1)
          legend(x="topleft",legend=c("BIASec","BIAqq","BIAq", "BIAt"),pch=c(1,16,16),col=all.cols[1:4],cex=0.5)
        
        ## for GLO
        plot(seq(50,500,50), MSE_Fitall_cal.list[[i]][,5], axes = FALSE, type = "l",
             ylim=c(minNum,maxNum),
             main = paste("GLO-",count[i]), xlab='', ylab='')
        if(i==3)
          axis(1,at=seq(50,500,50),labels=seq(50,500,50), cex.axis = 0.7)
        
        for(jj in 6:10) {
          lines(seq(50,500,50), RMAE_Fitall_cal.list[[i]][,jj], col=all.cols[jj-4])
          box()
        }
    
        if(i==1)
          legend(x="topleft",legend=names(MSE_Fit_cal.list[5:10]),pch=c(1,16,16),col=all.cols,cex=0.4)
      }
      mtext("BIA&GLO -RMAE for different tree diameters", las=1, side=3, adj=0.5, col="red", outer=TRUE)
      mtext("True Tph", las = 1, side = 1, adj = 0.5, cex = 0.7, outer = TRUE)
      box()
      
      
    }
  
  
}

