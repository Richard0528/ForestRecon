## Big plot function

Plot.fn = function(control)
{
  if (control == 'true tph') 
  {
    par(mfrow=c(3,3),las=2)
    for(i in 1:9)
    {
      plot(1:12,MSE_Fit_cal.list[i,],axes=FALSE,
           ylim=c(min(MSE_Fit_cal.list[i,],MSE_Fit10_cal.list[i,],MSE_Fit15_cal.list[i,]),
                  max(MSE_Fit_cal.list[i,],MSE_Fit10_cal.list[i,],MSE_Fit15_cal.list[i,])),
           main=paste("true tph=",tph[i]),ylab="MSE",xlab="")
      axis(2)
      axis(1,at=1:12,labels=names(MSE_Fit_cal.list))#,las=2)
      points(1:12,MSE_Fit10_cal.list[i,],pch=16,col="blue")
      points(1:12,MSE_Fit15_cal.list[i,],pch=16,col="forestgreen")
      if(i==1)
        legend(x="topleft",legend=c("All",expression(paste(dbh>=1,"0 cm")),expression(paste(dbh>=1,"5 cm"))),pch=c(1,16,16),col=c("black","blue","forestgreen"),cex=0.6)#,title="DBH threshold")
    }
    
    par(mfrow=c(3,3),las=2)
    for(i in 1:9)
    {
      plot(1:12,RME_Fit_cal.list[i,],axes=FALSE,
           ylim=c(min(RME_Fit_cal.list[i,],RME_Fit10_cal.list[i,],RME_Fit15_cal.list[i,]),max(RME_Fit_cal.list[i,],RME_Fit10_cal.list[i,],RME_Fit15_cal.list[i,])),
           main=paste("true tph=",tph[i]),ylab="RME",xlab="")
      axis(2)
      axis(1,at=1:12,labels=names(RME_Fit_cal.list))#,las=2)
      points(1:12,RME_Fit10_cal.list[i,],pch=16,col="blue")
      points(1:12,RME_Fit15_cal.list[i,],pch=16,col="forestgreen")
      if(i==1)
        legend(x="topleft",legend=c("All",expression(paste(dbh>=1,"0 cm")),expression(paste(dbh>=1,"5 cm"))),pch=c(1,16,16),col=c("black","blue","forestgreen"),cex=0.6)#,title="DBH threshold")
    }
  } else if (control == 'BIA_MSE') {
      for(i in 1:4)
      {
        par(mfrow=c(4,3), las = 1, mgp = c(2.75, 0.5, 0), mar = c(1.5, 3, 1, 0.75))
        plot(seq(50,500,50), MSE_Fit_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i], xlab='', ylab='')
        plot(seq(50,500,50), MSE_Fit10_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i], xlab='', ylab='')
        plot(seq(50,500,50), MSE_Fit15_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i], xlab='', ylab='')
      }
    } else if (control == 'BIA_RME') {
      for(i in 1:4)
      {
        par(mfrow=c(4,3), las = 1, mgp = c(2.75, 0.5, 0), mar = c(1.5, 3, 1, 0.75))
        plot(seq(50,500,50), RME_Fit_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i], xlab='', ylab='')
        plot(seq(50,500,50), RME_Fit10_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i], xlab='', ylab='')
        plot(seq(50,500,50), RME_Fit15_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i], xlab='', ylab='')
      }
    } else if (control == 'BIA_RMAE') {
      for(i in 1:4)
      {
        par(mfrow=c(4,3), las = 1, mgp = c(2.75, 0.5, 0), mar = c(1.5, 3, 1, 0.75))
        plot(seq(50,500,50), RMAE_Fit_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i], xlab='', ylab='')
        plot(seq(50,500,50), RMAE_Fit10_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i], xlab='', ylab='')
        plot(seq(50,500,50), RMAE_Fit15_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i], xlab='', ylab='')
      }
    } else if (control == 'GLO_MSE') {
      for(i in 5:12)
      {
        plot(seq(50,500,50), MSE_Fit_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i], xlab='', ylab='')
        plot(seq(50,500,50), MSE_Fit10_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i], xlab='', ylab='')
        plot(seq(50,500,50), MSE_Fit15_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i], xlab='', ylab='')
      }
    } else if (control == 'GLO_RME') {
      for(i in 5:12)
      {
        plot(seq(50,500,50), RME_Fit_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i], xlab='', ylab='')
        plot(seq(50,500,50), RME_Fit10_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i], xlab='', ylab='')
        plot(seq(50,500,50), RME_Fit15_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i], xlab='', ylab='')
      }
    } else if (control == 'GLO_RMAE') { 
      for(i in 5:12)
      {
        plot(seq(50,500,50), RMAE_Fit_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i], xlab='', ylab='')
        plot(seq(50,500,50), RMAE_Fit10_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i], xlab='', ylab='')
        plot(seq(50,500,50), RMAE_Fit15_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i], xlab='', ylab='')
      }
    } else if (control == 'BOX_tph') {
      par(mfrow = c(4,3), las=2,xpd = NA, mgp=c(2.25,0.5,0), mar = c(4,3,1.5, 0.5))
      for (tph_num in 1:10) {
        sep_tph.df = data.frame(matrix(NA, ncol = 10, nrow = 5000))
        for (ii in 1:10) {
          for (jj in 1:5000)
            sep_tph.df[jj,ii] = biaglo.list[[ii]][jj,tph_num]
        }
        boxplot(sep_tph.df, main=paste("true tph=",tph[tph_num]), axes = FALSE)
        axis(2)
        axis(1,at = 1:10,labels=names(biaglo.list[1:10]))
      }
    }
}

