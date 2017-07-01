##plot function

## boxplot function

# boxplot.fn = function(cur.est = biaglo.list[[j]],ymax=max(tph))
# {
#     par(las=1,mgp=c(2.25,0.5,0))
#     boxplot(data.frame(cur.est),ylim=c(0,ymax),xlab="True tph",ylab="Estimated tph",names=tph,main=names(cur.est))
#     points(1:10,seq(50,500,50),pch=16,col="blue",cex=2)
#     legend(x="topleft",legend="True tph",pch=16,col="blue")
# }

##BIA, vary by tree diameter
BIAplot.fn = function(cal_method)
{
  par(mfrow=c(4,3), las = 1, mgp = c(2.75, 0.5, 0))
  if (cal_method == 'MSE') {
    for(i in 1:4)
    {
      plot(seq(50,500,50), MSE_Fit_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i])
      plot(seq(50,500,50), MSE_Fit10_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i])
      plot(seq(50,500,50), MSE_Fit15_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i])
    }
  } else if (cal_method == 'RME'){
    for(i in 1:4)
    {
      plot(seq(50,500,50), RME_Fit_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i])
      plot(seq(50,500,50), RME_Fit10_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i])
      plot(seq(50,500,50), RME_Fit15_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i])
    }
  } else if (cal_method == 'RMAE'){ 
    for(i in 1:4)
    {
      plot(seq(50,500,50), RMAE_Fit_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i])
      plot(seq(50,500,50), RMAE_Fit10_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i])
      plot(seq(50,500,50), RMAE_Fit15_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i])
    }
  }
}

#GLO
GLOplot.fn = function(cal_method)
{
  par(mfrow=c(8,3), las = 1, mgp = c(2.75, 0.5, 0), par(mar = c(1.5, 3, 1, 0.75)))
  if (cal_method == 'MSE') {
    for(i in 5:12)
    {
      plot(seq(50,500,50), MSE_Fit_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i])
      plot(seq(50,500,50), MSE_Fit10_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i])
      plot(seq(50,500,50), MSE_Fit15_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i])
    }
  } else if (cal_method == 'RME'){
    for(i in 5:12)
    {
      plot(seq(50,500,50), RME_Fit_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i])
      plot(seq(50,500,50), RME_Fit10_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i])
      plot(seq(50,500,50), RME_Fit15_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i])
    }
  } else if (cal_method == 'RMAE'){ 
    for(i in 5:12)
    {
      plot(seq(50,500,50), RMAE_Fit_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i])
      plot(seq(50,500,50), RMAE_Fit10_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i])
      plot(seq(50,500,50), RMAE_Fit15_cal.list[,i], type = "l", main = names(MSE_Fit_cal.list)[i])
    }
  }
}
  
# 
# > plot(seq(50,500,50),RMAE_Fit15_cal.list[,1],type="l",ylim=c(0,max(RMAE_Fit15_cal.list)))
# > for(i in 2:ncol(MSE_Fit_cal.list))
#   + lines(seq(50,500,50),RMAE_Fit15_cal.list[,i],lty=i)

# > plot(1:ncol(MSE_Fit_cal.list),MSE_Fit_cal.list[1,])
# > plot(1:ncol(MSE_Fit_cal.list),MSE_Fit_cal.list[2,])
# > plot(1:ncol(MSE_Fit_cal.list),MSE_Fit_cal.list[3,])
# > plot(1:ncol(MSE_Fit_cal.list),MSE_Fit_cal.list[4,])
# > plot(1:ncol(MSE_Fit_cal.list),RMAE_Fit_cal.list[4,])

##mfrow, las for y-lab rotation
##plot(seq(50,500,50), MSE_fit15_cal.list[,1], type= "l")
##line(seq(50,500,50), MSE_fit15_cal.list[,i], lty = i)
# for(i in 2:ncol(MSE_fit15_cal.list))
# {
#   lines(seq(50,500,50), MSE_fit15_cal.list[,i], lty = i)
# }
# 
# for(i in 1:9)
# {
#   plot(seq(50,500,50), MSE_Fit15_cal.list[,i], type = "l", main = names(MSE_Fit15_cal.list)[i])
# }
# 
# plot(1:ncol(MSE_fit_cal.list), MSE_fit_cal.list[4,])
#?par
