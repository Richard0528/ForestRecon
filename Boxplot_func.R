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

#########
# MK: plot by TPH, compare estimators
dim(MSE_Fit_cal.list)
pdf(file="TestPlot1.pdf")
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
    legend(x="topleft",legend=c("All",expression(paste(dbh>=1,"0 cm")),expression(paste(dbh>=1,"5 cm"))),pch=c(1,16,16),col=c("black","blue","forestgreen"),cex=0.75)#,title="DBH threshold")
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
    legend(x="topleft",legend=c("All",expression(paste(dbh>=1,"0 cm")),expression(paste(dbh>=1,"5 cm"))),pch=c(1,16,16),col=c("black","blue","forestgreen"),cex=0.75)#,title="DBH threshold")
}
dev.off()

