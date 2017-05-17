####
# first try at code to calculate fit statistics
# MSE= sum((estimated-true)^2)/nsim; "Mean squared error"
# RME= sum((estimated-true)^2/true)/nsim "relative mean error"
# RMAE= sum(abs(estimated-true)/true)/nsim

cur.results[[scale.id]][[shape.id]]<-list(                                          sim1.df=as.data.frame(matrix(NA,ncol=length(stem.intensity.section),nrow=nsim)),
                                          sim15.df=as.data.frame(matrix(NA,ncol=length(stem.intensity.section),nrow=nsim)),
                                          sim10.df=as.data.frame(matrix(NA,ncol=length(stem.intensity.section),nrow=nsim)),
                                          mseDiamAll=data.frame(biaAll=rep(NA,length(stem.intensity.section)),biaQQ=NA,biaQ=NA,biaT=NA,glo1=NA,glo2=NA,glo3=NA,gloBaker1=NA,gloBaker2=NA,gloBaker3=NA,PCQCorr=NA,PCQUncorr=NA), # bia, bia15, glo
                                          mseDiam10=data.frame(biaAll=rep(NA,length(stem.intensity.section)),biaQQ=NA,biaQ=NA,biaT=NA,glo1=NA,glo2=NA,glo3=NA,gloBaker1=NA,gloBaker2=NA,gloBaker3=NA,PCQCorr=NA,PCQUncorr=NA), # bia, bia15, glo
                                          mseDiam15=data.frame(biaAll=rep(NA,length(stem.intensity.section)),biaQQ=NA,biaQ=NA,biaT=NA,glo1=NA,glo2=NA,glo3=NA,gloBaker1=NA,gloBaker2=NA,gloBaker3=NA,PCQCorr=NA,PCQUncorr=NA), # bia, bia15, glo
                                          RMAEDiamAll=data.frame(biaAll=rep(NA,length(stem.intensity.section)),biaQQ=NA,biaQ=NA,biaT=NA,glo1=NA,glo2=NA,glo3=NA,gloBaker1=NA,gloBaker2=NA,gloBaker3=NA,PCQCorr=NA,PCQUncorr=NA), # bia, bia15, glo
                                          RMEDiamAll=data.frame(biaAll=rep(NA,length(stem.intensity.section)),biaQQ=NA,biaQ=NA,biaT=NA,glo1=NA,glo2=NA,glo3=NA,gloBaker1=NA,gloBaker2=NA,gloBaker3=NA,PCQCorr=NA,PCQUncorr=NA), # bia, bia15, glo
                                          RMAEDiam10=data.frame(biaAll=rep(NA,length(stem.intensity.section)),biaQQ=NA,biaQ=NA,biaT=NA,glo1=NA,glo2=NA,glo3=NA,gloBaker1=NA,gloBaker2=NA,gloBaker3=NA,PCQCorr=NA,PCQUncorr=NA), # bia, bia15, glo
                                          RMEDiam10=data.frame(biaAll=rep(NA,length(stem.intensity.section)),biaQQ=NA,biaQ=NA,biaT=NA,glo1=NA,glo2=NA,glo3=NA,gloBaker1=NA,gloBaker2=NA,gloBaker3=NA,PCQCorr=NA,PCQUncorr=NA), # bia, bia15, glo
                                          RMAEDiam15=data.frame(biaAll=rep(NA,length(stem.intensity.section)),biaQQ=NA,biaQ=NA,biaT=NA,glo1=NA,glo2=NA,glo3=NA,gloBaker1=NA,gloBaker2=NA,gloBaker3=NA,PCQCorr=NA,PCQUncorr=NA), # bia, bia15, glo
                                          RMEDiam15=data.frame(biaAll=rep(NA,length(stem.intensity.section)),biaQQ=NA,biaQ=NA,biaT=NA,glo1=NA,glo2=NA,glo3=NA,gloBaker1=NA,gloBaker2=NA,gloBaker3=NA,PCQCorr=NA,PCQUncorr=NA), # bia, bia15, glo
                                          mean15=rep(NA,length(tph)),mean10=rep(NA,length(tph)))

# cur results was the new list used to contain the processed values
cur.results[[scale.id]][[shape.id]]$sim1.df[,tph.id]<-sim.density.section[[tph.id]][,which.section]
cur.results[[scale.id]][[shape.id]]$sim15.df[,tph.id]<-sim.density.section15[[tph.id]][,which.section]
cur.results[[scale.id]][[shape.id]]$sim10.df[,tph.id]<-sim.density.section10[[tph.id]][,which.section]
cur.results[[scale.id]][[shape.id]]$mean15[tph.id]<-mean(cur.results[[scale.id]][[shape.id]]$sim15.df[,tph.id])
cur.results[[scale.id]][[shape.id]]$mean10[tph.id]<-mean(cur.results[[scale.id]][[shape.id]]$sim10.df[,tph.id])


######## Now performance measures
sim.tph.all<-rowSums(sim.density.section[[tph.id]])/4
#### First BIA, at different transect aggregations  
### MSE by section
cur.results[[scale.id]][[shape.id]]$mseDiamAll$biaAll[tph.id]<-sum((est.density.section.bia[[tph.id]][,which.section]-sim.tph.all)^2)/nsim
cur.results[[scale.id]][[shape.id]]$mseDiam15$biaAll[tph.id]<-sum((est.density.section.bia[[tph.id]][,which.section]-cur.results[[scale.id]][[shape.id]]$mean15[tph.id])^2)/nsim
cur.results[[scale.id]][[shape.id]]$mseDiam10$biaAll[tph.id]<-sum((est.density.section.bia[[tph.id]][,which.section]-cur.results[[scale.id]][[shape.id]]$mean10[tph.id])^2)/nsim
### RME by section
cur.results[[scale.id]][[shape.id]]$RMEDiamAll$biaAll[tph.id]<-sum((est.density.section.bia[[tph.id]][,which.section]-sim.tph.all)/sim.tph.all)/nsim
cur.results[[scale.id]][[shape.id]]$RMEDiam15$biaAll[tph.id]<-sum((est.density.section.bia[[tph.id]][,which.section]-cur.results[[scale.id]][[shape.id]]$mean15[tph.id])/sim.tph.all)/nsim
cur.results[[scale.id]][[shape.id]]$RMEDiam10$biaAll[tph.id]<-sum((est.density.section.bia[[tph.id]][,which.section]-cur.results[[scale.id]][[shape.id]]$mean10[tph.id])/sim.tph.all)/nsim
### RMAE by section    
cur.results[[scale.id]][[shape.id]]$RMAEDiamAll$biaAll[tph.id]<-sum(abs(est.density.section.bia[[tph.id]][,which.section]-sim.tph.all)/sim.tph.all)/nsim
cur.results[[scale.id]][[shape.id]]$RMAEDiam15$biaAll[tph.id]<-sum(abs(est.density.section.bia[[tph.id]][,which.section]-cur.results[[scale.id]][[shape.id]]$mean15[tph.id])/sim.tph.all)/nsim
cur.results[[scale.id]][[shape.id]]$RMAEDiam10$biaAll[tph.id]<-sum(abs(est.density.section.bia[[tph.id]][,which.section]-cur.results[[scale.id]][[shape.id]]$mean10[tph.id])/sim.tph.all)/nsim

# Stop here for now
###########################

for(z in 2:4) # this looks at qq, q, and transect estimates
{
  ### MSE
  cur.results[[scale.id]][[shape.id]]$mseDiamAll[tph.id,z]<-sum((cur.results[[scale.id]][[shape.id]][[z]][,tph.id]-sim.tph.all)^2)/nsim
  cur.results[[scale.id]][[shape.id]]$mseDiam15[tph.id,z]<-sum((cur.results[[scale.id]][[shape.id]][[z]][,tph.id]-cur.results[[scale.id]][[shape.id]]$mean15[tph.id])^2)/nsim
  cur.results[[scale.id]][[shape.id]]$mseDiam10[tph.id,z]<-sum((cur.results[[scale.id]][[shape.id]][[z]][,tph.id]-cur.results[[scale.id]][[shape.id]]$mean10[tph.id])^2)/nsim
  ### RME
  cur.results[[scale.id]][[shape.id]]$RMEDiamAll[tph.id,z]<-sum((cur.results[[scale.id]][[shape.id]][[z]][,tph.id]-sim.tph.all)/sim.tph.all)/nsim
  cur.results[[scale.id]][[shape.id]]$RMEDiam15[tph.id,z]<-sum((cur.results[[scale.id]][[shape.id]][[z]][,tph.id]-cur.results[[scale.id]][[shape.id]]$mean15[tph.id])/sim.tph.all)/nsim
  cur.results[[scale.id]][[shape.id]]$RMEDiam10[tph.id,z]<-sum((cur.results[[scale.id]][[shape.id]][[z]][,tph.id]-cur.results[[scale.id]][[shape.id]]$mean10[tph.id])/sim.tph.all)/nsim
  ### RMAE
  cur.results[[scale.id]][[shape.id]]$RMAEDiamAll[tph.id,z]<-sum(abs(cur.results[[scale.id]][[shape.id]][[z]][,tph.id]-sim.tph.all)/sim.tph.all)/nsim
  cur.results[[scale.id]][[shape.id]]$RMAEDiam15[tph.id,z]<-sum(abs(cur.results[[scale.id]][[shape.id]][[z]][,tph.id]-cur.results[[scale.id]][[shape.id]]$mean15[tph.id])/sim.tph.all)/nsim
  cur.results[[scale.id]][[shape.id]]$RMAEDiam10[tph.id,z]<-sum(abs(cur.results[[scale.id]][[shape.id]][[z]][,tph.id]-cur.results[[scale.id]][[shape.id]]$mean10[tph.id])/sim.tph.all)/nsim
  
}
#### Now glo estimators
for(z in 1:ncol(est.density.glo[[tph.id]]))
{
  ### MSE
  cur.results[[scale.id]][[shape.id]]$mseDiamAll[tph.id,z+4]<-sum((est.density.glo[[tph.id]][,z]-sim.tph.all)^2)/nsim
  cur.results[[scale.id]][[shape.id]]$mseDiam15[tph.id,z+4]<-sum((est.density.glo[[tph.id]][,z]-cur.results[[scale.id]][[shape.id]]$mean15[tph.id])^2)/nsim
  cur.results[[scale.id]][[shape.id]]$mseDiam10[tph.id,z+4]<-sum((est.density.glo[[tph.id]][,z]-cur.results[[scale.id]][[shape.id]]$mean10[tph.id])^2)/nsim
  ### RME
  cur.results[[scale.id]][[shape.id]]$RMEDiamAll[tph.id,z+4]<-sum((est.density.glo[[tph.id]][,z]-sim.tph.all)/sim.tph.all)/nsim
  cur.results[[scale.id]][[shape.id]]$RMEDiam15[tph.id,z+4]<-sum((est.density.glo[[tph.id]][,z]-cur.results[[scale.id]][[shape.id]]$mean15[tph.id])/sim.tph.all)/nsim
  cur.results[[scale.id]][[shape.id]]$RMEDiam10[tph.id,z+4]<-sum((est.density.glo[[tph.id]][,z]-cur.results[[scale.id]][[shape.id]]$mean10[tph.id])/sim.tph.all)/nsim
  ### RMAE
  cur.results[[scale.id]][[shape.id]]$RMAEDiamAll[tph.id,z+4]<-sum(abs(est.density.glo[[tph.id]][,z]-sim.tph.all)/sim.tph.all)/nsim
  cur.results[[scale.id]][[shape.id]]$RMAEDiam15[tph.id,z+4]<-sum(abs(est.density.glo[[tph.id]][,z]-cur.results[[scale.id]][[shape.id]]$mean15[tph.id])/sim.tph.all)/nsim
  cur.results[[scale.id]][[shape.id]]$RMAEDiam10[tph.id,z+4]<-sum(abs(est.density.glo[[tph.id]][,z]-cur.results[[scale.id]][[shape.id]]$mean10[tph.id])/sim.tph.all)/nsim
}
# RME for bias = mean((tph_hat-tph)/tph)
# RMAE for accuracy = mean((|tph_hat-tph|)/tph) # these are relative values, so relative to actual value of tph. Better to compare performances
