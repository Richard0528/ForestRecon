##Use Fit_cal_function to loop throught everything


shape.vals<-seq(1,3,.25)
scale.vals<-seq(20,50,10)
filepath<-"~/Documents/ForestReconstructionSim/SimResults/WithPCQDiam/Clumped/"
source("../cal_function.R")
for (ii in 1:9)
{
  for (jj in 1:4)
  {
    load(paste(filepath,"ClumpedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    #MSE
    MSE_Fit_cal1.list<-Fit_cal_func.fn()
    MSE_Fit10_cal1.list<-Fit_cal_func.fn(sim_section = sim.density.section10)
    MSE_Fit15_cal1.list<-Fit_cal_func.fn(sim_section = sim.density.section15)
    #RME
    RME_Fit_cal1.list<-Fit_cal_func.fn(fit_cal = "RME")
    RME_Fit10_cal1.list<-Fit_cal_func.fn(sim_section = sim.density.section10, fit_cal = "RME")
    RME_Fit15_cal1.list<-Fit_cal_func.fn(sim_section = sim.density.section15, fit_cal = "RME")
    #RMAE
    RMAE_Fit_cal1.list<-Fit_cal_func.fn(fit_cal = "RMAE")
    RMAE_Fit10_cal1.list<-Fit_cal_func.fn(sim_section = sim.density.section10, fit_cal = "RMAE")
    RMAE_Fit15_cal1.list<-Fit_cal_func.fn(sim_section = sim.density.section15, fit_cal = "RMAE")
    
    load(paste(filepath,"Clumped4000/ClumpedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    
    #MSE
    MSE_Fit_cal2.list<-Fit_cal_func.fn()
    MSE_Fit10_cal2.list<-Fit_cal_func.fn(sim_section = sim.density.section10)
    MSE_Fit15_cal2.list<-Fit_cal_func.fn(sim_section = sim.density.section15)
    #RME
    RME_Fit_cal2.list<-Fit_cal_func.fn()
    RME_Fit10_cal2.list<-Fit_cal_func.fn(sim_section = sim.density.section10, fit_cal = "RME")
    RME_Fit15_cal2.list<-Fit_cal_func.fn(sim_section = sim.density.section15, fit_cal = "RME")
    #RMAE
    RMAE_Fit_cal2.list<-Fit_cal_func.fn()
    RMAE_Fit10_cal2.list<-Fit_cal_func.fn(sim_section = sim.density.section10, fit_cal = "RMAE")
    RMAE_Fit15_cal2.list<-Fit_cal_func.fn(sim_section = sim.density.section15, fit_cal = "RMAE")
    
    
    #MSE
    MSE_Fit_cal.list<-lapply(names(MSE_Fit_cal1.list),function(x) rbind(MSE_Fit_cal1.list[[x]],MSE_Fit_cal2.list[[x]]))
    names(MSE_Fit_cal.list)<-names(MSE_Fit_cal1.list)
    save(MSE_Fit_cal.list,file=paste("MSE_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    
    #MSE10
    MSE_Fit10_cal.list<-lapply(names(MSE_Fit10_cal1.list),function(x) rbind(MSE_Fit10_cal1.list[[x]],MSE_Fit10_cal2.list[[x]]))
    names(MSE_Fit10_cal.list)<-names(MSE_Fit10_cal1.list)
    save(MSE_Fit10_cal.list,file=paste("MSE10_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    
    #MSE15
    MSE_Fit15_cal.list<-lapply(names(MSE_Fit15_cal1.list),function(x) rbind(MSE_Fit15_cal1.list[[x]],MSE_Fit15_cal2.list[[x]]))
    names(MSE_Fit15_cal.list)<-names(MSE_Fit15_cal1.list)
    save(MSE_Fit15_cal.list,file=paste("MSE15_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    
    #RME
    RME_Fit_cal.list<-lapply(names(RME_Fit_cal1.list),function(x) rbind(RME_Fit_cal1.list[[x]],RME_Fit_cal2.list[[x]]))
    names(RME_Fit_cal.list)<-names(RME_Fit_cal1.list)
    save(RME_Fit_cal.list,file=paste("RME_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    
    #RME10
    RME_Fit10_cal.list<-lapply(names(RME_Fit10_cal1.list),function(x) rbind(RME_Fit10_cal1.list[[x]],RME_Fit10_cal2.list[[x]]))
    names(RME_Fit10_cal.list)<-names(RME_Fit10_cal1.list)
    save(RME_Fit10_cal.list,file=paste("RME10_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    
    #RME15
    RME_Fit15_cal.list<-lapply(names(RME_Fit15_cal1.list),function(x) rbind(RME_Fit15_cal1.list[[x]],RME_Fit15_cal2.list[[x]]))
    names(RME_Fit15_cal.list)<-names(RME_Fit15_cal1.list)
    save(RME_Fit15_cal.list,file=paste("RME15_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    
    #RMAE
    RMAE_Fit_cal.list<-lapply(names(RMAE_Fit_cal1.list),function(x) rbind(RMAE_Fit_cal1.list[[x]],RMAE_Fit_cal2.list[[x]]))
    names(RMAE_Fit_cal.list)<-names(RMAE_Fit_cal1.list)
    save(RMAE_Fit_cal.list,file=paste("RMAE_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    
    #RMAE10
    RMAE_Fit10_cal.list<-lapply(names(RMAE_Fit10_cal1.list),function(x) rbind(RMAE_Fit10_cal1.list[[x]],RMAE_Fit10_cal2.list[[x]]))
    names(RMAE_Fit10_cal.list)<-names(RMAE_Fit10_cal1.list)
    save(RMAE_Fit10_cal.list,file=paste("RMAE10_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    
    #RMAE15
    RMAE_Fit15_cal.list<-lapply(names(RMAE_Fit15_cal1.list),function(x) rbind(RMAE_Fit15_cal1.list[[x]],RMAE_Fit15_cal2.list[[x]]))
    names(RMAE_Fit15_cal.list)<-names(RMAE_Fit15_cal1.list)
    save(RMAE_Fit15_cal.list,file=paste("RMAE15_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
  }
}
