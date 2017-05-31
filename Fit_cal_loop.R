##Use Fit_cal_function to loop throught everything


shape.vals<-seq(1,3,.25)
scale.vals<-seq(20,50,10)
filepath<-"~/Documents/ForestReconstructionSim/SimResults/WithPCQDiam/Clumped/"
source("../cal_function.R")
source("../ForestReconGitRepo/ForestRecon/BIA_GLO_func.R")
for (ii in 1:9)
{
  for (jj in 1:4)
  {
    load(paste(filepath,"ClumpedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    
    #sim.density.section
    Combined1_sim.list <- sim.density.section
    
    #sim.density.section10
    Combined1_sim10.list <- sim.density.section10
    
    #sim.density.section15
    Combined1_sim15.list <- sim.density.section15
    
    load(paste(filepath,"Clumped4000/ClumpedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    
    #sim.density.section
    Combined2_sim.list <- sim.density.section
    #merge
    Combined_sim.list <- lapply(c(1:10),function(x) rbind(Combined1_sim.list[[x]],Combined2_sim.list[[x]]))
    names(Combined_sim.list)<-names(Combined1_sim.list)
    
    #sim.density.section10
    Combined2_sim10.list <- sim.density.section10
    #merge
    Combined_sim10.list <- lapply(c(1:10),function(x) rbind(Combined1_sim10.list[[x]],Combined2_sim10.list[[x]]))
    names(Combined_sim10.list)<-names(Combined1_sim10.list)
    
    #sim.density.section15
    Combined2_sim15.list <- sim.density.section15
    #merge
    Combined_sim15.list <- lapply(c(1:10),function(x) rbind(Combined1_sim15.list[[x]],Combined2_sim15.list[[x]]))
    names(Combined_sim15.list)<-names(Combined1_sim15.list)
    
    
    #MSE
    MSE_Fit_cal.list<-Fit_cal_func.fn(sim_section = Combined_sim.list)
    save(MSE_Fit_cal.list,file=paste("MSE_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    MSE_Fit10_cal.list<-Fit_cal_func.fn(sim_section = Combined_sim10.list)
    save(MSE_Fit10_cal.list,file=paste("MSE10_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    MSE_Fit15_cal.list<-Fit_cal_func.fn(sim_section = Combined_sim15.list)
    save(MSE_Fit15_cal.list,file=paste("MSE15_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    
    #RME
    RME_Fit_cal.list<-Fit_cal_func.fn(sim_section = Combined_sim.list, fit_cal = "RME")
    save(RME_Fit_cal.list,file=paste("RME_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    RME_Fit10_cal.list<-Fit_cal_func.fn(sim_section = Combined_sim10.list, fit_cal = "RME")
    save(RME_Fit10_cal.list,file=paste("RME10_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    RME_Fit15_cal.list<-Fit_cal_func.fn(sim_section = Combined_sim15.list, fit_cal = "RME")
    save(RME_Fit15_cal.list,file=paste("RME15_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    
    #RMAE
    RMAE_Fit_cal.list<-Fit_cal_func.fn(sim_section = Combined_sim.list, fit_cal = "RMAE")
    save(RMAE_Fit_cal.list,file=paste("RMAE_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    RMAE_Fit10_cal.list<-Fit_cal_func.fn(sim_section = Combined_sim10.list, fit_cal = "RMAE")
    save(RMAE_Fit10_cal.list,file=paste("RMAE10_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    RMAE_Fit15_cal.list<-Fit_cal_func.fn(sim_section = Combined_sim15.list, fit_cal = "RMAE")
    save(RMAE_Fit15_cal.list,file=paste("RMAE15_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    
    ##You need to operate Loop_the_whole_list.R first to get biaglo.list
    for (index in 2:12)
    {
      BIAGLO_MSE_Fit_cal.list <- Fit_cal_func.fn(col = 1, est_section = Combined_sim.list, biaglo.list[[index]], bySection=False)
      save(BIAGLO_MSE_Fit_cal.list,file=paste(names(biaglo.list)[index],"_MSE_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
      BIAGLO_MSE_Fit10_cal.list <- Fit_cal_func.fn(col = 1, sim_section = Combined_sim10.lis, est_section = biaglo.list[[index]], bySection=False)
      save(BIAGLO_MSE_Fit10_cal.list,file=paste(names(biaglo.list)[index],"_MSE10_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
      BIAGLO_MSE_Fit15_cal.list <- Fit_cal_func.fn(col = 1, sim_section = Combined_sim15.lis, est_section = biaglo.list[[index]], bySection=False)
      save(BIAGLO_MSE_Fit15_cal.list,file=paste(names(biaglo.list)[index],"_MSE15_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
      
      BIAGLO_RME_Fit_cal.list <- Fit_cal_func.fn(col = 1, est_section = biaglo.list[[index]], fit_cal = "RME", bySection=False)
      save(BIAGLO_RME_Fit_cal.list,file=paste(names(biaglo.list)[index],"_RME_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
      BIAGLO_RME_Fit10_cal.list <- Fit_cal_func.fn(col = 1, sim_section = Combined_sim10.lis,est_section = biaglo.list[[index]], fit_cal = "RME", bySection=False)
      save(BIAGLO_RME_Fit10_cal.list,file=paste(names(biaglo.list)[index],"_RME10_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
      BIAGLO_RME_Fit15_cal.list <- Fit_cal_func.fn(col = 1, sim_section = Combined_sim15.lis,est_section = biaglo.list[[index]], fit_cal = "RME", bySection=False)
      save(BIAGLO_RME_Fit15_cal.list,file=paste(names(biaglo.list)[index],"_RME15_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
      
      BIAGLO_RMAE_Fit_cal.list <- Fit_cal_func.fn(col = 1, est_section = biaglo.list[[index]], fit_cal = "RMAE", bySection=False)
      save(BIAGLO_RMAE_Fit_cal.list,file=paste(names(biaglo.list)[index],"_RMAE_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
      BIAGLO_RMAE_Fit10_cal.list <- Fit_cal_func.fn(col = 1, sim_section = Combined_sim10.lis,est_section = biaglo.list[[index]], fit_cal = "RMAE", bySection=False)
      save(BIAGLO_RAME_Fit10_cal.list,file=paste(names(biaglo.list)[index],"_RMAE10_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
      BIAGLO_RMAE_Fit15_cal.list <- Fit_cal_func.fn(col = 1, sim_section = Combined_sim15.lis,est_section = biaglo.list[[index]], fit_cal = "RMAE", bySection=False)
      save(BIAGLO_RAME_Fit15_cal.list,file=paste(names(biaglo.list)[index],"_RMAE15_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    }
  }
}