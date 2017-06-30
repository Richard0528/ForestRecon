##CSR
##Use Fit_cal_function to loop throught everything
## first run loop_the_whole_list to get biaglo.list

#shape.vals<-seq(1,3,1)
#scale.vals<-seq(20,50,20)
shape.vals<-seq(1,3,.25)
scale.vals<-seq(20,50,10)
filepath<-"~/Documents/ForestReconstructionSim/SimResults/WithPCQDiam/CSR/"
filepath<-"../Results/WithPCQDiam/CSR/"
source("../cal_function.R")
source("../ForestReconGitRepo/ForestRecon/BIA_GLO_func.R")
for (ii in 1:length(shape.vals))
{
  for (jj in 1:length(scale.vals))
  {
    load(paste(filepath,"CSRShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    
    ##CSR_biaglo.list for 1000
    CSR_biaglo1.list<-biaglo.fn(n.density=length(tph))
    
    #sim.density.section
    CSR_Combined1_sim.list <- sim.density.section
    
    #sim.density.section10
    CSR_Combined1_sim10.list <- sim.density.section10
    
    #sim.density.section15
    CSR_Combined1_sim15.list <- sim.density.section15
    
    load(paste(filepath,"CSR4000/CSRShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    
    ##CSR_biaglo.list for 4000
    CSR_biaglo2.list<-biaglo.fn(n.density=length(tph))
    
    #sim.density.section
    CSR_Combined2_sim.list <- sim.density.section
    #merge
    CSR_Combined_sim.list <- lapply(c(1:10),function(x) rbind(CSR_Combined1_sim.list[[x]],CSR_Combined2_sim.list[[x]]))
    names(CSR_Combined_sim.list)<-names(CSR_Combined1_sim.list)
    
    ##merge for biaglo.list
    CSR_biaglo.list<-lapply(names(biaglo1.list),function(x) rbind(CSR_biaglo1.list[[x]],CSR_biaglo2.list[[x]]))
    names(CSR_biaglo.list)<-names(CSR_biaglo1.list)
    
    #sim.density.section10
    CSR_Combined2_sim10.list <- sim.density.section10
    #merge
    CSR_Combined_sim10.list <- lapply(c(1:10),function(x) rbind(CSR_Combined1_sim10.list[[x]],CSR_Combined2_sim10.list[[x]]))
    names(CSR_Combined_sim10.list)<-names(CSR_Combined1_sim10.list)
    
    #sim.density.section15
    CSR_Combined2_sim15.list <- sim.density.section15
    #merge
    CSR_Combined_sim15.list <- lapply(c(1:10),function(x) rbind(CSR_Combined1_sim15.list[[x]],CSR_Combined2_sim15.list[[x]]))
    names(CSR_Combined_sim15.list)<-names(CSR_Combined1_sim15.list)
    
    #MSE
    #    MSE_Fit_cal.list<-Fit_cal_func.fn(sim_section = Combined_sim.list,est_section=)
    #    MSE_Fit_cal.list<-Fit_cal_func.fn(sim_section = Combined_sim.list,est_section=biaglo.list[[1]],bySection = FALSE)
    # call this .df, modify all of the other calls
    CSR_MSE_Fit_cal.list<-Fit_cal_func.fn(sim_section = CSR_Combined_sim.list,est_section=CSR_biaglo.list)#,bySection = FALSE)
    #save(MSE_Fit_cal.list,file=paste("MSE_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    CSR_MSE_Fit10_cal.list<-Fit_cal_func.fn(sim_section = CSR_Combined_sim10.list,est_section=CSR_biaglo.list)
    #save(MSE_Fit10_cal.list,file=paste("MSE10_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    CSR_MSE_Fit15_cal.list<-Fit_cal_func.fn(sim_section = CSR_Combined_sim15.list,est_section=CSR_biaglo.list)
    #save(MSE_Fit15_cal.list,file=paste("MSE15_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    
    #RME
    CSR_RME_Fit_cal.list<-Fit_cal_func.fn(sim_section = CSR_Combined_sim.list, est_section=CSR_biaglo.list, fit_cal = "RME")
    # save(RME_Fit_cal.list,file=paste("RME_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    CSR_RME_Fit10_cal.list<-Fit_cal_func.fn(sim_section = CSR_Combined_sim10.list, est_section=CSR_biaglo.list, fit_cal = "RME")
    #  save(RME_Fit10_cal.list,file=paste("RME10_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    CSR_RME_Fit15_cal.list<-Fit_cal_func.fn(sim_section = CSR_Combined_sim15.list, est_section=CSR_biaglo.list, fit_cal = "RME")
    # save(RME_Fit15_cal.list,file=paste("RME15_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    
    #RMAE
    CSR_RMAE_Fit_cal.list<-Fit_cal_func.fn(sim_section = CSR_Combined_sim.list,est_section=CSR_biaglo.list, fit_cal = "RMAE")
    #save(RMAE_Fit_cal.list,file=paste("RMAE_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    CSR_RMAE_Fit10_cal.list<-Fit_cal_func.fn(sim_section = CSR_Combined_sim10.list,est_section=CSR_biaglo.list, fit_cal = "RMAE")
    #save(RMAE_Fit10_cal.list,file=paste("RMAE10_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    CSR_RMAE_Fit15_cal.list<-Fit_cal_func.fn(sim_section = CSR_Combined_sim15.list,est_section=CSR_biaglo.list, fit_cal = "RMAE")
    #save(RMAE_Fit15_cal.list,file=paste("RMAE15_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    
    # save when you want to
    # save(MSE_Fit_cal.list, MSE_Fit10_cal.list, MSE_Fit15_cal.list, RME_Fit_cal.list, RME_Fit10_cal.list, RME_Fit15_cal.list,
    #      RMAE_Fit_cal.list, RMAE_Fit10_cal.list, RMAE_Fit15_cal.list, file=paste("AllFitStats_CombinedShape",shape.vals[ii],
    #                                                                               "Scale",scale.vals[jj],".RData",sep=""))
    # 
    
    ##You need to operate Loop_the_whole_list.R first to get biaglo.list
    # for (index in 2:12)
    # {
    #   BIAGLO_MSE_Fit_cal.list <- Fit_cal_func.fn(col = 1, est_section = Combined_sim.list, biaglo.list[[index]], bySection=False)
    #   save(BIAGLO_MSE_Fit_cal.list,file=paste(names(biaglo.list)[index],"_MSE_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    #   BIAGLO_MSE_Fit10_cal.list <- Fit_cal_func.fn(col = 1, sim_section = Combined_sim10.lis, est_section = biaglo.list[[index]], bySection=False)
    #   save(BIAGLO_MSE_Fit10_cal.list,file=paste(names(biaglo.list)[index],"_MSE10_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    #   BIAGLO_MSE_Fit15_cal.list <- Fit_cal_func.fn(col = 1, sim_section = Combined_sim15.lis, est_section = biaglo.list[[index]], bySection=False)
    #   save(BIAGLO_MSE_Fit15_cal.list,file=paste(names(biaglo.list)[index],"_MSE15_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    #   
    #   BIAGLO_RME_Fit_cal.list <- Fit_cal_func.fn(col = 1, est_section = biaglo.list[[index]], fit_cal = "RME", bySection=False)
    #   save(BIAGLO_RME_Fit_cal.list,file=paste(names(biaglo.list)[index],"_RME_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    #   BIAGLO_RME_Fit10_cal.list <- Fit_cal_func.fn(col = 1, sim_section = Combined_sim10.lis,est_section = biaglo.list[[index]], fit_cal = "RME", bySection=False)
    #   save(BIAGLO_RME_Fit10_cal.list,file=paste(names(biaglo.list)[index],"_RME10_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    #   BIAGLO_RME_Fit15_cal.list <- Fit_cal_func.fn(col = 1, sim_section = Combined_sim15.lis,est_section = biaglo.list[[index]], fit_cal = "RME", bySection=False)
    #   save(BIAGLO_RME_Fit15_cal.list,file=paste(names(biaglo.list)[index],"_RME15_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    #   
    #   BIAGLO_RMAE_Fit_cal.list <- Fit_cal_func.fn(col = 1, est_section = biaglo.list[[index]], fit_cal = "RMAE", bySection=False)
    #   save(BIAGLO_RMAE_Fit_cal.list,file=paste(names(biaglo.list)[index],"_RMAE_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    #   BIAGLO_RMAE_Fit10_cal.list <- Fit_cal_func.fn(col = 1, sim_section = Combined_sim10.lis,est_section = biaglo.list[[index]], fit_cal = "RMAE", bySection=False)
    #   save(BIAGLO_RAME_Fit10_cal.list,file=paste(names(biaglo.list)[index],"_RMAE10_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    #   BIAGLO_RMAE_Fit15_cal.list <- Fit_cal_func.fn(col = 1, sim_section = Combined_sim15.lis,est_section = biaglo.list[[index]], fit_cal = "RMAE", bySection=False)
    #   save(BIAGLO_RAME_Fit15_cal.list,file=paste(names(biaglo.list)[index],"_RMAE15_CombinedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
    # }
  }
}
