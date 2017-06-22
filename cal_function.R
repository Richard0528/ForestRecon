
##MSE
##RME
##RMAE
# first a line to load the current workspace
#shape.vals<-seq(1,3,.25)
#scale.vals<-seq(20,50,10)
filepath<-"../../ForestReconstructionSim/SimResults/WithPCQDiam/Clumped/"
load(paste(filepath,"ClumpedShape1Scale20.RData",sep = ""))
#load(paste(filepath,"ClumpedShape",shape.vals[ii],"Scale",scale.vals[jj],".RData",sep=""))
## Modify to use the biaglo list, replacing est_section
Fit_cal_func.fn<-function(col=12, row=10, sim_section=sim.density.section, 
                          est_section=est.density.section.bia, nsim=5000, fit_cal="MSE",bySection=TRUE)
{
  cur_matrix.df = data.frame(matrix(NA, ncol = col, nrow = row))
  for (i in 1:row) # for each tph
  {
    sim.tph.all = sim_section[[i]][,5] #mean sim tph across sections
    # if(bySection)
    #   {
         for (j in 1:col) # by estimator
         {
             if (fit_cal == "MSE"){
               cur_matrix.df[i,j] = sum((est_section[[j]][,i] - sim.tph.all)^2)/nsim
             } else if (fit_cal == "RME"){
               cur_matrix.df[i,j] = sum((est_section[[j]][,i] - sim.tph.all)/sim.tph.all)/nsim
             } else if (fit_cal == "RMAE"){
               cur_matrix.df[i,j] = sum(abs(est_section[[j]][,i] - sim.tph.all)/sim.tph.all)/nsim
             }
         }
     colnames(cur_matrix.df) = names(est_section)
    # }
    # else
    # {
    
    #    if (fit_cal == "MSE"){
    #     cur_matrix.df[i,col] = sum((est_section[,i] - sim.tph.all)^2)/nsim
    #   } else if (fit_cal == "RME"){
    #     cur_matrix.df[i,col] = sum((est_section[,i] - sim.tph.all)/sim.tph.all)/nsim
    #   } else if (fit_cal == "RMAE"){
    #     cur_matrix.df[i,col] = sum(abs(est_section[,i] - sim.tph.all)/sim.tph.all)/nsim
    #   }
    # #colnames(cur_matrix.df) = c(paste(fit_cal,"_",sim_section,sep=""))
    # }
  }
  return(cur_matrix.df)
}
