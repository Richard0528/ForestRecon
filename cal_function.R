
##MSE
##RME
##RMAE
# first a line to load the current workspace
sim_section<-sim.density.section
est_section<-est.density.section.bia

Fit_cal_func.fn<-function(ncol=4, nrow=10, sim_section, est_section, nsim=4000, fit_cal="MSE")
{
  #cur_matrix = paste(fit_cal,sim_section, sep="")
  cur_matrix.df = data.frame(matrix(NA, ncol = ncol, nrow = nrow))
  for (i in 1:10)
  {
    sim.tph.all = sim_section[[i]][,5]
    for (j in 1:4)
    {
#      if (sim.density.section[[1]] == sim_section[[1]]){
        if (fit_cal == "MSE"){
          cur_matrix.df[i,j] = sum((est_section[[i]][,j] - sim.tph.all)^2)/nsim
        } else if (fit_cal == "RME"){
          cur_matrix.df[i,j] = sum((est_section[[i]][,j] - sim.tph.all)/sim.tph.all)/nsim
        } else if (fit_cal == "RMAE"){
          cur_matrix.df[i,j] = sum(abs(est_section[[i]][,j] - sim.tph.all)/sim.tph.all)/nsim
        }
      # } else {
      #   if (fit_cal == 'MSE'){
      #     cur_matrix.df[i,j] = sum((est_section[[i]][,j] - mean(sim_section[[i]][,j]))^2)/nsim
      #   } else if (fit_cal == 'RME'){
      #     cur_matrix.df[i,j] = sum((est_section.bia[[i]][,j] - mean(sim_section[[i]][,j]))/sim.tph.all)/nsim
      #   } else if (fit_cal == 'RMAE'){
      #     cur_matrix.df[i,j] = sum(abs(est_section[[i]][,j] - mean(sim_section[[i]][,j]))/sim.tph.all)/nsim
      #   }
      # }
    }
  }
  colnames(cur_matrix.df) = c("section1", "section2", "section3", "section4")
  return(cur_matrix.df)
}
Fit_cal_func.fn(sim_section=sim_section,est_section=est_section,fit_cal="MSE")
