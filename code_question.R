



library(tidyverse)
library(gamlss)
library(gamlss.dist)

#Load data
load('DHT.RData')



#Run model
mod_pred<-gamlss(harvest_total ~ ct,
                          data = DHT,
                          family = DPO)



#Function to compute predictions based on model
compute_CI_trad_gamlss<-function(n.sims=200, mod){#,
 
  #DF for simulations
  df_sims<-as.data.frame(DHT)
  
  #Dateframe with new data to predict over
  new.data.ct<<-expand.grid(ct=seq(from=5, to=32, length.out=50))
  
  
  #matrix to store predictions
  preds.sim.trad.ct <<- matrix(NA, nrow=nrow(new.data.ct), ncol=n.sims)
  
  
  
  #Number of obs to simulate
  n<-nrow(df_sims)
  
  
  
  
  #
  for(i in 1:n.sims){
    
    
    
  tryCatch({
    
    
    y<-matrix(NA,n,1)
    df_sims$sim_harvest<-NA
    
    
    #Loop to simulate data
    for(t in 1:n){
      
      
      #Simulate data based on model parameters
      y[t]<-rDPO(n=1, mu=mod$mu.fv[t], sigma = mod$sigma.fv[t])
      
    }#enf of simulation loop

    
    #Here I want the result of the simulation loop to be pasted in the df_sims dataset
    df_sims$sim_harvest<-y
    
    #Analysis of simulated data
    mod_sim<-gamlss(sim_harvest ~ ct,
                        data = df_sims,
                        family = DPO)
    
    
    
    
    #Refit the model if convergence not attained  
    if(mod_sim$converged==T){
      #If converged do nothing
    } else {
    #If not converged refit model
    mod_sim<-refit(mod_sim)
    }
    
    
    cat('we make it to here!\n')
    #Store results in object
    ct <<-as.vector(predict(mod_sim,   newdata = new.data.ct,   type='response'))
    
    cat('but not to here :( \n')
    
    #If we made it down here, register err as '0' to be used in the if statement in the finally code
    err<<-0
    
    
    
    }, 
    
    #If error register the error and write it!
    error = function(e) {
    
      #If error occured, show it
       cat('error at',i,'\n')
       
      #Register err as 1 to be used in the if statement in the finally code below
      err<<-1
      
      
      
    }, 
    
    
    
    finally = {

      if(err==0){
        #if no error, do nothing and keep going outside of tryCatch


        }
      else if (err==1){
        #If error, re-simulate data and do the analysis again
        
        y<-matrix(NA,n,1)
        df_sims$sim_harvest<-NA
        
        
        #Loop to simulate data
        for(t in 1:n){
          #Simuler les données basées sur les résultats du modèle
          y[t]<-rDPO(n=1, mu=mod$mu.fv[t], sigma = mod$sigma.fv[t])
          
        }#enf of simulation loop
        
        
        #Here I want the result of the simulation loop to be pasted in the df_sims dataset
        df_sims$sim_harvest<-y
        
        #Analysis of simulated data
        mod_sim<-gamlss(sim_harvest ~ ct,
                        data = df_sims,
                        family = DPO)
        
        
        cat('we also make it here \n')
        #Store results in object
        ct <<-as.vector(predict(mod_sim,   newdata = new.data.ct,   type='response'))
        cat('but not here... \n')
        
        
       }#if error, do nothing and go in repeat loop again
    }#End finally 
    
    
    
    )#End tryCatch
    
    
    #Write predictions for this iteractions to the DF and start over
    preds.sim.trad.ct[,i] <<-ct
    
    
    
    #Show iteration number
    cat(i,'\n')
  }
  
  

#Do some more stuff here 


#Return results
  return(preds = list(ct= list(predictions=preds.sim.trad.ct)))
}



#Run simulation and store object
result<-compute_CI_trad_gamlss(n.sims=20, mod=mod_pred)
