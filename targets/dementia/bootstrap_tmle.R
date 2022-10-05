
#NOTE! RUN THIS ON SERVER 5, which has 240 cores! 

bootstrap_tmle <- function(spec_ltmle,  
                           SL.library="SL.glmnet",
                           abar_spec, 
                           det.Q.function,
                           ncores){


    boot_res <- NULL
    set.seed(235569)
    #set up parallelization
    #use about half of space
    ncores <- 4#floor(detectCores()/2)
    mycluster <- parallel::makeCluster(ncores)
    doSNOW::registerDoSNOW(cl=mycluster)
    

    boot_res_iteration <- foreach(1:ncores, .combine="rbind", 
                                  .errorhandling = "remove", .packages = c("SuperLearner","ltmle","tidyverse","matrixStats")) %dopar% 
        boot_fun(spec_ltmle=spec_ltmle, 
                 abar_spec=abar_spec, 
                 SL.library=SL.library,
                 det.Q.function=det.Q.function)
                 # , 
                 # package_stub=package_stub, 
                 # SuperLearner_override_ridge=SuperLearner_override, 
                 # Estimate_override=Estimate_override
                 # )
    # boot_res <- bind_rows(boot_res, boot_res_iteration)
    
  # return(boot_res)
    return(boot_res_iteration)
    
    
}

