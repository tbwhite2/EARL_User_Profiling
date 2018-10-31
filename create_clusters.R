optics_wrapper = function(tsne_result){
  ##############################################################################
  #Use the optics method to find several contendors for the best clustering 
  #parameters
  
  optics_result = dbscan::optics(tsne_result, minPts = 10, eps = 5)
  
  eps_optimizer = function(eps_test, optics_result){
    # eps_test = .5
    test_db_scan <- dbscan::extractDBSCAN(optics_result, 
                                          eps_cl = eps_test)
    
    data.table(n_clust = test_db_scan$cluster %>% table() %>% length(),
               n_zeros = sum(test_db_scan$cluster == 0),
               eps = eps_test
    )
  }
  eps_optim_result = lapply(10:50/10, eps_optimizer,
                            optics_result = optics_result) 
  
  eps_optim_result = rbindlist(eps_optim_result)
  
  x <- eps_optim_result$n_clust
  
  calc_res <- c()
  #Find that elbow##############################################################
  for(i in 1:length(x)){
    calc_res[i] <- x[i+1] + x[i-1] - 2 * x[i]
  }
  
  the_chosen_eps = eps_optim_result$eps[which.max(abs(calc_res))]
  
  db_scan_final = dbscan::extractDBSCAN(optics_result, eps_cl = the_chosen_eps)
  
  db_scan_final$cluster
}

