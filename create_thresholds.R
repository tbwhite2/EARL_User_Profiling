target_thresholds = function(target_var, source_data){
  
  mm_mod = lme4::lmer(reformulate(termlabels = c("1 + (1 |cluster) + (1|FUNCTION/USER_ID)"),
                                  response = target_var),
                      data = source_data)
  
  dat_predict = source_data[,.(count = .N),by = c("USER_ID","cluster","FUNCTION")]
  
  PI_95 =  merTools::predictInterval(merMod = mm_mod,
                                     newdata = dat_predict, 
                                     level = c(0.95),
                                     n.sims = 1000,
                                     stat = "median",
                                     type="linear.prediction",
                                     include.resid.var = TRUE)
  PI_95
  
}