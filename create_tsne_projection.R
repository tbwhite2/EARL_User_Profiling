tsne_wrapper = function(source_data, plot = F){
  cols = c("USER_ID", "session","session_start","session_end")
  
  source_data_matrix = as.matrix(source_data[,lapply(.SD, as.numeric),
                                             .SDcols = setdiff(names(source_data),
                                                               cols)])
  
  source_data_matrix[is.na(source_data_matrix) | is.nan(source_data_matrix)] = 0
  source_data_matrix = source_data_matrix[,colSums(source_data_matrix != 0)> nrow(source_data_matrix)*.1]
  
  
  source_data_matrix = scale(source_data_matrix)
  
  tsne_projection = Rtsne::Rtsne(source_data_matrix,
                                 perplexity = 20,
                                 theta = 1,
                                 check_duplicates = F, 
                                 max_iter = 500)
  if(plot){
    plot(tsne_projection$Y)
  }
  
  tsne_projection$Y
}
