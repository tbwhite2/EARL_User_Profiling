fake_data_generator = function(min_date, max_date){
  
  time = seq.POSIXt(min_date,
                    max_date,
                    by = 'secs')
  
  user_id = vapply(1:50, function(x){uuid::UUIDgenerate()}, character(1))
  
  tables = c('log_in',
             'dashboard_interact',
             'download_objects', 
             'call_report_interact',
             'log_off' )
  
  all_events_dt = lapply(tables, function(type, time, user_id){
    data.table(EVENT_TYPE = type, 
               TIMESTAMP = sample(time, 5000),
               USER_ID = sample(user_id, 5000, replace = T))
  }, time = time, user_id = user_id)
  
  all_events_dt = rbindlist(all_events_dt)
  all_events_dt
}

# In the actual code, the data is called via a SQL statement similar to the one 
# below.
# general_query_generator <- function(table_name, min_date){
#   paste0("SELECT DISTINCT
#          [EVENT_TYPE]
#          ,[TIMESTAMP]
#          ,[USER_ID]
#          FROM [DATABASE].[",table_name,"]
#          Where [USER_ID] IS NOT NULL
#          AND [EVENT_TYPE] IS NOT NULL
#          AND [TIMESTAMP] IS NOT NULL
#          AND [TIMESTAMP] >=",min_date)
# }
# 
# test <- lapply(tables,
#                general_query_generator,
#                min_date = global_min_date) %>% unlist()
# 
# full_query <- paste0(" SELECT DISTINCT * \n FROM((",
#                 paste(test, collapse = ") \n UNION \n ("),
#                 ")) dt \n ORDER BY [USER_ID],[TIMESTAMP]")
# 
# all_events<- msQuery(full_query, 
#                      control = control_l)
# 
# all_events_dt <- as.data.table(all_events)
# rm(list = c("all_events"))
# setkey(all_events_dt,NULL)

# all_events_dt <- unique(all_events_dt)