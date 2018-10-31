create_user_sessions = function(UID, min_date, source_data){
  user_subset = source_data[USER_ID == UID]
  setorderv(user_subset, 'TIMESTAMP')
  user_subset_logins = user_subset[EVENT_TYPE %in% c("log_in")]
  
  login_rows = which(user_subset$EVENT_TYPE == "log_in")
  empty_login = (login_rows + 1) == login_rows[2:(length(login_rows) + 1)]
  empty_login[is.na(empty_login)] = F
  user_subset = user_subset[!login_rows[empty_login]]
  
  if(nrow(user_subset) > 0 & nrow(user_subset_logins) > 0){
    #drop all usage before first login
    user_subset[,first_login := .SD[EVENT_TYPE == 'log_in',
                                    min(TIMESTAMP)],
                by = 'USER_ID']
    user_subset = user_subset[TIMESTAMP >= first_login]
    
    breaks = c(min_date, user_subset_logins$TIMESTAMP,
               max_date)%>% unique %>% sort()
    
    user_subset[,session := cut(TIMESTAMP,
                                breaks,
                                include.lowest = T, 
                                right = F, 
                                labels = paste0("range_",
                                                1:(length(breaks) - 1)))]
    
    
    if(any(user_subset$EVENT_TYPE == "log_off")){
      login_rows = which(user_subset$EVENT_TYPE == "log_in")
      logout_rows = which(user_subset$EVENT_TYPE == "log_off")
      
      #Get the indicies of all events between a logout and the next login
      logout_login_rows <- lapply(logout_rows,function(x,y){
        if((x+1) %in% y | !any((x) < y)){
          NULL
        }else{
          (x+1):(y[min(which((x) < y))] - 1)
        }
        
      },
      y = login_rows) %>% unlist()
      
      user_subset = user_subset[!logout_login_rows]
    }
    
    if(nrow(user_subset) > 0){
      
      
      user_subset[,time_in_event_type := as.numeric(difftime(shift(TIMESTAMP,
                                                                   type = 'lead'),
                                                             TIMESTAMP,
                                                             units = "secs"))]
      user_subset[EVENT_TYPE == 'log_off', time_in_event_type := 0]
      
      
      user_subset = user_subset[,.(time_in_event = sum(time_in_event_type),
                                   count_actions = .N,
                                   event_start = min(as.Date(TIMESTAMP)),
                                   event_end = max(as.Date(TIMESTAMP)),
                                   event_start_time = min(TIMESTAMP),
                                   event_end_time = max(TIMESTAMP))
                                ,.(USER_ID,
                                   session,
                                   EVENT_TYPE)]
      user_subset
    }else{
      NULL
    }
  }else{
    NULL
  }
  
}

get_meta_data = function(source_data){
  source_data[,session_start := min(event_start_time), by = .(USER_ID, session)]
  source_data[,session_end := max(event_end_time), by = .(USER_ID, session)]
  
  source_data[,total_actions := sum(count_actions), by = c("USER_ID","session")]
  
  source_data = dcast(source_data,
                      USER_ID + session + session_start + session_end + total_actions ~ EVENT_TYPE,
                      value.var = c("count_actions", "time_in_event"))
  source_data[is.na(source_data)] = 0
  
  
  
  source_data[,session_total_time := as.numeric(difftime(session_end,
                                                         session_start,
                                                         units = "secs"))]
  source_data[session_total_time == 0,session_total_time := .01]
  
  source_data[,total_actions_per_second := total_actions/session_total_time]
  
  source_data[,c("session_day_of_week",
                 "session_year",
                 "session_year_day",
                 "session_week",
                 "session_hour"):= .(as.character(wday(session_start)),
                                     as.character(year(session_start)),
                                     as.character(yday(session_start)),
                                     as.character(week(session_start)),
                                     as.character(hour(session_start)))]
  
  source_data[,weekend_session := as.integer(session_day_of_week %in% c("7","1"))]
  
  source_data[,count_sessions_day := .N, by = c("USER_ID",
                                                "session_year",
                                                "session_year_day")]
  
  source_data[,count_sessions_week := .N, by = c("USER_ID",
                                                 "session_year",
                                                 "session_week")]
  
  source_data[,count_sessions_hour := .N, by = c("USER_ID",
                                                 "session_year",
                                                 "session_year_day",
                                                 "session_hour")]
  
  source_data[,c("session_year",
                 "session_week",
                 "session_day_of_week",
                 "session_hour",
                 "session_year_day") := NULL]
  
  source_data[is.na(source_data)] <- 0
  
  
  
  unique_elements = lapply(source_data, uniqueN) %>% unlist
  source_data = source_data[,c(names(source_data)[unique_elements > 1]),
                            with = F]
  source_data
}