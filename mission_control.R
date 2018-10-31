#             _         _                               _             _ 
#   _ __ ___ (_)___ ___(_) ___  _ __     ___ ___  _ __ | |_ _ __ ___ | |
#  | '_ ` _ \| / __/ __| |/ _ \| '_ \   / __/ _ \| '_ \| __| '__/ _ \| |
#  | | | | | | \__ \__ \ | (_) | | | | | (_| (_) | | | | |_| | | (_) | |
#  |_| |_| |_|_|___/___/_|\___/|_| |_|  \___\___/|_| |_|\__|_|  \___/|_|
#                                                                       
################################################################################
library(data.table)
library(magrittr)

source('get_data.R')
source('data_construction_functions.R')
source('create_tsne_projection.R')
source('create_clusters.R')
source('create_thresholds.R')

min_date = as.POSIXct('2018-10-01 01:00:00')
max_date = min_date + 60*60*24*30
#Get some data##################################################################
all_events_dt = fake_data_generator(min_date = min_date,
                                    max_date = max_date)

UID_s = all_events_dt$USER_ID %>% unique()

session_dt = lapply(UID_s,create_user_sessions,
                    min_date = min_date,
                    source_data = all_events_dt)

all_events_dt = rbindlist(session_dt)

all_events_dt = get_meta_data(all_events_dt)
#Cluster Data###################################################################
tsne_projection = tsne_wrapper(all_events_dt)

clusters = optics_wrapper(tsne_projection)

all_events_dt[,cluster := clusters]

#Final Data Cleaning############################################################
analysis_vars <- names(all_events_dt)[!(names(all_events_dt) %in% c("USER_ID",
                                                                    "session",
                                                                    "session_start",
                                                                    "session_end",
                                                                    "cluster"))]

na_or_zero_or_id = lapply(analysis_vars, function(var, dat){
  all(is.na(dat[[var]]) | dat[[var]] == 0) | uniqueN(dat[[var]]) == 1
},
dat = all_events_dt) %>% unlist()

if(any(na_or_zero_or_id)){
  all_events_dt[,(analysis_vars[na_or_zero_or_id]) := NULL]
}
#fake jobs######################################################################
jobs = c('sales rep', 'sales manager', 'IT associate')
jobs_dt = data.table(USER_ID = all_events_dt$USER_ID %>% unique(),
                     FUNCTION = sample(jobs,
                                       uniqueN(all_events_dt$USER_ID),
                                       replace = T))

all_events_dt = merge(all_events_dt, jobs_dt, by = 'USER_ID', all = T)

#User Thresholds################################################################
user_thresholds = lapply(analysis_vars,
                         target_thresholds,
                         source_data = all_events_dt)


names(user_thresholds) = analysis_vars
