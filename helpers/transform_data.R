##
# Transform log files to data frame of jumps
##
# Final data frame: Each row corresponds to an attempt,
#   and contains user id, date of attempt, and relative date of last jump
#   for this attempt.
transform_data = function(logdate) {
  filepath = paste("data/1csv/", logdate, "_flappylog.csv", sep = "")
  
  dir.create("data/2jumps", showWarnings = FALSE)
  outfile = paste("data/2jumps/", logdate, "_jumps.RDS", sep = "")
  
  if(!file.exists(outfile)) {
    ##
    # Retrieve data from log
    ##
    # Load the log of flappy birds. 1 row = 1 attempt
    df_log = fread(filepath, head = FALSE)
    
    # user id
    id=df_log[[2]]
    id=gsub(", \"jumps\"", "", id)
    id=as.numeric(id)
    
    # relative dates of jumps
    jump=df_log[[3]]
    jump=gsub(" ], \"mydate\"", "", jump)
    jump=gsub("\\[ ", "", jump)
    jump=strsplit(jump, ", ")
    jump=sapply(jump, as.numeric)
    
    # date of attempt
    date=df_log[[5]]
    date=gsub(" }, \"_id\"", "", date)
    #http://www.convert-unix-time.com/?t=1393878149
    date=as.numeric(date)/1000
    unix2POSIXct  <-  function (time)  structure(time, class = c("POSIXt", "POSIXct")) 
    date=unix2POSIXct(date)
    
    ##
    # Get dataframe of jumps to export
    ##
    last_jump=sapply(jump, tail, 1)
    df_jumps = data.table(id=id, date=date, last_jump=last_jump)
    df_jumps = df_jumps %>% arrange(id, date)
    
    saveRDS(df_jumps, outfile)
  }
}