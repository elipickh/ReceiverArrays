prepare_tag_data = function(tag_data, dt_col, rec_col, rec_cols, max_time, pas_tol) {
  # Reshape detection data to 1 observation per row. 
  # Based on https://github.com/JennaVergeynst/prepare_toa_for_yaps/blob/master/Prepare_toa_data.py
  #
  # Args:
  #   tag_data: Data table or data frame with detection data for from all receivers for one transmitter.
  #   dt_col: Character name for the datetime POSIXct column.
  #   rec_col: Character name for the receiver columns.
  #   max_time: Numeric value indicating the max time (in seconds) that a sound signal would take between 
  #      furthest receivers.
  #   pas_tol: Numeric value indicating the max time (in minutes) between observations before a new 
  #      passing is started and the track is split up.
  #
  # Returns:
  #   Data table with an added columns: soundspeed,  
  data = copy(setDT(tag_data))
  data[, time_diff := as.numeric(difftime(get(dt_col),
                                        shift(get(dt_col)), 
                                        units = 'secs'))][,
          ':=' (sec_since_start = cumsum(ifelse(is.na(time_diff), FALSE, time_diff)),
                    groups_obs = cumsum(ifelse(is.na(time_diff), FALSE, time_diff) > max_time),
                    groups_pas = cumsum(ifelse(is.na(time_diff), FALSE, time_diff) > (pas_tol * 60))
               )][,
                  c('soundspeed', dt_col) := list(mean(soundspeed), first(get(dt_col)))
                 , by = groups_obs]
  
  setkeyv(dcast(data, groups_pas + groups_obs ~ get(rec_col), value.var = 'sec_since_start')[
           data, on = 'groups_obs', 
           c('soundspeed', dt_col) := list(i.soundspeed, get(dt_col))],
          dt_col)[, 
                  t_diff := rowMeans(.SD, na.rm = TRUE) - shift(rowMeans(.SD, na.rm = TRUE)), 
                 .SDcols = rec_cols]
 
}

clean_toa_data = function(toa_data, min_delay, rec_cols) {
  
  data = copy(toa_data)
  data[, receiver_amount := rowSums(!is.na(.SD))
     , .SDcols = rec_cols][
     , ':=' (true_error = ((shift(receiver_amount) > receiver_amount) & (t_diff < min_delay)) | 
                              ((shift(receiver_amount, -1) > receiver_amount) & (shift(t_diff, -1) < min_delay))
            )]

  setkeyv(data[is.na(true_error) | true_error==0][, 
          ':=' (trash_indices = (rowMeans(.SD, na.rm = TRUE) - shift(rowMeans(.SD, na.rm = TRUE))) < min_delay & 
                                rowSums(!is.na(.SD))==1
                ), .SDcols = rec_cols][
          trash_indices==0, 
          c('groups_pas', 'groups_obs', rec_list, 'soundspeed', 'SyncTime'), with=F
          ],
          dt_col)[, 
                  t_diff := rowMeans(.SD, na.rm = TRUE) - shift(rowMeans(.SD, na.rm = TRUE)), 
                 .SDcols = rec_cols]
  
}

fill_gaps = function(data, rec_cols, dt_col, mean_burst, min_track_length) {
  
  if (nrow(data) > min_track_length) {
    
    data = copy(data)
    
    data[, ':=' (times_ref = get(dt_col), 
               t_diff = NULL)]

    a = data.table(times = seq(min(data[, get(dt_col)]), 
                               max(data[, get(dt_col)]) + 1, 
                           by = mean_burst),
                    key = 'times')
    
    b = setnames(data[a, roll='nearest'], c('times_ref', dt_col), c(dt_col, 'times_ref'))
    
    dups = duplicated(b, by=setdiff(names(b), 'times_ref')) | 
           duplicated(b, by=setdiff(names(b), 'times_ref'), fromLast=T)
    
    toa_filled_nondup = b[!dups][, ':=' (times_ref = NULL)]
    
    toa_filled_dup = setkeyv(b[dups][, offset_sbi := abs(get(dt_col) - times_ref)],
                             c('groups_obs', 'offset_sbi'))

    dd = toa_filled_dup[ , .I[which.min(offset_sbi)], by = groups_obs]$V1
    
    result = setkeyv(rbindlist(list(toa_filled_dup[dd][, ':=' (times_ref = NULL, offset_sbi = NULL)], 
                                 setnames(toa_filled_dup[!dd, .(groups_pas, groups_obs, soundspeed, times_ref)],
                                          'times_ref', dt_col),
                                 toa_filled_nondup)
                            , fill=T), dt_col)[,
             t_diff := rowMeans(.SD, na.rm = TRUE) - shift(rowMeans(.SD, na.rm = TRUE)), 
             .SDcols = rec_cols
             ][is.na(t_diff) | t_diff <= (1.1 * max_burst)][, 
            t_diff := rowMeans(.SD, na.rm = TRUE) - shift(rowMeans(.SD, na.rm = TRUE)), 
           .SDcols = rec_cols]

    result          
  
    } else NA
    
}