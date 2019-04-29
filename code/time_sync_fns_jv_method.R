add_dtd = function(b_rec_data = base_receiver, o_rec_data = other_receiver, sync_id, dt_cols = dt_col, id_cols = id_col, t_sec = 100) {
  # Get the detection-time-difference (DTD) of a row. 
  # Adapted from https://github.com/JennaVergeynst/time_synchronization/blob/master/Time_sync_functions.py
  #
  # Args:
  #   b_rec_data: Data table with sync detections on the base-receiver.
  #   o_rec_data: Data table with sync detections on the other-receiver.
  #   sync_id: Character or numeric name of the sync-tag to use.
  #   dt_cols: Character name (or a vector of two names corresponding to b_rec_data and o_rec_data,
  #      respectively) for the datetime POSIXct column(s).
  #   id_cols: Character name (or a vector of two names corresponding to b_rec_data and o_rec_data,
  #      respectively) for the ID column(s).
  #   t_sec: Numeric value for the time, in seconds, in which to search datetimes between the datasets.
  #
  # Returns:
  #   Data table with an added DTD column ('dtd').
  dt_col_b = dt_cols[1]
  dt_col_o = na.omit(c(dt_cols[2], dt_cols[1]))[1]
  
  id_col_b = id_cols[1]
  id_col_o = na.omit(c(id_cols[2], id_cols[1]))[1]
  
  base_receiver_sync = setnames(b_rec_data[get(id_col_b)==sync_id, 
                                         .(get(dt_col_b), get(dt_col_b))], 
                                 c(dt_col_b, paste0('base_datetime')))
  
  other_receiver_sync = setnames(o_rec_data[get(id_col_o)==sync_id & !is.na(get(dt_col_o)), 
                                          .(get(dt_col_o))], 
                                  dt_col_b)
  
  setkeyv(base_receiver_sync, dt_col_b)
  setkeyv(other_receiver_sync, dt_col_b)
  
  other_receiver_sync = setnames(base_receiver_sync[other_receiver_sync, roll = 'nearest'],
                                  c(dt_col_o, 'base_datetime'))
  other_receiver_sync[, c('dtd', 'base_datetime') := 
                         list(as.numeric(difftime(get(dt_col_o), 
                                                  base_datetime, 
                                                  units = 'secs')), 
                              NULL)][, 
                                     dtd := ifelse(abs(dtd) > t_sec, 
                                                   NA, 
                                                   dtd)]

  return (other_receiver_sync)
  
}

smooth_dtd = function(data, dtd_col = 'dtd', outlier_lim = 0.0001, window_size = 6) {
  # Smooth DTD (in-place) using a moving average.
  # Adapted from https://github.com/JennaVergeynst/time_synchronization/blob/master/Time_sync_functions.py
  #
  # Args:
  #   data: Data table with sync detections on the other-receiver, with a DTD column.
  #   dtd_col: Character name for the DTD column. Default is 'dtd'.
  #   outlier_lim: Numeric value indicating max time (in sec) in adjact DTD to keep the detection.
  #   window_size: Integer value indicating rolling window size.
  #
  # Returns:
  #   None. Data is modified in-place, with an added smooth DTD column ('dtd_smooth').
  
  # Jenna: "put first and last value on nan to avoid border phenomena"
  # Not using this here, as it doesn't align with Jenna's output
  #data[, dtd_smooth := c(NA, get(dtd_col)[-c(1, .N)], NA)[1:.N]]
  # Using this instead:
  data[, dtd_smooth := get(dtd_col)]
  
  data[, dtd_smooth := ifelse(abs(dtd_smooth - shift(dtd_smooth)) > outlier_lim, NA, dtd_smooth)]
  data[, dtd_smooth := frollmean(dtd_smooth, 
                               window_size, 
                               hasNA = TRUE,
                               algo = 'exact'
                               )
    ]
  
  return (invisible())

}

model_spline_part = function(data, dt_cols = 'datetime', dtd_col = 'dtd_smooth', target_data, gap_lim = 0.1, row_thresh = 5, k_par = 4, s_par = 1e-3) {
  # Model the spline of the DTD.
  # Adapted from https://github.com/JennaVergeynst/time_synchronization/blob/master/Time_sync_functions.py
  #
  # Args:
  #   data: Data table with a DTD column (e.g., from smooth_dtd).
  #   dt_cols: Character name (or a vector of two names corresponding to data and target_data,
  #      respectively) for the datetime POSIXct column(s).
  #   dtd_col: Character name for the (smooth) DTD column.
  #   targret_data: Data frame or data table with the datetime column to apply the spline.
  #   gap_lim: Numeric value indicating the min time (in sec) of DTD to split the data . 
  #   row_thresh: Numeric value indicating the min number of observations per DTD group to model a spline. 
  #   k_par: "the desired equivalent number of degrees of freedom" ('df' in smooth.spline)
  #   s_par: "smoothing parameter, typically (but not necessarily) in (0,1]" ('spar' in smooth.spline)
  #
  # Returns:
  #   Data table with an added modelled DTD spline column ('spline').  
  dt_col_data = dt_cols[1]
  dt_col_target = na.omit(c(dt_cols[2], dt_cols[1]))[1]
  
  dtd_part_sync = setnames(
                            data[!is.na(dtd_smooth), 
                          .(get(dt_col_data), 
                            dtd_smooth,
                            gaps = abs(dtd_smooth - shift(dtd_smooth)) > gap_lim
                           )
                          ][, .(cumsum(ifelse(is.na(gaps), FALSE, gaps)),
                                V1,
                                dtd_smooth)],
                          c('groups', dt_col_target, 'dtd_smooth'))
                          
  setkeyv(dtd_part_sync, c('groups', dt_col_data))
  
  spline_groups = setnames(dtd_part_sync[, model_spline(.N, 
                                                        get(dt_col_target), 
                                                        get(dtd_col), 
                                                        ts = target_data[[dt_col_target]],
                                                row_thresh = row_thresh, k = k_par, s = s_par
                                                ), 
                                 by = groups][, !'groups'],
                           c(dt_col_target, 'spline'))
  
  setkeyv(spline_groups, dt_col_target)
  
  return(spline_groups)

}

model_spline = function(rows, dt_col = datetime, dtd_col = dtd_smooth, ts, row_thresh = 5, k=3, s=3e-4) {

  if (rows > row_thresh) {

     ts_part = ts[between(ts, 
                         min(dt_col), 
                         max(dt_col), 
                         incbounds=FALSE)
                  ]
    
     s = smooth.spline(x = as.numeric(dt_col), 
                       y = dtd_col,
                       df = k,
                       spar = s
                       )
    
     data_part = data.table(datetime = ts_part,
                          spline = predict(s, as.numeric(ts_part))$y)

  } else {
    
     data_part = data.table()

  }
  
   return(data_part)

}
