dt_linear_interp = function(data, dt_col, y_col) {
  # Fast, in-place linear interpolation for missing numeric values for time-series data
  #
  # Args:
  #   data: Data frame or data table. Data frame data will be coerced in-place to data table 
  #   dt_col: Character name for the datetime POSIXct column.
  #   y_col: Character name for the numeric column in which missing values shall be replaced.
  #
  # Returns:
  #   None.  
  setDT(data, key = dt_col)
  
  setkey(data[, grp0 := rleid(!is.na(get(y_col)))
            ], 
         grp0)
  
  data[na.omit(
              setkey(data[, .(f_sp = first(get(y_col)), 
                            l_sp = last(get(y_col)),
                            f_dt = as.numeric(first(get(dt_col))), 
                            l_dt = as.numeric(last(get(dt_col)))), 
                        by = grp0], 
                     grp0)
                        [, .(grp0, 
                             p_dt = shift(l_dt),
                             p_sp = shift(l_sp),
                             tot_dt = shift(f_dt, -1) - shift(l_dt),
                             tot_sp = shift(f_sp, -1) - shift(l_sp))
                         ]
              ), 
   (y_col) := 
                (((as.numeric(get(dt_col)) - i.p_dt) * 
                   i.tot_sp) / 
                i.tot_dt) + 
              i.p_sp
             
           ][, grp0 := NULL]
  
  return(invisible())
  
}
