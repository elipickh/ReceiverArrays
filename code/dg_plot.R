dg_plot = function(data, dt_col = 'datetime', y_cols = TRUE, plot_subset = 6, ylim = NULL, xlim = NULL, hlines = NULL, show_pts = FALSE, line_con = FALSE, line_w = 0.5, main = NULL, group = NULL, width = NULL, height = NULL, r_selector = TRUE) {
  # Interactive plotting for time-series data.
  #
  # Args:
  #   data: Data frame or data table
  #   dt_col: Character name for the datetime POSIXct column. Default s 'datetime'.
  #   y_cols: Optional vector of >= 1 character names of columns to plot. Default is TRUE, which plots
  #      all numeric columns.
  #   plot_subset: Optional integer used to subset the data to plot. E.g., plot_subset = 3 will plot
  #      every 3rd value. Set to FALSE (or 1) to plot all data points. Default is 6.
  #
  # Returns:
  #   Display a dygraph plot 
  if (length(intersect(dt_col, y_cols)) > 0) {print('dt_col and y_cols should not overlap'); break}
  
  setcolorder(setDT(data, dt_col))

  p = dygraph(as.xts.data.table(data[seq(1, nrow(data), max(1, plot_subset)), 
                                unlist(ifelse(isTRUE(y_cols), 
                                              list(names(data)), 
                                              list(c(dt_col, y_cols)))), 
                                with = FALSE]),
              main = main,
              group = group,
              width = width,
              height = height) %>% 
    dyOptions(useDataTimezone = TRUE, 
              drawPoints = show_pts,
              connectSeparatedPoints = line_con,
              strokeWidth = line_w)
  
  if(!is.null(ylim)) {
    
    p = p %>% dyAxis('y', valueRange = c(ylim[1], ylim[2]))
  }
  
  if(!is.null(xlim)) {
    
    p = p %>% dyAxis('x', valueRange = c(xlim[1], xlim[2]))
  }
  
  if(!is.null(hlines)) {
    
    for(l in hlines) {
      
      p = p %>% dyLimit(l, color = 'red', strokePattern = 'solid')
      
    }
    
  }  
  
  if(r_selector) {p = p %>% dyRangeSelector() }
    
  return (p)
  
}
