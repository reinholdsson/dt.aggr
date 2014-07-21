#' @export
aggr_by <- function(dt, j, by, grand.total = T, total.label = "(all)", envir = .GlobalEnv) {
    j = substitute(j)
    
    # Calculate by each combination
    lst <- lapply(1:length(by), function(i) {
      x <- dt[, eval(j), by = eval(by[1:i])]
      if (i != length(by)) x[, (by[-(1:i)]) := total.label]
      return(x)
    })
    
    # Grand total
    if (grand.total) lst <- c(lst, list(dt[, eval(j)][, (by) := total.label]))
    
    # Combine all tables
    res <- rbindlist(lst, use.names = T, fill = F)
  
    # Set proper column order
    setcolorder(res, c(by, colnames(res)[!colnames(res) %in% by]))
    
  # Sort values
  setkeyv(res, by)
  
  return(res)
}

mtd_interval <- function(date, yt = 0) c(date + 1 - day(date) + years(yt), date + years(yt))
ytd_interval <- function(date, yt = 0) c(as.Date(paste0(year(date) + yt, "-01-01")), date + years(yt))

#' @export
calculate_xtd <- function(x, date.col, ...,
	fun = .N, final.date = max(x[[date.col]]),
	measures = c('d0', 'd1' ,'m0', 'm1', 'y0', 'y1', 'mtd', 'ytd')
) {
	dt <- copy(x) # should at least select only relevant columns
  setnames(dt, date.col, "..date..")
	# Should use get (with envir) or quote/eval instead of setnames
  # But currently don't know how to solve this nicely.
  setkey(dt, ..date..)
  
	d <- final.date <- as.Date(final.date)
	env <- environment()
	fun <- substitute(fun)
  res <- aggr_by(dt,
    j = list(
      # Current year
      d1 = .SD[..date.. == get('d', envir), eval(get('fun', envir))],
      m1 = .SD[..date.. %between% mtd_interval(get('d', envir)), eval(get('fun', envir))],
      y1 = .SD[..date.. %between% ytd_interval(get('d', envir)), eval(get('fun', envir))],
      # Previous year
      d0 = .SD[..date.. == get('d', envir) - years(1), eval(get('fun', envir))],
      m0 = .SD[..date.. %between% mtd_interval(get('d', envir), -1), eval(get('fun', envir))],
      y0 = .SD[..date.. %between% ytd_interval(get('d', envir), -1), eval(get('fun', envir))]
    ),
  	envir = env,
    ...
  )
  
  res[, c('mtd', 'ytd') := list(100 * (m1 / m0 - 1), 100 * (y1 / y0 - 1))]
  
	old_names <- c('d1', 'd0', 'm1', 'm0', 'y1', 'y0', 'mtd', 'ytd')
	new_names <- c(
	  as.character(c(final.date, final.date - years(1))),
	  format(c(final.date, final.date - years(1)), '%Y-%m'),
	  format(c(final.date, final.date - years(1)), '%Y'),
		'MTD %', 'YTD %'
  )
	
	res <- res[, measures, with = F]
	cols_i <- match(measures, old_names)
	
  setnames(
    res,
    old_names[cols_i],
  	new_names[cols_i]
  )
  
  return(res)
}
