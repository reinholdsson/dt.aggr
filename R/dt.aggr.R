#' @export
aggr_by <- function(data, j, by, grand.total = T, total.label = "(all)", envir = .GlobalEnv) {
    j = substitute(j)
    
    # Calculate by each combination
    lst <- lapply(1:length(by), function(i) {
      x <- data[, eval(j), by = eval(by[1:i])]
      if (i != length(by)) x[, (by[-(1:i)]) := total.label]
      return(x)
    })
    
    # Grand total
    if (grand.total) lst <- c(lst, list(data[, eval(j)][, (by) := total.label]))
    
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
calculate_xtd <- function(x, date_col, last_date, fun = .N, fun.label = toupper(substitute(fun)), ...) {
  
  dt <- copy(x) # should at least select only relevant columns
  setnames(dt, date_col, "..date..")
		# Should use get (with envir) or quote/eval instead of setnames
  	# But currently don't know how to solve this nicely.
  setkey(dt, ..date..)
  
	d <- as.Date(last_date)
	env <- environment()
	fun <- substitute(fun)
  res <- aggr_by(dt,
    list(
      # Current year
      N_t1 = .SD[..date.. == get('d', envir), eval(get('fun', envir))],
      MTD_t1 = .SD[..date.. %between% mtd_interval(get('d', envir)), eval(get('fun', envir))],
      YTD_t1 = .SD[..date.. %between% ytd_interval(get('d', envir)), eval(get('fun', envir))],
      # Previous year
      N_t0 = .SD[..date.. == get('d', envir) - years(1), eval(get('fun', envir))],
      MTD_t0 = .SD[..date.. %between% mtd_interval(get('d', envir), -1), eval(get('fun', envir))],
      YTD_t0 = .SD[..date.. %between% ytd_interval(get('d', envir), -1), eval(get('fun', envir))]
    ),
  	envir = env,
    ...
  )
  
  res[, c('MTD %', 'YTD %') := list(100 * (MTD_t1 / MTD_t0 - 1), 100 * (YTD_t1 / YTD_t0 - 1))]
  
  setnames(
    res,
    c('N_t1', 'N_t0', 'MTD_t1', 'MTD_t0', 'YTD_t1', 'YTD_t0'),
    c(
      paste(paste0(fun.label, ','), as.character(c(last_date, last_date - years(1)))),
      paste('MTD,', format(c(last_date, last_date - years(1)), '%Y-%m')),
      paste('YTD,', format(c(last_date, last_date - years(1)), '%Y'))
    )
  )
  
  return(res)
}