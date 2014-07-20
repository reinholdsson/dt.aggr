dt.aggr
=======

r data.table helper functions

```
library(data.table)
library(lubridate)
d <- seq(as.Date("2013-01-01"), as.Date("2014-12-31"), by = 1)
l <- length(d)
x <- data.table(d = d, b = sample(LETTERS[1:3], l, replace = T), c = sample(LETTERS[1:3], l, replace = T), value = sample(1:100, l, replace = T))
calculate_xtd(x, date_col = 'd', last_date = as.Date('2014-06-15'), fun = sum(value), by = c('b'))
```