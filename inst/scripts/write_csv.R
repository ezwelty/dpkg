csv_empty <- 'x,y
,
"",""
'
csv_true <- 'x,y
1,TRUE
1,True
1,true
1,T
'
csv_false <- 'x,y
0,FALSE
0,False
0,false
0,F
'
csv_date <- 'x,y
2010-01-01,2010-01
'
csv_datetime <- 'x,y
2010-01-01T01:01:01Z,2010-01-01 01:01:01
'

csv <- 'empty,empty_quotes,character,numeric,integer,logical
,"",abc,1.23,1,TRUE
'

# base::read.csv
# String - missingValues: Does not distinguish between "" and NA
read.csv(text = csv_empty, colClasses = "character", na.strings = "") %>% str()
read.csv(text = csv_empty, colClasses = "character", na.strings = character()) %>% str()
# Boolean - trueValues: TRUE, True, true, T, not 1 (NO CUSTOM)
read.csv(text = csv_true, colClasses = "logical") %>% str()
read.csv(text = csv_true, colClasses = c("numeric", "logical")) %>% str()
# Boolean - falseValues: FALSE, False, false, F, not 0 (NO CUSTOM)
read.csv(text = csv_false, colClasses = "logical") %>% str()
read.csv(text = csv_false, colClasses = c("numeric", "logical")) %>% str()
# Numeric - groupChar not supported, decimalChar not supported (at field-level)
# Date - format: must be %Y-%m-%d
read.csv(text = csv_date, colClasses = "Date") %>% str()
read.csv(text = csv_date, colClasses = c("Date", "character")) %>% str()
# Datetime - format: must be %Y-%m-%d %H:%M:%S
read.csv(text = csv_datetime, colClasses = "POSIXct") %>% str()

# readr::read_csv
# String - missingValues: Does not distinguish between "" and NA
readr::read_csv(csv_empty, col_types = "cc", na = "") %>% str()
readr::read_csv(csv_empty, col_types = "cc", na = character()) %>% str()
# Boolean - trueValues: TRUE, True, true, T, 1 (no custom)
readr::read_csv(csv_true, col_types = "ll") %>% str()
# Boolean - falseValues: FALSE, False, false, F, 0 (no custom)
readr::read_csv(csv_false, col_types = "ll") %>% str()
# Numeric - groupChar not supported at field-level, decimalChar only , or . and not supported at field-level
# Date - custom format!
readr::read_csv(csv_date, col_types = readr::cols(readr::col_date("%Y-%m-%d"), readr::col_date("%Y-%m"))) %>% str()
# Datetime - custom format!
readr::read_csv(csv_datetime, col_types = readr::cols(readr::col_datetime("%Y-%m-%dT%H:%M:%SZ"), readr::col_datetime("%Y-%m-%d %H:%M:%S"))) %>% str()

# data.table::fread
# String - missingValues: Does not distinguish between "" and NA
data.table::fread(csv_empty, colClasses = "character", na.strings = "") %>% str()
data.table::fread(csv_empty, colClasses = "character", na.strings = character()) %>% str()
# Boolean - trueValues: T, TRUE or True if not mixed, NOT true, 1 (no custom)
data.table::fread(csv_true, colClasses = "logical") %>% str()
# Boolean - falseValues: F, FALSE, or False if not mixed, NOT false, 0 (no custom)
data.table::fread(csv_false, colClasses = "logical") %>% str()
# Numeric - groupChar not supported at field-level, decimalChar only , or . and not supported at field-level
# Date - BROKEN?
data.table::fread(csv_date, colClasses = c("Date", "character")) %>% str()
# Datetime - BROKEN?
data.table::fread(csv_datetime, colClasses = c("POSIXct")) %>% str()

read.csv(text = 'x\n1\n0\n', blank.lines.skip = FALSE, as.is = TRUE, colClasses = "character", na.strings = "") %>% str()

read.csv(text = csv, as.is = TRUE, colClasses = "") %>% str()
read.csv(csvfile, )

# readr::read_csv / readr::write_csv

# data.table::fread / data.table::fwrite


field <- rep(c("2010-01-01", "2010-01-01"), 1e5)
csv <- write_csv(data.frame(x = field, y = field))
microbenchmark::microbenchmark(
  {
    dt <- data.table::fread(csv, colClasses = "character")
    dt[, x := parse_date(x)]
    dt[, x := parse_date(y)]
  },
  data.table::fread(csv, colClasses = "Date"),
  readr::read_csv(csv, col_types = "DD"),
  times = 5
)
