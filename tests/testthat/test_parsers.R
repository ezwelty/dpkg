context("Field parsers")

# ---- Fields: String ----
# http://specs.frictionlessdata.io/table-schema/#string

test_that("parse_strings returns character", {
  expect_identical(parse_string("hello"), "hello")
  expect_identical(parse_string(1.1), "1.1")
  expect_identical(parse_string(NA), NA_character_)
})

# ---- Fields: Number ----
# http://specs.frictionlessdata.io/table-schema/#number

test_that("parse_number returns numeric", {
  expect_identical(parse_number(1), 1)
  expect_identical(parse_number("1"), 1)
  expect_identical(parse_number(NA), NA_real_)
})

test_that("parse_number supports leading signs (+ or -)", {
  expect_identical(parse_number("-1"), -1)
  expect_identical(parse_number("+1"), 1)
  expect_warning(parse_number("++1"))
  expect_warning(parse_number("1+"))
})

test_that("parse_number supports leading and trailing zeroes", {
  expect_identical(parse_number("001"), 1)
  expect_identical(parse_number("1.00"), 1)
})

test_that("parse_number supports special string values (NaN, Inf, -Inf)", {
  expect_identical(unique(parse_number(c("NaN", "NAN", "nan"))), NaN)
  expect_identical(unique(parse_number(c("Inf", "INF", "inf"))), Inf)
  expect_identical(unique(parse_number(c("-Inf", "-INF", "-inf"))), -Inf)
})

test_that("parse_number supports trailing exponents ([e|E][+|-]{0,1}[0-9]*)", {
  expect_identical(unique(parse_number(c("1e", "1e0", "1.0e0", "1e00", "1E0", "1E-0", "1E+0"))), 1)
  expect_identical(unique(parse_number(c("1e-1", "1.0e-1", "1e-01", "1E-1"))), 0.1)
  expect_identical(unique(parse_number(c("1e1", "1.0e1", "1e01", "1E1", "1E+1"))), 10)
})

test_that("parse_number supports percentages (%{0,1}$)", {
  expect_identical(parse_number("1%"), 0.01)
  expect_identical(parse_number("53E1%"), 5.3)
  expect_warning(parse_number("1%%"))
  expect_warning(parse_number("%1"))
  expect_warning(parse_number("53%E1"))
})

test_that("parse_number supports a custom decimal character", {
  expect_identical(parse_number("1.0"), 1)
  expect_identical(parse_number("1,0", decimalChar = ","), 1)
  expect_warning(parse_number("1,0", decimalChar = "."))
  expect_error(parse_number("", decimalChar = ".,"))
})

test_that("parse_number supports a grouping character", {
  expect_identical(parse_number("1,000", groupChar = ","), 1000)
  expect_warning(parse_number("1,000"))
  expect_error(parse_number("", groupChar = ".,"))
})

if ("units" %in% rownames(utils::installed.packages())) {
  test_that("parse_number supports units", {
    expect_is(parse_number(1, unit = "m"), "units")
    expect_is(parse_number("1", unit = "m"), "units")
    expect_identical(parse_number(1, unit = "m"), units::as.units(1, units::parse_unit("m")))
    expect_identical(parse_number("1", unit = "m"), units::as.units(1, units::parse_unit("m")))
  })
}

# ---- Fields: Integer ----
# http://specs.frictionlessdata.io/table-schema/#integer

test_that("parse_integer returns integer", {
  expect_identical(parse_integer(1), 1L)
  expect_identical(parse_integer("1"), 1L)
  expect_identical(parse_integer(NA), NA_integer_)
})

test_that("parse_integer supports only whole numbers", {
  expect_identical(unique(parse_integer(c(1.0))), 1L)
  expect_identical(unique(parse_integer(c("1", "1.0", "+1", "1e0", "0.1e1"))), 1L)
  expect_warning(parse_integer(1.1))
  expect_warning(parse_integer("1.1"))
})

if ("units" %in% rownames(utils::installed.packages())) {
  test_that("parse_integer supports units", {
    expect_is(parse_integer(1, unit = "m"), "units")
    expect_is(parse_integer("1", unit = "m"), "units")
    expect_identical(parse_integer(1, unit = "m"), units::as.units(1L, units::parse_unit("m")))
    expect_identical(parse_integer("1", unit = "m"), units::as.units(1L, units::parse_unit("m")))
  })
}

# ---- Fields: Boolean ----
# http://specs.frictionlessdata.io/table-schema/#boolean

test_that("parse_booleans returns logical", {
  expect_identical(parse_boolean(TRUE), TRUE)
  expect_identical(parse_boolean(NA_character_), NA)
})

test_that("parse_boolean assumes default true values (true, True, TRUE, 1)", {
  expect_identical(parse_boolean(TRUE), TRUE)
  expect_identical(parse_boolean(1), TRUE)
  expect_identical(unique(parse_boolean(c("true", "True", "TRUE", "1"))), TRUE)
})

test_that("parse_boolean assumes default false values (false, False, FALSE, 0)", {
  expect_identical(parse_boolean(FALSE), FALSE)
  expect_identical(parse_boolean(0), FALSE)
  expect_identical(unique(parse_boolean(c("false", "False", "FALSE", "0"))), FALSE)
})

test_that("parse_boolean supports custom true and false values", {
  expect_identical(parse_boolean(c("T", "F"), trueValues = "T", falseValues = "F"), c(TRUE, FALSE))
  expect_identical(parse_boolean(c("1", "2"), trueValues = "1", falseValues = "2"), c(TRUE, FALSE))
  expect_identical(parse_boolean(c("1", "2"), trueValues = 1, falseValues = 2), c(TRUE, FALSE))
  expect_identical(parse_boolean(c(1, 2), trueValues = "1", falseValues = "2"), c(TRUE, FALSE))
  expect_identical(parse_boolean(c(1, 2), trueValues = 1, falseValues = 2), c(TRUE, FALSE))
  expect_warning(parse_boolean("TRUE", trueValues = ""))
  expect_warning(parse_boolean("FALSE", falseValues = ""))
})

# ---- Fields: Date ----
# http://specs.frictionlessdata.io/table-schema/#date

test_that("parse_date returns Date", {
  expect_identical(parse_date(Sys.Date()), Sys.Date())
  expect_identical(parse_date("2010-01-01"), as.Date("2010-01-01"))
  expect_identical(parse_date(NA), as.Date(NA))
})

test_that("parse_date returns datetimes as dates", {
  expect_identical(parse_date("2010-01-01T01", format = "%Y-%m-%dT%H"), as.Date("2010-01-01"))
})

test_that("parse_date assumes default format (%Y-%m-%d)", {
  expect_identical(parse_date("2010-01-01"), as.Date("2010-01-01"))
  expect_identical(parse_date("2010-01-01", format = "default"), as.Date("2010-01-01"))
  expect_identical(parse_date("2010-01-01", format = NULL), as.Date("2010-01-01"))
  expect_warning(parse_date("2010/01/01"))
})

test_that("parse_date supports custom format", {
  expect_identical(parse_date("2010/01/01", format = "%Y/%m/%d"), as.Date("2010-01-01"))
})

test_that("parse_date supports automatic format detection (very limited)", {
  expect_identical(parse_date("2010-01-01", format = "any"), as.Date("2010-01-01"))
  expect_identical(parse_date("2010/01/01", format = "any"), as.Date("2010-01-01"))
})

test_that("parse_date snaps to first day of year or month", {
  expect_identical(parse_date("2010-01", format = "%Y-%m"), as.Date("2010-01-01"))
  expect_identical(parse_date("2010", format = "%Y"), as.Date("2010-01-01"))
})

# ---- Fields: Datetime ----
# http://specs.frictionlessdata.io/table-schema/#datetime

as_POSIXct <- function(x, tz = "UTC") {
  as.POSIXct(as.POSIXlt(x, tz = tz))
}

test_that("parse_datetime returns POSIXct", {
  expect_identical(parse_datetime(as.POSIXct("2010-01-01", tz = "UTC")), as_POSIXct("2010-01-01"))
  expect_identical(parse_datetime("2010-01-01T00:00:00Z"), as_POSIXct("2010-01-01"))
  expect_identical(parse_datetime(NA), as_POSIXct(NA))
})

test_that("parse_datetime returns Date and POSIXlt as POSIXct", {
  expect_identical(parse_datetime(as.Date("2010-01-01")), as_POSIXct("2010-01-01"))
  expect_identical(parse_datetime(as.POSIXlt("2010-01-01", tz = "UTC")), as_POSIXct("2010-01-01"))
})

test_that("parse_datetime assumes default format (%Y-%m-%dT%H:%M:%SZ)", {
  expect_identical(parse_datetime("2010-01-01T01:01:01Z"), as_POSIXct("2010-01-01 01:01:01"))
  expect_identical(parse_datetime("2010-01-01T01:01:01Z", format = "default"), as_POSIXct("2010-01-01 01:01:01"))
  expect_identical(parse_datetime("2010-01-01T01:01:01Z", format = NULL), as_POSIXct("2010-01-01 01:01:01"))
  expect_warning(parse_datetime("2010-01-01 01:01:01"))
})

test_that("parse_datetime supports custom format", {
  expect_identical(parse_datetime("2010/01/01T01:01:01Z", format = "%Y/%m/%dT%H:%M:%SZ"), as_POSIXct("2010-01-01 01:01:01"))
})

test_that("parse_datetime supports automatic format detection (very limited)", {
  expect_identical(parse_datetime("2010-01-01T01:01:01Z", format = "any"), as_POSIXct("2010-01-01 01:01:01"))
  expect_identical(parse_datetime("2010-01-01 01:01:01", format = "any"), as_POSIXct("2010-01-01 01:01:01"))
})

test_that("parse_datetime snaps to start of interval", {
  expect_identical(parse_datetime("2010", format = "%Y"), as_POSIXct("2010-01-01 00:00:00"))
  expect_identical(parse_datetime("2010-01", format = "%Y-%m"), as_POSIXct("2010-01-01 00:00:00"))
  expect_identical(parse_datetime("2010-01-01", format = "%Y-%m-%d"), as_POSIXct("2010-01-01 00:00:00"))
  expect_identical(parse_datetime("2010-01-01T01", format = "%Y-%m-%dT%H"), as_POSIXct("2010-01-01 01:00:00"))
  expect_identical(parse_datetime("2010-01-01T01:01", format = "%Y-%m-%dT%H:%M"), as_POSIXct("2010-01-01 01:01:00"))
})

# ---- Dispatcher ----

test_that("parse_field defaults to string", {
  expect_equivalent(parse_field("hello"), "hello")
  expect_warning(parse_field("hello", meta = list(type = "unknown")))
})

test_that("parse_field sets field metadata to result", {
  expect_identical(parse_field("hello", meta = list()), set_field("hello", type = "string"))
  expect_identical(parse_field(c("T"), meta = list(type = "boolean", trueValues = "T")), set_field(TRUE, type = "boolean", trueValues = "T"))
})

# ---- Field list ----

fields <- list(x = "1", y = "1")
test_that("parse_fields passes arguments to parsing function", {
  expect_equivalent(parse_fields(fields, meta = list(list(name = "x", type = "boolean"), list(name = "y", type = "boolean"))), list(TRUE, TRUE))
  expect_equivalent(parse_fields(fields, meta = list(list(name = "x", type = "boolean"), list(name = "y", type = "number"))), list(TRUE, 1))
})
