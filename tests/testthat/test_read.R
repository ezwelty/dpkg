context("Read-write package")

# ---- Simple ----

dr <- data.frame(
  string = set_field("hello", title = "Hello world"),
  numeric = 1.1,
  integer = 1L,
  units = units::as.units(1.1, units::parse_unit("m s-1")),
  logical = TRUE,
  Date = Sys.Date(),
  POSIXct = as.POSIXct("1970-01-01 00:00:01", tz = "UTC"), # hardcode for equality testing
  stringsAsFactors = FALSE
) %>%
  set_resource(
    name = "data",
    profile = "data-resource"
  )
for (i in seq_along(dr)) {
  dr[[i]] %<>% set_field() # cast all elements to dpkg_field for equality testing
}
dp <- list(dr) %>%
  set_package(
    name = "test",
    profile = "data-package"
  )
dir <- tempdir()

test_that("read_package(write_package) returns an equivalent package (csv file)", {
  write_package(dp, path = dir)
  dp2 <- read_package(path = dir)
  expect_equivalent(dp, dp2)
  expect_identical(get_package(dp), get_package(dp2))
})

test_that("read_package(write_package) returns an equivalent package (csv inline string)", {
  dp[[1]] %<>% set_resource(path = NULL, format = "csv")
  write_package(dp, path = dir, inline_data = TRUE)
  dp2 <- read_package(path = dir)
  expect_equivalent(dp, dp2)
  expect_identical(get_package(dp, inline_data = TRUE), get_package(dp2, inline_data = TRUE))
  expect_identical(get_package(dp, inline_data = FALSE), get_package(dp2, inline_data = FALSE))
})

test_that("read_package(write_package) returns an equivalent package (json inline object)", {
  dp[[1]] %<>% set_resource(path = NULL, format = "json")
  write_package(dp, path = dir, inline_data = TRUE)
  dp2 <- read_package(path = dir)
  expect_equivalent(dp, dp2)
  expect_identical(get_package(dp, inline_data = TRUE), get_package(dp2, inline_data = TRUE))
  expect_identical(get_package(dp, inline_data = FALSE), get_package(dp2, inline_data = FALSE))
})

# ---- Complex ----

schema <- list(
  fields = list(
    list(
      name = "id",
      type = "integer"
    ),
    list(
      name = "value",
      type = "number",
      unit = "m/s"
    )
  )
)
meta <- list(
  name = "test",
  resources = list(
    list(
      name = "csvdata",
      path = "data/data.csv",
      schema = "schema.json"
    ),
    list(
      name = "jsondata",
      data = list(
        list(id = 1L, value = 1.1),
        list(id = 2L, value = 2.2)
      ),
      schema = schema
    )
  )
)
csvdata <- data.frame(
  id = c(1L, 2L),
  value = c(1.1, 2.2)
)
dir <- tempdir()
write_json(meta, file.path(dir, "datapackage.json"))
write_json(schema, file.path(dir, "schema.json"))
suppressWarnings(dir.create(file.path(dir, "data")))
write_csv(csvdata, file.path(dir, "data", "data.csv"))

# ---- Metadata ----

test_that("read_meta reads external json schemas", {
  meta$resources[[1]]$schema <- schema
  expect_identical(read_meta(dir), meta)
})

# ---- Data format specifications ----

test_that("parse_resource_format supports csv", {
  expect_identical(parse_resource_format(list(format = "csv")), "csv")
  expect_identical(parse_resource_format(list(mediatype = "text/csv")), "csv")
  expect_identical(parse_resource_format(list(path = "data.csv")), "csv")
})

test_that("parse_resource_format supports json", {
  expect_identical(parse_resource_format(list(format = "json")), "json")
  expect_identical(parse_resource_format(list(mediatype = "application/json")), "json")
  expect_identical(parse_resource_format(list(path = "data.json")), "json")
})

test_that("parse_resource_format rejects unsupported formats", {
  expect_error(parse_resource_format(list(format = "xls")))
  expect_error(parse_resource_format(list(mediatype = "application/vnd.ms-excel")))
})

test_that("parse_resource_format rejects inconsistent formats", {
  expect_identical(parse_resource_format(list(format = "csv", path = "data")), "csv")
  expect_error(parse_resource_format(list(format = "csv", mediatype = "application/json")))
  expect_error(parse_resource_format(list(path = c("data.csv", "data.json"))))
})
