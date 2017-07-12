context("From/To formats")

# ---- JSON ----

x <- list(
  null = NULL,
  empty = "",
  character_chr = "abc",
  character_num = "123",
  numeric = 1.23,
  integer = as.integer(1),
  logical = TRUE,
  array_empty = list(),
  array_single = list("a"),
  array = list("a", "b"),
  object = list(a = TRUE, b = FALSE)
)
txt <- '{
  "null": null,
  "empty": "\",
  "character_chr": "abc",
  "character_num": "123",
  "numeric": 1.23,
  "integer": 1,
  "logical": true,
  "array_empty": [],
  "array_single": ["a"],
  "array": ["a", "b"],
  "object": {"a": true, "b": false}
}'

test_that("read_json returns the expected object", {
  expect_identical(read_json(txt), x)
})

test_that("read_json(write_json) returns the original object", {
  expect_identical(read_json(write_json(x)), x)
})

# ---- JSON Table ----

txt_rows <- '[
  ["a", "b", "c"],
  [1, 2, 3],
  [4, 5, 6]
]'
txt_objects <- '[
  {"a": 1, "b": 2, "c": 3},
  {"a": 4, "b": 5, "c": 6}
]'
x <- data.frame(
  a = c(1L, 4L),
  b = c(2L, 5L),
  c = c(3L, 6L)
)

test_that("read_json works for tabular data", {
  expect_identical(read_json(txt_rows, tabular = TRUE), x)
  expect_identical(read_json(txt_objects, tabular = TRUE), x)
})

test_that("read_json(write_json) works for tabular data", {
  expect_identical(read_json(write_json(x), tabular = TRUE), x)
})

# ---- JSON Table (single row) ----

txt_rows <- '[
["a", "b", "c"],
[1, 2, 3]
]'
txt_objects <- '[
{"a": 1, "b": 2, "c": 3}
]'
x <- data.frame(
  a = c(1L),
  b = c(2L),
  c = c(3L)
)

test_that("read_json works for tabular data", {
  expect_identical(read_json(txt_rows, tabular = TRUE), x)
  expect_identical(read_json(txt_objects, tabular = TRUE), x)
})

test_that("read_json(write_json) works for tabular data", {
  expect_identical(read_json(write_json(x), tabular = TRUE), x)
})

# ---- CSV ----

txt <- '
empty,character,numeric,integer,logical
,abc,1.23,1,TRUE
'
x <- data.frame(
  empty = NA_integer_, # data.table::fread default
  # empty_quotes = "", # not yet supported
  character = "abc",
  numeric = 1.23,
  integer = as.integer(1),
  logical = TRUE,
  stringsAsFactors = FALSE
)

test_that("read_csv returns the expected object", {
  expect_identical(read_csv(txt), x)
})

test_that("read_csv(write_csv) returns the original object", {
  expect_identical(read_csv(write_csv(x)), x)
})
