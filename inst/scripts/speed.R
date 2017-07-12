devtools::load_all()
df <- data.frame(id = 1L, value = 1.1) %>%
  fields(
    id = field(name = "id", title = "Identifier"),
    value = field(name = "value", title = "Value")
  ) %>%
  resource(name = "test", format = "csv", schema = schema(missingValues = ""))
dp <- list(df) %>%
  package(name = "test-package", contributors = list(contributor(title = "Ethan Welty", role = "author"))) %>%
  package(contributors = list(contributor(title = "Ethan Welty", role = "maintainer")))
get_resource(df) %>% str()
get_package(dp) %>% str()

x <- rep(c(TRUE, FALSE), 1e3)
csv <- write_csv(data.frame(a = x))
microbenchmark::microbenchmark(
  data.table::fread(csv, na.strings = NULL, sep = ",", encoding = "UTF-8", colClasses = "character", data.table = TRUE, stringsAsFactors = FALSE),
  data.table::fread(csv, na.strings = NULL, sep = ",", encoding = "UTF-8", colClasses = "character", data.table = FALSE, stringsAsFactors = FALSE),
  data.table::fread(csv, na.strings = NULL, sep = ",", encoding = "UTF-8", colClasses = "logical", data.table = FALSE, stringsAsFactors = FALSE),
  dpkg::read_csv(csv, colClasses = "character"),
  dpkg::read_csv(csv, colClasses = "logical"),
  readr::read_csv(csv, col_types = "c", na = character()),
  readr::read_csv(csv, col_types = "l", na = character()),
  times = 10
)

x <- rep(c("TRUE", "FALSE", "NA"), 1e4)
values <- c("TRUE", "FALSE", "A", "B", "C", "1")
in_fast <- function(x, values) {
  n <- length(x)
  result <- x == values[1]
  for (y in values[-1]) {
    result <- result | x == y
  }
}
microbenchmark::microbenchmark(x %in% values, match(x, values), in_fast(x, values), times = 10)
