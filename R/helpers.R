#' List arguments in call to parent function
#' @param ... Additional arguments passed to calling function.
get_called_args <- function(...) {
  envir <- parent.frame()
  called <- names(match.call(
    definition = sys.function(sys.parent()),
    call = sys.call(sys.parent())
  ))[-1]
  c(as.list(envir), list(...)) %>%
    extract(names(.) %in% called)
}

#' Return first non-null argument
#' @param ... Objects.
coalesce <- function(...) {
  Find(Negate(is.null), list(...))
}

#' Check whether string is a URL
#' @param string (character) String to test.
is_url <- function(string) {
  grepl("://", string)
}

#' Modify object list attribute
#' TODO: Set attribute by reference.
#' @param x Object.
#' @param which (character) Name of attribute to modify.
#' @param value (list) New attribute value.
modify_attr <- function(x, which, value) {
  old_value <- attr(x, which, exact = TRUE)
  if (!is.null(old_value)) {
    value %<>% modify_list(old_value, .)
  }
  attr(x, which) <- value
  x
}

#' Modify elements of a list
#' TODO: Modify nameless by position (delete with NULL) and append beyond length
#' @param x (list)
#' @param val (list)
#' @param keep.null (logical) Whether \code{NULL} elements in \code{val} become \code{NULL} elements in \code{x} or are deleted.
#' @param replace.nameless (logical) Whether nameless list elements of \code{x} are replaced by corresponding elements of \code{val}.
modify_list <- function(x, val, keep.null = FALSE, replace.nameless = TRUE) {
  stopifnot(
    is.list(x),
    is.list(val)
  )
  xnames <- names(x)
  vnames <- names(val)
  if (replace.nameless && all(!nzchar(xnames))) {
    return(val)
  }
  if (keep.null) {
    for (v in vnames) {
      if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]])) {
        x[v] <- list(modify_list(x[[v]], val[[v]], keep.null = keep.null, replace.nameless = replace.nameless))
      } else {
        x[v] <- val[v]
      }
    }
  } else {
    for (v in vnames) {
      if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]])) {
        x[[v]] <- modify_list(x[[v]], val[[v]], keep.null = keep.null, replace.nameless = replace.nameless)
      } else {
        x[[v]] <- val[[v]]
      }
    }
  }
  x
}

#' Set object metadata by name
#' @param x Object.
#' @param fun (function) Set function.
#' @param ... (name = value) Element names of \code{x} equal to list of properties to set.
set_by_name <- function(x, fun, ...) {
  args <- list(...)
  for (i in seq_along(args)) {
    name <- names(args)[i]
    if (is.null(name) || !name %in% names(x)) {
      stop(paste("Name not found:", name))
    }
    x[[name]] <- do.call(match.fun(fun), c(list(x[[name]]), args[[name]]))
  }
  x
}

#' Convert GitHub repository address to raw url
#'
#' @param repo Repository address in the format \code{username/repo[/subdir][@ref]}, where \code{ref} can be a commit, tag, or branch name. Defaults to "master".
github_raw_url <- function(repo) {
  m <- regexec("^/*([^/]+)/([^/@#]+)/*([^/@#]+)*/*(?:@(.*))*/*$", repo)
  regmatches(repo, m) %>%
    unlist() %>%
    extract(c(2, 3, 5, 4)) %>%
    ifelse(. == "", c(NA, NA, "master", NA), .) %>%
    Filter(Negate(is.na), .) %>%
    {do.call(paste, as.list(c("https://raw.githubusercontent.com", ., sep = "/")))}
}

#' Check whether an object is empty or contains NA or ""
#' @param x Object.
is_empty <- function(x) {
  if (length(x) == 0) {
    TRUE
  } else {
    if (is.character(x)) {
      is.na(x) | nchar(x) == 0
    } else {
      is.na(x)
    }
  }
}

#' Check whether an object is a list (but not a data.frame)
#' @param x Object.
is_list_not_df <- function(x) {
  inherits(x, "list") && !inherits(x, "data.frame")
}
