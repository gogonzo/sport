#' Coerce a rating model to a plain list
#'
#' Returns a JSON-friendly representation of a `"ratings"` object: rating
#' vectors and metadata only. The cached `coefficients` matrix and any
#' `trajectory` / `pairs` data.tables are dropped -- they are either regenerable
#' (`coefficients`) or too large to store as model parameters. The `formula` is
#' deparsed to a character string so the result serializes cleanly with
#' `jsonlite::toJSON()` or similar.
#'
#' Use [as.ratings()] (the `list` method) to reconstruct a `"ratings"` object
#' from the result.
#'
#' @param x a `"ratings"` object.
#' @param ... ignored.
#'
#' @return A plain list with elements `method`, `formula` (character),
#'   `settings`, `r`, `rd`, and (for glicko2) `sigma`.
#'
#' @examples
#' m <- glicko_run(rank | id ~ player(rider), data = gpheats[1:100, ])
#' params <- as.list(m)
#' str(params)
#'
#' # Round-trip
#' m2 <- as.ratings(params)
#' identical(m$r, m2$r)
#'
#' \dontrun{
#' # Persist to a database as JSON.
#' # Pass `digits = NA` so jsonlite preserves full floating-point precision --
#' # the default of 4 digits will quantise the ratings.
#' payload <- jsonlite::toJSON(as.list(m), auto_unbox = TRUE, digits = NA)
#' DBI::dbWriteTable(con, "models", data.frame(id = 1L, payload = payload))
#' }
#'
#' @seealso [as.ratings()], [coef.ratings()]
#' @export
as.list.ratings <- function(x, ...) {
  formula <- attr(x, "formula")
  # Convert named numerics to named lists so jsonlite::toJSON serializes
  # them as JSON objects (preserving names) rather than bare arrays.
  out <- list(
    method   = attr(x, "method"),
    formula  = paste(deparse(formula), collapse = " "),
    settings = attr(x, "settings"),
    r        = as.list(x$r),
    rd       = as.list(x$rd)
  )
  if (!is.null(x$sigma)) out$sigma <- as.list(x$sigma)
  out
}
