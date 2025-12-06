#' Retrieve Status and Details of a CoW Protocol Order
#'
#' Queries the CoW Protocol API for a specific order by its unique identifier (UID).
#' Returns comprehensive order information including current status, execution details,
#' trades, and metadata — ideal for monitoring, reconciliation, and analytics.
#'
#' @param uid Character string; the full 134-character order UID
#'   (starts with `0x`, 66 bytes hex-encoded). Example:
#'   `"0x1234...abcd5678...efgh100i..."`
#' @param conn Optional `cowr_connection` object. If `NULL` (default), uses the active
#'   connection set by [cowr_connect()].
#'
#' @return A named list with full order details if found, or `NULL` if the order
#'   does not exist or has been pruned. Key fields include:
#'   \item{uid}{Order identifier}
#'   \item{status}{Current status: `"open"`, `"fulfilled"`, `"cancelled"`, `"expired"`, etc.}
#'   \item{owner}{Address that created the order}
#'   \item{creationTime}{POSIXct timestamp of order creation}
#'   \item{trades}{List of settlement trades (if executed)}
#'   \item{executedSellAmount, executedBuyAmount}{Actual filled amounts}
#'   \item{invalidated}{Logical; whether order was cancelled}
#'   \item{meta$utm}{Developer attribution tags (if submitted via `cowr`)}
#'
#' @examples
#' # Connect first (only needed once per session)
#' cowr_connect("gnosis")
#'
#' # Check a real order (replace with a valid UID from cowr_get_orders())
#' status <- cowr_order_status("0x2a9e8...your-order-uid-here...1234")
#' if (!is.null(status)) {
#'   cat("Status:", status$status, "\n")
#'   print(status$executedBuyAmount)
#' }
#'
#' # Monitor your own recent order
#' my_orders <- cowr_get_orders(owner = "0xYourAddressHere", limit = 1)
#' if (nrow(my_orders) > 0) {
#'   latest <- cowr_order_status(my_orders$uid[1])
#'   print(latest$status)
#' }
#'
#' # Handle missing or expired orders gracefully
#' result <- cowr_order_status("0x0000000000000000000000000000000000000000000000000000000000000000")
#' if (is.null(result)) message("Order not found — may have never existed or been pruned.")
#'
#' # Use in a loop to monitor settlement (e.g., every 30 seconds)
#' \dontrun{
#' monitor_order <- function(uid, max_checks = 20) {
#'   for (i in seq_len(max_checks)) {
#'     s <- cowr_order_status(uid)
#'     if (is.null(s)) {
#'       message("Order no longer available")
#'       break
#'     }
#'     message(sprintf("[%s] Status: %s | Filled: %.2f%%",
#'             Sys.time(), s$status,
#'             100 * as.numeric(s$executedSellAmount) / as.numeric(s$sellAmount)))
#'     if (s$status %in% c("fulfilled", "cancelled", "expired")) break
#'     Sys.sleep(30)
#'   }
#' }
#' monitor_order("0xabc...")
#' }
#'
#' @seealso [cowr_get_orders()], [cowr_place_order()]
#'
#' @export
cowr_order_status <- function(uid, conn = NULL) {

  # Validate UID format early
  if (!is.character(uid) || length(uid) != 1) {
    stop("`uid` must be a single character string", call. = FALSE)
  }

  if (!grepl("^0x[0-9a-fA-F]{130}$", uid)) {
    stop("Invalid order UID format. Must be 134 characters starting with '0x' (66 bytes hex).",
         call. = FALSE)
  }

  conn <- .cowr_get_conn()
  url <- paste0(conn$api, "/orders/", uid)

  cli::cli_alert_info("Querying order status: {.field {substr(uid, 1, 10)}}...{substr(uid, nchar(uid)-8, nchar(uid))}")

  resp <- httr::GET(
    url = url,
    httr::user_agent("cowr R package (https://github.com/sawsimeon/cowr)"),
    httr::timeout(30)
  )

  # 404 = order not found (common and expected)
  if (httr::status_code(resp) == 404) {
    cli::cli_alert_warning("Order {.field {uid}} not found (may have been pruned or never existed)")
    return(NULL)
  }

  # All other errors → throw
  httr::stop_for_status(resp, task = paste("retrieve order", uid))

  raw_json <- httr::content(resp, as = "text", encoding = "UTF-8")
  order <- jsonlite::fromJSON(raw_json, flatten = TRUE)

  # Convert timestamps
  if (!is.null(order$creationTime)) {
    order$creationTime <- as.POSIXct(as.numeric(order$creationTime), origin = "1970-01-01", tz = "UTC")
  }
  if (!is.null(order$validTo)) {
    order$validTo <- as.POSIXct(as.numeric(order$validTo), origin = "1970-01-01", tz = "UTC")
  }

  # Friendly success message
  status_txt <- switch(order$status %||% "unknown",
                       open = cli::col_yellow("open"),
                       fulfilled = cli::col_green("fulfilled"),
                       cancelled = cli::col_red("cancelled"),
                       expired = cli::col_magenta("expired"),
                       presignaturePending = cli::col_cyan("awaiting presignature"),
                       order$status
  )

  cli::cli_alert_success(
    "Order status: {.strong {status_txt}} • ",
    "Owner: {.addr {substr(order$owner, 1, 10)}}... • ",
    "Created: {.timestamp {format(order$creationTime)}}"
  )

  # Return invisibly for programmatic use
  invisible(order)
}
