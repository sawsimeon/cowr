#' Retrieve Recent Orders from CoW Protocol
#'
#' Fetches orders from the CoW Protocol production API. Returns a tidy `tibble`
#' with all relevant order details — perfect for analysis, monitoring, or backtesting.
#'
#' @param limit Integer; maximum number of orders to return (max 1000, API limit).
#' @param status Character; filter by order status. Options: `"open"`, `"fulfilled"`,
#'   `"cancelled"`, `"expired"`, `"presignaturePending"`, or `NULL` for all.
#' @param kind Character; filter by order kind: `"sell"` or `"buy"`.
#' @param owner Character; Ethereum address (checksummed or lowercase) to filter by owner.
#' @param conn A `cowr_connection` object. If `NULL` (default), uses the active connection.
#'
#' @return A `tibble` with one row per order and columns including:
#'   \item{uid}{Order UID (unique identifier)}
#'   \item{owner}{Owner address}
#'   \item{sellToken}{Sell token address}
#'   \item{buyToken}{Buy token address}
#'   \item{sellAmount}{Amount to sell (raw)}
#'   \item{buyAmount}{Amount to buy (raw)}
#'   \item{creationTime}{POSIXct timestamp}
#'   \item{status}{Current status}
#'   \item{kind}{"sell" or "buy"}
#'   \item{isPartiallyFillable}{Logical}
#'
#' @examples
#' # Connect first (only needed once per session)
#' cowr_connect("gnosis")
#'
#' # Get 20 most recent open sell orders
#' open_orders <- cowr_get_orders(limit = 20, status = "open", kind = "sell")
#' print(open_orders)
#'
#' # Get all fulfilled orders in the last hour (advanced filtering via API)
#' recent_fills <- cowr_get_orders(limit = 100, status = "fulfilled")
#' dplyr::glimpse(recent_fills)
#'
#' # Filter by a specific trader
#' my_orders <- cowr_get_orders(owner = "0x1234...abcd")
#'
#' @export
cowr_get_orders <- function(
    limit = 100,
    status = NULL,
    kind = NULL,
    owner = NULL,
    conn = NULL
) {
  conn <- .cowr_get_conn()
  if (is.null(conn)) stop("No active connection. Run cowr_connect() first.")

  limit <- min(max(as.integer(limit), 1), 1000)

  # Build query parameters
  params <- list()
  if (!is.null(status)) params$status <- match.arg(status,
                                                   c("open", "fulfilled", "cancelled", "expired", "presignaturePending"))
  if (!is.null(kind))   params$kind   <- match.arg(kind, c("sell", "buy"))
  if (!is.null(owner))  params$owner  <- tolower(owner)

  params$limit <- limit

  cli::cli_alert_info("Fetching up to {limit} orders from {conn$name}...")

  resp <- httr::GET(
    url = paste0(conn$api, "/orders/"),
    query = params,
    httr::user_agent("cowr R package (https://github.com/sawsimeon/cowr)")
  )

  httr::stop_for_status(resp)

  raw <- httr::content(resp, as = "text", encoding = "UTF-8")
  data <- jsonlite::fromJSON(raw, flatten = TRUE)

  if (length(data) == 0) {
    cli::cli_alert_warning("No orders found matching criteria.")
    return(tibble::tibble())
  }

  df <- tibble::as_tibble(data)

  # Standardize column types
  if ("creationTime" %in% names(df)) {
    df$creationTime <- as.POSIXct(as.numeric(df$creationTime), origin = "1970-01-01", tz = "UTC")
  }
  if ("validTo" %in% names(df)) {
    df$validTo <- as.POSIXct(as.numeric(df$validTo), origin = "1970-01-01", tz = "UTC")
  }

  df <- df %>%
    dplyr::select(
      uid, owner, sellToken, buyToken, sellAmount, buyAmount,
      creationTime, status, kind, partiallyFillable = isPartiallyFillable,
      dplyr::everything()
    )

  cli::cli_alert_success("Retrieved {nrow(df)} orders")
  df
}


#' Get Status of a Specific Order
#'
#' Retrieves the current status and details of a single order by its UID.
#'
#' @param uid Character; the full order UID (64-char hex string starting with `0x`).
#' @param conn A `cowr_connection` object (optional).
#'
#' @return A list with order details, or `NULL` if not found.
#'
#' @examples
#' status <- cowr_order_status("0x1234...abcd")
#' if (!is.null(status)) print(status$status)
#'
#' @export
cowr_order_status <- function(uid, conn = NULL) {
  conn <- .cowr_get_conn()
  if (!grepl("^0x[0-9a-fA-F]{130}$", uid)) {
    stop("Invalid order UID format")
  }

  resp <- httr::GET(
    url = paste0(conn$api, "/orders/", uid),
    httr::user_agent("cowr R package")
  )

  if (httr::status_code(resp) == 404) {
    cli::cli_alert_warning("Order {uid} not found")
    return(NULL)
  }

  httr::stop_for_status(resp)
  jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
}
