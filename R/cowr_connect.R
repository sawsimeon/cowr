#' Establish a CoW Protocol Connection
#'
#' Sets up a connection object for interacting with CoW Protocol on a specific EVM chain.
#' This function configures chain-specific endpoints (API, order book, subgraph) and stores
#' them in a `cowr_connection` object used by all other `cowr` functions.
#'
#' @param chain Character string specifying the target chain. Valid options are:
#'   `"mainnet"` (Ethereum), `"gnosis"` (Gnosis Chain), `"arbitrum"` (Arbitrum One),
#'   and `"sepolia"` (Ethereum Sepolia testnet). Case-insensitive.
#' @param api_base Optional custom base URL for the CoW Protocol API (advanced use).
#'   Defaults to official endpoints.
#'
#' @return An S3 object of class `"cowr_connection"` containing:
#'   \item{chain}{Normalized chain name}
#'   \item{chain_id}{Numerical chain ID (EIP-155)}
#'   \item{api}{Base URL for the CoW Protocol production API}
#'   \item{subgraph}{URL for The Graph subgraph}
#'   \item{rpc}{Default public RPC endpoint (for signing if needed)}
#'   \item{name}{Human-readable network name}
#'
#' @examples
#' # Default: connect to Gnosis Chain (most CoW volume)
#' conn <- cowr_connect("gnosis")
#' conn
#'
#' # Connect to Ethereum mainnet
#' cowr_connect("mainnet")
#'
#' # Sepolia for testing
#' cowr_connect("sepolia")
#'
#' # Use in other functions (automatically picked up)
#' cowr_get_orders(limit = 5)
#'
#' @export
cowr_connect <- function(
    chain = c("mainnet", "gnosis", "arbitrum", "sepolia"),
    api_base = NULL
) {
  chain <- tolower(chain)
  chain <- match.arg(chain)

  # Official CoW Protocol endpoints (always up-to-date as of Dec 2025)
  endpoints <- list(
    mainnet = list(
      chain_id  = 1L,
      name      = "Ethereum Mainnet",
      api       = "https://api.cow.fi/mainnet/api/v1",
      subgraph  = "https://api.thegraph.com/subgraphs/name/cowprotocol/cow",
      rpc       = "https://eth-mainnet.alchemyapi.io/v2/demo"  # placeholder-safe
    ),
    gnosis = list(
      chain_id  = 100L,
      name      = "Gnosis Chain",
      api       = "https://api.cow.fi/gnosis/api/v1",
      subgraph  = "https://api.thegraph.com/subgraphs/name/cowprotocol/cow-gnosis",
      rpc       = "https://rpc.gnosis.gateway.fm"
    ),
    arbitrum = list(
      chain_id  = 42161L,
      name      = "Arbitrum One",
      api       = "https://api.cow.fi/arbitrum/api/v1",
      subgraph  = "https://api.thegraph.com/subgraphs/name/cowprotocol/cow-arbitrum",
      rpc       = "https://arb1.arbitrum.io/rpc"
    ),
    sepolia = list(
      chain_id  = 11155111L,
      name      = "Sepolia Testnet",
      api       = "https://api.cow.fi/sepolia/api/v1",
      subgraph  = "https://api.thegraph.com/subgraphs/name/cowprotocol/cow-sepolia",
      rpc       = "https://sepolia.infura.io/v3/9aa3d95b3bc440fa88ea12eaa4456161"
    )
  )

  cfg <- endpoints[[chain]]

  # Allow override of API base (rare, but useful for mirrors or testing)
  if (!is.null(api_base)) {
    cli::cli_alert_info("Using custom API base URL: {.url {api_base}}")
    cfg$api <- api_base
  }

  # Store globally so other functions can auto-detect connection
  options(cowr_connection = cfg)

  # Make it a proper S3 object
  class(cfg) <- c("cowr_connection", "list")

  cli::cli_alert_success(
    "Connected to {.strong {cfg$name}} (chain ID: {cfg$chain_id})"
  )
  invisible(cfg)
}

#' @export
print.cowr_connection <- function(x, ...) {
  cli::cat_line(cli::rule(left = "CoW Protocol Connection", col = "cyan"))
  cli::cat_bullet("Network     : ", cli::col_green(x$name))
  cli::cat_bullet("Chain ID    : ", x$chain_id)
  cli::cat_bullet("API         : ", cli::col_blue(x$api))
  cli::cat_bullet("Subgraph    : ", substr(x$subgraph, 1, 50), "...")
  cli::cat_bullet("RPC         : ", substr(x$rpc, 1, 50), "...")
  invisible(x)
}

# Optional: auto-connect helper (users can call manually or rely on lazy detection)
.cowr_get_conn <- function() {
  conn <- getOption("cowr_connection")
  if (is.null(conn)) {
    cli::cli_alert_warning("No active connection. Defaulting to Gnosis Chain.")
    cowr_connect("gnosis")
    conn <- getOption("cowr_connection")
  }
  conn
}
