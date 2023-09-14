#' Authenticate
#'
#' Authenticates with CellEngine.
#'
#' There are two ways of authenticating:
#'
#' * Username and password. Use this to authenticate a user in a dynamic
#'   session.
#' * API token. This is the preferred method for unattended applications (such
#'   as LIMS integrations) because it allows a limited set of
#'   permissions to be granted to the application.
#'
#' These values can either be passed to the `authenticate()` function, or
#' provided through environment variables named `CELLENGINE_USERNAME`,
#' `CELLENGINE_PASSWORD` and `CELLENGINE_AUTH_TOKEN`. `otp` cannot be provided
#' as an environment variable due to its short lifespan.
#'
#' For security, you should not store your password in plain text, and you
#' should ensure that your R history does not store your password. Instead,
#' consider setting an environment variable (see examples).
#'
#' @param username Your username or email address. If omitted, not available
#' from the environment variable `CELLENGINE_USERNAME`, and running in an
#' interactive session, a prompt will be displayed.
#' @param password Your password. If omitted, not available from the environment
#' variable `CELLENGINE_PASSWORD`, and running in RStudio or the getPass library
#' is installed, a prompt will be displayed.
#' @param otp A one-time passcode, if your account is configured with two-factor
#' authentication. If omitted and in an interactive session, a prompt will be
#' displayed if required. Note that this must be a string.
#' @param token A Personal Access Token. See the [CellEngine
#' documentation](https://docs.cellengine.com/api/#authentication) for
#' more information. If provided, username, password, and otp will be ignored.
#' @export
#' @examples
#' \dontrun{
#' # Attempts to retrieve credentials from environment variables, falling back
#' # to prompting for username and password (and OTP if configured):
#' authenticate()
#'
#' # Attempts to retrieve credentials from environment variables, falling back
#' # to prompting for password (and OTP if configured):
#' authenticate("username")
#'
#' # Using an API token:
#' authenticate(token = "cep_abcdefghijklmnopqrstuvwxyz1234567890")
#' }
authenticate <- function(username = NA, password = NA, otp = NA, token = NA) {
  if (is.na(token)) {
    token <- Sys.getenv("CELLENGINE_AUTH_TOKEN", unset = NA)
  }

  if (!is.na(token)) {
    if (!is.character(token)) {
      stop("token must be a string")
    }

    pkg.env$auth <- c("Authorization" = paste0("Bearer ", token))
    return(invisible())
  }

  if (is.na(username)) {
    username <- Sys.getenv("CELLENGINE_USERNAME", unset = NA)
  }

  if (is.na(username)) {
    if (rstudioapi::isAvailable()) {
      username <- rstudioapi::showPrompt("Username", "Please enter your username or email")
    } else if (interactive()) {
      username <- readline("Please enter your username or email: ")
    } else {
      stop("Username is required, but was not provided and no interactive prompts are available")
    }
  }

  if (is.na(password)) {
    password <- Sys.getenv("CELLENGINE_PASSWORD", unset = NA)
  }

  if (is.na(password)) {
    if (requireNamespace("getPass", quietly = TRUE)) {
      password <- getPass::getPass(msg = "Please enter your password", noblank = TRUE)
    } else if (rstudioapi::isAvailable()) {
      password <- rstudioapi::askForPassword()
    } else {
      stop("Password is required, but was not provided and no interactive prompts are available")
    }
  }

  body <- list(
    username = jsonlite::unbox(username),
    password = jsonlite::unbox(password)
  )

  if (!is.na(otp)) {
    if (!is.character(otp)) {
      stop("OTP must be a string")
    }

    body$otp <- jsonlite::unbox(otp)
  }

  ensureBaseUrl()
  fullURL <- paste0(pkg.env$baseURL, "/api/v1/signin")
  r <- httr::POST(fullURL,
    body = jsonlite::toJSON(body),
    httr::content_type_json(), httr::user_agent(ua)
  )

  if (httr::status_code(r) == 200) {
    return(invisible())
  }

  content <- httr::content(r, "text", encoding = "UTF-8")
  parsed <- jsonlite::fromJSON(content)

  if (httr::status_code(r) == 400 && parsed$error == '"otp" is required.') {
    if (requireNamespace("getPass", quietly = TRUE)) {
      otp <- getPass::getPass(msg = "Please enter your one-time code", noblank = TRUE)
    } else if (rstudioapi::isAvailable()) {
      otp <- rstudioapi::askForPassword("Please enter your one-time code")
    } else if (interactive()) {
      otp <- readline("Please enter your one-time code: ")
    } else {
      stop("OTP is required, but was not provided and no interactive prompts are available")
    }
    authenticate(username, password, otp)
  } else {
    httr::stop_for_status(r)
  }
}
