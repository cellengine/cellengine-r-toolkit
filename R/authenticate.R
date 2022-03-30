#' Authenticate
#'
#' Authenticates with the server.
#'
#' For security, you should not store your password in plain text, and you
#' should ensure that your R history does not store your password. Instead,
#' consider setting an environment variable (see examples).
#'
#' @param username Your username or email address. If omitted and in an
#' interactive session, a prompt will be displayed.
#' @param password Your password. If omitted and running in RStudio or the
#' getPass library is installed, a prompt will be displayed.
#' @param otp A one-time passcode, if your account is configured with two-factor
#' authentication. If omitted and in an interactive session, a prompt will be
#' displayed. Note that this must be a string.
#' @export
#' @examples
#' \dontrun{
#' authenticate() # prompts for username and password (and OTP if configured)
#' authenticate("username") # prompts for password (and OTP if configured)
#'
#' # For non-interactive sessions, use an environment variable or other means to
#' # avoid saving your password in plaintext.
#' authenticate("username", Sys.getenv("API_PASSWORD"))
#' }
authenticate <- function(username = NA, password = NA, otp = NA) {
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
    if (requireNamespace("getPass", quietly = TRUE)) {
      password <- getPass::getPass(msg = "Please enter your password", noblank = T)
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
  fullURL <- paste(pkg.env$baseURL, "signin", sep = "/")
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
      otp <- getPass::getPass(msg = "Please enter your one-time code", noblank = T)
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
