#' Authenticate
#'
#' Authenticates with the server.
#'
#' For security, you should not store your password in plain text, and you
#' should ensure that your R history does not store your password. Instead,
#' consider setting an environment variable (see examples).
#'
#' @param username Your username.
#' @param password Your password. If omitted and running in RStudio or the
#' getPass library is installed, a prompt will be displayed.
#' @param otp A one-time passcode, if your account is configured with two-factor
#' authentication. If omitted and in an interactive session, a prompt will be
#' displayed. Note that this must be a string.
#' @export
#' @examples
#' \dontrun{
#' # Use with caution, don't let prying eyes see your password.
#' authenticate("username", "password")
#' # Instead, you could store it in an environment variable.
#' authenticate("username", Sys.getenv("API_PASSWORD"))
#'
#' # If the password is omitted and you're running in RStudio or the getPass
#' library is installed, a prompt will be displayed.
#' authenticate("username")
#' }
authenticate = function(username, password=NA, otp=NA) {
  if (is.na(password)) {
    if (requireNamespace("getPass")) {
      password = getPass::getPass(msg = "Please enter your password", noblank=T)
    } else if (rstudioapi::isAvailable()) {
      password = rstudioapi.askForPassword()
    }
  }

  body = list(
    username = jsonlite::unbox(username),
    password = jsonlite::unbox(password)
  )

  if (!is.na(otp)) {
    if (!is.character(otp))
      stop("OTP must be a string")

    body$otp = jsonlite::unbox(otp)
  }

  ensureBaseUrl()
  fullURL = paste(pkg.env$baseURL, "signin", sep = "/")
  r = httr::POST(fullURL, body = jsonlite::toJSON(body),
    httr::content_type_json(), httr::user_agent(ua))

  if (httr::status_code(r) == 200)
    return(invisible())

  content = httr::content(r, "text", encoding = "UTF-8")
  parsed = jsonlite::fromJSON(content)

  if (httr::status_code(r) == 400 && parsed$error == '"otp" is required.') {
    if (requireNamespace("getPass")) {
      otp = getPass::getPass(msg = "Please enter your one-time code", noblank=T)
    } else if (rstudioapi::isAvailable()) {
      otp = rstudioapi.askForPassword("Please enter your one-time code")
    } else if (interactive()) {
      otp = readline("Please enter your one-time code");
    }
    authenticate(username, password, otp)
  } else {
    httr::stop_for_status(r)
  }
}
