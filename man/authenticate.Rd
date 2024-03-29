% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/authenticate.R
\name{authenticate}
\alias{authenticate}
\title{Authenticate}
\usage{
authenticate(username = NA, password = NA, otp = NA, token = NA)
}
\arguments{
\item{username}{Your username or email address. If omitted, not available
from the environment variable \code{CELLENGINE_USERNAME}, and running in an
interactive session, a prompt will be displayed.}

\item{password}{Your password. If omitted, not available from the environment
variable \code{CELLENGINE_PASSWORD}, and running in RStudio or the getPass library
is installed, a prompt will be displayed.}

\item{otp}{A one-time passcode, if your account is configured with two-factor
authentication. If omitted and in an interactive session, a prompt will be
displayed if required. Note that this must be a string.}

\item{token}{A Personal Access Token. See the \href{https://docs.cellengine.com/api/#authentication}{CellEngine documentation} for
more information. If provided, username, password, and otp will be ignored.}
}
\description{
Authenticates with CellEngine.
}
\details{
There are two ways of authenticating:
\itemize{
\item Username and password. Use this to authenticate a user in a dynamic
session.
\item API token. This is the preferred method for unattended applications (such
as LIMS integrations) because it allows a limited set of
permissions to be granted to the application.
}

These values can either be passed to the \code{authenticate()} function, or
provided through environment variables named \code{CELLENGINE_USERNAME},
\code{CELLENGINE_PASSWORD} and \code{CELLENGINE_AUTH_TOKEN}. \code{otp} cannot be provided
as an environment variable due to its short lifespan.

For security, you should not store your password in plain text, and you
should ensure that your R history does not store your password. Instead,
consider setting an environment variable (see examples).
}
\examples{
\dontrun{
# Attempts to retrieve credentials from environment variables, falling back
# to prompting for username and password (and OTP if configured):
authenticate()

# Attempts to retrieve credentials from environment variables, falling back
# to prompting for password (and OTP if configured):
authenticate("username")

# Using an API token:
authenticate(token = "cep_abcdefghijklmnopqrstuvwxyz1234567890")
}
}
