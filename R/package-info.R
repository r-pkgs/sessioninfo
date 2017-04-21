
#' Information about the currently loaded packages, or about a chosen set
#'
#' @param pkgs Either a vector of package names or NULL. If \code{NULL},
#'   displays all loaded packages. If a character vector, also, includes
#'   all dependencies of the package.
#' @param include_base Include base packages in summary? By default this is
#'   false since base packages should always match the R version.
#' @return A data frame with columns:
#'   * `package`: package name.
#'   * `version`: package version. This is the version of the loaded
#'     namespace if `pkgs` is `NULL`, and it is the version of the package
#'     on disk otherwise. The two of them are almost always the same,
#'     though.
#'   * `path`: path to the package on disk.
#'   * `attached`: logical, whether the package is attached to the search
#'     path.
#'   * `is_base`: logical, whether the package is a base package.
#'   * `date`: the date the package was installed or built.
#'   * `source`: where the package was installed from. E.g.
#'     `CRAN`, `GitHub`, `local` (from the local machine), etc.
#'
#' @export
#' @examples
#' package_info()
#' package_info("sessioninfo")

package_info <- function(pkgs = NULL, include_base = FALSE) {

  if (is.null(pkgs)) {
    pkgs <- loaded_packages()
  } else {
    pkgs <- dependent_packages(pkgs)
  }

  desc <- lapply(pkgs$package, utils::packageDescription)

  pkgs$is_base <- vapply(
    desc, function(x) identical(x[["Priority"]], "base"), logical(1)
  )

  pkgs$date <- vapply(desc, pkg_date, character(1))
  pkgs$source <- vapply(desc, pkg_source, character(1))

  if (!include_base) pkgs <- pkgs[! pkgs$is_base, ]

  class(pkgs) <- c("packages_info", "data.frame")
  pkgs
}

pkg_date <- function (desc) {
  if (!is.null(desc$`Date/Publication`)) {
    date <- desc$`Date/Publication`

  } else if (!is.null(desc$Built)) {
    built <- strsplit(desc$Built, "; ")[[1]]
    date <- built[3]

  } else {
    date <- NA_character_
  }

  as.character(as.Date(strptime(date, "%Y-%m-%d")))
}

pkg_source <- function(desc) {

  if (!is.null(desc$GithubSHA1)) {
    str <- paste0("Github (",
                  desc$GithubUsername, "/",
                  desc$GithubRepo, "@",
                  substr(desc$GithubSHA1, 1, 7), ")")
  } else if (!is.null(desc$RemoteType)) {
    # want to generate these:
    # remoteType (username/repo@commit)
    # remoteType (username/repo)
    # remoteType (@commit)
    # remoteType
    remote_type <- desc$RemoteType

    # RemoteUsername and RemoteRepo should always be present together
    if (!is.null(desc$RemoteUsername) && (!is.null(desc$RemoteRepo))) {
      user_repo <- paste0(desc$RemoteUsername, "/", desc$RemoteRepo)
    } else {
      user_repo <- NULL
    }

    if (!is.null(desc$RemoteSha)) {
      sha <- paste0("@", substr(desc$RemoteSha, 1, 7))
    } else {
      sha <- NULL
    }

    # in order to fulfill the expectation of formatting, we paste the user_repo
    # and sha together
    if (!is.null(user_repo) || !is.null(sha)) {
      user_repo_and_sha <- paste0(" (", user_repo, sha, ")")
    } else {
      user_repo_and_sha <- NULL
    }

    str <- paste0(remote_type, user_repo_and_sha)

  } else if (!is.null(desc$Repository)) {
    repo <- desc$Repository

    if (!is.null(desc$Built)) {
      built <- strsplit(desc$Built, "; ")[[1]]
      ver <- sub("$R ", "", built[1])

      repo <- paste0(repo, " (", ver, ")")
    }

    repo

  } else if (!is.null(desc$biocViews)) {
    "Bioconductor"
  } else {
    "local"
  }
}

#' @export

print.packages_info <- function(x, ...) {

  px <- data.frame(
    package = x$package,
    "*"     = ifelse(x$attached, "*", ""),
    version = x$version,
    date    = x$date,
    source  = x$source,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  print.data.frame(px, right = FALSE, row.names = FALSE)
}
