.ip2locationio_env <- new.env(parent = emptyenv())

#' @title Set IP2Location.io API key
#'
#' @description Set IP2Location.io API key for lookup. Free API key can be obtained from <https://www.ip2location.io/sign-up?ref=1/>
#' @param api_key IP2Location.io API key
#' @return No return value, called for side effects.
#' @import reticulate
#' @export
#' @examples \dontrun{
#' setApiKey("YOUR_API_KEY")
#' }
#'

setApiKey <- function(api_key) {
  .ip2locationio_env$ip2locationio <- reticulate::import("ip2locationio")
  .ip2locationio_env$configuration <- .ip2locationio_env$ip2locationio$Configuration(api_key)
}

#' @title Get the current IP2Location.io configuration
#'
#' @description Retrieve the Python configuration object created by \code{setApiKey()}. Internal helper used by the lookup functions to ensure an API key has been set before making a request.
#' @return Return the Python configuration object used for API calls
#' @keywords internal
#' @noRd
#'

.getConfiguration <- function() {
  if (is.null(.ip2locationio_env$configuration)) {
    stop("API key not set. Please call setApiKey() first.")
  }
  .ip2locationio_env$configuration
}

#' @title Lookup for IP address geolocation and proxy information
#'
#' @description Lookup for a comprehensive of the information such as location, ASN, ISP and proxy. The availability of the data will depends on the plan you have signed up for your API key.
#' @param ip IPv4 or IPv6 address
#' @return Return all the geolocation and proxy information about the IP address
#' @import reticulate
#' @import jsonlite
#' @export
#' @examples \dontrun{
#' lookup("1.0.241.135")
#' }
#'

lookup <- function(ip){
  configuration <- .getConfiguration()
  ipgeolocation <- .ip2locationio_env$ip2locationio$IPGeolocation(configuration)
  rec <- ipgeolocation$lookup(ip)
  result <- reticulate::py_to_r(rec)
  return(result)
}

#' @title Lookup for domains hosted on an IP address.
#'
#' @description Lookup for a list of hosted domain names by the IP address.
#' @param ip IPv4 or IPv6 address
#' @param page (optional) Pagination result returns of the hosted domains. If unspecified, 1st page will be used.
#' @return Return list of hosted domain names by the IP address
#' @import reticulate
#' @import jsonlite
#' @export
#' @examples \dontrun{
#' lookupHostedDomain("1.0.241.135")
#' }
#'

lookupHostedDomain <- function(ip, page){
  configuration <- .getConfiguration()
  hosteddomain <- .ip2locationio_env$ip2locationio$HostedDomain(configuration)

  if (missing(page)) {
    rec <- hosteddomain$lookup(ip)
  } else {
    rec <- hosteddomain$lookup(ip, page)
  }

  result <- reticulate::py_to_r(rec)
  return(result)
}


#' @title Lookup an IP address's country
#'
#' @description Lookup for the IP address's country
#' @param ip IPv4 or IPv6 address
#' @return Return the country name of the the IP address
#' @import reticulate
#' @export
#' @examples \dontrun{
#' lookupCountryByIP("1.0.241.135")
#' }
#'

lookupCountryByIP <- function(ip){
  rec <- lookup(ip)
  return(rec$country_name)
}

#' @title Lookup an IP address's coordinate
#'
#' @description Lookup for the IP address's coordinate
#' @param ip IPv4 or IPv6 address
#' @return Return the coordinate of the the IP address
#' @import reticulate
#' @export
#' @examples \dontrun{
#' lookupCoordinateByIP("1.0.241.135")
#' }
#'

lookupCoordinateByIP <- function(ip){
  rec <- lookup(ip)
  result <- c(rec$latitude, rec$longitude)
  return(result)
}

#' @title Lookup an IP address's Autonomous system name and number
#'
#' @description Lookup for the IP address's Autonomous system name and number
#' @param ip IPv4 or IPv6 address
#' @return Return the Autonomous system name and number of the the IP address
#' @import reticulate
#' @export
#' @examples \dontrun{
#' lookupASNByIP("1.0.241.135")
#' }
#'

lookupASNByIP <- function(ip){
  rec <- lookup(ip)
  result <- list(as_name = rec$as, as_number = rec$asn)
  return(result)
}

#' @title Lookup an IP address's location in text
#'
#' @description Lookup for the IP address's location in text
#' @param ip IPv4 or IPv6 address
#' @return Return the location of the the IP address in text
#' @import reticulate
#' @export
#' @examples \dontrun{
#' lookupLocationByIP("1.0.241.135")
#' }
#'

lookupLocationByIP <- function(ip){
  rec <- lookup(ip)
  result <- paste(rec$city_name, rec$region_name, rec$country_name, sep = ", ")
  return(result)
}
