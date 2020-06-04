#' Set finnhub.io API key
#'
#' @param api_key The API key
#'
#' @return Set the API key
#' @export
#'
#' @examples
#' \dontrun{
#' finnhubr_api_key("a1b2c3d4e5")
#' stock_quote("AAPL")
#' }
finnhubr_api_key <- function(api_key){
  if (!missing(api_key)) {
    options(finnhubr_api_key = api_key)
    if (is.null(getOption("finnhubr_max_calls_per_minute"))){
      finnhubr_api_setup()}
  }
  if (is.null(getOption("finnhubr_api_key"))) {
    stop("Set API key using finnhubr_api_key().",
         call. = FALSE)
  }
  finnhubr_api_wait()
  invisible(getOption("finnhubr_api_key"))
}

#' Set API call limits
#'
#' @param max_calls_per_minute Maximum allowed number of calls per minute,
#' defaults to 60.
#' @param silent If set to FALSE, a message is printed that shows the wait time
#' in seconds.
#'
#' @return Global options starting with "finnhubr_"
#'
#' @examples
#' \dontrun{
#' finnhubr_api_setup(max_calls_per_minute = 60,silent=TRUE)
#' }
finnhubr_api_setup <- function(max_calls_per_minute = 60,silent=TRUE){
  if (is.infinite(max_calls_per_minute)){max_calls_per_minute<-NULL}

  if (!is.null(max_calls_per_minute)){
  options(finnhubr_max_calls_per_minute = max_calls_per_minute)
  options(finnhubr_call_idx = 1 )
  options(finnhubr_silent = silent)
  options(as.list(setNames(rep(as.double(Sys.time())-61,max_calls_per_minute),
                           paste0("finnhubr_stamp",1:max_calls_per_minute))))
  } else {
    options(finnhubr_max_calls_per_minute = Inf)
  }
}

#' Wait until API is "free", thereby forestalling API endpoint limitation
#'
#' @return Nothing or a printed message.
#'
#' @examples
#' \dontrun{
#' finnhubr_api_setup(silent=FALSE)
#' sapply(1:61,function(x){print(x); finnhubr_api_wait()})
#' }
finnhubr_api_wait <- function(){
  if (getOption("finnhubr_max_calls_per_minute")<Inf){
    idx <- getOption("finnhubr_call_idx")
    #idx <- try_num %% getOption("finnhubr_max_calls_per_minute")
    wait_time <- ceiling(60 - as.double(Sys.time()) +
                           getOption(paste0("finnhubr_stamp",idx)))
    if (wait_time>0){
      if (!getOption("finnhubr_silent")){print(paste("waiting: ",wait_time))}
      Sys.sleep(wait_time)
    }
    options(as.list(setNames(as.double(Sys.time()),
                             paste0("finnhubr_stamp",idx))))
    if (idx==getOption("finnhubr_max_calls_per_minute")){
      options(finnhubr_call_idx=1)
    } else {
      options(finnhubr_call_idx=idx+1)
    }
  }
}

#' List all symbols available on an exchange
#'
#' @param exchange_code The exchange code. Latest list is available
#' \href{https://docs.google.com/spreadsheets/d/1I3pBxjfXB056-g_JYf_6o3Rns3BV2kMGG1nCatb91ls}{here}.
#'
#' @return A data frame with instrument description, display symbol and symbol.
#' @export
#'
#' @examples
#' \dontrun{
#' finnhubr_api_key("a1b2c3d4e5")
#' exchange_symbols("DE")
#'
#' }
exchange_symbols <- function(exchange_code){
  token    <- paste0("&token=",finnhubr_api_key())
  endpoint <- paste0("https://finnhub.io/api/v1/stock/symbol?exchange=",
                     exchange_code,token)
  jsonlite::fromJSON(httr::content(httr::GET(endpoint),as="text"),flatten=TRUE)
}

#' Get latest quote for a symbol
#'
#' @param symbol The instrument symbol
#'
#' @return A list with current quote, today's opening, high and low, as well as
#' previous day's closing and a timestamp.
#' @export
#'
#' @examples
#' \dontrun{
#' finnhubr_api_key("a1b2c3d4e5")
#' stock_quote("LHA.DE")
#' }
stock_quote <- function(symbol){
  token    <- paste0("&token=",finnhubr_api_key())
  endpoint <- paste0("https://finnhub.io/api/v1/quote?symbol=",symbol,token)
  x<- jsonlite::fromJSON(httr::content(httr::GET(endpoint),as="text"),
                         flatten=TRUE)
  x <- as.data.frame(x)
  names(x) <- c("current","high","low","open","previous","timestamp")
  x[,"timestamp"] <-as.POSIXct(x[,"timestamp"],origin="1970-01-01")
  x
}


#' Get stock candle data
#'
#' @param symbol The stock symbol
#' @param resolution Candle resolution from "1","5","15","30","60","D","W","M".
#' Not every resolution may be available
#' @param from First observation date / datetime. Defaults to 1970-01-01 00:00
#' @param to Last observation date / datetime. Defaults to 2099-12-31 23:59
#'
#' @importFrom jsonlite fromJSON
#' @return a data frame with candle data open,high,low, close, volume,timestamp
#' @export
#'
#' @examples
#' \dontrun{
#' finnhubr_api_key("a1b2c3d4e5")
#' stock_candles("LHA.DE")
#' stock_candles("LHA.DE",resolution="W")
#' stock_candles("LHA.DE",from="2020-05-01")
#' stock_candles("LHA.DE",from="2020-05-01", to="2020-05-31")
#'
#' }
stock_candles <- function(symbol,resolution = "D",
                    from = "1970-01-01 00:00:00",
                    to = "2099-12-31 23:59:00"){
  resolution <- toupper(toString(resolution))
  feasible_resolutions <-c("1","5","15","30","60","D","W","M")
  if (!(resolution %in% feasible_resolutions)){
    stop("Unknown candle resolution:'",resolution,"'. Must be one of ",
         paste0(feasible_resolutions, collapse="','"),call. = FALSE)
  }
  token    <- paste0("&token=",finnhubr_api_key())
  endpoint <- paste0("https://finnhub.io/api/v1/stock/candle?symbol=",symbol,
                     "&resolution=",resolution,
                     "&from=",as.numeric(as.POSIXct(from,origin="1970-01-01")),
                     "&to=",as.numeric(as.POSIXct(to,origin="1970-01-01")),
                     token)
  x<- jsonlite::fromJSON(httr::content(httr::GET(endpoint),as="text"),
                         flatten=TRUE)
  x <- as.data.frame(x)
  if (length(x)>1){
  names(x) <- c("close","high","low","open","status","timestamp","volume")
  x[,"timestamp"] <-as.POSIXct(x[,"timestamp"],origin="1970-01-01")
  } else {
    names(x)<-"status"
  }
  x
}


#' Obtain high level company profile information
#'
#' @param code Company code, either its symbol or an ISIN
#' @param type Corresponding to code, either "symbol" (default) or "isin"
#'
#' @return A list with high level company information, e.g. name, country of HQ,
#' currency of filings, listed exchange, industry type, market cap (m currency),
#' ticker symbol.
#' @export
#'
#' @examples
#' \dontrun{
#' finnhubr_api_key("a1b2c3d4e5")
#' stock_profile("LHA.DE")
#' stock_profile("DE0008232125", type="isin")
#'
#' }
stock_profile <- function(code,type="symbol"){
  type <- match.arg(type,choices=c("symbol","isin"))
  token    <- paste0("&token=",finnhubr_api_key())
  endpoint <- paste0("https://finnhub.io/api/v1/stock/profile2?",type,"=",
                     code,token)
  jsonlite::fromJSON(httr::content(httr::GET(endpoint),as="text"),flatten=TRUE)
}

finnhubr_http_result <- function(endpoint){
  result <- httr::GET(endpoint)
  sc     <- httr::status_code(result)
  if (!(sc < 300 && sc >= 200)){
    stop(httr::content(result, as = "text"), call. = FALSE)
  }
  jsonlite::fromJSON(httr::content(result,as = "text",encoding = "UTF-8"),
                     flatten=TRUE)
}

#' Generic GETter for Finnhub endpoints
#'
#' Query the Finnnhub.io API endpoint. For a documentation see
#' \href{https://finnhub.io/docs/api}{here}
#'
#' @param symbol Symbol code or NULL (default)
#' @param endpoint The API endpoint, e.g. "stock/profile2", or "stock/candle"
#' @param ... (Optional) parameters required by the endpoint. Refer to the
#' documentation.
#'
#' @return The endpoint's content.
#' @export
#'
#' @examples
#' \dontrun{
#' finnhubr_api_key("a1b2c3d4e5")
#' fh_get("AAPL","stock/profile2") # same as stock_profile() above
#' fh_get(NULL,"forex/exchange")
#' }
fh_get <- function(symbol=NULL,endpoint,...){
  # everything 'dateay' must be trasnformed to unix datetime
  dots <- list(...)
  dots$token <- finnhubr_api_key()
  dots$symbol <- symbol
  url <- paste0("https://finnhub.io/api/v1/",endpoint,"?",
                paste0(names(dots),"=",dots,collapse="&"))
  response <- httr::GET(url)
  sc <- httr::status_code(response)
  content <- httr::content(response,as="text",encoding = "UTF-8")
  if (!(sc>=200 && sc<300)){
    stop(content,call=FALSE)
  }
  if (!(httr::http_type(response) %in% c("text/csv","application/json"))){
    stop(content,call=FALSE)
  }
  if (httr::http_type(response)=="text/csv"){
    readr::read_csv(content)
  } else{ # must be json
    tibble::as_tibble(jsonlite::fromJSON(content,flatten=TRUE),
                      stringsAsFactors = FALSE)
  }
}

