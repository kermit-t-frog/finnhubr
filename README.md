# finnhubr
Package for calling the https://finnhub.io API from R (see https://github.com/finnhubio/Finnhub-API/)

# Installation

Install the package from github via 

    devtools::install_github("kermit-t-frog/finnhubr")

And get a (free) API key from https://finnhub.io/register - and write it down somewhere...

# Usage
Load/attach the package, initialise the API key and consume one of the following functions

    library(finnhubr)
    finnhubr_api_key("YOURAPIKEYHERE")

## List of symbols per exchange

    > exchange_symbols("DE")
                      description displaySymbol  symbol
    1            FACC AG INH.AKT.        1FC.DE  1FC.DE
    2     RAIFFEISEN BK INTL INH.        RAW.DE  RAW.DE
    3                     PORR AG       ABS2.DE ABS2.DE
    4                  LENZING AG        LEN.DE  LEN.DE
    5   ERSTE GROUP BNK INH. O.N.        EBO.DE  EBO.DE
    ...
    
## Obtain a stock quote 
    > finnhubr::stock_quote("LEN.DE")
      current high   low  open previous           timestamp
    1    46.7 47.4 46.35 46.35    45.65 2020-06-03 11:28:06

## Candles

    finnhubr::stock_candles("LHA.DE",resolution = "W")
        current    high     low    open status           timestamp   volume
    1   10.5326 10.9365 10.3281 10.6451     ok 1996-12-16 01:00:00  3432600
    2   10.6093 10.6349 10.4815 10.5837     ok 1996-12-23 01:00:00   207000
    3   10.6860 10.7627 10.4355 10.6093     ok 1996-12-30 01:00:00   830200
    4   11.0695 11.4018 10.7116 10.7116     ok 1997-01-06 01:00:00  3279900
    ...
    
 ## Simplified company profile
 
    > finnhubr::stock_profile("LHA.DE")
    $country
    [1] "DE"
    
    $currency
    [1] "EUR"

    $exchange
    [1] "XETRA"
    ...
