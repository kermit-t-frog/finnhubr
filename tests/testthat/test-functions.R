test_that("API key set / get works", {
expect_error(finnhubr_api_key())
  expect_invisible(finnhubr_api_key("brcfkm7rh5rap841kf2g")) # demo key
expect_identical(finnhubr_api_key(),"brcfkm7rh5rap841kf2g") # demo key
})



test_that("finnhubr functionality works", {

  finnhubr_api_key("brcfkm7rh5rap841kf2g")

  expect_equal(
    length(
      stock_candles("LHA.DE",resolution = "D",from="2020-05-01",to="2020-05-06")),
    7)

  expect_equal(
    length(
      stock_candles("LHA.DE",resolution = "W",from="2020-05-01",to="2020-05-06")),
    7)

  expect_equal(
    length(
      stock_candles("LHA.DE",resolution = "M",from="2020-05-01",to="2020-05-06")),
    7)

  expect_identical(stock_profile("LHA.DE"), stock_profile("DE0008232125","isin"))
  expect_equal(length(stock_profile("LHA.DE")),12)

  expect_equal(length(stock_quote("LHA.DE")),6)

  expect_equal(length(exchange_symbols("DE")),3)
  expect_true(nrow(exchange_symbols("DE"))>100)

})
