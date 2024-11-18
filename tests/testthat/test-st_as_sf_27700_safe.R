test_that("st_as_sf_27700_safe works", {
  expect_no_error(
    read_sql("Postcodes_Arc207912.OSPCode_Master") |> 
      dplyr::select(Eastings, Northings) |> 
      head() |> 
      dplyr::collect() |> 
      st_as_sf_27700_safe("Eastings", "Northings")
  )
})
