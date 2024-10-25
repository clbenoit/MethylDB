box::use(
  shiny[testServer],
  testthat[expect_true, test_that],
)
box::use(
  app/main[server],
)

test_that("add V1 to Base works", {
  testServer(server, {
    expect_true(grepl(x = output$message$html, pattern = "Check out Rhino docs!"))
  })
})
