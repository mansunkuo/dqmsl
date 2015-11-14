library(rmarkdown)

test_that("Render index.html", {
    expect_message(render("../../index.Rmd"), "Output created: index.html")
})


