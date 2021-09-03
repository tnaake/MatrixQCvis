## function biocrates
test_that("biocrates", {
    expect_is(biocrates, "function")
    expect_error(biocrates(file = ""), "File does not exist")

})

## function maxQuant
test_that("maxQuant", {
    expect_is(maxQuant, "function")
    expect_error(maxQuant(file = "", type = "foo"), "should be one of")
    expect_error(maxQuant(file = "", type = "iBAQ"), "File does not exist")
})