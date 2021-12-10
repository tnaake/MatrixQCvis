## function biocrates
test_that("biocrates", {
    expect_is(biocrates, "function")
    expect_error(biocrates(file = ""), "File does not exist")

})

## function maxQuant
test_that("maxQuant", {
    expect_is(maxQuant, "function")
    expect_error(maxQuant(file = "", intensity = "foo"), "should be one of")
    suppressWarnings(expect_error(maxQuant(file = "", intensity = "iBAQ", 
        type = "txt"), "no lines available"))
    expect_error(maxQuant(file = "", intensity = "iBAQ", type = "xlsx"), 
        "File does not exist")
    suppressWarnings(expect_error(maxQuant(file = "", intensity = "LFQ", 
        type = "txt"), "no lines available"))
    expect_error(maxQuant(file = "", intensity = "LFQ", type = "xlsx"), 
        "File does not exist")
    expect_error(maxQuant(file = "", intensity = "LFQ", type = "foo"),
        "should be one of")
})

## function spectronaut
test_that("spectronaut", {
    expect_is(spectronaut, "function")
    expect_error(spectronaut(file = ""), "File does not exist")
})