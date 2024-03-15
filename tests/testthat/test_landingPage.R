## function createLandingPage
test_that(".initialize_server", {
    expect_is(MatrixQCvis:::createLandingPage, "function")
    expect_is(MatrixQCvis:::createLandingPage(), "function")
    expect_is(MatrixQCvis:::createLandingPage(seUI = ""), "function")
    expect_is(MatrixQCvis:::createLandingPage(seLoad = ""), "function")
    expect_is(MatrixQCvis:::createLandingPage(requireButton = ""), "function")
})
