describe("utils.R", {
  describe("get_latest_pkg_info", {
    it("should return a list", {
      test <- get_latest_pkg_info("rpact")
      
      expect_type(test, "list")
    })
    it("should have the variables: Version, Maintainer,
       Author, License, Published, Title, Description in it",  {
      test <- get_latest_pkg_info("rpact")
      
      expect_equal(colnames(test), c("Version", "Maintainer", "Author",
                                     "License", "Published", "Title",
                                     "Description"))
    })
    it("should not return an empty list", {
      test <- get_latest_pkg_info("rpact")
      
      expect_true(length(test) > 0)
    })
  })
})