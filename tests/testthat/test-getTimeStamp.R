describe("utils.R", {
  describe("getTimeStamp", {
    it("should return a String", {
      time <- getTimeStamp()
      
      expect_type(time, "character")
      expect_equal(object = time,
                   expected = paste(gsub(x = Sys.time(), pattern = " ", replacement = "; "),
                                    Sys.timezone())
                   )
    })
    it("should correctly transform the system time", {
      time <- getTimeStamp()
      str = ";"
      
      expect_true(grepl(str, time, fixed = TRUE))
    })
    it("should correctly attach the timezone when returning the value", {
      time <- getTimeStamp()
      str <- Sys.timezone()
      
      expect_true(grepl(str, time, fixed = TRUE))
    })
  })
})
