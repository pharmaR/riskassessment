if(requireNamespace('spelling', quietly = TRUE))
  spelling::spell_check_test(vignettes = TRUE, error = TRUE,
                             skip_on_cran = TRUE)
# To check for spelling mistakes:
# spelling::spell_check_package()

# To add remainging (un-correctable) words to the wordlist:
# spelling::update_wordlist()
