
# Initial setup
usethis::use_gpl3_license("Lucas Gallindo")
usethis::use_readme_md()

# Dependencies
usethis::use_dev_package("CosW", type = "Imports")
usethis::use_package("SecKW", "Imports")
usethis::use_package("SinIW", "Imports")
usethis::use_package("TanB", "Imports")

# Tests
usethis::use_test("BuildLogLikelihood")
