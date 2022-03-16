usethis::use_build_ignore("golem_history.R")
golem::use_recommended_tests()
golem::use_recommended_deps()
# Remove current favicon
golem::remove_favicon()
# Add a new one
golem::use_favicon(path = "C:/Users/fabachet/Pictures/GMRC_Logo48.png")
golem::use_utils_ui()
golem::use_utils_server()
