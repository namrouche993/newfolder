x <- sample(10)
mt=mtcars[1:8,3:5]
usethis::use_data(x, mt,internal = TRUE,overwrite = TRUE)

dat <- readr::read_csv(paste0(getwd(),"/data-raw/swim.csv"), col_types = cols(name = "c", where = "c", temp = "d"))
usethis::use_data(dat,internal = FALSE)
