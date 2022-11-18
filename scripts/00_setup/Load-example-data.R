# Load some example datasets and write to file for future import and exercises

# Load gapminder ----------------------------------------------------------

# load library
library(gapminder)

# load data to env
gap <- gapminder

# write to csv -- note, need readr pkg
readr::write_csv(gap, 'data/gapminder.csv')

# Load penguins -----------------------------------------------------------

# load library
library(palmerpenguins)

# load data to env
peng <- penguins

# write to text -- base r, omit row names
# write.table(peng, 'data/penguins.txt', row.names = F)
readr::write_delim(peng, 'data/penguins.txt')

# Load mtcars -------------------------------------------------------------

mtc <- mtcars

# write to r data
readr::write_rds(mtc, 'data/mtcars.RDS')

# Clean up ----------------------------------------------------------------

rm(gap, mtc, peng)
