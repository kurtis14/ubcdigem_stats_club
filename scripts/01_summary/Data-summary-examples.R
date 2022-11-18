
# Goals -------------------------------------------------------------------

# Load data stored in different formats
# Overview of data, structure
# Simple data cleaning, wrangling
# Summarize - t-test, plots

# Load packages -----------------------------------------------------------

# if necessary, install.packages('tidyverse')
# this will load several helpful packages - see print out in console
library(tidyverse)

# Load data ---------------------------------------------------------------

#### load text data

# some examples - are these equivalent? 
penguins1 <- read.table('data/penguins.txt', header = T)
penguins2 <- read_table('data/penguins.txt')
penguins3 <- read_delim('data/penguins.txt', delim = ' ')

# checks - why are they not identical?
identical(penguins1, penguins2)
identical(penguins1, penguins3)
identical(penguins2, penguins3)
penguins1
penguins2
penguins3

# remove datasets and reload
rm(list = ls(pattern = 'penguins'))
penguins <- read_delim('data/penguins.txt', delim = ' ')


### load csv data
gapminder <- read_csv('data/gapminder.csv')


### load R data
mtc <- read_rds('data/mtcars.RDS')


### make your own datasets!
my_df <- data.frame(nums = c(1:10),
                    letters = letters[1:10])

my_tibble <- tibble(nums = seq(1, 100, 10),
                    letters = sample(letters, size = 10, replace = T))

# Check data format -------------------------------------------------------

# try some different overviews -- what is printed in the console?
class(penguins)
class(mtcars)
str(penguins)
head(penguins)
summary(penguins)
# View(penguins)

# Data wrangling ----------------------------------------------------------

### rename vars
names(penguins)

# change all vars
toupper(names(penguins))
names(penguins) <- toupper(names(penguins))
names(penguins) <- tolower(names(penguins))

# select and rename specific var
penguins %>% 
  rename(flip_lngth = flipper_length_mm)
# penguins <- penguins %>% 
#   rename(flip_lngth = flipper_length_mm)

# select var names by some characteristics - remove underscores
penguins %>% 
  rename_with(~str_remove_all(., '_'))

### column transformation/mutation

# copy a column
penguins %>% 
  mutate(bill_length_copy = bill_length_mm)

# make a new column - body mass to kilograms
penguins <- penguins %>% 
  mutate(body_mass_kg = body_mass_g / 1000)

# collapse or paste columns together
penguins <- penguins %>% 
  mutate(species_island = str_c(species, island, sep = '_'))

# add some random variation -- use later on with paired t-tests
penguins <- penguins %>% 
  mutate(bill_length_2 = bill_length_mm + rnorm(1))

### filter/subset data based on some column values

# by string value - select specific penguin species
# try different methods
penguins %>% 
  filter(species == 'Gentoo')

penguins %>% 
  filter(species != 'Adelie')

my_penguins <- c('Adelie', 'Gentoo', 'Emperor', 'King')

penguins %>% 
  filter(species %in% my_penguins)

penguins %>% 
  filter(grepl('chin', species, ignore.case = T))

# by numeric range
penguins %>% 
  filter(bill_length_mm > 55)

penguins %>% 
  filter(bill_length_mm > 55 | bill_length_mm < 35) %>% 
  arrange(bill_length_mm)

### remove missing values?

summary(penguins)

# remove rows missing sex
penguins %>% 
  count(sex, sort = T)

penguins %>% 
  filter(is.na(sex))

# generic method
na.omit(penguins)

# remove based on one column
penguins_nomiss <- penguins %>% 
  filter(!is.na(sex))

# T-tests -----------------------------------------------------------------

### paired t test
t.test(x = bill_length_mm, y = bill_length_2, data = penguins_nomiss, paired = T)
t.test(penguins_nomiss$bill_length_mm, penguins$bill_length_2)

### independent samples t test
# does bill length differ by sex? 
t.test(bill_length_mm ~ sex, data = penguins_nomiss)
test_out <- t.test(bill_length_mm ~ sex, data = penguins_nomiss)
test_out$p.value

# does bill length differ by sex within each species?
# how can we easily subset the data and re-run the test?
t.test(penguins_nomiss$bill_length_mm[penguins_nomiss$species == 'Adelie'] ~ sex, data = penguins_nomiss)

t.test(penguins_nomiss$bill_length_mm[penguins_nomiss$species == 'Adelie'] ~ 
         penguins_nomiss$sex[penguins_nomiss$species == 'Adelie'])

# two steps
penguins_adelie <- penguins_nomiss %>% 
  filter(species == 'Adelie')
t.test(bill_length_mm ~ sex, data = penguins_adelie)

# in one step?
penguins_nomiss %>% 
  filter(species == 'Adelie') %>% 
  # t.test(bill_length_mm ~ sex)
  summarize(broom::tidy(t.test(bill_length_mm ~ sex)))

# but repeat the test for each species?
penguins_nomiss %>% 
  group_by(species) %>% 
  summarize(mean(bill_length_mm))
penguins_nomiss %>% 
  group_by(species) %>% 
  summarize(broom::tidy(t.test(bill_length_mm ~ sex)))

# purrr method - split data frame and fit model to each piece
penguins_nomiss %>% 
  split(.$species) %>% 
  map(~ t.test(bill_length_mm ~ sex, data = .x))

# Visuals -----------------------------------------------------------------

# base r examples
plot(penguins$body_mass_g)
plot(penguins$body_mass_g, penguins$bill_length_mm)
hist(penguins$body_mass_g, breaks = 20)

### ggplot
# build up plots with layers, modify features
ggplot(penguins)

ggplot(penguins, aes(x = body_mass_g, y = bill_length_mm))

p <- ggplot(penguins, aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point()
p

ggplot(penguins, aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point() +
  geom_smooth(method = 'lm')

ggplot(penguins, aes(x = body_mass_g, y = bill_length_mm, colour = species)) +
  geom_point()

ggplot(penguins, aes(x = body_mass_g, y = bill_length_mm, colour = species)) +
  geom_point() +
  facet_wrap(~ island)

ggplot(penguins, aes(x = body_mass_g, y = bill_length_mm, colour = species)) +
  geom_point() +
  facet_wrap(~ island) +
  theme_minimal() +
  labs(title = 'Penguin body mass and bill length')

ggplot(penguins, aes(x = body_mass_g, y = bill_length_mm, colour = species)) +
  geom_point() +
  facet_wrap(~ island) +
  theme_minimal() +
  labs(title = 'Penguin body mass and bill length') +
  scale_color_brewer(type = 'qual')

# write plot to file
ggsave('outputs/penguin-plot.jpg', dpi = 300, width = 6, height = 4, units = 'in')
