# SILAC Analysis (Stable Isotope Labelling of Amino Acids in Cells)
# Rick Scavetta
# 13 June 2018
# Case study for workshop

# clear workspace
rm(list = ls())

# Load packages
# Includes dplyr, ggplot2, purrr and other packages
library(tidyverse)

# Read in the data:
protein.df <- read.delim("Protein.txt")

# Examine the data:
summary(protein.df)
str(protein.df)
glimpse(protein.df)
names(protein.df) # variable names


# print to screen -- make a tibble
typeof(protein.df) # Before tibble
class(protein.df) # Before tibble
protein.df <- as_tibble(protein.df)
class(protein.df) # After 
protein.df # better print to screen


# Transformations
# log10 of intensities:
protein.df$Intensity.H <- log10(protein.df$Intensity.H)
protein.df$Intensity.M <- log10(protein.df$Intensity.M)
protein.df$Intensity.L <- log10(protein.df$Intensity.L)

# Add log10 intensities:
protein.df$Intensity.H.M <- protein.df$Intensity.H + protein.df$Intensity.M
protein.df$Intensity.M.L <- protein.df$Intensity.M + protein.df$Intensity.L

# log2 of ratios: HM & ML
protein.df$Ratio.H.M <- log2(protein.df$Ratio.H.M)
protein.df$Ratio.M.L <- log2(protein.df$Ratio.M.L)

# Exercises p58 - 59:
# Find and remove contaminants:
protein.df %>% 
  filter(Contaminant == "+")

# Do math on a logical vector
sum(protein.df$Contaminant == "+")
# For logical vectors, TRUE == 1, and FALSE == 0
# So we can do math on logical vectors!

# Aside:
# get FALSE
sum(protein.df$Contaminant != "+")

# Or use table()
table(protein.df$Contaminant) 
# can also get a contigency table with two vectors

# Now remove them:
protein.df %>% 
  filter(Contaminant != "+")

# Get specific Uniprot IDs
protein.df %>% 
  filter(Uniprot %in% paste0(c("GOGA7", "PSA6", "S10AB"), "_MOUSE"))


# Get significant hits for HM
protein.df %>% 
  filter(Contaminant != "+", Ratio.H.M.Sig < 0.05)

# Extreme ratios:
protein.df %>% 
  filter(Contaminant != "+") %>% 
  filter(Ratio.H.M > 2 | Ratio.H.M < -2)



# We never plotted our data, so let's take a look:
ggplot(protein.df, aes(Ratio.H.M, Intensity.H.M)) +
  geom_point(alpha = 0.6)





