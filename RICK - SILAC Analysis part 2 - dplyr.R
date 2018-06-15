# SILAC Analysis using dplyr
# Rick Scavetta
# 14 June 2018
# Case study for workshop

# clear workspace
rm(list = ls())

# Load packages
library(tidyverse)

# Read in the data and make a tibble
protein.df <- read.delim("Protein.txt")
protein.df <- as_tibble(protein.df)

# Element 10: Introduction to dplyr
# 3 main components
# 1 - %>% pipe operator (shift + ctrl + m)
mean(1:10)
# Is the same as...
1:10 %>% 
  mean()

# 2 - 5 verbs (the "grammar" of data analysis)
# 2a - filter (row)
# 2b - arrange (rows, lowest to highest "ascending")
# 2c - select (columns)
protein.df %>% 
  filter(Contaminant != "+") %>% 
  arrange(Ratio.H.M) %>% 
  select(Uniprot, Ratio.M.L, Ratio.H.M)

# 2c+ - helper functions
protein.df %>% 
  filter(Contaminant != "+") %>% 
  arrange(Ratio.H.M) %>% 
  select(Uniprot, starts_with("R"), -ends_with("Sig")) %>% 
  slice(1:20)

# Alternatively, using regular expressions for Ratio and Intensity columns
protein.df %>% 
  filter(Contaminant != "+") %>% 
  arrange(Ratio.H.M) %>% 
  select(Uniprot, matches("^(R|I)"), -ends_with("Sig")) %>% 
  slice(1:20)

# 2d - mutate, for transformation functions
# Individually
protein.df %>% 
  filter(Contaminant != "+") %>% 
  mutate(Ratio.M.L = log2(Ratio.M.L),
         Ratio.H.M = log2(Ratio.H.M))

# Better, use the helper functions
# use mutate_at() with vars() to add the select helper functions 
protein.df %>% 
  filter(Contaminant != "+") %>% 
  mutate_at(vars(starts_with("R"), -ends_with("Sig")), log2) %>% 
  mutate_at(vars(starts_with("I")), log10) %>% 
  mutate(Intensity.H.M = Intensity.H + Intensity.M,
         Intensity.M.L = Intensity.M + Intensity.L) -> protein.new

# 2e - summarise, aggregration functions
# 3 - group_by, an adverb for splitting






