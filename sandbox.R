### Sandbox

# load package
library(fbutils)
library(tidyverse)

file <- "C:/Users/Jeff/Google Drive/Barley Lab/Projects/Genomic Selection/Phenotypic Data/Raw/2015/Field Book/2015-07-03-01-15-40_S2TP_StP15_field.book_table.csv"

fbt <- read.fb(file, other.cols = c("plot", "trial", "entry", "rep"))




# The sample file
fbt <- fbt.sample

# Convert to field book
fbt <- as.field.book.table(fbt, other.cols = c("plot", "tray_row", "tray_id", "seed_id", "pedigree"))

traits <- fb.traits(fbt, "numeric")

fbt.adjustment <- fb.spatial.adj(fbt, adj.traits = traits, checks = c("Kharkof", "TAM 107", "Scout 66"))







entry.matrix <- model.matrix(~ -1 + line_name, data = fbt)

n.entry <- ncol(entry.matrix)
n.obs <- nrow(entry.matrix)

V_G <- 10
V_E <- 50
V_e <- 25

# Vector of genotypic values
g <- rnorm(n = n.entry, mean = 0, sd = sqrt(V_G))

# Environmental effect
e <- rnorm(n = 1, mean = 0, sd = sqrt(V_E))

# Error
res <- rnorm(n = n.obs, mean = 0, sd = sqrt(V_e))

continuous.trait <- abs(entry.matrix %*% g + e + res)

## Discrete trait
# Geno.values
g <- rbinom(n = n.entry, size = 9, prob = 0.3)

# Residual
res <- rbinom(n = n.obs, size = 1, prob = 0.3) * sample(c(-1,1), n.obs, replace = T)

discrete.trait <- entry.matrix %*% g + res
discrete.trait[discrete.trait < 0] <- 0

## Multi-categorical trait
trait.scale <- seq(0, 100, 5)

# number of scale points
n.values <- length(trait.scale)

# Probability of observing each value on the scale
scale.prob <- ((n.values - 5) / (n.values + 5)) ^ seq(n.values)

# Average genotypic value
g <- sample(trait.scale, size = n.entry, prob = scale.prob,  replace = T)

# Matrix of genotypic values
g <- entry.matrix %*% g 

# The residual will be the deviations from the mean of the observed ratings
res <- rnorm(n = n.obs * 10, mean = g, sd = 5) %>% 
  matrix(nrow = n.obs, ncol = 10)

# Round to the nearest 5
res <- 5 * round(res / 5)

# Remove negative
res[res < 0] <- 0

# Add together in the matrix
multi.trait <- as.vector(g) + res

# Collapse the traits into strings
multi.trait.obs <- apply(X = multi.trait, MARGIN = 1, FUN = function(obs) str_c(obs, collapse = ":"))

## Add the traits to the data.frame
fbt <- fbt %>% 
  mutate(Continuous = continuous.trait,
         Discrete = discrete.trait,
         Multi = multi.trait.obs)




fbt <- as.field.book.table(fbt, other.cols = c("plot", "tray_row", "tray_id", "seed_id", "pedigree"))

# Traits
fbt.traits <- fb.traits(fbt, "numeric")

# Checks
checks <- c("Kharkof", "TAM 107", "Scout 66")








# Convert to field book
fbt1 <- as.field.book.table(fbt, line.name = "seed_name", 
                           other.cols = c("plot", "tray_row", 
                                          "tray_id", "seed_id", "pedigree"))

fbt.traits <- fb.traits(fbt, "numeric")

fbt.adjustment <- fb.spatial.adj(fbt1, adj.traits = fbt.traits, checks = c("Kharkof", "TAM 107", "Scout 66"))


# LEts see if the attributes are lost
fbt2 <- fbt1 %>% 
  mutate(Trait2 = Trait + 15)

# Pull out traits
fbt.traits <- fb.traits(fbt1, "numeric")

# Evaluate outliers
fb.summary <- fb.outliers(x = fbt1, max.sd = 3, fbt.traits)



# Load two fb
# fb.filename1 <- "C:/Users/Jeff/Google Drive/Barley Lab/Projects/Genomic Selection/Phenotypic Data/Raw/2016/Field Book/2016-07-13-12-47-43_S2C1R_STPFHB16_field_book_table.csv"
# fb.filename2 <- "C:/Users/Jeff/Google Drive/Barley Lab/Projects/Genomic Selection/Phenotypic Data/Raw/2016/Field Book/2016-07-25-10-30-44_S2C1R_CRMFHB16_field_book_table.csv"

## Load different field.books
fb.CRM16.file <- "C:/Users/Jeff/Google Drive/Barley Lab/Projects/Genomic Selection/Phenotypic Data/Raw/2016/Field Book/2016-08-04-03-05-51_S2_MET_CRM16_field_book_table.csv"
fb.STP16.file <- "C:/Users/Jeff/Google Drive/Barley Lab/Projects/Genomic Selection/Phenotypic Data/Raw/2016/Field Book/2016-07-29-04-16-23_S2_MET_STP16_field_book_table.csv"
fb.CRM16.file2 <- "C:/Users/Jeff/Google Drive/Barley Lab/Projects/Genomic Selection/Phenotypic Data/Raw/2016/Other/S2_MET_CRM16_heading_height.csv"

# FHB
fb.STPFHB16.file <- "C:/Users/Jeff/Google Drive/Barley Lab/Projects/Genomic Selection/Phenotypic Data/Raw/2016/Field Book/2016-07-13-12-47-43_S2C1R_STPFHB16_field_book_table.csv"



# Test reading in data
fb.CRM16.1 <- read.fb(file = fb.CRM16.file, unique.id = "plot_id", planting.date = "2016/05/04")
fb.CRM16.2 <- read.fb(fb.CRM16.file2)
fb.STP16 <- read.fb(fb.STP16.file)
fb.STPFHB16 <- read.fb(fb.STPFHB16.file, unique.id = "plot_id")

# Combine the CRM field books
fb.CRM16 <- fb.merge(fb.CRM16.1, fb.CRM16.2)

# Show a heatmap
fb.heatmap(fb.STP16, plot.traits = c("Heading", "Height"))
# Show outliers
fb.outliers(fb.STP16, max.sd = 3, outlier.traits = "Heading", show.on.map = T)

# Parse a multi-categorical trait
fb.STPFHB16 <- fb.multicat.parse(fb.STPFHB16, parse.traits = "FHB_Severity")
# Visualize with a heatmap
fb.heatmap(fb.STPFHB16, plot.traits = "FHB_Severity_mean")

# Read in processed fbs
fb.STP16 <- read.fb(file = "C:/Users/Jeff/Google Drive/Barley Lab/Projects/Genomic Selection/Phenotypic Data/Processed/2016/S2_MET_STP16_field_book_table_processed.csv", unique.id = "unique_id")
fb.CRM16 <- read.fb(file = "C:/Users/Jeff/Google Drive/Barley Lab/Projects/Genomic Selection/Phenotypic Data/Processed/2016/S2_MET_CRM16_field_book_table_processed.csv", unique.id = "unique_id")

# Spatially adjust
STP16.spat.adj <- fb.spatial.adj(field.book.table = fb.STP16, adj.traits = c("Heading", "Height"),
                                 checks = c("Pinnacle", "ND_Gensis", "Hockett", "AC_Metcalfe",
                                            "AAC_Syngergy", "LCS_Genie", "CDC_Copeland", "Conlon",
                                            "Conrad"))



# Test modelling
# y_ij = mu + g/c + r/e + ge + error
model.formula <- as.formula(paste(trait, "~ 1 + check/exp + trial/rep + exp:trial"))
fm <- lm(model.formula, data = fb.merged)
fm.anova <- anova(fm)
plot(fm)

MS_geno <- fm.anova["check:exp", "Mean Sq"]
MS_ge <- fm.anova["exp:trial", "Mean Sq"]
MS_error <- fm.anova["Residuals", "Mean Sq"]

n.rep <- length(unique(fb.merged$rep))
n.trial <- length(unique(fb.merged$trial))

V_g = (MS_geno - MS_ge) / (n.rep * n.trial)
V_ge = (MS_ge - MS_error) / n.rep
V_error = MS_error

H = V_g / ( V_g + (V_ge / n.trial) + (V_error / (n.rep * n.trial)) )


## Correlation anaylsis
fb.merged.subset <- fb.merged[fb.merged$exp != 0,]
# Remove NAs
fb.merged.subset <- na.omit(fb.merged.subset)

# Order the data.frame on trial, then line name
fb.merged.subset <- fb.merged.subset[order(fb.merged.subset$trial, fb.merged.subset$line_name),]

# Correlate
cor( fb.merged.subset[fb.merged.subset$trial == "S2_MET_STP16", trait], fb.merged.subset[fb.merged.subset$trial == "S2_MET_CRM16", trait] )
# Plot
plot( fb.merged.subset[fb.merged.subset$trial == "S2_MET_STP16", trait], fb.merged.subset[fb.merged.subset$trial == "S2_MET_CRM16", trait] )







# Just using checks

model.formula <- as.formula(paste(trait, "~ 1 + exp + trial/rep + exp:trial"))
fm <- lm(model.formula, data = fb.merged[fb.merged$check == 0,])
fm.anova <- anova(fm)

MS_geno <- fm.anova["check", "Mean Sq"]
MS_ge <- fm.anova["check:trial", "Mean Sq"]
MS_error <- fm.anova["Residuals", "Mean Sq"] 

n.rep <- length(unique(fb.merged$rep))
n.trial <- length(unique(fb.merged$trial))

V_g = (MS_geno - MS_ge) / (n.rep * n.trial)
V_ge = (MS_ge - MS_error) / n.rep
V_error = MS_error

H = V_g / ( V_g + (V_ge / n.trial) + (V_error / (n.rep * n.trial)) )




### Manipulating trait names in the t3.traits df

library(fbutils)
library(dplyr)
library(stringr)
library(tools)

# Copy and convert to tibble
t3.traits1 <- t3.traits %>%
  tbl_df()

# Extract the t3 trait names
traits <- t3.traits1 %>%
  select(Trait) %>%
  as.matrix() %>%
  as.character()

# Convert to title case
traits1 <- traits %>%
  toTitleCase()

# Collapse on whitespace, hyphens, and underscores
pattern <- paste0(c(" ", "\\+", "\\-"), collapse = "|"))

traits2 <- traits1 %>%
  str_replace_all(pattern = pattern, replacement = "") %>%
  # Then remove all numbers within parentheses at the end
  str_replace_all(pattern = '\\([0-9]{2}\\)', replacement = "")

# Add the column into the data.frame
t3.traits2 <- t3.traits1 %>%
  rename(Nickname = Jeff_Nickname) %>%
  mutate(Nickname = traits2)













