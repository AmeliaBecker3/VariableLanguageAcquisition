library(here)
Study1 <- read.csv("CDILEXexp1_encrypted.csv")
Study2 <- read.csv("CDILEXexp2_encrypted.csv")
LEX1 <- read.csv("LEX1.0.csv")
LEX2 <- read.csv("LEX2.0.csv")


# plot histogram of neighborhood density for Study1 and Study2

hist(Study1$NeighborhoodDensity, main = "Histogram of Neighborhood Density for Study 1", xlab = "Neighborhood Density", ylab = "Frequency", col = "lightblue", border = "black")
hist(Study2$NeighborhoodDensity, main = "Histogram of Neighborhood Density for Study 2", xlab = "Neighborhood Density", ylab = "Frequency", col = "lightgreen", border = "black")

# plot historgram of neighborhood deensity for LEX1 and LEX2

hist(LEX1$MaximalNeighborhoodDensity, main = "Histogram of Neighborhood Density for LEX1", xlab = "Neighborhood Density", ylab = "Frequency", col = "lightblue", border = "black")
hist(LEX2$Neighborhood.Density.2.0, main = "Histogram of Neighborhood Density for LEX2", xlab = "Neighborhood Density", ylab = "Frequency", col = "lightgreen", border = "black")

# compare distribution of neighborhood density for the CDIs and ASL-LEX using ks.test

ks.test(Study1$NeighborhoodDensity, LEX1$MaximalNeighborhoodDensity)
ks.test(Study2$NeighborhoodDensity, LEX2$Neighborhood.Density.2.0)

min(Study1$NeighborhoodDensity)
min(LEX1$MaximalNeighborhoodDensity)
max(Study1$NeighborhoodDensity)
max(LEX1$MaximalNeighborhoodDensity)
min(Study2$NeighborhoodDensity)
min(LEX2$Neighborhood.Density.2.0)
max(Study2$NeighborhoodDensity)
max(LEX2$Neighborhood.Density.2.0)


# check for any values of Sign in Study1 and Study2 where NeighborhoodDensity is not the same in both Study1 and Study2 for that sign
# Keep unique Sign-ND pairs in each df
Study1_unique <- unique(Study1[, c("Sign", "NeighborhoodDensity")])
Study2_unique <- unique(Study2[, c("Sign", "NeighborhoodDensity")])

# Merge on Sign
merged_df <- merge(Study1_unique, Study2_unique, by = "Sign", suffixes = c(".Study1", ".Study2"))

# Filter where NeighborhoodDensity differ
diff_df <- merged_df[merged_df$NeighborhoodDensity.Study1 != merged_df$NeighborhoodDensity.Study2, ]

