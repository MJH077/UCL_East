# Read data
pantheria <- read.table(
  file = "E:/UCL_East/R/Practical1_PanTHERIA_1-0_WR05_Aug2008.txt",
  header = TRUE,
  sep = "\t",
  na.strings = c("-999", "-999.00")
)

# Save as csv
write.csv(pantheria, file = "E:/UCL_East/R/Practical1_Pantheria.csv", row.names = FALSE)


# ======================================
# 1. Explore the structure of the dataset
dim(pantheria)
str(pantheria)
head(pantheria)

# 2. Examine the column names
names(pantheria)

# 3. Count the number of species
length(unique(na.omit(pantheria$MSW05_Binomial)))

# 4. Count the number of genera
length(unique(na.omit(pantheria$MSW05_Genus)))

# 5. Count the number of species in each order
order_table <- table(pantheria$MSW05_Order)
order_table


# ======================================
# 1. Minimum and maximum adult body mass
min(pantheria$X5.1_AdultBodyMass_g, na.rm = TRUE)
max(pantheria$X5.1_AdultBodyMass_g, na.rm = TRUE)

# 2. Mean litter size
mean(pantheria$X15.1_LitterSize, na.rm = TRUE)

# 3. Quantiles for maximum longevity
quantile(pantheria$X17.1_MaxLongevity_m, na.rm = TRUE)

# 4. Boxplot of adult body mass across trophic levels
box_data <- subset(
  pantheria,
  !is.na(X5.1_AdultBodyMass_g) &
    X5.1_AdultBodyMass_g > 0 &
    !is.na(X6.2_TrophicLevel)
)

boxplot(
  X5.1_AdultBodyMass_g ~ X6.2_TrophicLevel,
  data = box_data,
  xlab = "Trophic level",
  ylab = "Adult body mass (g)",
  main = "Adult body mass across trophic levels",
  log = "y",
  col = c("darkred", "darkgreen", "darkblue")
)

# 5. Mean, median and standard deviation
mean(pantheria$X5.1_AdultBodyMass_g, na.rm = TRUE)
median(pantheria$X5.1_AdultBodyMass_g, na.rm = TRUE)
sd(pantheria$X5.1_AdultBodyMass_g, na.rm = TRUE)

mean(pantheria$X17.1_MaxLongevity_m, na.rm = TRUE)
median(pantheria$X17.1_MaxLongevity_m, na.rm = TRUE)
sd(pantheria$X17.1_MaxLongevity_m, na.rm = TRUE)


# ===========================
# 1. Number of missing values in each column
na_per_column <- colSums(is.na(pantheria))
na_per_column

# 2. Show the name of the column with the most missing values
names(na_per_column)[which.max(na_per_column)]

# 3. Show any rows without missing values
rows_without_na <- complete.cases(pantheria)
sum(rows_without_na)

# 4. Check whether any complete rows exist
any(rows_without_na)


# ============================================
# 1. Filter mammals with adult body mass >= 15 kg
# Body mass is recorded in grams
large_mammals <- subset(
  pantheria,
  !is.na(X5.1_AdultBodyMass_g) &
    X5.1_AdultBodyMass_g >= 15000
)
# View the filtered data
head(large_mammals)
# Number of mammals in the subset
nrow(large_mammals)

# 2. Mean, median and standard deviation
body_mass <- large_mammals$X5.1_AdultBodyMass_g
mean(body_mass, na.rm = TRUE)
median(body_mass, na.rm = TRUE)
sd(body_mass, na.rm = TRUE)

# 3. Plot body mass values
hist(
  body_mass,
  main = "Adult Body Mass of Mammals >= 15 kg",
  xlab = "Adult body mass (g)",
  ylab = "Frequency",
  col = "lightblue",
  border = "white"
)

boxplot(
  body_mass,
  main = "Boxplot of Adult Body Mass",
  ylab = "Adult body mass (g)",
  col = "lightgreen"
)

# 4. Identify and remove outliers
Q1 <- quantile(body_mass, 0.25, na.rm = TRUE)
Q3 <- quantile(body_mass, 0.75, na.rm = TRUE)
IQR_value <- IQR(body_mass, na.rm = TRUE)

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

outliers <- body_mass[
  body_mass < lower_bound | body_mass > upper_bound
]

outliers
length(outliers)

outlier_mammals <- large_mammals[
  !is.na(large_mammals$X5.1_AdultBodyMass_g) &
    (
      large_mammals$X5.1_AdultBodyMass_g < lower_bound |
        large_mammals$X5.1_AdultBodyMass_g > upper_bound
    ),
]

outlier_mammals[
  ,
  c("MSW05_Binomial", "X5.1_AdultBodyMass_g")
]

body_mass_no_outliers <- body_mass[
  body_mass >= lower_bound & body_mass <= upper_bound
]

cat("Before removing outliers:\n")
cat("Mean:", mean(body_mass, na.rm = TRUE), "\n")
cat("Median:", median(body_mass, na.rm = TRUE), "\n")
cat("SD:", sd(body_mass, na.rm = TRUE), "\n")

cat("\nAfter removing outliers:\n")
cat("Mean:", mean(body_mass_no_outliers, na.rm = TRUE), "\n")
cat("Median:", median(body_mass_no_outliers, na.rm = TRUE), "\n")
cat("SD:", sd(body_mass_no_outliers, na.rm = TRUE), "\n")

boxplot(
  body_mass_no_outliers,
  main = "Adult Body Mass Without Outliers",
  ylab = "Adult body mass (g)",
  col = "orange"
)
