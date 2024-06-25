library(meta)
library(readxl)
library(dplyr)

#load data from excel file xlsx
df <- read_excel("../data_deresuscitation.xlsx")

#filter for Endpoint_FB = yes
data <- df[df$Endpoint_FB == "yes" & !is.na(df$Endpoint_FB), ]

# Convert relevant columns to numeric (if they are not already)
data$I_fluid_balance_mL <- as.numeric(data$I_fluid_balance_mL)
data$C_fluid_balance_mL <- as.numeric(data$C_fluid_balance_mL)
data$I_fluid_balance_mL_SD <- as.numeric(data$I_fluid_balance_mL_SD)
data$C_fluid_balance_mL_SD <- as.numeric(data$C_fluid_balance_mL_SD)

#correct the values depending on the number of days as measured in fluid_balance days
data$I_fluid_balance_mL_d <- data$I_fluid_balance_mL / data$fluid_balance_days
data$C_fluid_balance_mL_d <- data$C_fluid_balance_mL / data$fluid_balance_days
data$I_fluid_balance_mL_d_SD <- data$I_fluid_balance_mL_SD / data$fluid_balance_days #this is correct, as the standard deviation is also devided by the days
data$C_fluid_balance_mL_d_SD <- data$C_fluid_balance_mL_SD / data$fluid_balance_days

data = data %>% mutate(RCT = ifelse(RCT == "yes", "Randomized controlled trials", "Observational trials"))

# Select relevant columns for meta-analysis
meta_data <- data %>%
  select(
    Study, `Authors (First_et_al)`, Year_of_publication, RCT, fluid_balance_days,
    C_n, C_fluid_balance_mL_d, C_fluid_balance_mL_d_SD, C_fluid_balance_mL, C_fluid_balance_mL_SD, 
    I_n, I_fluid_balance_mL_d, I_fluid_balance_mL_d_SD, I_fluid_balance_mL, I_fluid_balance_mL_SD,
  )

#################################
##### Meta analysis Fig 1
#################################

# Calculate effect sizes (Mean Difference)
meta_data <- meta_data %>%
  mutate(
    TE = (I_fluid_balance_mL_d - C_fluid_balance_mL_d) / 1000,  # Treatment Effect converted to L/day
    seTE = (sqrt((I_fluid_balance_mL_d_SD^2 / I_n) + (C_fluid_balance_mL_d_SD^2 / C_n))) / 1000  # Standard Error
  )

# Run the meta-analysis
meta_result <- metagen(
  TE,
  seTE,
  data = meta_data,
  studlab = paste(meta_data$`Authors (First_et_al)`, meta_data$Year_of_publication),
  sm = "MD",  # Mean Difference
  comb.fixed = FALSE,
  comb.random = TRUE,
  byvar = RCT
)

png(filename = "../forest.png", width = 9.5, height = 5, units = "in", res = 300)

# Create a forest plot
forest(meta_result,
       layout = "RevMan5",
       print.subgroup.name = FALSE,
       label.left = "Favours Albumin",
       label.right = "Favours no Albumin",
       digits = 2,
       digits.tau2 = 3,
       digits.weights = 2,
       colgap = "0.7cm",
       colgap.forest = "1cm",
       col.by = "black",
       col.square = "black",
       col.inside = "black",
       col.square.lines = "black")

dev.off()

# print the p value for the overall effect random effects model
cat("P-value for the overall effect (random effects model):", meta_result$pval.random)


#################################
##### Prediction interval
#################################

# Extract necessary components from the meta-analysis result
TE <- meta_result$TE.random
seTE <- meta_result$seTE.random
tau2 <- meta_result$tau2

# Critical value for 95% confidence (z-value)
z <- 1.96

# Calculate the prediction interval
lower_bound <- TE - z * sqrt(seTE^2 + tau2)
upper_bound <- TE + z * sqrt(seTE^2 + tau2)

# Print the prediction interval rounded to two digits

cat("The 95% prediction interval is from", round(upper_bound,2), "to", round(lower_bound,2))

#################################
##### Bias assessment
#################################

# Creating a funnel plot
png(filename = "../funnel_plot.png", width = 15, height = 9, units = "in", res = 300)
funnel(meta_result, main = "Funnel Plot of fluid balance", xlim = c(-1.5, 1.5))
dev.off()

#################################
##### Meta regression
#################################

# Calculate effect sizes (Mean Difference)
meta_data <- meta_data %>%
  mutate(
    TE = (I_fluid_balance_mL - C_fluid_balance_mL) / 1000,  # Treatment Effect converted to L/day
    seTE = (sqrt((I_fluid_balance_mL_SD^2 / I_n) + (C_fluid_balance_mL_SD^2 / C_n))) / 1000  # Standard Error
  )

# Calculate effect sizes (Mean Difference)
meta_data <- meta_data %>%
  mutate(
    days_stand = fluid_balance_days - 2,  # standart treatment days to two
  )

# Conduct meta-regression
res <- metareg(metagen(TE = TE, seTE = seTE, data = meta_data), ~ days_stand)

# Summary of the meta-regression
summary(res)
