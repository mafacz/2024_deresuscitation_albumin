library(meta)
library(readxl)
library(dplyr)

#load data from excel file xlsx
df <- read_excel("../data_deresuscitation.xlsx")

#filter for Endpoint_FB = yes and hypoalbuminemia
data <- df[df$Endpoint_FB == "yes" & !is.na(df$Endpoint_FB), ]
data <- data[data$Hypoalbuminemia == "yes" & !is.na(data$RCT), ]

# Convert relevant columns to numeric (if they are not already)
data$I_fluid_balance_mL <- as.numeric(data$I_fluid_balance_mL)
data$C_fluid_balance_mL <- as.numeric(data$C_fluid_balance_mL)
data$I_fluid_balance_mL_SD <- as.numeric(data$I_fluid_balance_mL_SD)
data$C_fluid_balance_mL_SD <- as.numeric(data$C_fluid_balance_mL_SD)

#correct the values depending on the number of days as measured in fluid_balance days
data$I_fluid_balance_mL <- data$I_fluid_balance_mL / data$fluid_balance_days
data$C_fluid_balance_mL <- data$C_fluid_balance_mL / data$fluid_balance_days
data$I_fluid_balance_mL_SD <- data$I_fluid_balance_mL_SD / data$fluid_balance_days #this is correct, as the standard deviation is also devided by the days
data$C_fluid_balance_mL_SD <- data$C_fluid_balance_mL_SD / data$fluid_balance_days

# Select relevant columns for meta-analysis
meta_data <- data %>%
  select(
    Study, `Authors (First_et_al)`, Year_of_publication, 
    C_n, C_fluid_balance_mL, C_fluid_balance_mL_SD, 
    I_n, I_fluid_balance_mL, I_fluid_balance_mL_SD
  )

# Calculate effect sizes (Mean Difference)
meta_data <- meta_data %>%
  mutate(
    TE = (I_fluid_balance_mL - C_fluid_balance_mL) / 1000,  # Treatment Effect
    seTE = sqrt((I_fluid_balance_mL_SD^2 / I_n) + (C_fluid_balance_mL_SD^2 / C_n)) / 1000  # Standard Error
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
  TElabel = "Mean FB Difference (Lday)"
)

png(filename = "../forest_fb_hypoalb.png", width = 15, height = 9, units = "in", res = 300)

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
