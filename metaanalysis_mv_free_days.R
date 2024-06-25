library(meta)
library(readxl)
library(dplyr)

#load data from excel file xlsx, use sheet Outcomes
df <- read_excel("../data_deresuscitation.xlsx", sheet = "Outcomes")

#filter for Endpoint_FB = yes and hypoalbuminemia
data <- df[df$Endpoint_MV_duration == "yes" & !is.na(df$Endpoint_MV_duration), ]

# Convert relevant columns to numeric (if they are not already)
data$I_mechanical_ventilation_duration_d <- as.numeric(data$I_mechanical_ventilation_duration_d)
data$C_mechanical_ventilation_duration_d <- as.numeric(data$C_mechanical_ventilation_duration_d)
data$I_mechanical_ventilation_duration_IQR_25 <- as.numeric(data$I_mechanical_ventilation_duration_IQR_25)
data$I_mechanical_ventilation_duration_IQR_75 <- as.numeric(data$I_mechanical_ventilation_duration_IQR_75)
data$C_mechanical_ventilation_duration_IQR_25 <- as.numeric(data$C_mechanical_ventilation_duration_IQR_25)
data$C_mechanical_ventilation_duration_IQR_75 <- as.numeric(data$C_mechanical_ventilation_duration_IQR_75)

# Select relevant columns for meta-analysis
meta_data <- data %>%
  select(
    Study, `Authors (First_et_al)`, Year_of_publication, 
    C_n, C_mechanical_ventilation_duration_d, C_mechanical_ventilation_duration_IQR_25, C_mechanical_ventilation_duration_IQR_75,
    I_n, I_mechanical_ventilation_duration_d, I_mechanical_ventilation_duration_IQR_25, I_mechanical_ventilation_duration_IQR_75
  )

# Calculate effect sizes (Mean Difference) and standard errors
meta_data <- meta_data %>%
  mutate(
    TE = I_mechanical_ventilation_duration_d - C_mechanical_ventilation_duration_d,  # Treatment Effect
    SD_I = (I_mechanical_ventilation_duration_IQR_75 - I_mechanical_ventilation_duration_IQR_25) / 1.35,  # Approximate SD for intervention group
    SD_C = (C_mechanical_ventilation_duration_IQR_75 - C_mechanical_ventilation_duration_IQR_25) / 1.35,  # Approximate SD for control group
    SE_I = SD_I / sqrt(I_n),  # Standard Error for intervention group using sample size
    SE_C = SD_C / sqrt(C_n),  # Standard Error for control group using sample size
    seTE = sqrt(SE_I^2 + SE_C^2)  # Combined SE assuming independent samples
  )

# manually replace value for Martin et. al. 2005, as data given in paper directly TE 4.5, CI 95% -2.5 to 11.5 days (SE = (Upperlimmit - Lowerlimit) / 2*1.96) 
meta_data$TE[meta_data$`Authors (First_et_al)` == "Martin et al"] <- 4.5
meta_data$seTE[meta_data$`Authors (First_et_al)` == "Martin et al"] <- 3.57

# Run the meta-analysis
meta_result <- metagen(
  TE,
  seTE,
  data = meta_data,
  studlab = paste(meta_data$`Authors (First_et_al)`, meta_data$Year_of_publication),
  sm = "MD",  # Mean Difference
  comb.fixed = FALSE,
  comb.random = TRUE
)

png(filename = "../forest_mv.png", width = 15, height = 9, units = "in", res = 300)

# Create a forest plot
forest(meta_result,
       layout = "RevMan5",
       print.subgroup.name = FALSE,
       label.left = "Favours no Albumin",
       label.right = "Favours Albumin",
       digits = 2,
       digits.tau2 = 0,
       digits.weights = 2,
       colgap = "0.7cm",
       colgap.forest = "1cm",
       col.by = "black",
       col.square = "black",
       col.inside = "black",
       col.square.lines = "black",
       overall=FALSE, overall.hetstat = FALSE)

dev.off()

# print the p value for the overall effect random effects model
cat("P-value for the overall effect (random effects model):", meta_result$pval.random)