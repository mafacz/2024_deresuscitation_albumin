library(meta)
library(readxl)
library(dplyr)

# Load data from an Excel file, use sheet Outcomes
df <- read_excel("../data_deresuscitation.xlsx", sheet = "Outcomes")

# Filter for relevant criteria and ensure the mortality rate column isn't NA
data <- df[df$Endpoint_mortality == "yes" & !is.na(df$Endpoint_mortality), ]

# Convert mortality rates to proportion (as they are likely in percentages) and rename columns
data <- data %>%
  mutate(
    C_mortality_proportion_30_d = as.numeric(`C_mortality_rate_30_d_%`),
    C_mortality_rate_30_d_n = as.numeric(`C_mortality_rate_30_d_n`),
    I_mortality_rate_30_d_n = as.numeric(`I_mortality_rate_30_d_n`),
    I_mortality_proportion_30_d = as.numeric(`I_mortality_rate_30_d_%`)
  )

str(data$C_mortality_proportion_30_d)

# Select relevant columns for meta-analysis
meta_data <- data %>%
  select(
    Study, `Authors (First_et_al)`, Year_of_publication,
    C_n, C_mortality_rate_30_d_n, C_mortality_proportion_30_d,
    I_n, I_mortality_rate_30_d_n, I_mortality_proportion_30_d
  )

# Calculate effect sizes (Risk Ratios) and standard errors
meta_data <- meta_data %>%
  mutate(
    event_e = I_mortality_rate_30_d_n,  # Number of events in intervention group
    event_c = C_mortality_rate_30_d_n,  # Number of events in control group
    n_e = I_n,  # Total number in intervention group
    n_c = C_n   # Total number in control group
  )

# Run the meta-analysis for Risk Ratios
meta_result <- metabin(
  event_e,
  n_e,
  event_c,
  n_c,
  data = meta_data,
  studlab = paste(meta_data$`Authors (First_et_al)`, meta_data$Year_of_publication),
  sm = "RR",  # Risk Ratios
  method = "Inverse",  # Inverse variance method
  comb.fixed = FALSE,
  comb.random = TRUE
)

png(filename = "../forest_mortality.png", width = 15, height = 9, units = "in", res = 300)

# Create a forest plot
forest(meta_result,
       layout = "RevMan5",
       print.subgroup.name = FALSE,
       label.left = "Favours Albumin",
       label.right = "Favours no Albumin",
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
