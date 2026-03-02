# install.packages("arrow")  # if needed
library(arrow)

df <- read.csv("data_clean/equation_application_clean.csv", stringsAsFactors = FALSE)

# Snappy (fast, decent compression)
write_parquet(df, "data_clean/equation_application_clean.snappy.parquet", compression = "snappy")

# ZSTD (usually smaller)
write_parquet(df, "data_clean/equation_application_clean.zstd.parquet", compression = "zstd")

file.info(c(
  "db/allometry.sqlite",
  "data_clean/equation_application_clean.snappy.parquet",
  "data_clean/equation_application_clean.zstd.parquet"
))[, c("size")]
