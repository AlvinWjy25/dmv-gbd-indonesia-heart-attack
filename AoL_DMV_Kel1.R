install.packages("corrplot")
install.packages('scales')
install.packages('forcats')

library(ggplot2)
library(dplyr)
library(forcats)
library(scales)
library(corrplot)

df = read.csv2("heart_attack_prediction_indonesia.csv", sep = ',')
df2 = read.csv2("IHME-GBD_2021_DATA-1cf18c61-1.csv", sep = ',')
View(df2)
View(df)
summary(df2)

x <- df$hypertension
y <- df$heart_attack

x1 = as.numeric(x)
y1 = as.numeric(y)
cor.test(x, y, method = "pearson")
ggplot(df, aes(x = heart_attack, y = alcohol_consumption, color = as.factor(heart_attack))) +
  geom_point()

ggplot(df, aes(x = age))+
  geom_boxplot()

#  --- Corrplot to test relevance -----------------------------------------------------------------


num_data <- df[, sapply(df, is.numeric)]
cor_matrix <- cor(num_data)

corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)

#  --- Heart Attack count based on physical_activity (Bar chart) -----------------------------------------------------------------

# Ubah ke faktor jika perlu
df$heart_attack <- as.factor(df$heart_attack)
df$alcohol_consumption <- as.factor(df$alcohol_consumption)

# Buat tabel kontingensi
table_data <- table(df$heart_attack, df$obesity)
table_data
# Lakukan uji chi-square
chisq.test(table_data)

ggplot(df, aes(x = physical_activity, fill = heart_attack))+
  geom_bar(position = 'dodge')+
  theme_minimal()

#Jadiin Persentage
df_summary <- df %>%
  count(obesity, heart_attack) %>%
  group_by(obesity) %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(df_summary, aes(x = obesity, y = percentage, fill = heart_attack)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), color = "white") +
  ylab("Percentage") +
  theme_minimal()

#  --- Heart Attack from previous Heart Disease (Bar chart) -----------------------------------------------------------------
# Hitung jumlah dan proporsi
df_summary <- df %>%
  count(previous_heart_disease, heart_attack) %>%
  group_by(previous_heart_disease) %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(df_summary, aes(x = previous_heart_disease, y = percentage, fill = heart_attack)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), color = "white") +
  ylab("Percentage") +
  theme_light()


#  --- Distribution of Heart Attack from previous History (Donut) -----------------------------------------------------------------

# Sample data
df = read.csv2("heart_attack_prediction_indonesia.csv", sep = ',')

# Summarize data
df_summary <- df %>%
  count(previous_heart_disease, heart_attack) %>%
  group_by(previous_heart_disease) %>%
  mutate(percentage = n / sum(n) * 100)

# Combine alcohol_consumption and heart_attack for labeling
df_summary <- df_summary %>%
  mutate(group = paste(previous_heart_disease, heart_attack, sep = " - "))

# Donut chart
ggplot(df_summary, aes(x = 2, y = percentage, fill = group)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +  # Creates the hole in the middle
  theme_void() +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Distribution of Heart Attack by Previous Heart Disease") +
  theme(legend.title = element_blank())

x <- df2$val
val2 <- as.numeric(x)

df2 <- df2 %>%
  mutate(kategori = fct_reorder(cause_name, val2))

ggplot(df2, aes(x = kategori, y = val2, fill = kategori)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  coord_flip() +
  theme_minimal() +
  labs(x = "Cause", y = "Value", fill = "Cause")

#  --- Heart Attack vs Cholesterol numeric Level -----------------------------------------------------------------

df_clean = df
df_clean$cholesterol_level <- as.numeric(df$cholesterol_level)

df_clean <- df %>%
  mutate(
    cholesterol_level = case_when(
      cholesterol_level < 100 ~ "Very Low",
      cholesterol_level >= 100 & cholesterol_level < 130 ~ "Low",
      cholesterol_level >= 130 & cholesterol_level < 200 ~ "Normal",
      cholesterol_level >= 200 & cholesterol_level < 240 ~ "High",
      cholesterol_level >= 240 ~ "Very High",
      is.na(cholesterol_level) | cholesterol_level == 0 ~ NA_character_,
      TRUE ~ NA_character_
    )
  ) %>%
 
df_clean$cholesterol_level <- factor(df$cholesterol_level, 
                               levels = c("Very Low", "Low", "Normal", "High", "Very High"))
summary(df_clean$cholesterol_level)

df_clean <- df_clean %>%
  mutate(
    risk_level = cut(
      cholesterol_level,
      breaks = c(0, 100, 130, 200, 240, Inf),
      labels = c("Very Low", "Low", "Normal", "High", "Very High"),
      include.lowest = TRUE
    )
  )
summary(df_clean)

ggplot(df_clean, aes(x = cholesterol_level, fill = risk_level)) +
  geom_histogram(binwidth = 20, color = "white") +
  scale_fill_manual(
    values = c("Very Low" = "#2ecc71", "Low" = "#f1c40f", "Normal" = "#e67e22", 
               "High" = "#e74c3c", "Very High" = "#c0392b"),
    name = "Tingkat Risiko"
  ) +
  labs(
    title = "Distribusi Kolesterol Darah (Numerik)",
    x = "Nilai Kolesterol (mg/dL)",
    y = "Jumlah Kasus"
  ) +
  theme_minimal()
  


# --- Heart Attack vs Cholesterol Category(Low, Normal, High, Very High)---------------------------------------------
library(dplyr)
library(ggplot2)

df_clean <- df %>%
  mutate(
    cholesterol_level = as.numeric(cholesterol_level)  
  ) %>%
  filter(!is.na(cholesterol_level) & cholesterol_level > 0)  

df_clean <- df_clean %>%
  mutate(
    cholesterol_category = case_when(
      cholesterol_level < 100 ~ "Very Low",
      cholesterol_level >= 100 & cholesterol_level < 130 ~ "Low",
      cholesterol_level >= 130 & cholesterol_level < 200 ~ "Normal",
      cholesterol_level >= 200 & cholesterol_level < 240 ~ "High",
      cholesterol_level >= 240 ~ "Very High",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(cholesterol_category)) 



df_clean$cholesterol_category <- factor(
  df_clean$cholesterol_category,
  levels = c("Very Low", "Low", "Normal", "High", "Very High"),
  ordered = TRUE
)

summary(df_clean$cholesterol_category)

risk_colors <- c(
  "Very Low" = "#2ecc71",  
  "Low" = "#f1c40f",       
  "Normal" = "#e67e22",    
  "High" = "#e74c3c",      
  "Very High" = "#c0392b"  
)

ggplot(df_clean, aes(x = cholesterol_category, fill = cholesterol_category)) +
  geom_bar(position = "dodge", aes(alpha = heart_attack)) +
  labs(
    title = "Distribusi Serangan Jantung berdasarkan Tingkat Kolesterol",
    subtitle = "Warna mencerminkan tingkat risiko kolesterol",
    x = "Tingkat Kolesterol Darah",
    y = "Jumlah Kasus Serangan Jantung",
    fill = "Tingkat Risiko",
    alpha = "Serangan Jantung"
  ) +
  theme_minimal() +
  scale_fill_manual(values = risk_colors) + 
  scale_alpha_manual(values = c(0.6, 1))
