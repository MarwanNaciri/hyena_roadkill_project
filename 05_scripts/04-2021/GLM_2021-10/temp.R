
table.glm_5 <- read_csv("06_processed_data/glm_data_2021-10_high_certainty/table.glm_5.csv")

table.glm_6 <- read_csv("06_processed_data/glm_data_2021-09/table.glm_6.csv")
sum(table.glm_6$nbr_carcasses)
table.glm_6$nbr_carcasses <- table.glm_5$nbr_carcasses
sum(table.glm_6$nbr_carcasses)

write_csv(table.glm_6, "06_processed_data/glm_data_2021-10_high_certainty/table.glm_6.csv")




table.glm_7 <- read_csv("06_processed_data/glm_data_2021-09/table.glm_7.csv")
sum(table.glm_7$nbr_carcasses)
table.glm_7$nbr_carcasses <- table.glm_5$nbr_carcasses
sum(table.glm_7$nbr_carcasses)

write_csv(table.glm_7, "06_processed_data/glm_data_2021-10_high_certainty/table.glm_7.csv")



table.glm_8 <- read_csv("06_processed_data/glm_data_2021-09/table.glm_8.csv")
sum(table.glm_8$nbr_carcasses)
table.glm_8$nbr_carcasses <- table.glm_5$nbr_carcasses
sum(table.glm_8$nbr_carcasses)

write_csv(table.glm_8, "06_processed_data/glm_data_2021-10_high_certainty/table.glm_8.csv")


table.glm_9 <- read_csv("06_processed_data/glm_data_2021-09/table.glm_9.500m.csv")
sum(table.glm_9$nbr_carcasses)
table.glm_9$nbr_carcasses <- table.glm_5$nbr_carcasses
sum(table.glm_9$nbr_carcasses)

write_csv(table.glm_9, "06_processed_data/glm_data_2021-10_high_certainty/table.glm_9.500m.csv")
