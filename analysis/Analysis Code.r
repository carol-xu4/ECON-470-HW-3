# Analysis Code

# SUMMARIZE THE DATA

# 1 proportion of states with a change in their cigarette tax in each year from 1970 to 1985.
final.data = final.data %>%
  filter(Year >= 1970 & Year <= 1985)

tax_change = final.data %>%
  group_by(Year) %>%
  summarize(unique_tax_states = n_distinct(tax_state))

states = final.data %>%
    group_by(Year) %>%
    summarize(total_states = n_distinct(state))

tax_change = left_join(tax_change, states, by = "Year") %>%
  mutate(proportion_changed = unique_tax_states / 51)

ggplot(tax_change, aes(x = as.factor(Year), y = proportion_changed)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Year", y = "Proportion of States with Tax Change",
       title = "Change in Cigarette Tax by Year") + theme_minimal() + theme(aspect.ratio = 0.75)
  ggsave("tax_change.png")

# 2 Average tax (in 2012 dollars) on cigarettes and the average price of a pack of cigarettes from 1970 to 2018.

final.data = final.data %>%
  filter(Year >= 1970 & Year <= 2018)

average_tax = final.data %>%
  group_by(Year) %>%
  summarize(average_tax = mean(tax_dollar))

average_price = final.data %>%
  group_by(Year) %>%
  summarize(average_price = mean(cost_per_pack))

ggplot() +
  geom_line(data = average_tax, aes(x = Year, y = average_tax, color = "Average Tax"), size = 1.5) +
  geom_line(data = average_price, aes(x = Year, y = average_price, color = "Average Price"), linetype = "dashed", size = 1.5) +
  scale_color_manual(values = c("Average Tax" = "skyblue", "Average Price" = "plum")) +
  labs(x = "Year", y = "Average Price / Tax", 
       title = "Average Tax and Average Price of Cigarettes (1970-2018)",
       color = "Variable") +
  theme_minimal() + theme(aspect.ratio = 0.75)
  ggsave("avg_tax_price.png")
