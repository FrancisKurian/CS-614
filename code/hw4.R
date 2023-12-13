#install_github("vancebee/MarkovSCD")
#install.packages("devtools")
library(lubridate)
library(ggplot2)
library(dplyr)
library(devtools)
library(MarkovSCD)
library(reshape2)

#Load Baseline and Treatment Phase data for one home
BL = HM2$MassAve[HM2$Phase == "BL"]
TX = HM2$MassAve[HM2$Phase == "TX"]
#Define state boundaries
sb = seq(30,90,10)

cv = dynamicsconv(tseries1 = BL, tseries2 = TX, nitvl = 10,statebounds = sb,lag = 6)
il1 = cv$ilength1[7]
il2 = cv$ilength2[7]
vv = validitycheck(tseries1 = BL, tseries2 = TX, ilength1 = il1,ilength2 = il2, statebounds = sb,lag = 6)
vv$norm

offset_values <- c(-4, -3, -2, -1, 0, 1, 2)
# Create a data frame
df <- data.frame(norm = vv$norm, offset = offset_values)
# Print the data frame
print(df)


# 1. Reproduce Figure 6, top center panel using the following code:

ggplot(df, aes(x = offset, y = norm)) +
  geom_line(color = "black", size = 0.5) +
  geom_point(size = 3, shape = 19, color = "black") +
  scale_x_continuous(breaks = df$offset) +
  labs(title = "Home 209", x = "Iteration Offset from A", y = "Norm") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightgrey"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 14,color = "grey"),
    axis.text.y = element_text(size = 14,color = "grey"),
    axis.line = element_line(color = "transparent"),
    plot.background = element_rect(fill = "white", color = "white") ,
    plot.title = element_text(size = 20, hjust = 0.5) 
  )

# 2 Reproduce Figure 6, bottom center panel.

vv$diagconfig
df <- as.data.frame(vv$diagconfig)
colnames(df) <- -4:2
df$Pos <- c(1, 0, -1)

# Print the resulting data frame
print(df)

melted_df <- melt(df, id.vars = "Pos", variable.name = "X", value.name = "Value")
print(melted_df)

# Define custom colors
custom_colors <- c("1" = "#009E73", "0" = "#0072B2", "-1" = "#E69F00")

# Create a line plot with custom colors for each 'Pos' group
ggplot(melted_df, aes(x = X, y = Value, group = Pos)) +
  geom_line(aes(color = factor(Pos))) +
  geom_point(aes(color = factor(Pos)), size = 3, shape = 19) +
  scale_color_manual(values = custom_colors) +
  labs(title = "Home 209", x = "Iteration Offset from A", y = "Mean Value") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightgrey"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 14, color = "grey"),
    axis.text.y = element_text(size = 14, color = "grey"),
    axis.line = element_line(color = "transparent"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = c(0.08, 0.98),   
    legend.title = element_blank(),   
    legend.text = element_text(size = 14)
  ) +
  guides(color = guide_legend(direction = "horizontal"))   

#3. Reproduce the center panel of Figure B.8. using the following code to get the data:

le = lageval(tseries = TX,statebounds = sb, lagrange = c(1,2,seq(3,60,3)))

df2 <- data.frame(le)
df2 <- df2 %>%
  mutate(Lag = lagrange / 6) %>%
  select(-lagrange)

new_colnames <- c( "S1", "S2", "S3", "S4", "S5", "S6", "Lag")
colnames(df2) <- new_colnames
print(df2)

df2_long <- melt(df2, id.vars = "Lag", variable.name = "State")
print(df2_long)


ggplot(df2_long, aes(x = Lag, y = value, shape = State, color = State)) +
  geom_line(size = 0.5) +
  geom_point(aes(fill = State), size = 3) +
  labs(x = "Lag", y = "Probabilities") +
  scale_x_continuous(breaks = seq(0, 10, by = 2.5)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
  scale_shape_manual(values = c("S1" = 0, "S2" = 1, "S3" = 2, "S4" = 3, "S5" = 4, "S6" = 5)) +
  scale_color_manual(values = c("S1" = "blue", "S2" = "red", "S3" = "green", "S4" = "purple", "S5" = "orange", "S6" = "brown")) +
  scale_fill_manual(values = c("S1" = "blue", "S2" = "red", "S3" = "green", "S4" = "purple", "S5" = "orange", "S6" = "brown")) +
  theme_minimal() +
  theme(legend.position = "top") +
  theme(
    panel.background = element_rect(fill = "lightgrey"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 14, color = "grey"),
    axis.text.y = element_text(size = 14, color = "grey"),
    axis.line = element_line(color = "transparent"),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.text = element_text(size = 14)
  )

# 4. Reproduce the center figure of the second row of Figure 4 using the following code:

B = transmat(tseries = TX,statebounds = sb,lag = 6)
B$prob
df3 <- as.data.frame(B$prob)
rownames(df3) <- colnames(df3) <- paste0("S", 1:6)
df3$Y <- rownames(df3)




# Create a data frame from the matrix and transpose it
df <- as.data.frame(B$prob)
df <- t(df)
df <- melt(df)

df_sorted <- df %>%
  arrange(Var1, desc(Var2))

ggplot(df_sorted, aes(x = Var1, y = Var2, fill = value, label = round(value, 2))) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkorange", trans = "sqrt") +
  geom_text(size = 3, color = "black") +
  labs(title = "Home 209", x = "DestBin", y = "SourceBin") +
  scale_x_discrete(limits = rev(unique(df_sorted$Var2))) +
  scale_y_discrete(limits = rev(unique(df_sorted$Var1))) +
  theme_minimal()


