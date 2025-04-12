# ===================== MM   ==========================
# =====================================================
# Luis Chávez, 2025

# A) Función C-D -----------------------
# Instalar paquetes si no están
if (!require("plotly")) install.packages("plotly")
library(plotly)

# Definir la función Cobb-Douglas
cobb_douglas <- function(K, L, alpha = 0.3, A = 1) {
  A * (K ^ alpha) * (L ^ (1 - alpha))
}

K_vals <- seq(1, 10, length.out = 100)
L_vals <- seq(1, 10, length.out = 100)
K_grid <- matrix(rep(K_vals, each = 100), nrow = 100)
L_grid <- matrix(rep(L_vals, times = 100), nrow = 100)

# Calcular Y
Y_grid <- cobb_douglas(K_grid, L_grid)

# Gráfico 3D interactivo con plotly
fig <- plot_ly(x = ~K_vals, y = ~L_vals, z = ~Y_grid) %>%
  add_surface() %>%
  layout(title = "Cobb-Douglas Production Function",
         scene = list(xaxis = list(title = "K"),
                      yaxis = list(title = "L"),
                      zaxis = list(title = "Y")))

fig
