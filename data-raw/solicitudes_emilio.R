#############################################
# Actividad: Proyecciones congreso
# Rafael López
# 23/02/2024
#############################################

## Visualizaciones senadores
bd <- readxl::read_excel("~/Desktop/calculo_senado_rp.xlsx") |>
  janitor::clean_names()

colores <- c("#023e8a", "#3a5a40", "#780000")

bd_2024 <- tibble(ano = 2024, partido =  "MORENA", votos_relativos = 0.53,
                  colores = "#780000", escanos_estimados = 16) |>
  mutate(label = paste(partido, ano, "*"))

bd <- bd |>
  filter(voto_efectivos == max(voto_efectivos, na.rm = T), .by = ano) |>
  select(ano, partido, voto_efectivos, escanos_asignados) |>
  mutate(colores = colores, label = paste(partido, ano))

bd |>
  ggplot(aes(x = voto_efectivos, y = escanos_asignados, label = label)) +
  # geom_smooth(se = T, color = "gray77") +
  geom_point(aes(color = colores), size = 3, alpha = 0.8) +
  geom_line(data = tibble(y = 1:32, x = y/32), aes(x = x, y = y), inherit.aes = F, linetype = "dashed", color = "gray66", alpha = 0.65) +
  ggrepel::geom_label_repel() +
  scale_x_continuous(limits = c(0.25, 0.7), labels = scales::percent) +
  scale_y_continuous(breaks = seq(10, 20, 1), limits = c(10, 20)) +
  scale_color_identity(guide = FALSE) +
  geom_point(data = bd_2024, aes(x = votos_relativos, y = escanos_estimados, color = colores),
             size = 3) +
  ggrepel::geom_label_repel(data = bd_2024, aes(x = votos_relativos, y = escanos_estimados,label = label)) +
  theme_minimal(base_size = 12, base_family = "Poppins") +
  labs(x = "Votos efectivos", y = "Escaños obtenidos", caption = "* Estimación obtenida con gppolls 2024")




