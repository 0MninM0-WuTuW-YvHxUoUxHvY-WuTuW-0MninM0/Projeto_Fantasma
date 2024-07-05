# Projeto Fantasma Lucas.

library(readr)
banco_final <- read_csv("Banco de dados/banco_final.csv")
View(banco_final)

library(tidyverse)


## Tema da Estat
estat_colors <- c(
  "#A11D21", "#003366", "#CC9900",
  "#663333", "#FF6600", "#CC9966",
  "#999966", "#006606", "#008091", 
  "#041835", "#666666" )

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = estat_colors),
      scale_colour_manual(values = estat_colors)
    )
  )
}

# Número de lançamentos a cada década por formato de lançamento


### manipulação

categorias <- unique(banco_final$format)
categorias
#mudando o nome das categorias
banco_final <- banco_final %>% 
  mutate(format=recode(format,
                       "Movie"= "Filme",
                       "Serie"= "Série"))
#mudando o nome das colunas
banco_final <- banco_final %>% 
  rename(Formato = format, Estreia = date_aired)
view(banco_final)


# Arrumando as décadas
banco_final <- banco_final %>% 
  mutate(D_Estreia=substr(Estreia, 1,4))

banco_final$D_Estreia <- paste0(substr(banco_final$D_Estreia, 1,3), "0")
banco_final$D_Estreia

View(banco_final)
### gráfico

banco_dados <- banco_final %>% 
  group_by(D_Estreia, Formato) %>% 
  summarise(frequência = n())
view(banco_dados)

ggplot(banco_dados) +
  aes(x = D_Estreia, y = frequência, group = Formato, colour = Formato) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Décadas", y = "Lançamentos") +
  theme_estat()
ggsave("Lançamentos.pdf", width = 158, height = 93, units = "mm")

