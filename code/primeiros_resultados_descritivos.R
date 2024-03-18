packages = c(
  "tjsp", #pacote que fará a extração dos dados da plataforma do tjsp
  "dplyr", #para facilitar o manejo dos dados
  "stringr", #manipulação de strings
  "jsonlite", #o formato json é o ideal para esse caso, tendo em vista o limite de caracteres de células do excel
  "ggplot2",
  "ggchicklet",
  "stringi",
  "sf",
  "geobr",
  "grid",
  "tidyverse",
  "shadowtext",
  "ggspatial",
  "viridis",
  "kableExtra",
  "openxlsx")

# Instalamos (se necessário) e carregamos os pacotes
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      require(x, character.only = TRUE)
    }
  }
)

# Abrimos a base

df <- fromJSON("C:/Users/Gabriel/Documents/dadosgit.json")

# Vamos criar uma nova variável que irá conter o resultado das sentenças e recursos

df <- df %>%
  mutate(sentença = tjsp_classificar_sentenca(julgado))

df <- df %>%
  mutate(Recurso = tjsp_classificar_recurso(ementa))

# Vamos checar quais são as comarcas com maiores processos
df %>%
  dplyr::count(comarca.x, sort = TRUE) %>% # contar ocorrências
  mutate(Comarca2 = sum(n)) %>%
  mutate(porcentagem = round(n/Comarca2*100, 2)) %>%
  select(-Comarca2) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  column_spec(1, bold = TRUE)

# Podemos fazer um gráfico com as regiões administrativas e a quantidade de processos por rajs
municipios_sp <- geobr::read_municipality("SP", 2022) #usamos o pacote geobr para baixar o shapefile de sp

municipios_sp <- municipios_sp %>%
  mutate(name_muni = str_to_lower(name_muni)) %>% #tudo para minusculo
  mutate(name_muni = stri_trans_general(name_muni, "latin-ascii")) %>% #tiramos caracteres especiais
  mutate(name_muni = str_replace_all(name_muni, "-", "")) %>%
  mutate(name_muni = str_replace_all(name_muni, "'", " ")) %>%
  mutate(name_muni = str_replace_all(name_muni, "mogi mirim", "mogimirim ")) %>%
  mutate(name_muni = str_replace_all(name_muni, "mogi guacu", "mogiguacu")) %>%
  mutate(name_muni = str_replace_all(name_muni, "santa barbara d oeste", "santa barbara d'oeste")) %>%
  mutate(name_muni = str_replace_all(name_muni, "palmeira d oeste", "palmeira d'oeste")) %>%
  mutate(name_muni = str_trim(name_muni)) #remove espaços a mais


comarcas <- df %>%
  select(comarca.x) %>%
  group_by(comarca.x) %>%
  mutate(comarca.x = str_to_lower(comarca.x)) %>%
  mutate(comarca.x = stri_trans_general(comarca.x, "latin-ascii")) %>%
  mutate(comarca.x = str_replace_all(comarca.x, "-", "")) %>%
  mutate(comarca.x = str_replace_all(comarca.x, "'", " ")) %>%
  mutate(comarca.x = str_replace_all(comarca.x, "mogi guacu", "mogiguacu")) %>%
  mutate(comarca.x = str_replace_all(comarca.x, "santa barbara d oeste", "santa barbara d'oeste")) %>%
  mutate(comarca.x = str_replace_all(comarca.x, "palmeira d oeste", "palmeira d'oeste")) %>%
  mutate(comarca.x = str_trim(comarca.x)) %>%
  count(sort=T) %>%
  summarise(total = sum(n)) %>%
  mutate(perc = round(total / sum(total) * 100, 2))

comarcas <- municipios_sp %>%
  left_join(comarcas, by = c("name_muni" = "comarca.x"))

comarcas <- comarcas %>%
  select(-c("code_state","abbrev_state","code_region","name_region","name_state")) %>%
  filter(perc != is.na(perc))


sp_rajs <- read.xlsx("C:/Users/Gabriel/Documents/rajssp.xlsx")

sp_rajs <- sp_rajs %>%
  mutate(code_muni = str_to_lower(code_muni)) %>%
  mutate(code_muni = stri_trans_general(code_muni, "latin-ascii")) %>%
  mutate(code_muni = str_replace_all(code_muni, "-", "")) %>%
  mutate(code_muni = str_replace_all(code_muni, "'", " ")) %>%
  mutate(code_muni = str_replace_all(code_muni, "mogi guacu", "mogiguacu")) %>%
  mutate(code_muni = str_replace_all(code_muni, "santa barbara d oeste", "santa barbara d'oeste")) %>%
  mutate(code_muni = str_replace_all(code_muni, "palmeira d oeste", "palmeira d'oeste")) %>%
  mutate(code_muni = str_trim(code_muni))

comarcas <- comarcas %>%
  left_join(sp_rajs, by = c("name_muni" = "code_muni"))

comarcas <- comarcas %>%
  group_by(Região) %>%
  mutate(total_2 = sum(total))

tema_mapa <-
  theme_bw() + # Escolhe o tema. Eu gosto do theme_bw() por ser bem simples/limpo

  # Os códigos abaixo são referentes à estética do tema,
  # como o tamanho da fonte, direção do texto,
  # linhas ao fundo, etc.

  theme(
    axis.text.y = element_text(
      angle = 90,
      hjust = 0.5,
      size = 8
    ),
    axis.text.x = element_text(size = 8),
    axis.title.y = element_text(size = rel(0.8)),
    axis.title.x = element_text(size = rel(0.8)),
    panel.grid.major = element_line(
      color = gray(0.9),
      linetype = "dashed",
      size = 0.1
    ),
    panel.background = element_rect(fill = "white") +
      annotation_scale(location = "br", width_hint = 0.30)
  )

comarcas2 <- comarcas %>%
  distinct(Região, .keep_all = T)

cores <- c("red3", "darkslateblue", "firebrick1", "forestgreen", "orange2", "slategray", "yellow1", "turquoise4", "maroon", "darkseagreen4")

comarcas$Região <- factor(comarcas$Região,ordered = T ,levels = c(
  "1ª RAJ - Grande São Paulo",
  "2ª RAJ – Araçatuba",
  "3ª RAJ - Bauru",
  "4ª RAJ – Campinas",
  "5ª RAJ - Presidente Prudente",
  "6ª RAJ - Ribeirão Preto",
  "7ª RAJ – Santos",
  "8ª RAJ - SJ Rio Preto",
  "9ª RAJ - SJ dos Campos",
  "10ª RAJ – Sorocaba"
))

comarcas2 %>%
  ggplot() +
  geom_sf(data = comarcas, alpha = 0.8,size = 4, aes(fill = Região))+
  geom_sf_label(aes(label = total_2), label.size = 0.25, size = 6)+
  scale_fill_manual(values = cores) +
  labs(fill = "Regiões Administrativas Judiciárias/TJSP") +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  ggspatial::annotation_scale() +
  tema_mapa


# Gráfico em que vemos a distribuição das deciões em primeira instância
df %>%
  dplyr::count(sentença) %>%
  filter(!is.na(sentença)) %>%
  ggplot(aes(x = forcats::fct_reorder(sentença, n, .desc = T),
             y = n,
             fill = sentença)) +
  geom_chicklet() +
  theme_classic() +
  labs(x = "Decisões em 1º instância", y = "Quant. decisões",
       title = "",
       subtitle = "") +
  theme(axis.text.x = element_text(angle = 90))

# Gráfico em que vemos a distribuição das deciões em segunda instância
windowsFonts(`Fira Sans` = windowsFont('Fira Sans'))

df %>%
  dplyr::count(Recurso) %>%
  filter(!is.na(Recurso)) %>%
  mutate(perc = (n/sum(n))) %>%
  mutate(perc = paste0(sprintf("%4.1f", (perc)*100), "%" )) %>%
  ggplot(aes(x = forcats::fct_reorder(Recurso, n, .desc = T),
             y = perc,
             fill = Recurso)) +
  geom_chicklet() +
  geom_label(aes(label = perc), hjust = .7, nudge_x = .05,
             size = 4, fill = "white", label.size = 0.5, fontface = "bold", family = "Fira Sans")+
  theme_classic() +
  labs(x = "Recursos em 2º instância", y = "Quant. decisões",
       title = "",
       subtitle = "") +
  theme(axis.text.x = element_text(angle = 90), axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y=element_blank())

# Vamos verificar qual o resultado dos recursos versus o resultado em primeira instância

df %>%
  filter(!is.na(Recurso)) %>%
  filter(!is.na(sentença)) %>%
  dplyr::count(sentença, Recurso, sort = TRUE) %>%
  mutate(total_recurso = sum(n)) %>%
  mutate(porcentagem = round(n/total_recurso*100, 2)) %>% # contar ocorrências
  select(-total_recurso) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  column_spec(1, bold = TRUE)

