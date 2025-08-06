#table1 -----
# Criando a tabela como dataframe (table1 - resultado m1)
table1 <- data.frame(
  treatment = c("forest", "restoration","agroforestry"),
  Estimate = c("0.69", "-0.04","-1.29"),
  SE = c("0.32", "0.44","0.25"),
  z.value = c("2.11", "-0.11","-5.05"),
  p.value = c("0.03", "0.91","<0.001"),
  stringsAsFactors = FALSE)

# Ver a tabela
print(table1)

library(gridExtra)
library(grid)

rownames(table1) <- NULL

# Criar a tabela como um objeto gráfico
table1_obj <- tableGrob(
  table1,
  rows = NULL,  # remove a numeração das linhas
  theme = ttheme_default(base_size = 22)  # aumenta a fonte
)

# Salvar como PNG com tamanho ajustado
png("table1.png", width = 800, height = 250)
grid.newpage()
grid.draw(table1_obj)
dev.off()

#################################
#table2----
# Criando a tabela como dataframe (table2 - resultado m2)
table2 <- data.frame(
  nome da primeira coluna = c("valores"),
  Estimate = c("x", "x", "-x"),
  SE = c("x", "x", "x"),
  z.value = c("-x", "x", "-x"),
  p.value = c("<x", "x", "x"),
  stringsAsFactors = FALSE
)

# Ver a tabela
print(table2)

rownames(table2) <- NULL

# Criar a tabela como um objeto gráfico
table2_obj <- tableGrob(
  table2,
  rows = NULL,  # remove a numeração das linhas
  theme = ttheme_default(base_size = 22)  # aumenta a fonte
)

# Salvar como PNG com tamanho ajustado
png("table2.png", width = 800, height = 250)
grid.newpage()
grid.draw(table2_obj)
dev.off()
