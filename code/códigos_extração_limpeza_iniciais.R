
#Leitur dos pacotes
# Abrimos os pcts e instalamos os que não estão na máquina
# Listamos os pacotes que precisamos
packages = c(
  "tjsp", #pacote que fará a extração dos dados da plataforma do tjsp
  "dplyr", #para facilitar o manejo dos dados
  "stringr", #manipulação de strings
  "jsonlite" #o formato json é o ideal para esse caso, tendo em vista o limite de caracteres de células do excel
)

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


# temos uma gama de palavras que podem ser utilizadas para encontrar processos que fazem referência ao que queremos, então listamos todas facilitar

palavras <- c(r"("transporte especial" E "Educação Especial")",
              r"("educação especial" NAO "magistério")",
              r"("tecnologia assistiva" E "Educação Especial")",
              r"("13.146" E "educação especial")",
              r"("9.394" E "especial")")

#Definimos um diretório onde será feito o download dos processos
diretorio <- "C:/Users/Gabriel/Documents/git-"

# Autenticamos, para isso é necessário que você crie uma conta no site do tjsp, ela será a ponte que nos possibilitará o download dos dados, caso
tjsp::autenticar(login = 'seucpf', password = 'suasenha') #caso tenha dado certo o console retornará com a mensagem "you're logged in [true]


# Extração das deciões em primeiro grau ----------------
# a função tjsp_baixar_cjpg permite é o espelho do site do tjsp onde podemos realizar buscas, no campo "Livre" colocamos as palavras a serem buscadas e no diretório o nosos diretório
#porém, como mais de uma palavra será utilizada colcomos a função dentro de um loop para que ele faça o download de todas palavras definidas

for (i in palavras){
  tjsp_baixar_cjpg(livre = i, diretorio = diretorio)
} # O resultado será html's com o processos baixados

# Agora abrimos os processos julgados em primeira instância, para isso apenas definimos o diretório onde baixamos os htmls
processosedespecial <- tjsp_ler_cjpg(diretorio = diretorio)

# Análise do dataset ---------------

#verificamos o número de linhas e colunas
dim(processosedespecial)

#checamos os nomes das colunas
names(processosedespecial)

# checamos quais são as variaveis e suas estruturas
str(processosedespecial)

summary(processosedespecial)


# Limpeza --------------
# Primeiro checamos se há  valores duplicados, usamos a função which que nos retorna o indice das linhas com os valores identicos
which(duplicated(processosedespecial))

#usamos o pacote dplyr para limpar essas duplicadas
processosedespecial <- processosedespecial %>%
  distinct(across(-pagina), .keep_all = T) #O uso da função across vai fazer com que ele verifique se o conteudo de todas as colunas são iguais, desconsiderando a coluna página

#Removemos valores faltantes caso haja algum
processosedespecial <- processosedespecial %>%
  filter(if_all(-hora_coleta, ~!is.na(.)))

# Realizando um primeiro check para vermos quais são os principais assuntos e se eles fazem parte do escopo

assuntos <- processosedespecial %>%
  distinct(assunto)

print(assuntos, n =324)

# Agora vamos filtrar apenas os assuntos que podem fazer parte do escopo da pesquisa

processosedespecial <- processosedespecial %>%
  filter(assunto %in% c("TRANSPORTE",
                         "AUSÊNCIA DE VAGA",
                         "Garantias Constitucionais",
                         "Pessoas com deficiência",
                         "PROFISSIONAIS DE APOIO",
                         "Auxílio-transporte",
                         "Transporte de Pessoas",
                         "Ensino Fundamental e Médio",
                         "Transporte Terrestre",
                         "Estabelecimentos de Ensino",
                         "Município",
                         "Assistência Pré-escolar",
                         "INSTITUCIONALIZAÇÃO PEDAGÓGICA DO ATENDIMENTO EDUCACIONAL ESPECIALIZADO",
                         "Matrícula - Ausência de Pré-Requisito",
                         "Reserva de Vagas para Pessoas com Deficiência",
                         "PRIORIDADE DE MATRÍCULA PARA ALUNOS COM DEFICIÊNCIA",
                         "PROGRAMAS DE ASSISTÊNCIA ESTUDANTIL - ALIMENTAÇÃO, MORADIA, CRECHE, TRANSPORTE",
                         "Pessoa com Deficiência",
                         "Crimes contra portadores de deficiência",
                         "Não Discriminação",
                         "Vaga em ensino pré-escolar",
                         "ACESSIBILIDADE FÍSICA",
                         "Vaga em creche",
                         "DIREITO DA CRIANÇA E DO ADOLESCENTE"))

# Agora, sabemos que por muitas as partes recorrem o que faz com que os processos 'subam' de instância, então vamos procurar esses processos em segunda instância

diretorio2 <- "...."
processo <- processosedespecial$processo

# Agora usamos a função tjsp_baixat_cjpg, aqui nós baixamos todos os detalhes, mas não a ementa
for(i in processo){
  tjsp::tjsp_baixar_cposg(processo = i, diretorio = diretorio2)
}


# Ler os processos de segunda instância
processossegedespecial <- tjsp_ler_dados_cposg(diretorio = diretorio2)

#baixar o texto das ementas
num_pro <- processossegedespecial$processo
diretorio3 <- "......"

for(i in num_pro){
  tjsp::tjsp_baixar_cjsg(livre = i, diretorio = diretorio3)
  Sys.sleep(5)
}

# Ler os processos de segunda instâncias baixados
processosedespecial2 <- ler_cjsg(diretorio = diretorio3)

# Primeiro checamos se há  valores duplicados, usamos a função which que nos retorna o indice das linhas com os valores identicos
which(duplicated(processosedespecial2))

#usamos o pacote dplyr para limpar essas duplicadas
processosedespecial2 <- processosedespecial2 %>%
  distinct(., .keep_all = T)

# Juntamos as bases de primeira e segunda instância
base_final <- processosedespecial %>%
  full_join(processosedespecial2, by = "processo")

# Verificamos a existência de duplicados
which(duplicated(base_final))

# salvamos o arquivo em json

write_json("sei camminho/dados.json",base_final)
