##################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
##################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools","ggstance",
             "magick","cowplot","beepr","Rcpp", "correlation", "readxl", "dplyr" )

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

install.packages("correlation")
install.packages("jtools")
install.packages("readxl")
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(correlation)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr) 
library(readxl)
library(jtools)

##################################################################################
#                               REGRESSÃO LINEAR SIMPLES                         #
#                       EXEMPLO 01 - CARREGAMENTO DA BASE DE DADOS               #
##################################################################################

#Listar os arquivos do nosso project
list.files()

#Carregando a base de dados
Base_Custos <- read_excel("Base_Custos.xlsx")


#Carregando a base de dados
Base_Custos <- read_excel("Base_Custos_v3.xlsx")


View ( Base_Custos )

#################################################################################
#                 OBSERVANDO OS DADOS CARREGADOS DO DATASET tempodist           #
#################################################################################
Base_Custos %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 22)

#Visualizando as observações e as especificações referentes às variáveis do dataset
glimpse(Base_Custos) 

#Estatísticas univariadas
summary(Base_Custos)


#################################################################################
#                             GRÁFICO DE DISPERSÃO                              #
#################################################################################
ggplotly(
  ggplot(Base_Custos, aes(x = Combustivel, y = ValorY)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", se = F, size = 2) +
    xlab("ValorX") +
    ylab("ValorY") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_classic()
)



#################################################################################
#            MODELAGEM DE UMA REGRESSÃO LINEAR SIMPLES PARA O EXEMPLO 01        #
#################################################################################
#Estimando o modelo valor
Base_Custos_Pred <- lm(formula = ValorY ~ Combustivel,
                       data = Base_Custos)

#Estimando o modelo prod
Base_Custos_Prod <- lm(formula = ProdY ~ Combustivel,
                       data = Base_Custos)


#Observando os parâmetros do modelo_tempodist
summary(Base_Custos_Pred)
summary(Base_Custos_Prod)



#Fazendo predições em modelos OLS - e.g.: qual seria o tempo gasto, em média, para
#gastando o valor de combustivel 100k, quanto gastaria de Preparo?
predict(object = Base_Custos_Pred,
        data.frame(Combustivel = 100000
                   ))


#gastando o valor de combustivel 100k, quanto daria para fazer preparo?
predict(object = Base_Custos_Prod,
        data.frame(Combustivel = 100000
        ))


#Caso se queira obter as predições com os IC
predict(object = Base_Custos_Pred,
        data.frame(Combustivel = 250000),
        interval = "confidence", level = 0.95)


#Outras maneiras de apresentar os outputs do modelo
#função summ do pacote jtools
summ(Base_Custos_Pred, confint = T, digits = 4, ci.width = .95)
export_summs(Base_Custos_Pred, scale = F, digits = 4)


#Salvando fitted values (variável yhat) e residuals (variável erro) no dataset
Base_Custos$yhat <- Base_Custos_Pred$fitted.values
Base_Custos$erro <- Base_Custos_Pred$residuals

#Visualizando a base de dados com as variáveis yhat e erro
Base_Custos %>%
  select(ValorY, Combustivel, yhat, erro) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)


#Cálculo manual do R²
R2 <- (sum((Base_Custos$yhat - mean(Base_Custos$ValorY))^2))/
  ((sum((Base_Custos$yhat - mean(Base_Custos$ValorY))^2)) + (sum((Base_Custos$erro)^2)))

round(R2, digits = 4)

#coeficiente de ajuste (R²) é a correlação ao quadrado
cor(Base_Custos[3:14])



##################################################################################
#                       ESTIMAÇÃO DA REGRESSÃO LINEAR MÚLTIPLA                   #
##################################################################################

#Carregando a base de dados
Base_Custos <- read_excel("Base_Custos_v3.xlsx")

View(Base_Custos)

#Visualizando a base de dados
Base_Custos %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 20)

summary(Base_Custos)



##################################################################################
#                               ESTUDO DAS CORRELAÇÕES                           #
##################################################################################


#A função chart.Correlation do pacote PerformanceAnalytics apresenta as
#distribuições das variáveis, scatters, valores das correlações e suas
#respectivas significâncias
chart.Correlation((Base_Custos[3:14]), histogram = TRUE)


#A função correlation do pacote correlation faz com que seja estruturado um
#diagrama interessante que mostra a inter-relação entre as variáveis e a
#magnitude das correlações entre elas
#Requer instalação e carregamento dos pacotes see e ggraph para a plotagem

Base_Custos %>% select(3:10) %>% 
  correlation(method = "pearson") %>%
  plot()


Base_Custos %>%
  na.omit() %>%
  correlation(method = "pearson") %>%
  .$correlations %>%
  heatmap(symm = TRUE, margins = c(4, 6))


#A função corr_plot do pacote metan também apresenta as distribuições
#das variáveis, scatters, valores das correlações e suas respectivas
#significâncias
Base_Custos %>%
  corr_plot(ValorY, Combustivel, Manutencao, Operador,
            shape.point = 21,
            col.point = "black",
            fill.point = "#FDE725FF",
            size.point = 2,
            alpha.point = 0.6,
            maxsize = 4,
            minsize = 2,
            smooth = TRUE,
            col.smooth = "black",
            col.sign = "#440154FF",
            upper = "corr",
            lower = "scatter",
            diag.type = "density",
            col.diag = "#440154FF",
            pan.spacing = 0,
            lab.position = "bl")


##################################################################################
#                      PROCEDIMENTO STEPWISE                          #
##################################################################################
# Ajustando o modelo Step Wize manual Estimando o modelo
Base_Custos_Pred <- lm(formula = ValorY ~ . - Cenario - Simulacoes ,
                       data = Base_Custos)

#Observando os parâmetros do modelo_tempodist
summary(Base_Custos_Pred)


#Aplicando o procedimento Stepwise, temos o seguinte código:
Base_Custos_Step <- step(Base_Custos_Pred, k = 3.841459)

summary(Base_Custos_Step)

#Este procedimento no R removeu a variável 'endividamento'. Note que a variável
#'disclosure' também acabou sendo excluída após o procedimento Stepwise, nesta
#forma funcional linear!

export_summs(Base_Custos_Step, scale = F, digits = 5)


#### Parou aqui
#Parâmetros padronizados

plot_summs(Base_Custos_Step, scale = TRUE, colors = "#440154FF")

#Adicionando a caracterização da distribição normal no IC de cada parâmetro beta
plot_summs(Base_Custos_Step, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = "#440154FF")

#Comparando os ICs dos betas dos modelos sem e com procedimento Stepwise
plot_summs(Base_Custos_Pred, Base_Custos_Step, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = c("#FDE725FF", "#440154FF"))



##################################################################################
#                      ESTIMANDO O MODELO   Normal                  #
##################################################################################


#Estimando o modelo
Base_Custos_Pred <- lm(formula = ValorY ~ MOUrbano + Ecargos + Manutencao + Combustivel + 
                         Operador, data = Base_Custos)


#Estimando o modelo 2
Base_Custos_Pred <- lm(formula = ValorY ~ Manutencao + Combustivel + 
                         Operador, data = Base_Custos)

#Estimando o modelo prod
Base_Custos_Prod <- lm(formula = ProdY ~  Manutencao + Combustivel,
                       data = Base_Custos)


#Observando os parâmetros do modelo_tempodist
summary(Base_Custos_Pred)

summary(Base_Custos_Prod)

##################################################################################
#                      CRIANDO O MODELO PREDICAO                          #
##################################################################################

#Fazendo predições em modelos OLS - e.g.: qual seria o tempo gasto, em média, para
#precicão valores com base me duas colunas?
predict(object = Base_Custos_Pred,
        data.frame(Combustivel = 100000,
                   Manutencao = 40000,
                   Operador = 5000
        ))


#precicão producao com base me duas colunas?
predict(object = Base_Custos_Prod,
        data.frame(Combustivel = 100000,
                   Manutencao = 40000
        ))


#Caso se queira obter as predições com os IC
predict(object = Base_Custos_Pred,
        data.frame(Combustivel = 100000,
                   Manutencao = 4000),
        interval = "confidence", level = 0.95)



#precicão valores modelo step?
predict(object = Base_Custos_Step,
        data.frame(Combustivel = 100000,
                   Manutencao = 20000,
                   MOUrbano = 20000,
                   Ecargos = 15000,
                   Operador = 40000
        ))


#Outras maneiras de apresentar os outputs do modelo
#função summ do pacote jtools
summ(Base_Custos_Pred, confint = T, digits = 4, ci.width = .95)
export_summs(Base_Custos_Pred, scale = F, digits = 4)
summary(Base_Custos_Pred)


#Salvando fitted values (variável yhat) e residuals (variável erro) no dataset
Base_Custos$yhat <- Base_Custos_Pred$fitted.values
Base_Custos$erro <- Base_Custos_Pred$residuals

#Visualizando a base de dados com as variáveis yhat e erro
Base_Custos %>%
  select(ValorY, Manutencao, yhat, erro) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)


#Cálculo manual do R²
R2 <- (sum((Base_Custos$yhat - mean(Base_Custos$ValorY))^2))/
  ((sum((Base_Custos$yhat - mean(Base_Custos$ValorY))^2)) + (sum((Base_Custos$erro)^2)))

round(R2, digits = 4)

#coeficiente de ajuste (R²) é a correlação ao quadrado
cor(Base_Custos[3:14])


##################################################################################
#            DIAGNÓSTICO DE MULTICOLINEARIDADE EM MODELOS DE REGRESSÃO           #
#                   CARREGAMENTO DA BASE DE DADOS                   #
##################################################################################

#Listar os arquivos do nosso project
list.files()

#Carregando a base de dados
Base_Custos <- read_excel("Base_Custos_v3.xlsx")

#Estatísticas univariadas
summary(Base_Custos)

##CORRELAÇÃO PERFEITA:
cor(Base_Custos$ValorY, Base_Custos$Combustivel)

Base_Custos %>% select(10:14) %>% 
  correlation(method = "pearson") %>%
  plot()

modelo1 <- lm(formula = ValorY ~ Combustivel + Manutencao + Operador ,
              data = Base_Custos)

summary(modelo1)

install.packages("olsrr") 

library(olsrr) 


#Diagnóstico de multicolinearidade (Variance Inflation Factor e Tolerance)
ols_vif_tol(modelo1)
#função ols_vif_tol do pacote olsrr



##################################################################################
#                       ESTIMAÇÃO DO MODELO DE REGRESSÃO E                       #
#                       DIAGNÓSTICO DE HETEROCEDASTICIDADE                       #                                                            
##################################################################################
#Estimação do modelo
Base_Custos_Teste_Heter <- lm(formula = ValorY ~ Combustivel + Manutencao + Operador ,
                              data = Base_Custos)

summary(Base_Custos_Teste_Heter)

#Teste de Breusch-Pagan para diagnóstico de heterocedasticidade
ols_test_breusch_pagan(Base_Custos_Teste_Heter)

#função ols_test_breusch_pagan do pacote olsrr
#Presença de heterocedasticidade -> omissão de variável(is) explicativa(s) relevante(s)

#H0 do teste: ausência de heterocedasticidade.
#H1 do teste: heterocedasticidade, ou seja, correlação entre resíduos e uma ou mais
#variáveis explicativas, o que indica omissão de variável relevante!






##################################################################################
#          TESTE DE VERIFICAÇÃO DA ADERÊNCIA DOS RESÍDUOS À NORMALIDADE          #
#                               SHAPIRO-FRANCIA                                  #
##################################################################################
#Shapiro-Francia: n > 30

install.packages("nortest")
library(nortest)

sf.test(Base_Custos$residuals) #função sf.test do pacote nortest

#Plotando os resíduos do modelo step_empresas
empresas %>%
  mutate(residuos = Base_Custos$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(color = "white", 
                 fill = "#440154FF", 
                 bins = 30,
                 alpha = 0.6) +
  labs(x = "Resíduos",
       y = "Frequência") + 
  theme_bw()
