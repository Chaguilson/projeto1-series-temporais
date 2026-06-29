library(rmet)
library(tidyverse)
library(forecast)
library(tseries)

dados <- rmet::inmet_read(years = 2000:2026, stations = "A001")

dados_mes <- dados %>%
  mutate(datetime = parse_date_time(datetime, orders = c("ymd_HMS", "ymd_HM", "dmy_HMS", "dmy_HM", "ymd", "dmy"))) %>%
  mutate(mes_ano = floor_date(datetime, "month")) %>%
  group_by(mes_ano) %>%
  summarise(temp = mean(temp_dry_c, na.rm = TRUE))
 

start = c(year(min(dados_mes$mes_ano)),
          month(min(dados_mes$mes_ano)))

#SERIE
temp_ts <- ts(dados_mes$temp, start = start, frequency = 12)

#GRAFICO DA SERIE
plot(temp_ts,
     main = "Temperatura Média Mensal - Brasília (A001)",
     ylab = "Temperatura (°C)",
     xlab = "Ano")

#COMPORTAMENTO SAZONAL
ggseasonplot(temp_ts, year.labels=FALSE, continuous=FALSE) +
  ggtitle("Gráfico Sazonal: Temperaturas em Brasília") +
  scale_color_viridis_d(option = "turbo") +
  labs(colour = "Ano") 

###PARA MELHOR VISUALIZACAO
meses <- factor(cycle(temp_ts), levels = 1:12, labels = month.abb)
valores <- as.numeric(temp_ts)
dados_boxplot <- data.frame(Mês = meses, Temperatura = valores)

ggplot(dados_boxplot, aes(x = Mês, y = Temperatura, fill = Mês)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  scale_fill_viridis_d(option = "turbo") +
  labs(
    title = "Distribuição da Temperatura por Mês - Brasília (2000-2026)",
    x = "Mês",
    y = "Temperatura (°C)"
  ) +
  theme_minimal()
###

#CALCULO DA MEDIA HISTORICA PRA CADA MES
medias_mensais <- tapply(as.numeric(temp_ts), cycle(temp_ts), mean)
medias_mensais

#DECOMPOSICAO STL
decomp <- stl(temp_ts, s.window = "periodic")
plot(decomp, main = "Decomposição STL da Temperatura")

#DIVISAO DE TREINO E TESTE (ULTIMOS 12 MESES)
temp_treino <- subset(temp_ts, end = length(temp_ts) - 12)
temp_teste <- subset(temp_ts, start = length(temp_ts) - 11)

#DIFERENCAS SUGERIDAS
nsdiffs(temp_treino)#SAZONAIS
ndiffs(temp_treino)#REGULARES

#ACF/PACF DA SERIE ORIGINAL
ggtsdisplay(temp_treino,
            main = "Série Original: ACF e PACF")
#APOS DIFERENCA SAZONAL
ggtsdisplay(diff(temp_treino, 12),
            main = "Série com Diferença Sazonal (lag=12)")

adf <- adf.test(temp_treino)#H0: TEM RAIZ UNITARIA (NAO ESTACIONARIA)
kpss <- kpss.test(temp_treino)#H0: SERIE ESTACIONARIA

adf$p.value
kpss$p.value

#MODELO SARIMA
fit_sarima <- auto.arima(
  temp_treino,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE
)

fit_sarima2 <- Arima(temp_treino, order=c(1,0,0), seasonal=c(0,1,2), include.drift=TRUE)

fit_sarima3 <- Arima(temp_treino, order=c(0,0,1), seasonal=c(0,1,1), include.drift=TRUE)

summary(fit_sarima)
summary(fit_sarima2)
summary(fit_sarima3)

#DIAGNOSTICO
checkresiduals(fit_sarima)
ggtsdisplay(residuals(fit_sarima))

#VALIDACAO E PREVISAO
prev_teste <- forecast(fit_sarima, h = 12)
autoplot(prev_teste) + autolayer(temp_teste)

#ACURACIA FORA DA AMOSTRA
accuracy(prev_teste, temp_teste)

#MODELO FINAL COM SERIE
fit_final <- Arima(temp_ts, model = fit_sarima)

#ATE DEZEMBRO
h_2026 <- 7 
prev_2026 <- forecast(fit_final, h = h_2026)

autoplot(prev_2026) +
  autolayer(temp_teste) +
  ggtitle("Previsão de Temperatura Média até Dezembro de 2026") +
  ylab("Temperatura (°C)") + xlab("Tempo") +
  theme_minimal()

autoplot(prev_2026, include = 60) +
  ggtitle("Previsão de Temperatura Média até Dez/2026 (Visão Recente)") +
  ylab("Temperatura (°C)") + xlab("Ano") +
  theme_minimal()

