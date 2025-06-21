library(plumber)

#* @apiTitle API Previsão de Defeitos

#* Prever NUMDEFECTS
#* @param coupling valor de COUPLING_BETWEEN_OBJECTS
#* @get /prever
function(coupling) {
  coupling <- as.numeric(coupling)
  pred <- predict(modelo, newdata = data.frame(COUPLING_BETWEEN_OBJECTS = coupling))
  list(previsao_NUMDEFECTS = pred)
}
