\name{comparar_generadores}
\alias{comparar_generadores}
\title{Comparación de Generadores Aleatorios}
\description{
Esta función permite comparar diferentes generadores de números aleatorios en R,
evaluando su desempeño en términos de tiempo de ejecución y visualizando los datos generados.
}
\usage{
comparar_generadores(n, generadores = c("Base", "PCG", "MT19937"))
}
\arguments{
  \item{n}{Número de datos a generar para cada generador.}
  \item{generadores}{Vector con los nombres de los generadores que se desean comparar.}
}
\details{
La función genera datos simulados (pacientes, edades y frecuencia cardiaca) usando distintos generadores
de números aleatorios. Luego mide el tiempo de ejecución y permite visualizar la distribución de edades
en gráficas comparativas.
}
\value{
Un \code{list} que contiene:
\itemize{
  \item Tiempos de ejecución por generador.
  \item Resultados de la prueba chi-cuadrado.
}
}
\examples{
# Comparar generadores con 1000 datos
comparar_generadores(1000)
}
