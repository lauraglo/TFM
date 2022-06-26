Este repositorio contiene todo el código utilizado para desarrollar el Trabajo de Fin de Máster "Un sistema para anotar términos en corpus multilingües", además de los datasets empleados para los experimentos y la herramienta interactiva desarrollada para la edición de corpus orientados a la extracción de terminologías.
A continuación se detallará como ejecutar la aplicación.

Se deben disponer de dos archivos principales:
- Fichero de entrenamiento: se debe disponer de un dataset que contenga corpus de abstracts con los tokens etiquetados en formato BIO o IOB.
- Código fuente (solamente necesario al ejecutarse en local): El archivo app.R que contiene el código fuente de la app

Una vez dispongamos de estos ficheros, podemos ejecutar la aplicación localmente o en el servidor:
1. Ejecución local: Para ejecutar la aplicación existen dos formas:
  - Usando RStudio: Se importa el archivo en RStudio, y simplemente se pulsa el botón RunApp para continuar. Es recomendable ejecutar
  la aplicación en un navegador para conservar todas las funciones disponibles.
  - Línea de comandos: Ejecutando el siguiente comando en la terminal se lanza la aplicación:
  RScript app.R
  
2. Ejecución en el servidor: Accediendo a la siguiente dirección web se puede lanzar la app https://terminologyeditor.shinyapps.io/TerminologyEditor/ (temporal)

El manuscrito del proyecto, que contiene todos los detalles de la herramienta, estará disponible en la siguiente dirección:
https://oa.upm.es/view/masters/Ciencia_de_Datos.html


