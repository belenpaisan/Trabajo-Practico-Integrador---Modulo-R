# Trabajo Práctico Integrador - Módulo R

Este trabajo fue realizado como cierre de la segunda parte del curso de "Programación Estadística Avanzada: Python y R" de la Maestría de Econometría de la Universidad Torcuato Di Tella en el segundo trimestre de 2026.

## Contenido del repositorio
- Código de R
- Informe en PDF que contiene la explicación detallada de las partes del trabajo

## Contenido del trabajo

Parte 1: Elección del dataset y la técnica. 

Se eligió como dataset la Encuesta Permanente de Hogares publicada por el INDEC, específicamente el último relevamiento disponible (1T 2026). El objetivo es un análisis acerca de la capacidad predictiva de variables del individuo y de la vivienda para el estado de ocupación de las personas sin eduación universitaria. Las técnicas vistas en el curso utilizadas fueron la regresión logística, la utilización de submuestras de test y train, matrices de confusión, curvas ROC y el algoritmo de selección automática de "stepwise".

Parte 2: Análisis exploratorio de datos (EDA). 

Se analizó el dataset en general y las variables a utilizar. Se realizaron distintas acciones para dejar listos los datos (renombrar, recodificar, agrupar, etc.) y se analizaron casos posibles de no respuesta o NA. 

Parte 3: Aplicación de la técnica e interpretación. 

Se aplicaron las técnicas definidas en la Parte 1. Primero se realizó una estimación solamente con las variables referidas a los individuos, una segunda estimación agregando las variables referidas a las viviendas y por último una estimación con variables seleccionadas por el algoritmo "stepwise". Para cada una se reportan los coeficientes, errores estándares, p-values, matriz de confusión y curva ROC.

## Realización

Todo fue realizado en R. Las librerías utilizadas fueron tidyverse (para el manejo de datos), eph (para la carga de la base), pROC (para la curva ROC) y openxlsx (para el guardado de tablas en excel). Las fuentes utilizadas para hacerlo fueron los scripts utilizados en las clases del curso, apuntes de clase e información del INDEC acerca de la EPH (https://www.indec.gob.ar/indec/web/Institucional-Indec-OperacionesEstadisticas y https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_registro_1T2026.pdf)

## Cómo ejecutar el código

El archivo .R en el repositorio puede ser descargado y ejecutado en R. Es necesario tener instaladas las librerías anteriormente mencionadas. Al principio del código también hay un espacio para completar con un working directory local. Los comandos para guardar los gráficos están como comentario (con un #) pero puede removerse el # y ejecutarse.
La semilla generada para replicabilidad de la división entre grupos de entrenamiento y testeo se encuentra detallada en la parte pertinente.
