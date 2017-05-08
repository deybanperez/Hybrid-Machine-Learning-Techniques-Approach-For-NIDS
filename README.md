# Análisis, Diseño e Implementación de Técnicas Híbridas de Aprendizaje Automático en la Detección de Intrusos en Redes de Computadoras
Este repositorio corresponde al Trabajo Especial de Grado de mi persona, Deyban Andrés Pérez Abreu. El mismo tiene el siguiente título *Análisis, Diseño e Implementación de Técnicas Híbridas de Aprendizaje Automático en la Detección de Intrusos en Redes de Computadoras*.

Este trabajo busca responder la siguiente pregunta: ¿Es posible obtener un modelo eficaz al aplicar técnicas de aprendizaje automático en el aŕea de detección de intrusos en redes de computadoras? Para ello se plantearon los siguientes objetivos:

## Objetivo general
Analizar, diseñar e implementar modelos híbridos basados en técnicas de aprendizaje automático mediante la evaluación de su desempeño en la tarea de detección de intrusos en redes de computadoras.

## Objetivos específicos
+ Indagar en el estado actual de la investigación del uso de técnicas de aprendizaje automático en la detección de intrusos en redes de computadoras.

+ Hacer uso de conjuntos de datos puúblicos bien conocidos y confiables avalados por la comunidad científica para su utilización en el área de seguridad en redes de computadoras.

+ Diseñar e implementar modelos híbridos de detección de intrusos en redes de computadoras basados en técnicas de aprendizaje automático.

+ Evaluar el desempeño de los modelos frente a los ataques DOS, Probing, R2L y U2R.

+ Evaluar la poibilidad de crear un modelo conjunto que combine los modelos creados e incremente el desempeño contra determinados grupos de ataques.

+ Registrar los resultados y consideraciones encontradas durante la implementación de la solución.

## Organización del repositorio

`README.md`: documento explicativo del Trabajo Especial de Grado y de la organización del repositorio.

+ `dataset`: directorio que contiene el conjunto de datos NSL-KDD. El mismo fue solicitado mediante la información obtenida en el sitio web de la [Universidad de Nueva Brunswick](http://www.unb.ca/research/iscx/dataset/iscx-NSL-KDD-dataset.html). Los archivos *NSLKDD\_Testing\_New.csv* y *NSLKDD\_Training\_New.csv* corresponden a vistas minables realizadas durante el proceso de elaboración del trabajo de grado.

+ `PDF`: en este directorio se encuentra *TEG.pdf* y *TEG\_paper\_CLEI\_2017\_English.Rmd* que corresponden a la documentación de las actividades técnicas realizadas durante la implementación del Trabajo Especial de Grado y a un artículo escrito de la investigación para ser presentado en la sección de inteligencia artificial del CLEI respectivamente.

+ `source`: contiene los directorios de los scripts crudos implementados para cada una de las fases del Trabajo Especial de Grado:

  + `default_parameters`: modelos entrenados con los parametros por defecto.

  + `feature_selection`: scripts para la selección de parámetros.

  + `final_model`: modelo que mejor se ajusta a a la tarea de detección de intrusos en redes de computadoras.

  + `functions`: funciones utilizadas durante la implementación.

  + `parameter_selection`: scripts para la selección de parámetros.

  + `pre_processing`: scripts para el pre-procesamiento del conjunto de datos.

  + `tuned_models`: scripts de los modelos que que tuvieron parámetros seleccionados.

## Consideraciones de implementación

+ `Lenguaje`: R versión 3.3.1.

+ `Paquetes`: nnet, e1071 y factoextra.

+ `Ambiente de desarrollo`: Ubuntu Server 16.04 de 64 bits.

+ `Hardware`: Intel core 2 duo, 2 GB RAM.