
# Proyecto TRIP-COVID

Este repositorio contiene los archivos utilizados en la Aplicación Shiny
del Proyecto *La implementación de políticas públicas para dar respuesta
a la crisis desatada por la pandemia COVID-19: Una mirada desde las
relaciones intergubernamentales y las redes de políticas* (Res 119-2020
PISAC-COVID-19-00021).

# Cómo citar

Para citar esta aplicación utilice:

> Serrati, P. (2021). Aplicación TRIP-COVID (1.0) \[Aplicación Shiny\].
> Proyecto PISAC TRIP-COVID. <https://doi.org/10.5281/zenodo.6496271>.
> Acceso 26 abr. 2022.

# Versión online

Una versión de la aplicación puede consultarse en:
<https://trip-pisac.shinyapps.io/COVID/>

# Correr la aplicación localmente

Para correr la aplicación localmente en su computadora utilice el
siguiente código:

``` r

packages <- c("devtools",  "shiny","shinyWidgets", "shinyFeedback", "waiter", 
              "tidyverse", "lubridate", "plotly", 
              "viridis", "ggsci",
              "sf", "leaflet", "leaflet.extras")
    
to_install <- packages[!packages %in% installed.packages()[, "Package"]]

install.packages(to_install)
```

Alternativamente puede optar por ejecutar la aplicación directamente
desde Github:

``` r

library(shiny)
runGitHub("TRIP-COVID", "estedeahora", ref = "main")
```

O bien descargue el repositorio en su computadora y luego de abir el
proyecto mediante el archivo “TRIP-COVID.Rproj” ejecute el siguiente
código:

``` r

library(shiny)
runApp()
```

### Preguntas / Porblemas / Contacto

<pabloserrati@gmail.com>, o abra un hilo en Github
