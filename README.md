ANOVAlab
================
E. Vázquez
2025-10-27

# ANOVAlab

**ANOVAlab** es una aplicación didáctica interactiva desarrollada en **R
/ Shiny**, diseñada para reforzar la comprensión conceptual del
**Análisis de la Varianza (ANOVA)**. Permite visualizar la
descomposición de la variabilidad total en sus componentes (suma de
cuadrados explicada y residual) y comprender cómo se interpreta la
influencia de uno o varios factores mediante diferentes representaciones
gráficas.

## 🎯 Objetivos didácticos

- Comprender la **descomposición de la suma de cuadrados (SC)** en
  ANOVA.
- Visualizar la relación entre la **variabilidad explicada** por el
  factor y la **variabilidad residual**.
- Explorar la **interacción entre factores** en el ANOVA de dos vías.
- Promover el razonamiento estadístico y la toma de decisiones basada en
  datos.

## 📁 Estructura del proyecto

    ANOVAlab/
    ├─ app/
    │  ├─ app.R
    │  ├─ modules/
    │  │  ├─ mod_edit_table.R
    │  │  ├─ mod_one_factor.R
    │  │  └─ mod_two_factors.R
    │  ├─ R/
    │  │  ├─ gen_data.R
    │  │  ├─ anova_oneway.R
    │  │  ├─ anova_twoway.R
    │  │  └─ effects_twoway.R
    │  ├─ www/
    │  └─ help.md
    ├─ renv.lock
    ├─ LICENSE.txt
    ├─ README.Rmd
    └─ ANOVAlab.Rproj

## 👩‍🏫 Uso para **colaboradores docentes**

Si vas a colaborar en el desarrollo o mejora de la aplicación:

1.  **Clona el repositorio** desde GitHub  
    En RStudio:

    - Menú: *File → New Project → Version Control → Git*  

    - URL del repositorio:

          https://github.com/evb-education/ANOVAlab.git

2.  **Restaura el entorno de trabajo**  
    En la consola de R:

    ``` r
    install.packages("renv")
    renv::restore()
    ```

3.  **Abre y ejecuta la app**  
    Abre el proyecto `ANOVAlab.Rproj` y ejecuta:

    ``` r
    shiny::runApp("app")
    ```

4.  **Trabaja en una rama nueva**

    - Desde el menú *Git* de RStudio: botón *New Branch* (o *Branch →
      New Branch*).  

    - O desde la Terminal:

      ``` bash
      git checkout -b dev-nombre
      ```

5.  **Guarda y sube tus cambios**

    - En la pestaña *Git* de RStudio:

      - Marca los archivos modificados  
      - Pulsa *Commit* → escribe un mensaje descriptivo  
      - Luego pulsa *Push* para subir los cambios  

    - O desde la Terminal:

      ``` bash
      git add .
      git commit -m "feat: descripción de los cambios realizados"
      git push origin dev-nombre
      ```

6.  **Fusiona los cambios en GitHub**  
    Abre un *Pull Request* en GitHub para fusionar tu rama con `main`.

## 🎓 Uso para **estudiantes / usuarios**

Puedes ejecutar ANOVAlab en tu ordenador sin modificar el código.

> ⚠️ **No necesitas una cuenta de GitHub.** Solo necesitas tener **R** y
> **RStudio** instalados.

### Requisitos previos

- R (versión ≥ 4.0): <https://cran.r-project.org/>
- RStudio Desktop: <https://posit.co/download/rstudio/>

### Opción 1: Descarga directa (recomendada)

1.  Ve a la página principal del proyecto:  
    <https://github.com/evb-education/ANOVAlab>

2.  Haz clic en el botón verde **Code → Download ZIP**.  

3.  Descomprime el archivo ZIP en tu ordenador.  

4.  Abre el archivo `ANOVAlab.Rproj` en RStudio.  

5.  En la consola, ejecuta:

    ``` r
    install.packages("renv")
    renv::restore()
    shiny::runApp("app")
    ```

### Opción 2: Clonar el repositorio (usuarios avanzados)

1.  En RStudio: *File → New Project → Version Control → Git*  

2.  Pega la URL del repositorio:

        https://github.com/evb-education/ANOVAlab.git

3.  Ejecuta en la consola:

    ``` r
    install.packages("renv")
    renv::restore()
    shiny::runApp("app")
    ```

> Este repositorio es **de solo lectura** para usuarios no
> colaboradores. Podrás ejecutar la aplicación, pero no modificar ni
> subir cambios a GitHub.

## 📜 Créditos y licencia

Desarrollado en el marco de la asignatura *Modelos Estadísticos para la
Toma de Decisiones II* del *Grado en Ciencia de Datos* de la
*Universitat Politècnica de València*.

Autores: **Elena Vázquez**, **Vicente Chirivella** y **Rosa Alcover**  
Departamento de Estadística e Investigación Operativa Aplicadas y
Calidad – UPV

© 2025 – Licencia MIT (ver `LICENSE.txt`).  
Puedes usar, modificar y distribuir el código citando la fuente.

## 💡 Sugerencias futuras

- Integrar ejemplos adicionales de interpretación de efectos simples e
  interacción.
- Añadir nuevos módulos con funcionalidades extendidas.
- Publicar versión en *shinyapps.io* para ejecución sin instalación.
