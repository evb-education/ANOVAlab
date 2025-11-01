# ANOVAlab — Ayuda

ANOVAlab es una herramienta didáctica interactiva para comprender la descomposición de la variabilidad en el Análisis de la Varianza (ANOVA), tanto con **un factor** como con **dos factores**.

Se enmarca en la asignatura de *Modelos Estadísticos para la Toma de Decisiones II* del *Grado de Ciencia de Datos* de la *Universitat Politècnica de València* (*ETSINF*)

------------------------------------------------------------------------

## ANOVA de un factor (1F)

### 1. Diagrama de violines

Representa la **distribución de las observaciones** de cada variante o nivel del factor:

\- Cada violín muestra la forma de la distribución (densidad).

\- Los puntos indican las observaciones individuales.

\- Los puntos negros representan las **medias de cada grupo**.

\- La línea discontinua corresponde a la **media general (μ)**.

📘 **Interpretación:**\
Permite comparar visualmente las diferencias de medias entre los grupos y observar la variabilidad interna (residual).

### 2. Representación geométrica (segmentos)

Muestra la **descomposición geométrica de la variabilidad** total:

\- **Segmentos** **rojos**: diferencias entre cada observación y la media de su variante / nivel (grupo). Estas segmentos representan las distancias usadas en el cálculo de la **Suma de Cuadrados Residual (SCR)**.\
- **Segmentos** **negros**: diferencias entre la media decada variante / nivel (grupo) y la media general. Estas segmentos representan las distancias usadas en el cálculo de la **Suma de Cuadrados Explicada o Entre grupos (SCE)**.

📘 **Interpretación:**\
Visualiza cómo la variabilidad total se descompone en una parte **explicada por el factor** y otra **debida al error aleatorio**.

### 3. Tabla resumen del ANOVA

Presenta los cálculos numéricos de la descomposición de la variabilidad:

| Fuente       | SC  | gl  | CM        | F   | p-valor |
|--------------|-----|-----|-----------|-----|---------|
| Entre grupos | SCE | I−1 | SCE/(I−1) | F₀  | valor-p |
| Residual     | SCR | N−I | SCR/(N−I) |     |         |
| Total        | SCT | N−1 |           |     |         |

📘 **Interpretación:**\
- **F₀** es el estadístico F que permite comparar la variabilidad entre grupos respecto a la variabilidad residual.\
- Si el valor-p es pequeño (\< α), se rechaza la hipótesis de igualdad de medias.

### 4. Descomposición de la variabilidad total (barras apiladas)

Representa gráficamente la **Suma de Cuadrados Total (SCT)** y su descomposición en **SCE** y **SCR**:

\- SCE (parte explicada por el factor)

\- SCR (parte no explicada)

📘 **Interpretación:**\
Ayuda a visualizar la proporción de la variabilidad total explicada por el factor con respecto a la residual.

------------------------------------------------------------------------

## ANOVA de dos factores (2F)

### 1. Diagrama de violines A×B

Muestra la **distribución de las observaciones por cada combinación de variantes o niveles de A y B**.

\- Cada violín corresponde a un tratamiento A×B.

\- Los puntos negros son las medias de cada combinación.

\- La línea discontinua representa la media general μ.

📘 **Interpretación:**\
Permite observar diferencias de medias entre combinaciones y la posible existencia de interacción.

### 2. Representación geométrica (segmentos)

Descompone visualmente la variabilidad total en:

\- **SCA**: variabilidad debida al factor A.\
- **SCB**: variabilidad debida al factor B.\
- **SCA×B**: variabilidad debida a la interacción A×B.\
- **SCR**: variabilidad residual.

📘 **Interpretación:**\
Permite apreciar la parte de la variabilidad explicada por cada factor y por la interacción.

### 3. Tabla resumen del ANOVA

Tabla con los resultados numéricos de la descomposición:

| Fuente   | SC    | gl         | CM                | F    | p-valor    |
|----------|-------|------------|-------------------|------|------------|
| A        | SCA   | I−1        | SCA/(I−1)         | F_A  | valor-p_A  |
| B        | SCB   | J−1        | SCB/(J−1)         | F_b  | valor-p_B  |
| A×B      | SCA×B | (I−1)(J−1) | SCAB/[(I−1)(J−1)] | F_AB | valor-p_AB |
| Residual | SCR   | N−I·J      | SCR/(N−I·J)       |      |            |
| Total    | SCT   | N−1        |                   |      |            |

📘 **Interpretación:**\
Permite evaluar si existen efectos significativos de A, B o su interacción.\
El valor-p asociado a cada F indica si el factor o interacción tiene un efecto estadísticamente significativo.

### 4. Descomposición de la variabilidad total (barras apiladas)

Gráfico que muestra la **SCT** y su reparto entre los distintos componentes SCA, SCB, SCA×B y SCR.

📘 **Interpretación:**\
Permite comparar visualmente qué proporción de la variabilidad total explica cada fuente de variación.

### 5. Gráfico de interacción

Representa las **medias de los tratamientos A×B**:

\- En el eje X: niveles de un factor (por ejemplo, A).

\- Líneas de colores: niveles del otro factor (B).

\- Si las líneas **no son paralelas**, hay indicio de **interacción**.

📘 **Interpretación:**\
Permite detectar si el efecto de un factor depende del nivel del otro.

### 6. Intervalos LSD (A y B)

Muestra los **intervalos LSD** para las medias marginales de A y de B, calculados con el mismo nivel de significación.

📘 **Interpretación:**\
Permiten comparar pares de medias y determinar qué niveles difieren significativamente.

------------------------------------------------------------------------

## Supuestos del ANOVA

Los **supuestos del modelo ANOVA** se mantienen inalterados:

1.  **Normalidad:** las observaciones dentro de cada grupo (tratamiento) provienen de una población normal.\
2.  **Homocedasticidad:** todas las poblaciones tienen la misma varianza (σ²).\
3.  **Independencia:** las observaciones son independientes entre sí.

------------------------------------------------------------------------

Además,

-   Se asume **nivel de significación alfa = 0,05.**

-   Se asume **diseño equilibrado**.

------------------------------------------------------------------------

© Universitat Politècnica de València — Asignatura *Modelos Estadísticos para la Toma de Decisiones II* - E. Vázquez, V. Chirivella y R. Alcover
