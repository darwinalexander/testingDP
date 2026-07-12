# Prompt maestro — Construir el informe técnico + oficio (40 árboles) en Antigravity

> Pega TODO lo que sigue (desde “ROL” hasta el final) en Antigravity.
> Reutiliza los scripts ya probados del repositorio del estudio e inserta TU mapa
> satelital y TUS fotos descargadas. Así el informe sale idéntico al validado, con
> los 40 árboles, la Sección de Salud y las fotos/mapa embebidos.

---

ROL: Actúa como ingeniero de datos / automatización con Python.

OBJETIVO: Generar el Informe Técnico (Word y PDF) y el Oficio de respuesta (Word y PDF)
del arbolado del Parque Central y la Avenida El Oro de Saraguro (40 árboles), con el
MAPA satelital y las FOTOS de cada árbol embebidos.

CONTEXTO IMPORTANTE:
- Toda la lógica del informe (veredictos por árbol, sección de salud/estado fitosanitario,
  tablas coloreadas, mapa, registro fotográfico, oficio) ya está implementada y probada
  en los scripts del repositorio. NO reimplementes el informe desde cero: usa esos scripts.
- Los scripts insertan automáticamente las fotos si están en `assets/fotos/<ID>_n.jpg`,
  y usan `assets/mapa_arboles.png` como mapa de la metodología.

PASO 1 — OBTENER LOS SCRIPTS DEL ESTUDIO
Clona la rama del repositorio (o haz `git pull` si ya la tienes):

    git clone -b claude/saraguro-tree-risk-report-m9f8t8 https://github.com/darwinalexander/testingDP
    cd testingDP/Estudio_Arbolado_Parque_Central_Saraguro

Estructura relevante:
    scripts/_data.py            (carga los 40 árboles, veredictos y variables de salud)
    scripts/generar_mapa.py     (mapa; descarga teselas satelitales de Esri si hay internet)
    scripts/generar_pdf.py      (informe PDF)
    scripts/generar_informe.py  (informe Word)
    scripts/generar_oficio.py   (oficio Word + genera también su PDF vía generar_pdf)
    assets/                     (logos, Excel de 40 árboles, carpeta fotos/)

PASO 2 — COLOCAR LOS INSUMOS EN `assets/`
1. Excel de 40 árboles: debe estar en `assets/Reporte_Arboles_40.xlsx`. Ya viene en el
   repo; si tu versión es más reciente, reemplázala con ese MISMO nombre.
2. Fotos por árbol: copia a `assets/fotos/` las fotos que ya descargaste, con el nombre
   `<ID>_1.jpg`, `<ID>_2.jpg`, `<ID>_3.jpg` (IDs A01…A36, AV01…AV04). Los IDs de los 28
   árboles previos NO cambiaron, así que esas fotos siguen siendo válidas. Descarga las
   de los 12 árboles nuevos (A25…A36) con el prompt de fotos (PROMPT_FOTOS_ArboLEC).
3. Mapa: NO hace falta ponerlo a mano. El paso 4 ejecuta `generar_mapa.py`, que como
   Antigravity tiene internet descargará la imagen satelital de Esri y armará el mapa de
   los 40 árboles. Si prefieres usar tu mapa ya hecho, cópialo como
   `assets/mapa_arboles.png` y omite `generar_mapa.py`.

PASO 3 — INSTALAR DEPENDENCIAS

    pip install matplotlib openpyxl python-docx reportlab pillow requests

PASO 4 — GENERAR TODO (desde la carpeta `scripts/`)

    cd scripts
    python3 generar_mapa.py       # mapa satelital (Esri) con los 40 árboles
    python3 generar_pdf.py        # Informe_Tecnico_...pdf  + Oficio_...pdf
    python3 generar_informe.py    # Informe_Tecnico_...docx
    python3 generar_oficio.py     # Oficio_Respuesta_...docx

PASO 5 — SALIDAS (en la carpeta del estudio, un nivel arriba de scripts/)
    Informe_Tecnico_Arbolado_Parque_Central_Saraguro.pdf   / .docx
    Oficio_Respuesta_GAD_Saraguro.pdf                        / .docx

Verifica que:
- Al inicio (después de la portada) aparece el RESUMEN GRÁFICO: el «árbol tipo» con el
  semáforo de estado por componente + resumen descriptivo. La imagen ya viene
  pre-renderizada en `assets/infografia_arbol_promedio.png` (no requiere regenerarse).
- El mapa de la Sección 3 (Metodología) muestra el fondo satelital con los 40 árboles.
- La Sección 5 (Salud y estado fitosanitario) tiene sus tablas y la ficha por árbol.
- La Sección 9 (Registro fotográfico) muestra las fotos reales (no los recuadros
  “insertar foto”) para los árboles que sí tienen imagen en `assets/fotos/`.

NOTAS:
- El veredicto es objetivo y ya está fijado: 1 derribo (A06), 21 conservar con
  intervención, 18 conservar. No lo cambies salvo que revises la evidencia.
- Si algún árbol no tiene foto, el informe deja el espacio reservado; no inventes fotos.
- Si `generar_mapa.py` no lograra descargar teselas (sin internet), usará un mapa
  esquemático de respaldo; en Antigravity debería descargar la imagen satelital sin
  problema.
- Si quieres regenerar los prompts de mapa/fotos a partir de los datos, ejecuta
  `python3 generar_prompts.py`.
