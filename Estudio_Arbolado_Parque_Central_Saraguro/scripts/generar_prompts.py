# -*- coding: utf-8 -*-
"""Genera los prompts de Antigravity (mapa satelital y extracción de fotos) desde _data."""
import os
import _data as D

OUT = os.path.join(os.path.dirname(__file__), "..")
CATES = {"DERRIBO":"Derribo","CONSERVAR-INT":"Conservar con intervención","CONSERVAR":"Conservar"}

lats=[float(t["lat"]) for t in D.trees]; lons=[float(t["lon"]) for t in D.trees]
dx=max(lons)-min(lons); dy=max(lats)-min(lats); padx=dx*0.15+0.00006; pady=dy*0.15+0.00006
xmin,ymin,xmax,ymax = min(lons)-padx, min(lats)-pady, max(lons)+padx, max(lats)+pady

# ---------------- PROMPT MAPA ----------------
rows="\n".join("%s, %.6f, %.6f, %s, %s, %s, %s, %s"%(
    t["id"],float(t["lat"]),float(t["lon"]),t["codigo"],t["especie"],t["comun"],
    ("Parque" if t["src"]=="A" else "Avenida"),CATES[t["cat"]]) for t in D.trees)

mapa = """# Prompt para generar el mapa con fondo satelital (Antigravity / Claude)

> Copia y pega TODO lo que sigue (desde “ROL” hasta el final) en Antigravity.
> El agente tiene acceso a internet, por lo que podrá descargar la imagen satelital.

---

ROL: Actúa como experto en SIG y visualización de datos con Python.

OBJETIVO: Genera un mapa profesional en alta resolución (PNG, 200 dpi) que muestre la
ubicación de {N} árboles inventariados en el Parque Central y la Avenida El Oro de
Saraguro (Ecuador), **sobre una imagen satelital/aérea real de fondo** descargada de
Esri World Imagery. Cada árbol se colorea según su veredicto técnico.

ENTORNO: Usa Python con matplotlib, pillow, requests y numpy. Instala lo que falte.
Debes tener acceso a internet para descargar las teselas; si el primer intento falla,
reintenta con otro nivel de zoom.

FONDO SATELITAL (requisito principal):
- Fuente: Esri World Imagery, teselas XYZ:
  https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{{z}}/{{y}}/{{x}}
  (ojo al orden z/y/x). Envía un User-Agent normal en la petición.
- Proyección de trabajo: Web Mercator (EPSG:3857). Convierte las coordenadas
  geográficas de los árboles a Web Mercator para que coincidan con las teselas.
- Área a cubrir (recuadro WGS84, lon/lat):
    min_lon = {xmin:.6f}   min_lat = {ymin:.6f}
    max_lon = {xmax:.6f}   max_lat = {ymax:.6f}
- Zoom: usa z=19 (si no hay cobertura, prueba z=20 y luego z=18). Descarga TODAS las
  teselas que intersecten el recuadro, móntalas en un mosaico y encuádralo al recuadro.
  Interpolación “lanczos”. Añade el crédito “Esri World Imagery” abajo a la derecha.

DATOS DE LOS ÁRBOLES ({N} registros; columnas: ID, lat, lon, codigo, especie, nombre_comun, sitio, veredicto):
{rows}

SIMBOLOGÍA (por veredicto):
- Derribo → triángulo invertido, rojo (#E53935). ({d} árbol: A06)
- Conservar con intervención → círculo, naranja (#FB8C00). ({i} árboles)
- Conservar → círculo, verde (#43A047). ({c} árboles)
- Todos los marcadores con borde blanco, tamaño ~180 pt².

ETIQUETAS Y ELEMENTOS CARTOGRÁFICOS:
- Junto a cada marcador, la etiqueta debe mostrar el NOMBRE COMÚN y, entre paréntesis,
  el CÓDIGO (Plus Code). Ejemplo: “Ciprés (67829QG6+WJG5)”. En dos líneas, negrita,
  ~6.5 pt, texto blanco con contorno negro de ~2 px para que se lea sobre la foto.
- Como los {N} árboles están muy juntos, evita el solape: usa desplazamiento de la
  etiqueta, líneas guía (leader lines) o repulsión de textos (librería adjustText).
  Si se amontona, admite mostrar solo los 4 caracteres finales del código
  (“Ciprés (…WJG5)”) manteniendo el nombre común completo.
- Rótulos de zona: “PARQUE CENTRAL” sobre los ID A## y “AVENIDA EL ORO” sobre AV##.
- Flecha de norte (N) arriba a la derecha.
- Barra de escala de 20 m abajo a la izquierda (en Web Mercator divide el largo por
  cos(latitud)).
- Leyenda abajo a la derecha: “Derribo ({d})”, “Conservar con intervención ({i})”,
  “Conservar ({c})”.
- Rejilla suave (punteada blanca semitransparente). Ejes en grados (reproyecta de
  Web Mercator a grados para los rótulos).
- Título (dos líneas):
  “Mapa de ubicación y veredicto del arbolado — Parque Central y Avenida El Oro, Saraguro”
  “{N} árboles georreferenciados (GPS) · Datum WGS84 · Fondo: Esri World Imagery · Inventario ArboLEC–UNL, 2026”

SALIDA:
- Guarda `mapa_arboles.png` (~11×8.5 in, 200 dpi, fondo blanco, bbox_inches='tight').
- Verifica que los árboles caen sobre el parque en la imagen satelital; si el fondo
  aparece desplazado, revisa la conversión a Web Mercator y el extent del mosaico.

INTEGRACIÓN (si trabajas dentro del repositorio del estudio): coloca el PNG en
`assets/mapa_arboles.png` y ejecuta `python3 generar_pdf.py && python3 generar_informe.py`.
""".format(N=D.n, xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax, rows=rows,
           d=D.n_derribo, i=D.n_int, c=D.n_cons)

open(os.path.join(OUT,"PROMPT_MAPA_SATELITAL_Antigravity.md"),"w",encoding="utf-8").write(mapa)

# ---------------- PROMPT FOTOS ----------------
ids=[(t["id"],t["codigo"]) for t in D.trees]
half=(len(ids)+1)//2
map_lines=[]
for k in range(half):
    left="%-5s = %s"%(ids[k][0],ids[k][1])
    right=""
    if k+half < len(ids):
        right="        %-5s = %s"%(ids[k+half][0],ids[k+half][1])
    map_lines.append("  "+left+right)
mapping="\n".join(map_lines)

fotos = """# Prompt para extraer las fotos de cada árbol desde ArboLEC (Antigravity / Claude)

> Pégalo en Antigravity (tiene acceso a internet). Descarga las fotos de la
> plataforma ArboLEC y las guarda con el nombre correcto para que los scripts del
> informe las inserten automáticamente.

---

ROL: Actúa como ingeniero de datos / web scraping con Python.

OBJETIVO: Descargar las fotografías de cada uno de los {N} árboles del estudio de
Saraguro publicadas en la plataforma ArboLEC y guardarlas con una convención de
nombres para integrarlas al informe técnico.

FUENTE: https://arbolec.unl.edu.ec/ec/Saraguro  (plataforma pública de la UNL).

PASO 1 — RECONOCIMIENTO (antes de descargar):
- Determina cómo sirve los datos y las imágenes:
  a) ¿Hay un API/JSON? Revisa las peticiones de red (XHR/fetch); busca un endpoint que
     devuelva la lista de árboles de Saraguro con las URLs de sus fotos. Prueba rutas
     tipo `/api/...`, parámetros con “Saraguro” o un GeoJSON de puntos.
  b) Si es una SPA sin API accesible, usa un navegador headless (Playwright con
     Chromium) para abrir la ficha de cada árbol y extraer las URLs de las imágenes.
- Identifica el campo con el que se referencia cada árbol (código Plus Code, id interno
  o nombre) y mapéalo a MI identificador (columna ID de la tabla de abajo).

PASO 2 — DESCARGA Y NOMBRADO:
- Para cada árbol descarga hasta 3 fotografías (las que haya).
- Guárdalas en `assets/fotos/` con este formato EXACTO:
      <ID>_1.jpg    <ID>_2.jpg    <ID>_3.jpg
  usando MI identificador (A01…A{maxA}, AV01…AV04). Convierte a .jpg si vienen en otro
  formato. Si un árbol tiene una sola foto, guarda solo `<ID>_1.jpg`.
- Tabla de equivalencia ID ↔ código (Plus Code):

{mapping}

PASO 3 — VERIFICACIÓN:
- Imprime un resumen: cuántas fotos por árbol y cuáles quedaron sin foto.
- Verifica que cada archivo abra correctamente (no corrupto ni de 0 bytes).

PASO 4 — INTEGRACIÓN AL INFORME (dentro del repositorio del estudio):
- Con las fotos en `assets/fotos/`, ejecuta:
      cd scripts
      python3 generar_pdf.py && python3 generar_informe.py
  Los scripts detectan `assets/fotos/<ID>_n.*` y sustituyen automáticamente los
  espacios reservados por las fotos en la Sección 7 (cipreses) y la Sección 9
  (registro fotográfico).

NOTAS:
- Es la plataforma de tu propia institución (UNL) y de consulta pública; la descarga es
  legítima. Respeta un ritmo prudente de peticiones.
- Si la plataforma exige inicio de sesión para ver las fotos en alta resolución,
  detente y avísame qué credenciales/permiso se necesita.
- Si no hay 3 fotos por árbol, no inventes: guarda solo las que existan.
""".format(N=D.n, maxA=max(int(t["id"][1:]) for t in D.trees if t["src"]=="A"), mapping=mapping)

open(os.path.join(OUT,"PROMPT_FOTOS_ArboLEC_Antigravity.md"),"w",encoding="utf-8").write(fotos)
print("Prompts regenerados (mapa y fotos) con", D.n, "árboles.")
