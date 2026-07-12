# Prompt para generar el mapa con fondo satelital (Antigravity / Claude)

> Copia y pega TODO lo que sigue (desde “ROL” hasta el final) en Antigravity.
> El agente tiene acceso a internet, por lo que podrá descargar la imagen satelital.

---

ROL: Actúa como experto en SIG y visualización de datos con Python.

OBJETIVO: Genera un mapa profesional en alta resolución (PNG, 200 dpi) que muestre la
ubicación de 40 árboles inventariados en el Parque Central y la Avenida El Oro de
Saraguro (Ecuador), **sobre una imagen satelital/aérea real de fondo** descargada de
Esri World Imagery. Cada árbol se colorea según su veredicto técnico.

ENTORNO: Usa Python con matplotlib, pillow, requests y numpy. Instala lo que falte.
Debes tener acceso a internet para descargar las teselas; si el primer intento falla,
reintenta con otro nivel de zoom.

FONDO SATELITAL (requisito principal):
- Fuente: Esri World Imagery, teselas XYZ:
  https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}
  (ojo al orden z/y/x). Envía un User-Agent normal en la petición.
- Proyección de trabajo: Web Mercator (EPSG:3857). Convierte las coordenadas
  geográficas de los árboles a Web Mercator para que coincidan con las teselas.
- Área a cubrir (recuadro WGS84, lon/lat):
    min_lon = -79.239013   min_lat = -3.623029
    max_lon = -79.237334   max_lat = -3.621956
- Zoom: usa z=19 (si no hay cobertura, prueba z=20 y luego z=18). Descarga TODAS las
  teselas que intersecten el recuadro, móntalas en un mosaico y encuádralo al recuadro.
  Interpolación “lanczos”. Añade el crédito “Esri World Imagery” abajo a la derecha.

DATOS DE LOS ÁRBOLES (40 registros; columnas: ID, lat, lon, codigo, especie, nombre_comun, sitio, veredicto):
A01, -3.622756, -79.238565, 67829QG6+VHVQ, Tecoma stans, Lame negro, Parque, Conservar con intervención
A02, -3.622770, -79.238523, 67829QG6+VHX3, Melaleuca citrina, Calistemo, Parque, Conservar
A03, -3.622826, -79.238398, 67829QG6+VJ9V, Chionanthus pubescens, Arupo, Parque, Conservar con intervención
A04, -3.622704, -79.238688, 67829QG6+WG7X, Cupressus sempervirens, Cipre, Parque, Conservar con intervención
A05, -3.622641, -79.238656, 67829QG6+WGX6, Hesperocyparis macrocarpa, Cipré, Parque, Conservar con intervención
A06, -3.622696, -79.238414, 67829QG6+WJG5, Hesperocyparis macrocarpa, Cipré, Parque, Derribo
A07, -3.622588, -79.238773, 67829QG6+XF9F, Cupressus sempervirens, Cipre, Parque, Conservar con intervención
A08, -3.622535, -79.238758, 67829QG6+XFQG, Platanus x hispanica, Platanera, Parque, Conservar
A09, -3.622579, -79.238634, 67829QG6+XG9W, Cupressus sempervirens, Cipre, Parque, Conservar
A10, -3.622542, -79.238375, 67829QG6+XJQ9, Juglans neotropica, Nogal, Parque, Conservar con intervención
A11, -3.622549, -79.238259, 67829QG6+XMQ4, Populus alba, Álamo blanco, Parque, Conservar
A12, -3.622397, -79.238591, 67829QH6+2HV2, Hesperocyparis macrocarpa, Cipré, Parque, Conservar con intervención
A13, -3.622484, -79.238198, 67829QH6+2P3P, Washingtonia filifera, Palma abanico, Parque, Conservar
A14, -3.622412, -79.238114, 67829QH6+2QJF, Tecoma stans, Lame negro, Parque, Conservar con intervención
A15, -3.622337, -79.238632, 67829QH6+3G9H, Platanus x hispanica, Platanera, Parque, Conservar
A16, -3.622259, -79.238614, 67829QH6+3HRM, Jacaranda mimosifolia, Arabisco, Parque, Conservar
A17, -3.622314, -79.238438, 67829QH6+3JFH, Populus alba, Álamo blanco, Parque, Conservar
A18, -3.622272, -79.238470, 67829QH6+3JR5, Casuarina equisetifolia, Casuarina, Parque, Conservar con intervención
A19, -3.622373, -79.238333, 67829QH6+3M33, Populus alba, Álamo blanco, Parque, Conservar
A20, -3.622324, -79.238267, 67829QH6+3MH3, Schinus molle, Molle, Parque, Conservar con intervención
A21, -3.622226, -79.238608, 67829QH6+4H2W, Platanus x hispanica, Platanera, Parque, Conservar con intervención
A22, -3.622184, -79.238520, 67829QH6+4HHM, Phoenix canariensis, Palma fenix, Parque, Conservar
A23, -3.622126, -79.238583, 67829QH6+4HVV, Cupressus sempervirens, Cipre, Parque, Conservar con intervención
A24, -3.622228, -79.238469, 67829QH6+4J2X, Tecoma stans, Lame negro, Parque, Conservar con intervención
A25, -3.622859, -79.238503, 67829QG6+VH5Q, Jacaranda mimosifolia, Arabisco, Parque, Conservar con intervención
A26, -3.622751, -79.238408, 67829QG6+VJWX, Washingtonia filifera, Palma abanico, Parque, Conservar
A27, -3.622626, -79.238709, 67829QG6+WGVV, Populus alba, Álamo blanco, Parque, Conservar
A28, -3.622658, -79.238526, 67829QG6+WHQJ, Phoenix canariensis, Palma fenix, Parque, Conservar
A29, -3.622646, -79.238453, 67829QG6+WJV4, Melaleuca citrina, Calistemo, Parque, Conservar con intervención
A30, -3.622525, -79.238279, 67829QG6+XMX2, Jacaranda mimosifolia, Arabisco, Parque, Conservar
A31, -3.622388, -79.238678, 67829QH6+2GWF, Tecoma stans, Lame negro, Parque, Conservar con intervención
A32, -3.622440, -79.238403, 67829QH6+2JHC, Phoenix canariensis, Palma fenix, Parque, Conservar
A33, -3.622416, -79.238478, 67829QH6+2JJ8, Melaleuca citrina, Calistemo, Parque, Conservar con intervención
A34, -3.622384, -79.238471, 67829QH6+2JRQ, Melaleuca armillaris, Cepillo blanco, Parque, Conservar
A35, -3.622456, -79.238167, 67829QH6+2P8P, Acacia baileyana, Acacia morada, Parque, Conservar
A36, -3.622368, -79.238191, 67829QH6+3P39, Melaleuca citrina, Calistemo, Parque, Conservar
AV01, -3.622562, -79.237643, 67829QG6+XWHF, Jacaranda mimosifolia, Arabisco, Avenida, Conservar con intervención
AV02, -3.622533, -79.237688, 67829QG6+XWMQ, Schinus molle, Molle, Avenida, Conservar con intervención
AV03, -3.622606, -79.237574, 67829QG6+XX3P, Jacaranda mimosifolia, Arabisco, Avenida, Conservar con intervención
AV04, -3.622489, -79.237753, 67829QH6+2V5H, Acacia sp., Acacia sp., Avenida, Conservar con intervención

SIMBOLOGÍA (por veredicto):
- Derribo → triángulo invertido, rojo (#E53935). (1 árbol: A06)
- Conservar con intervención → círculo, naranja (#FB8C00). (21 árboles)
- Conservar → círculo, verde (#43A047). (18 árboles)
- Todos los marcadores con borde blanco, tamaño ~180 pt².

ETIQUETAS Y ELEMENTOS CARTOGRÁFICOS:
- Junto a cada marcador, la etiqueta debe mostrar el NOMBRE COMÚN y, entre paréntesis,
  el CÓDIGO (Plus Code). Ejemplo: “Ciprés (67829QG6+WJG5)”. En dos líneas, negrita,
  ~6.5 pt, texto blanco con contorno negro de ~2 px para que se lea sobre la foto.
- Como los 40 árboles están muy juntos, evita el solape: usa desplazamiento de la
  etiqueta, líneas guía (leader lines) o repulsión de textos (librería adjustText).
  Si se amontona, admite mostrar solo los 4 caracteres finales del código
  (“Ciprés (…WJG5)”) manteniendo el nombre común completo.
- Rótulos de zona: “PARQUE CENTRAL” sobre los ID A## y “AVENIDA EL ORO” sobre AV##.
- Flecha de norte (N) arriba a la derecha.
- Barra de escala de 20 m abajo a la izquierda (en Web Mercator divide el largo por
  cos(latitud)).
- Leyenda abajo a la derecha: “Derribo (1)”, “Conservar con intervención (21)”,
  “Conservar (18)”.
- Rejilla suave (punteada blanca semitransparente). Ejes en grados (reproyecta de
  Web Mercator a grados para los rótulos).
- Título (dos líneas):
  “Mapa de ubicación y veredicto del arbolado — Parque Central y Avenida El Oro, Saraguro”
  “40 árboles georreferenciados (GPS) · Datum WGS84 · Fondo: Esri World Imagery · Inventario ArboLEC–UNL, 2026”

SALIDA:
- Guarda `mapa_arboles.png` (~11×8.5 in, 200 dpi, fondo blanco, bbox_inches='tight').
- Verifica que los árboles caen sobre el parque en la imagen satelital; si el fondo
  aparece desplazado, revisa la conversión a Web Mercator y el extent del mosaico.

INTEGRACIÓN (si trabajas dentro del repositorio del estudio): coloca el PNG en
`assets/mapa_arboles.png` y ejecuta `python3 generar_pdf.py && python3 generar_informe.py`.
