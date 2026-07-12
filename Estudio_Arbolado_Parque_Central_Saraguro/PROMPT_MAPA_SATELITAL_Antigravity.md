# Prompt para generar el mapa con fondo satelital (Antigravity / Claude)

> Copia y pega TODO lo que sigue (desde “ROL” hasta el final) en Antigravity.
> El agente tiene acceso a internet, por lo que podrá descargar la imagen satelital.

---

ROL: Actúa como experto en SIG y visualización de datos con Python.

OBJETIVO: Genera un mapa profesional en alta resolución (PNG, 200 dpi) que muestre la
ubicación de 28 árboles inventariados en el Parque Central y la Avenida El Oro de
Saraguro (Ecuador), **sobre una imagen satelital/aérea real de fondo** descargada de
Esri World Imagery. Cada árbol se colorea según su veredicto técnico.

ENTORNO: Usa Python con matplotlib, pillow, requests y numpy. Instala lo que falte.
Debes tener acceso a internet para descargar las teselas; si el primer intento falla,
reintenta con otro nivel de zoom.

FONDO SATELITAL (requisito principal):
- Fuente: Esri World Imagery, teselas XYZ:
  https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}
  (ojo al orden {z}/{y}/{x}). Envía un User-Agent normal en la petición.
- Proyección de trabajo: Web Mercator (EPSG:3857). Convierte las coordenadas
  geográficas de los árboles a Web Mercator para que coincidan con las teselas.
- Área a cubrir (recuadro WGS84, lon/lat):
    min_lon = -79.239013   min_lat = -3.622991
    max_lon = -79.237334   max_lat = -3.621961
- Zoom: usa z=19 (si no hay cobertura, prueba z=20 y luego z=18). Descarga TODAS las
  teselas que intersecten el recuadro, móntalas en un mosaico y recórtalo/encuádralo
  al recuadro. Interpolación “lanczos”. Añade el crédito “Esri World Imagery” abajo
  a la derecha, en letra pequeña.

DATOS DE LOS ÁRBOLES (28 registros; columnas: ID, lat, lon, especie, nombre_comun, sitio, veredicto):
A01, -3.622756, -79.238565, Tecoma stans, Lame negro, Parque, Conservar con intervención
A02, -3.622770, -79.238523, Melaleuca citrina, Calistemo, Parque, Conservar
A03, -3.622826, -79.238398, Chionanthus pubescens, Arupo, Parque, Conservar con intervención
A04, -3.622704, -79.238688, Cupressus sempervirens, Ciprés, Parque, Conservar con intervención
A05, -3.622641, -79.238656, Hesperocyparis macrocarpa, Ciprés, Parque, Conservar con intervención
A06, -3.622696, -79.238414, Hesperocyparis macrocarpa, Ciprés, Parque, Derribo
A07, -3.622588, -79.238773, Cupressus sempervirens, Ciprés, Parque, Conservar con intervención
A08, -3.622535, -79.238758, Platanus x hispanica, Platanera, Parque, Conservar
A09, -3.622579, -79.238634, Cupressus sempervirens, Ciprés, Parque, Conservar
A10, -3.622542, -79.238375, Juglans neotropica, Nogal andino, Parque, Conservar con intervención
A11, -3.622549, -79.238259, Populus alba, Álamo blanco, Parque, Conservar
A12, -3.622397, -79.238591, Hesperocyparis macrocarpa, Ciprés, Parque, Conservar con intervención
A13, -3.622484, -79.238198, Washingtonia filifera, Palma abanico, Parque, Conservar
A14, -3.622412, -79.238114, Tecoma stans, Lame negro, Parque, Conservar con intervención
A15, -3.622337, -79.238632, Platanus x hispanica, Platanera, Parque, Conservar
A16, -3.622259, -79.238614, Jacaranda mimosifolia, Arabisco, Parque, Conservar
A17, -3.622314, -79.238438, Populus alba, Álamo blanco, Parque, Conservar
A18, -3.622272, -79.238470, Casuarina equisetifolia, Casuarina, Parque, Conservar con intervención
A19, -3.622373, -79.238333, Populus alba, Álamo blanco, Parque, Conservar
A20, -3.622324, -79.238267, Schinus molle, Molle, Parque, Conservar con intervención
A21, -3.622226, -79.238608, Platanus x hispanica, Platanera, Parque, Conservar con intervención
A22, -3.622184, -79.238520, Phoenix canariensis, Palma fénix, Parque, Conservar
A23, -3.622126, -79.238583, Cupressus sempervirens, Ciprés, Parque, Conservar con intervención
A24, -3.622228, -79.238469, Tecoma stans, Lame negro, Parque, Conservar con intervención
AV01, -3.622562, -79.237643, Jacaranda mimosifolia, Arabisco, Avenida, Conservar con intervención
AV02, -3.622533, -79.237688, Schinus molle, Molle, Avenida, Conservar con intervención
AV03, -3.622606, -79.237574, Jacaranda mimosifolia, Arabisco, Avenida, Conservar con intervención
AV04, -3.622489, -79.237753, Acacia sp., Acacia, Avenida, Conservar con intervención

SIMBOLOGÍA (por veredicto):
- Derribo → marcador triángulo invertido, color rojo (#E53935). (1 árbol: A06)
- Conservar con intervención → círculo, color naranja (#FB8C00). (17 árboles)
- Conservar → círculo, color verde (#43A047). (10 árboles)
- Todos los marcadores con borde blanco, tamaño ~210 pt², z alto.

ETIQUETAS Y ELEMENTOS CARTOGRÁFICOS:
- Junto a cada marcador, su ID (A01…AV04) en negrita, con halo/contorno para que se
  lea sobre la foto (texto blanco con contorno negro de ~2 px).
- Rótulos de zona: “PARQUE CENTRAL” sobre el grupo A01–A24 y “AVENIDA EL ORO” sobre
  AV01–AV04, en negrita con halo.
- Flecha de norte (N) arriba a la derecha.
- Barra de escala de 20 m abajo a la izquierda (calcúlala en metros reales; recuerda
  que en Web Mercator hay que dividir por cos(latitud) para el largo en pantalla).
- Leyenda abajo a la derecha con las 3 categorías y su conteo:
  “Derribo (1)”, “Conservar con intervención (17)”, “Conservar (10)”.
- Rejilla suave (líneas punteadas blancas semitransparentes).
- Ejes con etiquetas de Longitud/Latitud en grados (reproyecta las marcas de Web
  Mercator de vuelta a grados para los rótulos de los ejes).
- Título (dos líneas):
  “Mapa de ubicación y veredicto del arbolado — Parque Central y Avenida El Oro, Saraguro”
  “28 árboles georreferenciados (GPS) · Datum WGS84 · Fondo: Esri World Imagery · Inventario ArboLEC–UNL, 2026”

SALIDA:
- Guarda el archivo como `mapa_arboles.png` (figura ~11×8.5 in, 200 dpi, fondo blanco,
  bbox_inches='tight').
- Verifica visualmente que los árboles caen sobre el parque en la imagen satelital
  (deben verse las parcelas/senderos del Parque Central bajo los puntos). Si el fondo
  aparece desplazado respecto a los puntos, revisa la conversión a Web Mercator y el
  extent del mosaico, y corrige antes de entregar.

OPCIONAL (si quieres integrarlo al informe):
- El repositorio del estudio ya tiene los scripts `generar_pdf.py` y `generar_informe.py`
  que insertan `assets/mapa_arboles.png` en la sección de Metodología. Si trabajas dentro
  de ese repo, coloca el PNG en `assets/` con ese nombre y vuelve a ejecutarlos para
  regenerar el informe (Word y PDF) con el mapa satelital de fondo.
