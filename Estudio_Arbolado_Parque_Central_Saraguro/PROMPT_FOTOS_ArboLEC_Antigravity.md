# Prompt para extraer las fotos de cada árbol desde ArboLEC (Antigravity / Claude)

> Pégalo en Antigravity (tiene acceso a internet). Descarga las fotos de la
> plataforma ArboLEC y las guarda con el nombre correcto para que los scripts del
> informe las inserten automáticamente.

---

ROL: Actúa como ingeniero de datos / web scraping con Python.

OBJETIVO: Descargar las fotografías de cada uno de los 40 árboles del estudio de
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
  usando MI identificador (A01…A36, AV01…AV04). Convierte a .jpg si vienen en otro
  formato. Si un árbol tiene una sola foto, guarda solo `<ID>_1.jpg`.
- Tabla de equivalencia ID ↔ código (Plus Code):

  A01   = 67829QG6+VHVQ        A21   = 67829QH6+4H2W
  A02   = 67829QG6+VHX3        A22   = 67829QH6+4HHM
  A03   = 67829QG6+VJ9V        A23   = 67829QH6+4HVV
  A04   = 67829QG6+WG7X        A24   = 67829QH6+4J2X
  A05   = 67829QG6+WGX6        A25   = 67829QG6+VH5Q
  A06   = 67829QG6+WJG5        A26   = 67829QG6+VJWX
  A07   = 67829QG6+XF9F        A27   = 67829QG6+WGVV
  A08   = 67829QG6+XFQG        A28   = 67829QG6+WHQJ
  A09   = 67829QG6+XG9W        A29   = 67829QG6+WJV4
  A10   = 67829QG6+XJQ9        A30   = 67829QG6+XMX2
  A11   = 67829QG6+XMQ4        A31   = 67829QH6+2GWF
  A12   = 67829QH6+2HV2        A32   = 67829QH6+2JHC
  A13   = 67829QH6+2P3P        A33   = 67829QH6+2JJ8
  A14   = 67829QH6+2QJF        A34   = 67829QH6+2JRQ
  A15   = 67829QH6+3G9H        A35   = 67829QH6+2P8P
  A16   = 67829QH6+3HRM        A36   = 67829QH6+3P39
  A17   = 67829QH6+3JFH        AV01  = 67829QG6+XWHF
  A18   = 67829QH6+3JR5        AV02  = 67829QG6+XWMQ
  A19   = 67829QH6+3M33        AV03  = 67829QG6+XX3P
  A20   = 67829QH6+3MH3        AV04  = 67829QH6+2V5H

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
