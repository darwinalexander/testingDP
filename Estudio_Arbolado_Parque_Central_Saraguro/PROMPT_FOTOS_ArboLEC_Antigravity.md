# Prompt para extraer las fotos de cada árbol desde ArboLEC (Antigravity / Claude)

> Pégalo en Antigravity (tiene acceso a internet). Descarga las fotos de la
> plataforma ArboLEC y las guarda con el nombre correcto para que los scripts del
> informe las inserten automáticamente.

---

ROL: Actúa como ingeniero de datos / web scraping con Python.

OBJETIVO: Descargar las fotografías de cada uno de los 28 árboles del estudio de
Saraguro publicadas en la plataforma ArboLEC, y guardarlas con una convención de
nombres para integrarlas al informe técnico.

FUENTE: https://arbolec.unl.edu.ec/ec/Saraguro  (plataforma pública de la UNL).

PASO 1 — RECONOCIMIENTO (hazlo antes de descargar):
- Abre la página y determina cómo sirve los datos y las imágenes:
  a) ¿Hay un API/JSON? Revisa las peticiones de red (XHR/fetch) que hace la página;
     busca un endpoint que devuelva la lista de árboles de Saraguro con las URLs de
     sus fotos. Suele ser lo más rápido y limpio. Prueba rutas tipo `/api/...`,
     parámetros con el sitio “Saraguro”, o un GeoJSON de puntos.
  b) Si es una SPA (todo se renderiza con JavaScript y no hay API accesible), usa un
     navegador headless (Playwright con Chromium) para navegar, abrir la ficha de
     cada árbol y extraer las URLs de las imágenes (`<img>`, fondos, galería).
- Identifica el campo con el que se referencia cada árbol (código Plus Code, un id
  interno, o el nombre). Necesitarás mapearlo a MI identificador (columna ID abajo).

PASO 2 — DESCARGA Y NOMBRADO:
- Para cada árbol, descarga hasta 3 fotografías (las que haya).
- Guárdalas en la carpeta `assets/fotos/` con este formato EXACTO:
      <ID>_1.jpg    <ID>_2.jpg    <ID>_3.jpg
  usando MI identificador (A01…A24, AV01…AV04). Convierte a .jpg si vienen en otro
  formato. Si un árbol tiene una sola foto, guarda solo `<ID>_1.jpg`.
- Usa esta tabla de equivalencia ID ↔ código (Plus Code) para el emparejamiento:

  A01 = 67829QG6+VHVQ        A13 = 67829QH6+2P3P
  A02 = 67829QG6+VHX3        A14 = 67829QH6+2QJF
  A03 = 67829QG6+VJ9V        A15 = 67829QH6+3G9H
  A04 = 67829QG6+WG7X        A16 = 67829QH6+3HRM
  A05 = 67829QG6+WGX6        A17 = 67829QH6+3JFH
  A06 = 67829QG6+WJG5        A18 = 67829QH6+3JR5
  A07 = 67829QG6+XF9F        A19 = 67829QH6+3M33
  A08 = 67829QG6+XFQG        A20 = 67829QH6+3MH3
  A09 = 67829QG6+XG9W        A21 = 67829QH6+4H2W
  A10 = 67829QG6+XJQ9        A22 = 67829QH6+4HHM
  A11 = 67829QG6+XMQ4        A23 = 67829QH6+4HVV
  A12 = 67829QH6+2HV2        A24 = 67829QH6+4J2X

  AV01 = 67829QG6+XWHF       AV03 = 67829QG6+XX3P
  AV02 = 67829QG6+XWMQ       AV04 = 67829QH6+2V5H

PASO 3 — VERIFICACIÓN E INFORME:
- Imprime un resumen: cuántas fotos descargó por árbol y cuáles árboles quedaron sin
  foto (para revisarlos manualmente).
- Verifica que cada archivo abra correctamente (no esté corrupto ni sea un placeholder
  de 0 bytes).

PASO 4 — INTEGRACIÓN AL INFORME (si trabajas dentro del repositorio del estudio):
- Con las fotos ya en `assets/fotos/`, ejecuta:
      cd scripts
      python3 generar_pdf.py && python3 generar_informe.py
  Los scripts detectan `assets/fotos/<ID>_n.*` y sustituyen automáticamente los
  espacios reservados por las fotos, en la Sección 6 (cipreses) y la Sección 8
  (registro fotográfico) del informe (Word y PDF).

NOTAS:
- Es la plataforma de tu propia institución (UNL) y de consulta pública; la descarga
  de tus fotos de estudio es legítima. Respeta un ritmo prudente de peticiones.
- Si la plataforma exige inicio de sesión para ver las fotos en alta resolución,
  detente y avísame qué credenciales/permiso se necesita antes de continuar.
- Si no encuentras 3 fotos por árbol, no inventes: guarda solo las que existan.
