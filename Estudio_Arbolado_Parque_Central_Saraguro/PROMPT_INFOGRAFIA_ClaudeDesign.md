# Prompt para Claude (Design / Artefactos) — Infografía «Árbol tipo»

> Pega TODO lo que sigue (desde “ROL” hasta el final). Produce un artefacto:
> una infografía científica del «árbol promedio» con semáforo de estado por componente.

---

ROL: Actúa como diseñador de infografías científicas / ilustrador botánico.

OBJETIVO: Crea un ARTEFACTO con una infografía de una sola página (SVG autocontenido, o
HTML con SVG en línea) titulada «Árbol tipo del arbolado urbano de Saraguro». Debe verse
como una figura para publicación científica: fondo BLANCO, ilustración botánica realista
de un árbol (de la raíz a la cima) y un «semáforo» de color que codifica el estado más
frecuente (moda) de cada componente, más una tabla de datos. Formato vertical (aprox.
1400 × 1900), escalable.

ESTILO (importante):
- Fondo blanco, limpio, tipografía sobria (títulos con serif tipo Georgia/Times; datos y
  etiquetas con sans-serif tipo Arial/Helvetica). Nada de degradados de fondo ni colores
  llamativos de relleno de página.
- Ilustración REALISTA, no caricatura: copa con follaje texturizado por muchos cúmulos de
  hojas superpuestos con luz y sombra (degradado radial verde), tronco con degradado y
  estrías de corteza y contrafuertes en la base, ramas que salen del tronco hacia la copa,
  y raíces finas y ramificadas. Copa SIMÉTRICA con forma INTERMEDIA entre conífera y
  latifoliada (ápice superior agudo + cuerpo ancho y redondeado).
- Anotaciones tipo figura científica: líneas guía finas grises desde cada parte hacia una
  etiqueta con una barra de color de estado, el nombre del componente, su estado y el
  porcentaje. Leyenda de semáforo. Pie de figura.

SEMÁFORO DE ESTADO (colores):
- Buen estado = verde #2E7D32
- Estado regular = ámbar #C77D14
- Mal estado = rojo #C0392B
- No visible / no evaluable = gris #9E9E9E

ESTADO POR COMPONENTE (moda de n = 40 árboles) — pinta cada parte con su color:
- RAÍZ → “No visible” · 70 % (28/40) → dibújala ATENUADA / translúcida y punteada, en gris.
- FUSTE (tronco) → “Estado regular” · 50 % (20/40) → tronco en tono ÁMBAR/marrón cálido.
- CORTEZA → “Buen estado” · 60 % (24/40) → verde (acento en el contorno/estrías del tronco).
- RAMAS → “Estado regular” · 75 % (30/40) → ámbar.
- HOJAS → “Buen estado” · 52 % (21/40) → copa VERDE.
- CIMA (punta de la copa) → “Buen estado” · 75 % (30/40) → verde, con un marcador en el ápice.
- COPA (forma) → “Simétrica” · 70 % (28/40) → forma simétrica (verde).

FORMA DEL ÁRBOL:
- Intermedia conífera/latifoliada: 7 coníferas (ápice agudo) + 33 latifoliadas (copa ancha).
- La silueta debe combinar ambas: punta superior en pico + cuerpo ancho redondeado, simétrica.

ETIQUETAS (con línea guía a cada parte; nombre en negrita + estado + porcentaje):
  Cima · Buen estado · 75 % (30/40)
  Copa · Simétrica · 70 % (28/40)
  Hojas · Buen estado · 52 % (21/40)
  Ramas · Estado regular · 75 % (30/40)
  Corteza · Buen estado · 60 % (24/40)
  Fuste · Estado regular · 50 % (20/40)
  Raíz · No visible · 70 % (28/40)
Sugerencia de disposición: Cima, Copa y Hojas a la IZQUIERDA; Ramas, Corteza y Fuste a la
DERECHA; Raíz abajo a la izquierda. Cada etiqueta con una barra vertical del color de estado.

TABLA DE PARÁMETROS (abajo, estilo tabla científica con filetes finos, título
“Parámetros dendrométricos y estructurales del arbolado (n = 40)”):
  Altura total media: 8,5 m (rango 2,7 – 27,1 m)
  Diámetro medio (DAP): 42 cm (rango 1 – 126 cm)
  Follaje medio: 83 % (densidad de copa)
  Madurez dominante: Adulto (80 % de los individuos)
  Porte del fuste (moda): Recto (40 %)
  Espacio de crecimiento: Moderado (62 %)
  Enfermedad más frecuente: Decoloración de hojas (32 % de árboles)
  Agente más frecuente: Epífitas / líquenes (62 % de árboles)
  Veredicto técnico global: 1 derribo · 21 conservar con intervención · 18 conservar

ENCABEZADO Y PIE:
- Título: “Figura 1. Árbol tipo del arbolado urbano de Saraguro”.
- Subtítulo: “Estado sanitario por componente según el valor más frecuente (moda) de
  n = 40 árboles · Inventario ArboLEC–UNL, 2026”.
- Leyenda de semáforo (los 4 colores) en un recuadro.
- Pie: “Cada componente del árbol se representa con el estado más frecuente (moda) entre
  los 40 árboles inventariados; la raíz se muestra atenuada por no ser evaluable a simple
  vista (70 %).”

REQUISITOS TÉCNICOS:
- SVG autocontenido (sin recursos externos), texto como <text> (no imágenes rasterizadas).
- Para el follaje: genera ~120–150 círculos verdes de tamaño y tono variables (más oscuros
  abajo, más claros arriba) dentro de una envolvente elíptica simétrica, más una espiga
  superior para el ápice; añade unos realces claros arriba-derecha y sombras abajo-izquierda.
- Que sea legible y equilibrado; evita solapes de etiquetas.
- Entrega el resultado como un artefacto visible (no solo código).
