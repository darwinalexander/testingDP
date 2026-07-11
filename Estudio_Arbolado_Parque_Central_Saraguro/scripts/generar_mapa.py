# -*- coding: utf-8 -*-
"""Mapa profesional de ubicación de todos los árboles (Parque + Avenida).
Si existe assets/ortofoto.* se usa como imagen aérea de fondo (georreferenciada)."""
import math
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.image as mpimg
from matplotlib.patches import Rectangle
from matplotlib.lines import Line2D
from matplotlib.patheffects import withStroke
import _data as D

COL = {"DERRIBO":"#E53935", "CONSERVAR-INT":"#FB8C00", "CONSERVAR":"#43A047"}
HAS_IMG = D.ORTOFOTO is not None

pts = [t for t in D.trees if t["lat"] and t["lon"]]
lats = [float(t["lat"]) for t in pts]; lons = [float(t["lon"]) for t in pts]
mlat = sum(lats)/len(lats)
aspect = 1/math.cos(math.radians(mlat))
dx = max(lons)-min(lons); dy = max(lats)-min(lats)
padx = dx*0.12+0.00005; pady = dy*0.12+0.00005
xmin, xmax = min(lons)-padx, max(lons)+padx
ymin, ymax = min(lats)-pady, max(lats)+pady

fig, ax = plt.subplots(figsize=(11, 8.5), dpi=200)
fig.patch.set_facecolor("white")
ax.set_xlim(xmin, xmax); ax.set_ylim(ymin, ymax); ax.set_aspect(aspect)

# ---- fondo: ortofoto o color plano ----
if HAS_IMG:
    img = mpimg.imread(D.ORTOFOTO)
    ext = D.ORTOFOTO_BOUNDS if D.ORTOFOTO_BOUNDS else [xmin, ymin, xmax, ymax]
    # ext viene como [min_lon,min_lat,max_lon,max_lat] -> extent=[left,right,bottom,top]
    extent = [ext[0], ext[2], ext[1], ext[3]]
    ax.imshow(img, extent=extent, origin="upper", zorder=0, aspect=aspect, interpolation="lanczos")
    grid_col = "#FFFFFF"; grid_alpha = 0.35; halo = "black"; txt_col = "white"
else:
    ax.set_facecolor("#F7FAF6")
    grid_col = "#B0BEC5"; grid_alpha = 0.8; halo = "white"; txt_col = "#212121"

# ---- puntos ----
for cat in ["CONSERVAR","CONSERVAR-INT","DERRIBO"]:
    xs = [float(t["lon"]) for t in pts if t["cat"]==cat]
    ys = [float(t["lat"]) for t in pts if t["cat"]==cat]
    ax.scatter(xs, ys, s=210, c=COL[cat], edgecolors="white", linewidths=1.6, zorder=5,
               marker=("v" if cat=="DERRIBO" else "o"))
# ---- etiquetas con halo ----
for t in pts:
    ax.annotate(t["id"], (float(t["lon"]), float(t["lat"])), xytext=(5,5),
                textcoords="offset points", fontsize=7.4, fontweight="bold", color=txt_col,
                zorder=6, path_effects=[withStroke(linewidth=2.2, foreground=halo)])

# ---- rótulos de zona ----
ax.annotate("PARQUE CENTRAL", (min(lons)+(max(lons)-min(lons))*0.30, max(lats)+0.00004),
            fontsize=11, fontweight="bold", color=("white" if HAS_IMG else "#1B5E20"),
            alpha=0.9, ha="center", path_effects=[withStroke(linewidth=2.4, foreground=("black" if HAS_IMG else "white"))])
av = [t for t in pts if t["src"]=="AV"]
if av:
    axc = sum(float(t["lon"]) for t in av)/len(av); ayc = max(float(t["lat"]) for t in av)+0.00003
    ax.annotate("AVENIDA\nEL ORO", (axc, ayc), fontsize=9, fontweight="bold",
                color=("white" if HAS_IMG else "#0D47A1"), alpha=0.9, ha="center",
                path_effects=[withStroke(linewidth=2.2, foreground=("black" if HAS_IMG else "white"))])

# ---- rejilla y ejes ----
ax.grid(True, linestyle=":", linewidth=0.6, color=grid_col, alpha=grid_alpha, zorder=1)
ax.set_xlabel("Longitud (°)", fontsize=9); ax.set_ylabel("Latitud (°)", fontsize=9)
ax.tick_params(labelsize=7.5); ax.ticklabel_format(useOffset=False, style="plain")
for s in ax.spines.values(): s.set_color("#607D8B")

# ---- barra de escala (20 m) ----
m_per_deg_lon = 111320*math.cos(math.radians(mlat)); seg = 20.0/m_per_deg_lon
x0 = xmin + dx*0.05; y0 = ymin + dy*0.06
ax.add_patch(Rectangle((x0, y0), seg, dy*0.012, color="black", zorder=7,
                       ec="white", lw=0.8))
ax.text(x0+seg/2, y0+dy*0.03, "20 m", fontsize=7.6, ha="center", color=txt_col, fontweight="bold",
        path_effects=[withStroke(linewidth=2, foreground=halo)])

# ---- flecha norte ----
nx = xmax - dx*0.04; ny = ymax - dy*0.16
ax.annotate("N", xy=(nx, ny+dy*0.10), xytext=(nx, ny),
            arrowprops=dict(facecolor="white" if HAS_IMG else "#212121", edgecolor="black", width=3.5, headwidth=11, headlength=11),
            ha="center", fontsize=12, fontweight="bold", color=txt_col, zorder=8,
            path_effects=[withStroke(linewidth=2.4, foreground=halo)])

# ---- leyenda ----
leg = [Line2D([0],[0], marker="v", color="w", markerfacecolor=COL["DERRIBO"], markeredgecolor="white", markersize=12, label="Derribo (1)"),
       Line2D([0],[0], marker="o", color="w", markerfacecolor=COL["CONSERVAR-INT"], markeredgecolor="white", markersize=12, label="Conservar con intervención (%d)"%D.n_int),
       Line2D([0],[0], marker="o", color="w", markerfacecolor=COL["CONSERVAR"], markeredgecolor="white", markersize=12, label="Conservar (%d)"%D.n_cons)]
ax.legend(handles=leg, loc="lower right", fontsize=8, framealpha=0.95, title="Veredicto técnico", title_fontsize=8.5)

sub = "sobre ortofoto (imagen aérea)" if HAS_IMG else "esquema georreferenciado"
ax.set_title("Mapa de ubicación y veredicto del arbolado — Parque Central y Avenida El Oro, Saraguro\n"
             "%d árboles georreferenciados (GPS) · Datum WGS84 · %s · Fuente: inventario ArboLEC–UNL, 2026" % (len(pts), sub),
             fontsize=10.5, fontweight="bold", color="#1B5E20", pad=12)

plt.tight_layout()
plt.savefig(D.MAPA, dpi=200, bbox_inches="tight", facecolor="white")
print("Mapa generado:", D.MAPA, "| ortofoto de fondo:", "SÍ" if HAS_IMG else "NO")
print("Recuadro (WGS84) que debe cubrir la imagen aérea para calzar con los árboles:")
print("  min_lon=%.6f  min_lat=%.6f  max_lon=%.6f  max_lat=%.6f" % (xmin, ymin, xmax, ymax))
print("  (esquina SO: %.6f, %.6f)  (esquina NE: %.6f, %.6f)" % (ymin, xmin, ymax, xmax))
