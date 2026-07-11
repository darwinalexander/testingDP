# -*- coding: utf-8 -*-
"""Mapa profesional de ubicación de todos los árboles (Parque + Avenida)."""
import math
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import FancyArrow, Rectangle
from matplotlib.lines import Line2D
import _data as D

COL = {"DERRIBO":"#B71C1C", "CONSERVAR-INT":"#E65A00", "CONSERVAR":"#1B5E20"}

pts = [t for t in D.trees if t["lat"] and t["lon"]]
lats = [float(t["lat"]) for t in pts]; lons = [float(t["lon"]) for t in pts]
mlat = sum(lats)/len(lats)
aspect = 1/math.cos(math.radians(mlat))

fig, ax = plt.subplots(figsize=(11, 8.5), dpi=200)
fig.patch.set_facecolor("white")
ax.set_facecolor("#F7FAF6")

# puntos por categoría
for cat in ["CONSERVAR","CONSERVAR-INT","DERRIBO"]:
    xs = [float(t["lon"]) for t in pts if t["cat"]==cat]
    ys = [float(t["lat"]) for t in pts if t["cat"]==cat]
    ax.scatter(xs, ys, s=190, c=COL[cat], edgecolors="white", linewidths=1.3, zorder=5,
               marker=("v" if cat=="DERRIBO" else "o"))

# etiquetas
for t in pts:
    ax.annotate(t["id"], (float(t["lon"]), float(t["lat"])),
                xytext=(4.5, 4.5), textcoords="offset points",
                fontsize=7.2, fontweight="bold", color="#212121", zorder=6)

# separacion visual Parque / Avenida (anotaciones de zona)
ax.annotate("PARQUE CENTRAL", (min(lons)+ (max(lons)-min(lons))*0.30, max(lats)+0.00004),
            fontsize=11, fontweight="bold", color="#1B5E20", alpha=0.55, ha="center")
av = [t for t in pts if t["src"]=="AV"]
if av:
    axc = sum(float(t["lon"]) for t in av)/len(av)
    ayc = max(float(t["lat"]) for t in av)+0.00003
    ax.annotate("AVENIDA\nEL ORO", (axc, ayc), fontsize=9, fontweight="bold",
                color="#0D47A1", alpha=0.6, ha="center")

# rejilla y ejes
ax.grid(True, linestyle=":", linewidth=0.6, color="#B0BEC5", alpha=0.8, zorder=0)
ax.set_xlabel("Longitud (°)", fontsize=9); ax.set_ylabel("Latitud (°)", fontsize=9)
ax.tick_params(labelsize=7.5)
ax.ticklabel_format(useOffset=False, style="plain")
for s in ax.spines.values(): s.set_color("#607D8B")

# margenes
dx = (max(lons)-min(lons)); dy = (max(lats)-min(lats))
padx = dx*0.12+0.00005; pady = dy*0.12+0.00005
ax.set_xlim(min(lons)-padx, max(lons)+padx)
ax.set_ylim(min(lats)-pady, max(lats)+pady)
ax.set_aspect(aspect)

# barra de escala (20 m)
m_per_deg_lon = 111320*math.cos(math.radians(mlat))
seg = 20.0/m_per_deg_lon
x0 = min(lons)-padx + dx*0.05; y0 = min(lats)-pady + dy*0.06
ax.add_patch(Rectangle((x0, y0), seg, dy*0.012, color="#212121", zorder=7))
ax.text(x0+seg/2, y0+dy*0.03, "20 m", fontsize=7.5, ha="center", color="#212121")

# flecha norte
nx = max(lons)+padx - dx*0.04; ny = max(lats)+pady - dy*0.16
ax.annotate("N", xy=(nx, ny+dy*0.10), xytext=(nx, ny),
            arrowprops=dict(facecolor="#212121", width=3.5, headwidth=11, headlength=11),
            ha="center", fontsize=11, fontweight="bold", color="#212121", zorder=8)

# leyenda
leg = [Line2D([0],[0], marker="v", color="w", markerfacecolor=COL["DERRIBO"], markeredgecolor="white",
              markersize=12, label="Derribo (1)"),
       Line2D([0],[0], marker="o", color="w", markerfacecolor=COL["CONSERVAR-INT"], markeredgecolor="white",
              markersize=12, label="Conservar con intervención (%d)"%D.n_int),
       Line2D([0],[0], marker="o", color="w", markerfacecolor=COL["CONSERVAR"], markeredgecolor="white",
              markersize=12, label="Conservar (%d)"%D.n_cons)]
ax.legend(handles=leg, loc="lower right", fontsize=8, framealpha=0.95, title="Veredicto técnico",
          title_fontsize=8.5)

ax.set_title("Mapa de ubicación y veredicto del arbolado — Parque Central y Avenida El Oro, Saraguro\n"
             "%d árboles georreferenciados (GPS) · Datum WGS84 · Fuente: inventario ArboLEC–UNL, 2026" % len(pts),
             fontsize=10.5, fontweight="bold", color="#1B5E20", pad=12)

plt.tight_layout()
plt.savefig(D.MAPA, dpi=200, bbox_inches="tight", facecolor="white")
print("Mapa generado:", D.MAPA)
