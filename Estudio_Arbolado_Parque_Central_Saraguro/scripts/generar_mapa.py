# -*- coding: utf-8 -*-
"""Mapa profesional de ubicación de los árboles (Parque + Avenida).

Fondo, en orden de preferencia:
  1) assets/ortofoto.*  (imagen aérea provista por el usuario)
  2) Teselas de Esri World Imagery descargadas en vivo  (requiere red abierta)
  3) Esquema georreferenciado (sin imagen) — respaldo si no hay red

El método (2) es EXACTAMENTE el usado para el mapa del estudio de manglares:
funciona en cualquier entorno con acceso de red a server.arcgisonline.com.
En este entorno la política de red bloquea ese host (403), por lo que se usa (3).
"""
import os, math, io
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.image as mpimg
from matplotlib.patches import Rectangle
from matplotlib.lines import Line2D
from matplotlib.ticker import FuncFormatter
from matplotlib.patheffects import withStroke
from PIL import Image
import _data as D

COL = {"DERRIBO":"#E53935", "CONSERVAR-INT":"#FB8C00", "CONSERVAR":"#43A047"}
R = 6378137.0
CA = os.environ.get("REQUESTS_CA_BUNDLE") or "/root/.ccr/ca-bundle.crt"
ESRI = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"

pts = [t for t in D.trees if t["lat"] and t["lon"]]
lats = [float(t["lat"]) for t in pts]; lons = [float(t["lon"]) for t in pts]
mlat = sum(lats)/len(lats)
dx = max(lons)-min(lons); dy = max(lats)-min(lats)
padx = dx*0.15+0.00006; pady = dy*0.15+0.00006
xmin, xmax = min(lons)-padx, max(lons)+padx
ymin, ymax = min(lats)-pady, max(lats)+pady

def merc(lon, lat):
    return R*math.radians(lon), R*math.log(math.tan(math.pi/4+math.radians(lat)/2))
def deg2tile(lon, lat, z):
    n = 2**z
    xt = (lon+180.0)/360.0*n
    yt = (1.0-math.asinh(math.tan(math.radians(lat)))/math.pi)/2.0*n
    return xt, yt
def tile_extent_merc(xt, yt, z):
    n = 2**z; world = 2*math.pi*R
    left = -math.pi*R + xt*world/n
    top = math.pi*R - yt*world/n
    return left, top - world/n, left + world/n, top  # l,b,r,t

def fetch_esri(zoom):
    """Descarga y mosaica teselas Esri que cubren el bbox. Devuelve (PIL, extent_merc) o None."""
    import requests
    x0f, y0f = deg2tile(xmin, ymax, zoom)   # esquina NO
    x1f, y1f = deg2tile(xmax, ymin, zoom)   # esquina SE
    x0, x1 = int(math.floor(x0f)), int(math.floor(x1f))
    y0, y1 = int(math.floor(y0f)), int(math.floor(y1f))
    if (x1-x0+1)*(y1-y0+1) > 40:
        return None
    canvas = Image.new("RGB", ((x1-x0+1)*256, (y1-y0+1)*256))
    sess = requests.Session()
    for yt in range(y0, y1+1):
        for xt in range(x0, x1+1):
            url = ESRI.format(z=zoom, x=xt, y=yt)
            r = sess.get(url, timeout=15, verify=CA)   # una sola pasada; si 403 -> excepción -> respaldo
            r.raise_for_status()
            tile = Image.open(io.BytesIO(r.content)).convert("RGB")
            canvas.paste(tile, ((xt-x0)*256, (yt-y0)*256))
    l, _, _, t = tile_extent_merc(x0, y0, zoom)
    _, b, rr, _ = tile_extent_merc(x1, y1, zoom)
    return canvas, (l, b, rr, t)

# ---- resolver fondo ----
bg_img = None; bg_extent = None; use_merc = False; source = "esquema georreferenciado"
if D.ORTOFOTO:
    bg_img = mpimg.imread(D.ORTOFOTO)
    ext = D.ORTOFOTO_BOUNDS if D.ORTOFOTO_BOUNDS else [xmin, ymin, xmax, ymax]
    bg_extent = [ext[0], ext[2], ext[1], ext[3]]  # l,r,b,t en lon/lat
    source = "ortofoto (imagen aérea provista)"
else:
    try:
        got = fetch_esri(19)
        if got:
            canvas, (l, b, rr, t) = got
            bg_img = canvas; bg_extent = [l, rr, b, t]; use_merc = True
            source = "Esri World Imagery (teselas en vivo)"
            print("Ortofoto Esri descargada correctamente.")
    except Exception as e:
        print("No se pudieron descargar teselas Esri (se usa respaldo):", type(e).__name__, str(e)[:120])

# ---- figura ----
fig, ax = plt.subplots(figsize=(11, 8.5), dpi=200); fig.patch.set_facecolor("white")

def X(lon, lat):  # proyección de puntos según el fondo
    if use_merc:
        return merc(lon, lat)
    return lon, lat

if use_merc:
    xmn, ymn = merc(xmin, ymin); xmx, ymx = merc(xmax, ymax)
    ax.set_xlim(xmn, xmx); ax.set_ylim(ymn, ymx); ax.set_aspect("equal")
else:
    ax.set_xlim(xmin, xmax); ax.set_ylim(ymin, ymax); ax.set_aspect(1/math.cos(math.radians(mlat)))

if bg_img is not None:
    ax.imshow(bg_img, extent=bg_extent, origin="upper", zorder=0,
              aspect=("equal" if use_merc else 1/math.cos(math.radians(mlat))), interpolation="lanczos")
    grid_c, grid_a, halo, txt = "#FFFFFF", 0.35, "black", "white"
else:
    ax.set_facecolor("#F7FAF6"); grid_c, grid_a, halo, txt = "#B0BEC5", 0.8, "white", "#212121"

for cat in ["CONSERVAR","CONSERVAR-INT","DERRIBO"]:
    xy = [X(float(t["lon"]), float(t["lat"])) for t in pts if t["cat"]==cat]
    if xy:
        ax.scatter([p[0] for p in xy], [p[1] for p in xy], s=210, c=COL[cat],
                   edgecolors="white", linewidths=1.6, zorder=5, marker=("v" if cat=="DERRIBO" else "o"))
for t in pts:
    px, py = X(float(t["lon"]), float(t["lat"]))
    label = "%s\n(%s)" % (t["comun"], t["codigo"])
    ax.annotate(label, (px, py), xytext=(5,4), textcoords="offset points", fontsize=5.6,
                fontweight="bold", color=txt, zorder=6, ha="left", va="bottom",
                path_effects=[withStroke(linewidth=1.8, foreground=halo)])

# rótulos de zona
def zc(lon, lat): return X(lon, lat)
pcx, pcy = zc(min(lons)+(max(lons)-min(lons))*0.30, max(lats)+0.00004)
ax.annotate("PARQUE CENTRAL", (pcx, pcy), fontsize=11, fontweight="bold",
            color=("white" if bg_img is not None else "#1B5E20"), ha="center",
            path_effects=[withStroke(linewidth=2.4, foreground=("black" if bg_img is not None else "white"))])
av = [t for t in pts if t["src"]=="AV"]
if av:
    avx, avy = zc(sum(float(t["lon"]) for t in av)/len(av), max(float(t["lat"]) for t in av)+0.00003)
    ax.annotate("AVENIDA\nEL ORO", (avx, avy), fontsize=9, fontweight="bold",
                color=("white" if bg_img is not None else "#0D47A1"), ha="center",
                path_effects=[withStroke(linewidth=2.2, foreground=("black" if bg_img is not None else "white"))])

ax.grid(True, linestyle=":", linewidth=0.6, color=grid_c, alpha=grid_a, zorder=1)
ax.set_xlabel("Longitud (°)", fontsize=9); ax.set_ylabel("Latitud (°)", fontsize=9); ax.tick_params(labelsize=7.5)
if use_merc:
    ax.xaxis.set_major_formatter(FuncFormatter(lambda v,p: "%.4f" % math.degrees(v/R)))
    ax.yaxis.set_major_formatter(FuncFormatter(lambda v,p: "%.4f" % math.degrees(2*math.atan(math.exp(v/R))-math.pi/2)))
else:
    ax.ticklabel_format(useOffset=False, style="plain")
for s in ax.spines.values(): s.set_color("#607D8B")

# escala 20 m
xl0,xl1=ax.get_xlim(); yl0,yl1=ax.get_ylim(); gw=xl1-xl0; gh=yl1-yl0
if use_merc:
    seg = 20.0/math.cos(math.radians(mlat))    # metros en mercator
else:
    seg = 20.0/(111320*math.cos(math.radians(mlat)))
sx, sy = xl0+gw*0.05, yl0+gh*0.06
ax.add_patch(Rectangle((sx, sy), seg, gh*0.012, color="black", zorder=7, ec="white", lw=0.8))
ax.text(sx+seg/2, sy+gh*0.03, "20 m", fontsize=7.6, ha="center", color=txt, fontweight="bold",
        path_effects=[withStroke(linewidth=2, foreground=halo)])
# norte
nx, ny = xl1-gw*0.04, yl1-gh*0.16
ax.annotate("N", xy=(nx, ny+gh*0.10), xytext=(nx, ny),
            arrowprops=dict(facecolor=("white" if bg_img is not None else "#212121"), edgecolor="black", width=3.5, headwidth=11, headlength=11),
            ha="center", fontsize=12, fontweight="bold", color=txt, zorder=8, path_effects=[withStroke(linewidth=2.4, foreground=halo)])
# leyenda
leg=[Line2D([0],[0],marker="v",color="w",markerfacecolor=COL["DERRIBO"],markeredgecolor="white",markersize=12,label="Derribo (1)"),
     Line2D([0],[0],marker="o",color="w",markerfacecolor=COL["CONSERVAR-INT"],markeredgecolor="white",markersize=12,label="Conservar con intervención (%d)"%D.n_int),
     Line2D([0],[0],marker="o",color="w",markerfacecolor=COL["CONSERVAR"],markeredgecolor="white",markersize=12,label="Conservar (%d)"%D.n_cons)]
ax.legend(handles=leg, loc="lower right", fontsize=8, framealpha=0.95, title="Veredicto técnico", title_fontsize=8.5)
ax.set_title("Mapa de ubicación y veredicto del arbolado — Parque Central y Avenida El Oro, Saraguro\n"
             "%d árboles georreferenciados (GPS) · Datum WGS84 · Fondo: %s · Inventario ArboLEC–UNL, 2026" % (len(pts), source),
             fontsize=10.3, fontweight="bold", color="#1B5E20", pad=12)
plt.tight_layout(); plt.savefig(D.MAPA, dpi=200, bbox_inches="tight", facecolor="white")
print("Mapa generado:", D.MAPA, "| fondo:", source)
print("Recuadro WGS84 a cubrir por la imagen aérea:  SO(%.6f, %.6f)  NE(%.6f, %.6f)" % (ymin, xmin, ymax, xmax))
