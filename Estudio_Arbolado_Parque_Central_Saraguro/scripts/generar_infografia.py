# -*- coding: utf-8 -*-
"""Infografía 'El árbol promedio': un árbol raíz→cima con semáforo de estado
según la MODA de los 40 árboles inventariados."""
import os, collections
import _data as D

OUT_SVG = os.path.join(os.path.dirname(__file__), "..", "assets", "infografia_arbol_promedio.svg")

# ---------- Colores ----------
GREEN="#2E7D32"; GREEN_L="#7CB342"; AMBER="#F39C12"; AMBER_D="#E08E0B"
RED="#E53935"; GREY="#9E9E9E"; INK="#243B2E"; PANEL="#0E3B2E"
SKY0="#EAF5EC"; SKY1="#F7FBF6"; SOIL="#8D6E63"

def state(v):
    v=(v or "").strip().lower()
    if v.startswith(("buen",)): return GREEN,"Buen estado"
    if v.startswith("regular"): return AMBER,"Estado regular"
    if v.startswith("mal"): return RED,"Mal estado"
    if "poco" in v: return AMBER,"Poco simétrica"
    if "simétr" in v or "simetr" in v: return GREEN,"Simétrica"
    if "irregular" in v: return RED,"Irregular"
    return GREY,"No visible"

def moda(comp):
    c=collections.Counter(t["fito"][comp] for t in D.trees)
    val,cnt=c.most_common(1)[0]
    return val,cnt,round(100*cnt/D.n)

M={c:moda(c) for c in ["Raíz","Fuste","Corteza","Ramas","Hojas","Cima","Copa"]}
# stats puntuales
fol=[float(t["follaje"]) for t in D.trees if t["follaje"]]
htv=[float(t["ht"]) for t in D.trees if t["ht"]]; dv=[float(t["d"]) for t in D.trees if t["d"]]
def top(field):
    c=collections.Counter(t[field] for t in D.trees); v,n=c.most_common(1)[0]; return v,round(100*n/D.n)
mad=top("madurez"); rec=top("rectitud"); esp=top("espacio")
con=sum(1 for t in D.trees if t["familia"]=="Cupressaceae"); lat=D.n-con
enf=list(D.enf_freq.items())[0]; pla=list(D.plaga_freq.items())[0]

W,H=1240,1620
cx=610

# ---------- helpers ----------
def esc(s): return s.replace("&","&amp;").replace("<","&lt;").replace(">","&gt;")
def chip(x,y,titulo,val,pct,color,w=286,h=104,align="left"):
    """Tarjeta de elemento."""
    fill,lab=color
    tx=x+22
    return f'''
  <g>
    <rect x="{x}" y="{y}" rx="16" ry="16" width="{w}" height="{h}" fill="#ffffff" stroke="{fill}" stroke-width="2.5" filter="url(#sh)"/>
    <rect x="{x}" y="{y}" rx="16" ry="16" width="12" height="{h}" fill="{fill}"/>
    <circle cx="{x+40}" cy="{y+34}" r="13" fill="{fill}"/>
    <text x="{x+64}" y="{y+40}" font-family="Poppins,Segoe UI,Arial" font-size="26" font-weight="700" fill="{INK}">{esc(titulo)}</text>
    <text x="{x+22}" y="{y+72}" font-family="Poppins,Segoe UI,Arial" font-size="21" font-weight="600" fill="{fill}">{esc(lab)}</text>
    <text x="{x+w-20}" y="{y+80}" text-anchor="end" font-family="Poppins,Segoe UI,Arial" font-size="34" font-weight="800" fill="{INK}">{pct}%</text>
    <text x="{x+22}" y="{y+95}" font-family="Poppins,Segoe UI,Arial" font-size="15.5" fill="#5b6b62">{esc(val)}</text>
  </g>'''

def leader(x1,y1,x2,y2,color):
    return f'<path d="M {x1},{y1} L {x2},{y2}" stroke="{color}" stroke-width="2.5" fill="none" stroke-dasharray="2,6" stroke-linecap="round"/>' \
           f'<circle cx="{x2}" cy="{y2}" r="6" fill="{color}"/>'

# ---------- copa (forma intermedia conífera/latifoliada, simétrica) ----------
apex=(cx,300)
# path simétrico: ápice agudo (conífera) + copa ancha y redondeada (latifoliada)
path=(f"M {cx},300 "
      f"C {cx+80},330 {cx+150},400 {cx+160},485 "
      f"C {cx+170},575 {cx+145},662 {cx+90},714 "
      f"C {cx+55},746 {cx+30},766 {cx},792 "
      f"C {cx-30},766 {cx-55},746 {cx-90},714 "
      f"C {cx-145},662 {cx-170},575 {cx-160},485 "
      f"C {cx-150},400 {cx-80},330 {cx},300 Z")

copa_fill,copa_lab=state(M["Copa"][0])
hojas_fill,_=state(M["Hojas"][0])
cima_fill,_=state(M["Cima"][0])
fuste_fill,_=state(M["Fuste"][0])
corteza_fill,_=state(M["Corteza"][0])
ramas_fill,_=state(M["Ramas"][0])
raiz_fill,_=state(M["Raíz"][0])

# ---------- SVG ----------
svg=[]
svg.append(f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" font-family="Poppins,Segoe UI,Arial">')
svg.append(f'''<defs>
 <linearGradient id="bg" x1="0" y1="0" x2="0" y2="1">
   <stop offset="0" stop-color="{SKY0}"/><stop offset="1" stop-color="{SKY1}"/></linearGradient>
 <radialGradient id="crown" cx="0.42" cy="0.34" r="0.85">
   <stop offset="0" stop-color="{GREEN_L}"/><stop offset="0.7" stop-color="{hojas_fill}"/>
   <stop offset="1" stop-color="#1B5E20"/></radialGradient>
 <linearGradient id="trunk" x1="0" y1="0" x2="1" y2="0">
   <stop offset="0" stop-color="{AMBER_D}"/><stop offset="0.5" stop-color="{fuste_fill}"/>
   <stop offset="1" stop-color="{AMBER_D}"/></linearGradient>
 <filter id="sh" x="-20%" y="-20%" width="140%" height="140%">
   <feDropShadow dx="0" dy="3" stdDeviation="4" flood-color="#204030" flood-opacity="0.18"/></filter>
</defs>''')
svg.append(f'<rect width="{W}" height="{H}" fill="url(#bg)"/>')

# encabezado
svg.append(f'<text x="{W/2}" y="70" text-anchor="middle" font-size="42" font-weight="800" fill="{PANEL}">EL ÁRBOL PROMEDIO DE SARAGURO</text>')
svg.append(f'<text x="{W/2}" y="108" text-anchor="middle" font-size="22" font-weight="600" fill="#3f6b52">Estado del arbolado urbano según la MODA de {D.n} árboles · Parque Central y Avenida El Oro · ArboLEC–UNL 2026</text>')

# leyenda semáforo
lx=W/2-360; ly=140
items=[(GREEN,"Buen estado"),(AMBER,"Estado regular"),(RED,"Mal estado"),(GREY,"No visible")]
svg.append(f'<rect x="{lx-20}" y="{ly-26}" rx="20" width="760" height="46" fill="#ffffff" stroke="#d6e4da" filter="url(#sh)"/>')
step=185
for i,(c,l) in enumerate(items):
    xx=lx+i*step
    svg.append(f'<circle cx="{xx}" cy="{ly-3}" r="11" fill="{c}"/><text x="{xx+20}" y="{ly+5}" font-size="20" font-weight="600" fill="{INK}">{l}</text>')

# ---- ESCENA ÁRBOL ----
groundY=1200
# suelo
svg.append(f'<ellipse cx="{cx}" cy="{groundY}" rx="330" ry="26" fill="#dfeadf"/>')
svg.append(f'<line x1="{cx-360}" y1="{groundY}" x2="{cx+360}" y2="{groundY}" stroke="#c2d3c4" stroke-width="3"/>')

# raíces (moda: No visible -> translúcidas y punteadas)
roots=[(-150,1360),(-70,1392),(10,1400),(90,1388),(168,1352)]
rp=f'<g opacity="0.5">'
for dx,ey in roots:
    rp+=f'<path d="M {cx-26},{groundY-6} Q {cx+dx*0.4},{ (groundY+ey)/2 } {cx+dx},{ey}" stroke="{raiz_fill}" stroke-width="16" fill="none" stroke-linecap="round" stroke-dasharray="3,12"/>'
rp+='</g>'
svg.append(rp)

# tronco (fuste ámbar) + corteza (contorno/estrías verdes)
tx=22; bx=52
svg.append(f'<path d="M {cx-tx},705 L {cx-bx},{groundY} L {cx+bx},{groundY} L {cx+tx},705 Z" fill="url(#trunk)" stroke="{corteza_fill}" stroke-width="5"/>')
for off in (-26,-6,16,34):
    svg.append(f'<path d="M {cx+off*0.5},720 C {cx+off},900 {cx+off*1.2},1050 {cx+off*1.3},{groundY-10}" stroke="{corteza_fill}" stroke-width="2.2" fill="none" opacity="0.55"/>')

# ramas (ámbar) — dos ramas simétricas que salen del fuste y entran en la copa
svg.append(f'<path d="M {cx-28},852 C {cx-70,816} {cx-105,776} {cx-120,742}" stroke="{ramas_fill}" stroke-width="14" fill="none" stroke-linecap="round"/>')
svg.append(f'<path d="M {cx+28},852 C {cx+70,816} {cx+105,776} {cx+120,742}" stroke="{ramas_fill}" stroke-width="14" fill="none" stroke-linecap="round"/>')

# copa (relleno hojas, contorno = estado copa)
svg.append(f'<path d="{path}" fill="url(#crown)" stroke="{copa_fill}" stroke-width="6"/>')
# textura hojas (puntos claros)
import math
dots=""
for i in range(46):
    a=(i*137.5)*math.pi/180; r=40+ (i%7)*24
    px=cx+math.cos(a)*r*1.5; py=540+math.sin(a)*r*1.3
    if 320<py<760: dots+=f'<circle cx="{px:.0f}" cy="{py:.0f}" r="5.5" fill="#ffffff" opacity="0.12"/>'
svg.append(dots)

# cima (ápice) marcador
svg.append(f'<circle cx="{apex[0]}" cy="{apex[1]}" r="15" fill="{cima_fill}" stroke="#fff" stroke-width="4"/>')
svg.append(f'<path d="M {apex[0]},{apex[1]-40} l 7,20 -14,0 z" fill="{cima_fill}"/>')

# ---- CALLOUTS ----
def M3(c): v,cnt,pct=M[c]; return v,cnt,pct
# izquierda
c="Cima"; v,cnt,pct=M3(c)
svg.append(leader(320,262,apex[0]-14,apex[1],cima_fill)); svg.append(chip(20,214,"Cima",f"{cnt}/{D.n} árboles · punta de copa",pct,state(v)))
c="Copa"; v,cnt,pct=M3(c)
svg.append(leader(320,470,cx-150,470,copa_fill)); svg.append(chip(20,420,"Copa",f"{cnt}/{D.n} · forma dominante",pct,state(v)))
c="Hojas"; v,cnt,pct=M3(c)
svg.append(leader(320,634,cx-70,590,hojas_fill)); svg.append(chip(20,590,"Hojas",f"{cnt}/{D.n} · follaje medio {round(sum(fol)/len(fol))}%",pct,state(v)))
# derecha
c="Ramas"; v,cnt,pct=M3(c)
svg.append(leader(W-320,470,cx+96,800,ramas_fill)); svg.append(chip(W-306,420,"Ramas",f"{cnt}/{D.n} árboles",pct,state(v)))
c="Corteza"; v,cnt,pct=M3(c)
svg.append(leader(W-320,770,cx+bx-6,900,corteza_fill)); svg.append(chip(W-306,720,"Corteza",f"{cnt}/{D.n} árboles",pct,state(v)))
c="Fuste"; v,cnt,pct=M3(c)
svg.append(leader(W-320,980,cx+30,1010,fuste_fill)); svg.append(chip(W-306,930,"Fuste",f"{cnt}/{D.n} · tronco principal",pct,state(v)))
# raíz (abajo izquierda)
c="Raíz"; v,cnt,pct=M3(c)
svg.append(leader(320,1300,cx-70,1360,raiz_fill)); svg.append(chip(20,1250,"Raíz",f"{cnt}/{D.n} · no evaluable a simple vista",pct,state(v)))

# nota forma intermedia
svg.append(f'<g><rect x="{W-306}" y="1170" rx="14" width="286" height="150" fill="#ffffff" stroke="#d6e4da" filter="url(#sh)"/>'
           f'<text x="{W-288}" y="1205" font-size="22" font-weight="700" fill="{PANEL}">Forma intermedia</text>'
           f'<text x="{W-288}" y="1236" font-size="16.5" fill="#3f6b52">Conífera + latifoliada:</text>'
           f'<text x="{W-288}" y="1262" font-size="16.5" fill="{INK}">{con} coníferas (ápice agudo)</text>'
           f'<text x="{W-288}" y="1288" font-size="16.5" fill="{INK}">{lat} latifoliadas (copa ancha)</text>'
           f'<text x="{W-288}" y="1312" font-size="14.5" fill="#6b7d72">→ silueta combinada</text></g>')

# ---- PANEL DATOS PUNTUALES ----
py=1360
svg.append(f'<rect x="30" y="{py}" rx="22" width="{W-60}" height="228" fill="{PANEL}"/>')
svg.append(f'<text x="60" y="{py+44}" font-size="24" font-weight="800" fill="#EAF5EC">DATOS PUNTUALES DEL ARBOLADO (n={D.n})</text>')
tiles=[("Altura media",f"{sum(htv)/len(htv):.1f} m","rango 2,7–27,1"),
       ("Diámetro medio",f"{sum(dv)/len(dv):.0f} cm","rango 1–126"),
       ("Follaje medio",f"{round(sum(fol)/len(fol))}%","densidad de copa"),
       ("Madurez",f"{mad[0]}",f"{mad[1]}% de los árboles"),
       ("Porte del fuste",f"{rec[0]}",f"{rec[1]}% (moda)"),
       ("Espacio",f"{esp[0]}",f"{esp[1]}% (moda)"),
       ("Afección + común","Decoloración",f"de hojas · {round(100*enf[1]/D.n)}%"),
       ("Agente + común","Epífitas",f"{round(100*pla[1]/D.n)}% (líquenes)"),
       ("Veredicto global","1 derribo",f"{D.n_int} interv. · {D.n_cons} conservar")]
cols=5; tw=(W-60-40)/cols; ty=py+70
for i,(t,big,sub) in enumerate(tiles):
    col=i%cols; rowi=i//cols
    x=60+col*tw; yy=ty+rowi*78
    svg.append(f'<text x="{x}" y="{yy}" font-size="15.5" font-weight="600" fill="#9cccb0">{esc(t)}</text>')
    svg.append(f'<text x="{x}" y="{yy+30}" font-size="27" font-weight="800" fill="#ffffff">{esc(big)}</text>')
    svg.append(f'<text x="{x}" y="{yy+50}" font-size="13.5" fill="#7fae94">{esc(sub)}</text>')

svg.append(f'<text x="{W/2}" y="{H-14}" text-anchor="middle" font-size="15" fill="#6b7d72">Cada elemento refleja el valor más frecuente (moda) entre los {D.n} árboles inventariados. Semáforo: verde = bueno · ámbar = regular · rojo = malo · gris = no visible.</text>')
svg.append('</svg>')

open(OUT_SVG,"w",encoding="utf-8").write("\n".join(svg))
print("SVG:",OUT_SVG)
for c in M: print(c, M[c])
