# -*- coding: utf-8 -*-
"""Infografía científica 'Árbol tipo': ilustración botánica del árbol promedio con
semáforo del estado por componente (moda de los 40 árboles). Fondo blanco."""
import os, math, random, collections
import _data as D

OUT_SVG = os.path.join(os.path.dirname(__file__), "..", "assets", "infografia_arbol_promedio.svg")
random.seed(7)

# ---------- estado / colores (versión impresa, apagada) ----------
GREEN="#2E7D32"; AMBER="#C77D14"; RED="#C0392B"; GREY="#9E9E9E"
INK="#1e2b22"; SUB="#5c6b62"; RULE="#c9d4cc"
def status(v):
    v=(v or "").strip().lower()
    if v.startswith("buen"): return GREEN,"Buen estado"
    if v.startswith("regular"): return AMBER,"Estado regular"
    if v.startswith("mal"): return RED,"Mal estado"
    if "poco" in v: return AMBER,"Poco simétrica"
    if "simétr" in v or "simetr" in v: return GREEN,"Simétrica"
    if "irregular" in v: return RED,"Irregular"
    return GREY,"No visible"
def moda(c):
    cc=collections.Counter(t["fito"][c] for t in D.trees); val,n=cc.most_common(1)[0]
    return val,n,round(100*n/D.n)
Mo={c:moda(c) for c in ["Raíz","Fuste","Corteza","Ramas","Hojas","Cima","Copa"]}
fol=[float(t["follaje"]) for t in D.trees if t["follaje"]]
htv=[float(t["ht"]) for t in D.trees if t["ht"]]; dv=[float(t["d"]) for t in D.trees if t["d"]]
def topf(f):
    cc=collections.Counter(t[f] for t in D.trees); v,n=cc.most_common(1)[0]; return v,round(100*n/D.n)
mad=topf("madurez"); rec=topf("rectitud"); esp=topf("espacio")
con=sum(1 for t in D.trees if t["familia"]=="Cupressaceae"); lat=D.n-con
enf=list(D.enf_freq.items())[0]; pla=list(D.plaga_freq.items())[0]

W,H=1400,1930
cx=560                       # eje del árbol
GY=1240                      # nivel del suelo
S=[]
def esc(s): return s.replace("&","&amp;").replace("<","&lt;").replace(">","&gt;")

S.append(f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" font-family="Arial,Helvetica,sans-serif">')
S.append(f'''<defs>
 <linearGradient id="trunk" x1="0" y1="0" x2="1" y2="0">
  <stop offset="0" stop-color="#6b4a22"/><stop offset="0.28" stop-color="#946a30"/>
  <stop offset="0.5" stop-color="#c79a55"/><stop offset="0.72" stop-color="#946a30"/>
  <stop offset="1" stop-color="#5f4120"/></linearGradient>
 <radialGradient id="canopy" cx="0.60" cy="0.30" r="0.85">
  <stop offset="0" stop-color="#7cc47f"/><stop offset="0.5" stop-color="#4a9e50"/>
  <stop offset="1" stop-color="#1f5f27"/></radialGradient>
 <radialGradient id="soil" cx="0.5" cy="0.5" r="0.5">
  <stop offset="0" stop-color="#00000022"/><stop offset="1" stop-color="#00000000"/></radialGradient>
 <filter id="soft"><feGaussianBlur stdDeviation="1.1"/></filter>
</defs>''')
S.append(f'<rect width="{W}" height="{H}" fill="#ffffff"/>')

# ---- título académico ----
S.append(f'<text x="70" y="66" font-family="Georgia,\'Times New Roman\',serif" font-size="34" font-weight="700" fill="{INK}">Figura 1. Árbol tipo del arbolado urbano de Saraguro</text>')
S.append(f'<text x="70" y="100" font-family="Georgia,serif" font-size="20" fill="{SUB}">Estado sanitario por componente según el valor más frecuente (moda) de n = {D.n} árboles · Inventario ArboLEC–UNL, 2026</text>')
S.append(f'<line x1="70" y1="118" x2="{W-70}" y2="118" stroke="{RULE}" stroke-width="1.5"/>')

# ---- leyenda semáforo (arriba derecha) ----
lx,ly=W-470,150
S.append(f'<rect x="{lx-16}" y="{ly-24}" width="452" height="120" rx="10" fill="#ffffff" stroke="{RULE}"/>')
S.append(f'<text x="{lx}" y="{ly}" font-size="17" font-weight="700" fill="{INK}">Semáforo de estado (por componente)</text>')
leg=[(GREEN,"Buen estado"),(AMBER,"Estado regular"),(RED,"Mal estado"),(GREY,"No visible / no evaluable")]
for i,(c,l) in enumerate(leg):
    yy=ly+26+(i//2)*30; xx=lx+(i%2)*230
    S.append(f'<rect x="{xx}" y="{yy-13}" width="17" height="17" rx="3" fill="{c}"/><text x="{xx+26}" y="{yy}" font-size="16" fill="{INK}">{l}</text>')

# ================= ÁRBOL =================
# sombra en suelo
S.append(f'<ellipse cx="{cx}" cy="{GY+6}" rx="300" ry="34" fill="url(#soil)"/>')
S.append(f'<line x1="{cx-330}" y1="{GY}" x2="{cx+330}" y2="{GY}" stroke="#cfd8d0" stroke-width="2"/>')

# ---- raíces (moda No visible -> tenues, punteadas) ----
raiz_c,_=status(Mo["Raíz"][0])
S.append('<g opacity="0.42">')
def root(x0,y0,ang,ln,wd,depth):
    if depth==0 or ln<26: return
    x1=x0+math.cos(ang)*ln; y1=y0+math.sin(ang)*ln
    S.append(f'<path d="M {x0:.0f},{y0:.0f} Q {(x0+x1)/2+random.uniform(-8,8):.0f},{(y0+y1)/2:.0f} {x1:.0f},{y1:.0f}" stroke="{raiz_c}" stroke-width="{wd:.1f}" fill="none" stroke-linecap="round" stroke-dasharray="1,10"/>')
    for k in (-1,1):
        root(x1,y1,ang+k*random.uniform(0.25,0.5),ln*0.7,wd*0.65,depth-1)
for a in (2.25,2.6,math.pi/2,2.85,0.9):  # abanico hacia abajo
    root(cx,GY-4,a,92,14,3)
S.append('</g>')

# ---- ramas (moda Regular -> ámbar) detrás de la copa ----
ramas_c,_=status(Mo["Ramas"][0])
brtips=[]
def branch(x0,y0,ang,ln,wd,depth):
    if depth==0 or ln<24: return
    x1=x0+math.cos(ang)*ln; y1=y0+math.sin(ang)*ln
    S.append(f'<path d="M {x0:.0f},{y0:.0f} Q {(x0+x1)/2+random.uniform(-10,10):.0f},{(y0+y1)/2-14:.0f} {x1:.0f},{y1:.0f}" stroke="url(#trunk)" stroke-width="{wd:.1f}" fill="none" stroke-linecap="round"/>')
    if depth<=2: brtips.append((x1,y1))
    for k in (-1,1):
        branch(x1,y1,ang-math.pi/2*0 + (ang if False else ang)+k*random.uniform(0.34,0.6)-0, ln*0.72, wd*0.62, depth-1)
# tronco recto (moda Recto) con ramas simétricas
random.seed(11)
for (yb,spread) in [(792,0.62),(700,0.5)]:
    for k in (-1,1):
        branch(cx+k*10, yb, -math.pi/2 + k*spread, 150, 18, 3)
# rama central sube
branch(cx,760,-math.pi/2,120,20,3)

# ---- tronco (moda Fuste Regular -> ámbar) ----
fuste_c,_=status(Mo["Fuste"][0])
tw,bw=26,64
S.append(f'<path d="M {cx-tw},790 '
         f'C {cx-tw-6},980 {cx-bw+18},1130 {cx-bw},{GY} '
         f'Q {cx-bw-30},{GY+6} {cx-bw-46},{GY+10} '     # contrafuerte izq
         f'L {cx+bw+46},{GY+10} Q {cx+bw+30},{GY+6} {cx+bw},{GY} '
         f'C {cx+bw-18},1130 {cx+tw+6},980 {cx+tw},790 Z" '
         f'fill="url(#trunk)" stroke="#4f3617" stroke-width="2"/>')
# corteza (moda Buena -> textura + acento verde): estrías
corteza_c,_=status(Mo["Corteza"][0])
random.seed(3)
for i in range(9):
    off=random.uniform(-bw*0.7,bw*0.7)
    S.append(f'<path d="M {cx+off*0.4:.0f},812 C {cx+off:.0f},1000 {cx+off*1.15:.0f},1120 {cx+off*1.2:.0f},{GY-8}" stroke="#5f4120" stroke-width="{random.uniform(1.4,2.8):.1f}" fill="none" opacity="0.5"/>')

# ---- copa: forma intermedia (elipse latifoliada + espiga conífera), simétrica ----
cyc,rx,ry=620,250,285; apexY=318
# base con degradado
S.append(f'<ellipse cx="{cx}" cy="{cyc}" rx="{rx}" ry="{ry}" fill="url(#canopy)"/>')
S.append(f'<path d="M {cx},{apexY} C {cx+70},{apexY+70} {cx+95},{cyc-ry+40} {cx+60},{cyc-ry+70} L {cx-60},{cyc-ry+70} C {cx-95},{cyc-ry+40} {cx-70},{apexY+70} {cx},{apexY} Z" fill="url(#canopy)"/>')
# follaje texturizado (simétrico: se genera mitad derecha y se refleja)
def leafcolor(ny):  # ny: -1 arriba .. 1 abajo
    t=(ny+1)/2
    stops=[(120,188,126),(104,178,110),(82,164,88),(58,144,66),(42,118,50),(28,94,36)]
    idx=min(len(stops)-1,int(t*len(stops)))
    r,g,b=stops[idx]; j=random.randint(-8,8)
    return f'rgb({max(0,min(255,r+j))},{max(0,min(255,g+j))},{max(0,min(255,b+j))})'
random.seed(21)
blobs=[]
n=0; att=0
while n<115 and att<6000:
    att+=1
    x=random.uniform(cx,cx+rx); y=random.uniform(cyc-ry,cyc+ry)
    nx=(x-cx)/rx; ny=(y-cyc)/ry
    if nx*nx+ny*ny<=0.98:
        blobs.append((x,y)); n+=1
# espiga superior
for _ in range(20):
    y=random.uniform(apexY+6,cyc-ry+80); mh=70*((y-apexY)/(cyc-ry+80-apexY))
    blobs.append((random.uniform(cx,cx+max(6,mh)),y))
# dibujar: sombra (grande, oscuro) detrás; luego hoja
layer=""
for (x,y) in blobs:
    ny=(y-cyc)/ry
    for (dx,mult,op) in [(0,1.0,0.9)]:
        r=random.uniform(20,40)
        for sx in (x,2*cx-x):  # reflejo simetría
            layer+=f'<circle cx="{sx:.0f}" cy="{y:.0f}" r="{r:.0f}" fill="{leafcolor(ny)}" opacity="0.88"/>'
S.append(layer)
# realces (luz arriba-derecha) y sombra (abajo-izq)
random.seed(5); hl=""
for _ in range(14):
    x=random.uniform(cx+20,cx+rx*0.8); y=random.uniform(cyc-ry*0.8,cyc-20)
    if ((x-cx)/rx)**2+((y-cyc)/ry)**2<=0.75:
        hl+=f'<circle cx="{x:.0f}" cy="{y:.0f}" r="{random.uniform(9,18):.0f}" fill="#bfe0c0" opacity="0.15"/>'
for _ in range(22):
    x=random.uniform(cx-rx*0.8,cx-20); y=random.uniform(cyc,cyc+ry*0.8)
    if ((x-cx)/rx)**2+((y-cyc)/ry)**2<=0.85:
        hl+=f'<circle cx="{x:.0f}" cy="{y:.0f}" r="{random.uniform(12,26):.0f}" fill="#14400f" opacity="0.20"/>'
S.append(hl)

# ---- cima (moda Buena -> verde): ápice destacado ----
cima_c,_=status(Mo["Cima"][0])
S.append(f'<path d="M {cx},{apexY-30} L {cx-9},{apexY} L {cx+9},{apexY} Z" fill="#1f5f27"/>')
S.append(f'<circle cx="{cx}" cy="{apexY-2}" r="10" fill="{cima_c}" stroke="#fff" stroke-width="3"/>')

# ================= ANOTACIONES =================
def label(ax,ay,tx,ty,name,state_val,anchor="start"):
    col,lab=status(state_val[0]); pct=state_val[2]; cnt=state_val[1]
    S.append(f'<circle cx="{ax}" cy="{ay}" r="6" fill="{col}"/>')
    S.append(f'<path d="M {ax},{ay} L {tx},{ty}" stroke="#98a79d" stroke-width="1.6" fill="none"/>')
    ex = tx if anchor=="start" else tx
    le = 250
    x0 = tx if anchor=="start" else tx-le
    S.append(f'<line x1="{tx}" y1="{ty}" x2="{tx+(le if anchor=="start" else -le)}" y2="{ty}" stroke="#98a79d" stroke-width="1.6"/>')
    S.append(f'<rect x="{(tx if anchor=="start" else tx-le)}" y="{ty-30}" width="7" height="60" fill="{col}"/>')
    tx2 = tx+16 if anchor=="start" else tx-le+16
    an = "start"
    S.append(f'<text x="{tx2}" y="{ty-6}" text-anchor="{an}" font-size="24" font-weight="700" fill="{INK}">{esc(name)}</text>')
    S.append(f'<text x="{tx2}" y="{ty+20}" text-anchor="{an}" font-size="18" fill="{col}" font-weight="600">{esc(lab)} · {pct}% ({cnt}/{D.n})</text>')

# izquierda (anchor end): línea horizontal hacia la izquierda desde tx
def label_L(ax,ay,tx,ty,name,sv):
    col,lab=status(sv[0]); pct=sv[2]; cnt=sv[1]; le=252
    S.append(f'<circle cx="{ax}" cy="{ay}" r="6" fill="{col}"/>')
    S.append(f'<path d="M {ax},{ay} L {tx},{ty}" stroke="#98a79d" stroke-width="1.6"/>')
    S.append(f'<line x1="{tx}" y1="{ty}" x2="{tx-le}" y2="{ty}" stroke="#98a79d" stroke-width="1.6"/>')
    S.append(f'<rect x="{tx-le}" y="{ty-30}" width="7" height="60" fill="{col}"/>')
    S.append(f'<text x="{tx-le+16}" y="{ty-6}" font-size="24" font-weight="700" fill="{INK}">{esc(name)}</text>')
    S.append(f'<text x="{tx-le+16}" y="{ty+20}" font-size="18" fill="{col}" font-weight="600">{esc(lab)} · {pct}% ({cnt}/{D.n})</text>')
def label_R(ax,ay,tx,ty,name,sv):
    col,lab=status(sv[0]); pct=sv[2]; cnt=sv[1]; le=252
    S.append(f'<circle cx="{ax}" cy="{ay}" r="6" fill="{col}"/>')
    S.append(f'<path d="M {ax},{ay} L {tx},{ty}" stroke="#98a79d" stroke-width="1.6"/>')
    S.append(f'<line x1="{tx}" y1="{ty}" x2="{tx+le}" y2="{ty}" stroke="#98a79d" stroke-width="1.6"/>')
    S.append(f'<rect x="{tx}" y="{ty-30}" width="7" height="60" fill="{col}"/>')
    S.append(f'<text x="{tx+16}" y="{ty-6}" font-size="24" font-weight="700" fill="{INK}">{esc(name)}</text>')
    S.append(f'<text x="{tx+16}" y="{ty+20}" font-size="18" fill="{col}" font-weight="600">{esc(lab)} · {pct}% ({cnt}/{D.n})</text>')

# izquierda
label_L(cx-6,apexY-2, 300,360, "Cima", Mo["Cima"])
label_L(cx-rx*0.72,cyc-70, 300,560, "Copa", Mo["Copa"])
label_L(cx-rx*0.5,cyc+80, 300,720, "Hojas", Mo["Hojas"])
# derecha
label_R(cx+150,700, W-300,500, "Ramas", Mo["Ramas"])
label_R(cx+bw-6,1030, W-300,720, "Corteza", Mo["Corteza"])
label_R(cx+22,1140, W-300,940, "Fuste", Mo["Fuste"])
# raíz (abajo izq)
label_L(cx-70,GY+90, 300,1150, "Raíz", Mo["Raíz"])

# nota forma + follaje (recuadro discreto abajo-derecha del árbol)
nx,ny=W-300,1080
S.append(f'<text x="{nx-252+16}" y="{ny}" font-size="17" font-weight="700" fill="{INK}">Forma: intermedia</text>')
S.append(f'<text x="{nx-252+16}" y="{ny+24}" font-size="15" fill="{SUB}">{con} coníferas + {lat} latifoliadas</text>')
S.append(f'<text x="{nx-252+16}" y="{ny+46}" font-size="15" fill="{SUB}">ápice agudo + copa ancha</text>')

# ================= TABLA DE DATOS =================
ty0=1548
S.append(f'<text x="70" y="{ty0}" font-family="Georgia,serif" font-size="22" font-weight="700" fill="{INK}">Parámetros dendrométricos y estructurales del arbolado (n = {D.n})</text>')
S.append(f'<line x1="70" y1="{ty0+14}" x2="{W-70}" y2="{ty0+14}" stroke="{INK}" stroke-width="1.5"/>')
data=[("Altura total media",("%.1f m"%(sum(htv)/len(htv))).replace(".",","),"rango 2,7 – 27,1 m"),
      ("Diámetro medio (DAP)",f"{sum(dv)/len(dv):.0f} cm","rango 1 – 126 cm"),
      ("Follaje medio",f"{round(sum(fol)/len(fol))} %","densidad de copa"),
      ("Madurez dominante",f"{mad[0]}",f"{mad[1]} % de los individuos"),
      ("Porte del fuste (moda)",f"{rec[0]}",f"{rec[1]} %"),
      ("Espacio de crecimiento",f"{esp[0]}",f"{esp[1]} % (moda)"),
      ("Enfermedad más frecuente","Decoloración de hojas",f"{round(100*enf[1]/D.n)} % de árboles"),
      ("Agente más frecuente","Epífitas / líquenes",f"{round(100*pla[1]/D.n)} % de árboles"),
      ("Veredicto técnico global","1 derribo",f"{D.n_int} con intervención · {D.n_cons} conservar")]
cols=3; colw=(W-140)/cols; rowh=88
for i,(t,big,sub) in enumerate(data):
    c=i%cols; r=i//cols; x=70+c*colw; y=ty0+50+r*rowh
    S.append(f'<text x="{x}" y="{y}" font-size="15" font-weight="600" fill="{SUB}">{esc(t.upper())}</text>')
    S.append(f'<text x="{x}" y="{y+30}" font-size="26" font-weight="700" fill="{INK}">{esc(big)}</text>')
    S.append(f'<text x="{x}" y="{y+52}" font-size="14.5" fill="{SUB}">{esc(sub)}</text>')
    if c<cols-1: S.append(f'<line x1="{x+colw-24}" y1="{y-22}" x2="{x+colw-24}" y2="{y+58}" stroke="{RULE}" stroke-width="1"/>')
    if r>0 and c==0: S.append(f'<line x1="70" y1="{y-42}" x2="{W-70}" y2="{y-42}" stroke="{RULE}" stroke-width="1"/>')

S.append(f'<text x="70" y="{H-26}" font-size="14.5" fill="{SUB}">Cada componente del árbol se representa con el estado más frecuente (moda) entre los {D.n} árboles inventariados; la raíz se muestra atenuada por no ser evaluable a simple vista (70 %).</text>')
S.append('</svg>')

open(OUT_SVG,"w",encoding="utf-8").write("\n".join(S))
print("SVG:",OUT_SVG)
for c in Mo: print(c,Mo[c],"->",status(Mo[c][0])[1])
