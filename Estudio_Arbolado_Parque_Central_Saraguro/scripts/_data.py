# -*- coding: utf-8 -*-
"""Módulo de datos compartido: carga el inventario del Parque y de la Avenida,
asigna veredictos, URLs y estadísticos. Usado por los generadores docx y pdf."""
import os, openpyxl

ASSETS = os.path.join(os.path.dirname(__file__), "..", "assets")
XLSX_PARQUE = os.path.join(ASSETS, "Reporte_Arboles_Parque.xlsx")
XLSX_AV = os.path.join(ASSETS, "Reporte_Arboles_Avenida.xlsx")
LOGO_UNL = os.path.join(ASSETS, "LOGO_UNL.png")
LOGO_ARBOLEC = os.path.join(ASSETS, "ArbolEC.jpeg")
MAPA = os.path.join(ASSETS, "mapa_arboles.png")

# Patrón de URL por árbol en la plataforma ArboLEC (confirmar/ajustar si difiere)
BASE_URL = "https://arbolec.unl.edu.ec/ec/Saraguro"
def tree_url(codigo):
    return "%s/%s" % (BASE_URL, codigo)

def _load(path, prefix, sitio_label):
    wb = openpyxl.load_workbook(path, data_only=True)
    ws = wb["Reporte"]
    headers = {c: ws.cell(row=1, column=c).value for c in range(1, ws.max_column+1)}
    def col(name):
        for c, h in headers.items():
            if h == name: return c
    out = []
    idx = 0
    for r in range(2, ws.max_row+1):
        if ws.cell(row=r, column=col("Código")).value is None:
            continue
        idx += 1
        def g(name):
            c = col(name); return ws.cell(row=r, column=c).value if c else None
        def rec(name):
            c = col(name); v = ws.cell(row=r, column=c).value if c else None
            return bool(v and str(v).strip())
        out.append(dict(
            src=prefix, row=r, id="%s%02d" % (prefix, idx), sitio=sitio_label,
            codigo=g("Código"), especie=g("Especie"), familia=g("Familia"), comun=g("Nombre Común"),
            ref=(str(g("Comentario")) if g("Comentario") else ""),
            lon=g("Longitud"), lat=g("Latitud"), cf=g("Circunferencia de fuste"), d=g("Diámetro"),
            ht=g("Altura total"), hc=g("Altura comercial"),
            copaNS=g("Tam. copa (NS)"), copaEW=g("Tam. copa (EW)"), follaje=g("Follaje"),
            madurez=g("Estado de maduréz"), rectitud=g("Rectitud de fuste"), espacio=g("Espacio de crecimiento"),
            riesgo=g("Riesgo total"), valriesgo=g("Valor Riesgo total"),
            raiz=g("Raíz"), fuste_est=g("Fuste"), caida=g("Caida"),
            afec_constr=g("Afectación a construcciones"), afec_circ=g("Inteferencia con la circulación"),
            afec_raiz=g("Daño a infraestructura por raíces"),
            rec_poda=rec("Poda"), rec_derribo=rec("Derribo")))
    return out

parque = _load(XLSX_PARQUE, "A", "Parque Central")
avenida = _load(XLSX_AV, "AV", "Avenida El Oro (isleta)")
trees = parque + avenida

# ---------------- Veredictos ----------------
# Parque (clave = fila del Excel del parque)
VER_PARQUE = {
2:("CONSERVAR-INT","Riesgo total alto asociado a fuste inclinado y daños mecánicos (heridas, ramas quebradas, poda inadecuada), no a inestabilidad de anclaje; caída Media. Ejemplar pequeño (D=6,3 cm; 3,2 m). Poda de reequilibrio, tratamiento de heridas y manejo de epífitas."),
3:("CONSERVAR","Buen estado estructural (fuste, corteza y hojas buenos); riesgo y caída bajos. Retirar cuerdas del fuste y poda ligera de formación."),
4:("CONSERVAR-INT","Riesgo medio por fuste torcido, copa irregular y cima mala; caída Media. Especie nativa de valor ornamental. Poda de saneamiento y formación; tratar el descortezamiento."),
5:("CONSERVAR-INT","Condición fitosanitaria deficiente (hongos, exudados, pústulas, pudrición de ramas, ápice muerto), pero riesgo de CAÍDA BAJO por porte moderado (9,1 m) y raíz buena. Poda sanitaria, tratamiento fungicida, manejo de epífitas y monitoreo."),
6:("CONSERVAR-INT","Riesgo alto por afección foliar y de cima (hojas y cima malas, ápice muerto) y fuste inclinado; caída Media. Árbol joven-pequeño (D=19,7 cm; 7,4 m), recuperable. Poda sanitaria, fertilización y tratamiento; monitorear evolución."),
7:("DERRIBO","Único ejemplar con riesgo de CAÍDA ALTO y recomendación de derribo en campo. Es el árbol de mayor porte del parque (D=118,2 cm; 27,1 m), con copa amplia y poco simétrica —gran brazo de palanca frente a los fuertes vientos de la zona— exudados de resina (indicador de estrés/patología del fuste) y ubicación junto a los baños (máxima concurrencia de personas). La probabilidad de fallo combinada con la alta exposición vuelve inaceptable el riesgo. Se recomienda DERRIBO técnico controlado, previa verificación instrumental (resistógrafo/tomografía) si se dispone del equipo."),
8:("CONSERVAR-INT","Caída y riesgo Medios, pero RAÍZ en MAL estado en un árbol alto (17,8 m): punto crítico de anclaje. Poda de aligeramiento/reducción de la vela para disminuir la carga de viento e inspección radicular; si se confirma pérdida de anclaje, reevaluar derribo. Monitoreo PRIORITARIO."),
9:("CONSERVAR","Estructura y anclaje buenos; riesgo y caída bajos. Retiro de clavos/alambres y mantenimiento."),
10:("CONSERVAR","Ejemplar alto (20,7 m) en buen estado general, raíz buena y caída baja. Mantenimiento menor: retiro de clavos/alambres y manejo de epífitas."),
11:("CONSERVAR-INT","Mayor VALOR de riesgo del inventario del parque (2,97), pero originado por daño a infraestructura por raíces (Alto) e inclinación en espacio estrecho, NO por deterioro estructural: fuste, ramas, hojas y cima BUENOS. Especie NATIVA (nogal andino). Poda de reequilibrio y manejo de raíz (barreras / reparación de adoquín); no procede derribo."),
12:("CONSERVAR","Buen estado; riesgo y caída bajos. Poda menor y tratamiento de heridas."),
13:("CONSERVAR-INT","Buen anclaje (raíz buena) y caída baja. Poda ligera, manejo de epífitas y retiro de objetos extraños."),
14:("CONSERVAR","Palma en buen estado; riesgo mínimo. Mantenimiento (retiro de hojas secas)."),
15:("CONSERVAR-INT","Riesgo alto por fuste MUY inclinado en espacio estrecho y daño de raíz a infraestructura (Alto); caída Media. Poda de reequilibrio de copa para reducir carga en el lado inclinado y VIGILANCIA de la inclinación; si progresa, reevaluar."),
16:("CONSERVAR","Ejemplar joven en buen estado; riesgo mínimo. Mantenimiento."),
17:("CONSERVAR","Buen estado general; riesgo mínimo. Mantenimiento."),
18:("CONSERVAR","Riesgo bajo pese a fuste torcido; caída baja. Poda de formación y tratamiento de heridas."),
19:("CONSERVAR-INT","Riesgo bajo; cima mala y presencia de hongos. Poda sanitaria y manejo."),
20:("CONSERVAR","Riesgo bajo, buen anclaje. Poda menor y manejo de epífitas."),
21:("CONSERVAR-INT","Especie NATIVA (molle). Riesgo alto por fuste MUY inclinado; caída Media, estructura buena (fuste y corteza buenos). Poda de reequilibrio y VIGILANCIA de la inclinación."),
22:("CONSERVAR-INT","Riesgo alto por inclinación y copa irregular; caída Media, fuste bueno. Poda de reequilibrio y de saneamiento."),
23:("CONSERVAR","Palma en buen estado; riesgo mínimo. Mantenimiento."),
24:("CONSERVAR-INT","Fuste bueno y raíz buena → riesgo de CAÍDA no crítico (Media); el problema es la afectación a construcciones, circulación y raíces (Alto) por su ubicación en esquina/espacio estrecho con raíces descubiertas. Corresponde manejo de raíz (barreras, reparación de adoquín) y poda, NO derribo. Caso típico de daño al ornato con árbol estructuralmente sano."),
25:("CONSERVAR-INT","Riesgo alto por fuste muy inclinado; caída Media, ejemplar pequeño (D=7,6 cm). Poda de reequilibrio y tratamiento de heridas/descortezamiento."),
}
# Avenida El Oro (clave = fila del Excel de la avenida)
VER_AV = {
2:("CONSERVAR-INT","Riesgo alto por fuste inclinado en espacio estrecho, tumores en el fuste e interferencia con redes eléctricas; caída Media. Estructura general aceptable (cima y hojas buenas). Poda de reequilibrio y aclareo, tratamiento de daños mecánicos y manejo de epífitas; vigilancia de los tumores."),
3:("CONSERVAR-INT","Especie NATIVA (molle). Presenta PUDRICIÓN DEL TRONCO, fuste y corteza en mal estado y tumores: defectos estructurales que, pese a una caída calificada Media, obligan a una EVALUACIÓN INSTRUMENTAL del fuste (resistógrafo/tomografía). Poda de descarga; si se confirma pérdida de resistencia del tronco, reevaluar derribo. Es el ejemplar de la avenida que requiere mayor atención."),
4:("CONSERVAR-INT","Riesgo medio por fuste torcido en espacio estrecho y daño de raíz a infraestructura (Alto); caída Media, fuste bueno. Manejo de raíz (barreras / reparación), poda de reequilibrio y tratamiento del descortezamiento."),
5:("CONSERVAR-INT","Árbol VIEJO y de gran copa (15,8 m) con raíz, fuste y corteza en mal estado, tumores, exudados y raíces descubiertas; caída Media pero con múltiples indicadores de senescencia. Poda de reducción para disminuir la carga de viento, inspección radicular y del fuste, y coordinación con la empresa eléctrica por la alta interferencia con redes. Reevaluar derribo si progresa el deterioro estructural. Monitoreo PRIORITARIO."),
}
for t in trees:
    ver = VER_PARQUE if t["src"] == "A" else VER_AV
    t["cat"], t["fund"] = ver[t["row"]]
    t["url"] = tree_url(t["codigo"])

# ---------------- Estadísticos ----------------
CAT_LABEL = {"DERRIBO":"Derribo","CONSERVAR-INT":"Conservar con intervención","CONSERVAR":"Conservar"}
n = len(trees); n_parque = len(parque); n_av = len(avenida)
species = {}; families = {}
for t in trees:
    species[t["especie"]] = species.get(t["especie"], 0) + 1
    families[t["familia"]] = families.get(t["familia"], 0) + 1
risk_counts = {"Alto":0,"Medio":0,"Bajo":0}; caida_counts = {"Alto":0,"Medio":0,"Bajo":0}
for t in trees:
    risk_counts[t["riesgo"]] = risk_counts.get(t["riesgo"],0)+1
    caida_counts[t["caida"]] = caida_counts.get(t["caida"],0)+1
cupres = [t for t in trees if t["familia"] == "Cupressaceae"]
n_derribo = sum(1 for t in trees if t["cat"]=="DERRIBO")
n_int = sum(1 for t in trees if t["cat"]=="CONSERVAR-INT")
n_cons = sum(1 for t in trees if t["cat"]=="CONSERVAR")
comun_by = {}; fam_by = {}
for t in trees:
    comun_by.setdefault(t["especie"], t["comun"]); fam_by.setdefault(t["especie"], t["familia"])

def fnum(v, dec=1):
    if v is None: return "-"
    try: return ("%.*f" % (dec, float(v))).replace(".", ",")
    except: return str(v)

# árbol de derribo (para referencias)
DERRIBO_TREE = next(t for t in trees if t["cat"]=="DERRIBO")

if __name__ == "__main__":
    print("Total:", n, "| Parque:", n_parque, "| Avenida:", n_av)
    print("Especies:", len(species), "| Familias:", len(families), "| Cipreses:", len(cupres))
    print("Riesgo:", risk_counts, "| Caída:", caida_counts)
    print("Derribo:", n_derribo, "| Conservar-int:", n_int, "| Conservar:", n_cons)
    print("Derribo tree:", DERRIBO_TREE["id"], DERRIBO_TREE["codigo"])
