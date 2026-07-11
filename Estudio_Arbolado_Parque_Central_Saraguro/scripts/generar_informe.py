# -*- coding: utf-8 -*-
"""Genera el Informe Técnico del Arbolado Urbano del Parque Central de Saraguro
y el Oficio de respuesta (formato Word)."""
import openpyxl
from docx import Document
from docx.shared import Pt, Cm, RGBColor
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.enum.table import WD_TABLE_ALIGNMENT
from docx.enum.section import WD_ORIENT
from docx.oxml.ns import qn
from docx.oxml import OxmlElement

XLSX = "/root/.claude/uploads/4220178d-38bf-5cec-aaf2-15ce148fe2a2/6d02b559-Reporte__rboles_20260711__GAD_de_Saraguro.xlsx"
OUT_DOCX = "/home/user/testingDP/Informe_Tecnico_Arbolado_Parque_Central_Saraguro.docx"

# ---------- Colores institucionales ----------
VERDE = RGBColor(0x1B, 0x5E, 0x20)
VERDE_CLARO = "C8E6C9"
GRIS = RGBColor(0x42, 0x42, 0x42)
ROJO = RGBColor(0xB7, 0x1C, 0x1C)
NARANJA = RGBColor(0xE6, 0x5A, 0x00)
AZUL = RGBColor(0x0D, 0x47, 0xA1)
HDR_FILL = "1B5E20"
ROJO_FILL = "F4C7C3"
NARANJA_FILL = "FCE5CD"
VERDE_FILL = "D9EAD3"

# ---------- Cargar datos ----------
wb = openpyxl.load_workbook(XLSX, data_only=True)
ws = wb["Reporte"]
headers = {c: ws.cell(row=1, column=c).value for c in range(1, ws.max_column + 1)}
# indice por nombre de encabezado (primera aparicion)
def col(name):
    for c, h in headers.items():
        if h == name:
            return c
    return None

trees = []
for r in range(2, ws.max_row + 1):
    def g(name):
        c = col(name)
        return ws.cell(row=r, column=c).value if c else None
    # Recomendaciones se detectan por celda no vacia en su columna
    def rec(name):
        c = col(name)
        v = ws.cell(row=r, column=c).value if c else None
        return bool(v and str(v).strip())
    trees.append({
        "row": r,
        "id": "A%02d" % (r - 1),
        "codigo": g("Código"),
        "especie": g("Especie"),
        "familia": g("Familia"),
        "comun": g("Nombre Común"),
        "ref": (str(g("Comentario")) if g("Comentario") else ""),
        "lon": g("Longitud"), "lat": g("Latitud"),
        "cf": g("Circunferencia de fuste"), "d": g("Diámetro"),
        "ht": g("Altura total"), "hc": g("Altura comercial"),
        "copaNS": g("Tam. copa (NS)"), "copaEW": g("Tam. copa (EW)"),
        "follaje": g("Follaje"),
        "madurez": g("Estado de maduréz"),
        "rectitud": g("Rectitud de fuste"),
        "espacio": g("Espacio de crecimiento"),
        "riesgo": g("Riesgo total"), "valriesgo": g("Valor Riesgo total"),
        "raiz": g("Raíz"), "fuste_est": g("Fuste"),
        "caida": g("Caida"),
        "afec_constr": g("Afectación a construcciones"),
        "afec_circ": g("Inteferencia con la circulación"),
        "afec_raiz": g("Daño a infraestructura por raíces"),
        "rec_poda": rec("Poda"),
        "rec_derribo": rec("Derribo"),
        "rec_fertil": rec("Fertilización"),
        "rec_epifitas": rec("Control de plantas epífitas o parásitas"),
        "rec_mecanicos": rec("Tratamiento de daños mecánicos"),
        "rec_objetos": rec("Eliminación de objetos extraños en tallo y ramas"),
        "rec_resiembra": rec("Resiembra"),
    })

# ---------- Veredictos (análisis del arboricultor) ----------
# categoria: "DERRIBO" / "CONSERVAR-INT" / "CONSERVAR"
VER = {
2:("CONSERVAR-INT","Riesgo total alto asociado a fuste inclinado y daños mecánicos (heridas, ramas quebradas, poda inadecuada), no a inestabilidad de anclaje; caída Media. Ejemplar pequeño (D=6,3 cm; 3,2 m). Poda de reequilibrio, tratamiento de heridas y manejo de epífitas."),
3:("CONSERVAR","Buen estado estructural (fuste, corteza y hojas buenos); riesgo y caída bajos. Retirar cuerdas del fuste y poda ligera de formación."),
4:("CONSERVAR-INT","Riesgo medio por fuste torcido, copa irregular y cima mala; caída Media. Especie nativa de valor ornamental. Poda de saneamiento y formación; tratar el descortezamiento."),
5:("CONSERVAR-INT","Condición fitosanitaria deficiente (hongos, exudados, pústulas, pudrición de ramas, ápice muerto), pero riesgo de CAÍDA BAJO por porte moderado (9,1 m) y raíz buena. Poda sanitaria, tratamiento fungicida, manejo de epífitas y monitoreo."),
6:("CONSERVAR-INT","Riesgo alto por afección foliar y de cima (hojas y cima malas, ápice muerto) y fuste inclinado; caída Media. Árbol joven-pequeño (D=19,7 cm; 7,4 m), recuperable. Poda sanitaria, fertilización y tratamiento; monitorear evolución."),
7:("DERRIBO","Único ejemplar con riesgo de CAÍDA ALTO y recomendación de derribo en campo. Es el árbol de mayor porte del parque (D=118,2 cm; 27,1 m), con copa amplia y poco simétrica —gran brazo de palanca frente a los fuertes vientos de la zona— exudados de resina (indicador de estrés/patología del fuste) y ubicación junto a los baños (máxima concurrencia de personas). La probabilidad de fallo combinada con la alta exposición vuelve inaceptable el riesgo. Se recomienda DERRIBO técnico controlado, previa verificación instrumental (resistógrafo/tomografía) si se dispone del equipo."),
8:("CONSERVAR-INT","Caída y riesgo Medios, pero RAÍZ en MAL estado en un árbol alto (17,8 m): punto crítico de anclaje. Poda de aligeramiento/reducción de la vela para disminuir la carga de viento e inspección radicular; si se confirma pérdida de anclaje, reevaluar derribo. Monitoreo PRIORITARIO."),
9:("CONSERVAR","Estructura y anclaje buenos; riesgo y caída bajos. Retiro de clavos/alambres y mantenimiento."),
10:("CONSERVAR","Ejemplar alto (20,7 m) en buen estado general, raíz buena y caída baja. Mantenimiento menor: retiro de clavos/alambres y manejo de epífitas."),
11:("CONSERVAR-INT","Mayor VALOR de riesgo del inventario (2,97), pero originado por daño a infraestructura por raíces (Alto) e inclinación en espacio estrecho, NO por deterioro estructural: fuste, ramas, hojas y cima BUENOS. Especie NATIVA (nogal andino). Poda de reequilibrio y manejo de raíz (barreras / reparación de adoquín); no procede derribo."),
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
for t in trees:
    t["cat"], t["fund"] = VER[t["row"]]

CAT_LABEL = {"DERRIBO":"Derribo","CONSERVAR-INT":"Conservar con intervención","CONSERVAR":"Conservar"}

def fnum(v, dec=1):
    if v is None: return "-"
    try:
        f = float(v)
        return ("%.*f" % (dec, f)).replace(".", ",")
    except Exception:
        return str(v)

# ================= CONSTRUCCIÓN DEL DOCUMENTO =================
doc = Document()
# estilo base
st = doc.styles["Normal"]
st.font.name = "Calibri"; st.font.size = Pt(10.5)
sec = doc.sections[0]
sec.top_margin = Cm(2); sec.bottom_margin = Cm(2)
sec.left_margin = Cm(2.2); sec.right_margin = Cm(2.2)

def set_cell_bg(cell, hexcolor):
    tcPr = cell._tc.get_or_add_tcPr()
    shd = OxmlElement("w:shd"); shd.set(qn("w:val"), "clear")
    shd.set(qn("w:fill"), hexcolor); tcPr.append(shd)

def cell_text(cell, text, bold=False, color=None, size=9, align="left", white=False):
    cell.text = ""
    p = cell.paragraphs[0]
    p.alignment = {"left":WD_ALIGN_PARAGRAPH.LEFT,"center":WD_ALIGN_PARAGRAPH.CENTER,"right":WD_ALIGN_PARAGRAPH.RIGHT}[align]
    run = p.add_run(str(text))
    run.bold = bold; run.font.size = Pt(size)
    if white: run.font.color.rgb = RGBColor(0xFF,0xFF,0xFF)
    elif color: run.font.color.rgb = color

def H(text, level=1, color=VERDE):
    h = doc.add_heading(level=level)
    run = h.add_run(text); run.font.color.rgb = color
    if level == 1: run.font.size = Pt(15)
    elif level == 2: run.font.size = Pt(12.5)
    return h

def P(text, size=10.5, bold=False, italic=False, align="justify", color=None, space_after=6):
    p = doc.add_paragraph()
    p.alignment = {"justify":WD_ALIGN_PARAGRAPH.JUSTIFY,"left":WD_ALIGN_PARAGRAPH.LEFT,"center":WD_ALIGN_PARAGRAPH.CENTER,"right":WD_ALIGN_PARAGRAPH.RIGHT}[align]
    r = p.add_run(text); r.bold = bold; r.italic = italic; r.font.size = Pt(size)
    if color: r.font.color.rgb = color
    p.paragraph_format.space_after = Pt(space_after)
    return p

def bullet(text, size=10.5):
    p = doc.add_paragraph(style="List Bullet")
    r = p.add_run(text); r.font.size = Pt(size)
    return p

# ---------------- PORTADA ----------------
for _ in range(2): doc.add_paragraph()
P("UNIVERSIDAD NACIONAL DE LOJA", size=13, bold=True, align="center", color=VERDE)
P("Facultad Agropecuaria y de Recursos Naturales Renovables", size=11, align="center", color=GRIS)
P("Laboratorio de Dendrocronología y Anatomía de la Madera", size=11, align="center", color=GRIS, space_after=24)
doc.add_paragraph()
P("INFORME TÉCNICO DE EVALUACIÓN DEL ARBOLADO URBANO", size=18, bold=True, align="center", color=VERDE)
P("Parque Central del cantón Saraguro", size=15, bold=True, align="center", color=GRIS, space_after=6)
P("Diagnóstico dendrométrico, estado fitosanitario y evaluación del riesgo de caída, con veredicto técnico de conservación o derribo", size=11, italic=True, align="center", color=GRIS, space_after=24)
doc.add_paragraph()
P("Solicitado por: Gobierno Autónomo Descentralizado Municipal Intercultural de Saraguro", size=10.5, align="center")
P("Oficio Nro. 0286-A-GADMIS (19/05/2026) — Autorización Rectorado UNL-R-2026-2083-M (28/05/2026)", size=10, align="center", color=GRIS)
P("Referencia: UNL-SG-2026-0421-EX", size=10, align="center", color=GRIS, space_after=24)
doc.add_paragraph(); doc.add_paragraph()
P("Responsable técnico: Ph.D. Darwin Alexander Pucha Cofrep", size=11, bold=True, align="center")
P("Responsable del Laboratorio de Dendrocronología — Director de la Maestría en Biodiversidad y Cambio Climático", size=10, align="center", color=GRIS)
P("Equipo de campo: Darwin Pucha · Ariel Arévalo", size=10, align="center", color=GRIS, space_after=18)
P("Plataforma de consulta pública: https://arbolec.unl.edu.ec/ec/Saraguro", size=10, align="center", color=AZUL)
P("Loja, 11 de julio de 2026", size=10.5, bold=True, align="center", color=GRIS)
doc.add_page_break()

# ---------------- 1. ANTECEDENTES ----------------
H("1. Antecedentes y justificación", 1)
P("El Gobierno Autónomo Descentralizado Municipal Intercultural de Saraguro, mediante Oficio Nro. 0286-A-GADMIS del 19 de mayo de 2026, suscrito por el Lic. Segundo Abel Sarango Quizhpe, Alcalde del cantón, solicitó a la Universidad Nacional de Loja el apoyo técnico para la inspección, evaluación y emisión de un criterio especializado sobre el estado actual de los árboles del Parque Central de Saraguro.")
P("El Municipio expuso su preocupación por: (i) el crecimiento radicular de varios árboles, que ha ocasionado afectaciones visibles en verjas, adoquinado y estructuras de concreto de las parcelas ornamentales; y (ii) el estado fitosanitario y la estabilidad estructural de dichos árboles frente a las fuertes corrientes de viento propias de la zona, situación que podría representar un riesgo para la seguridad de la ciudadanía. El oficio destaca de manera particular la presencia histórica de cipreses (registros fotográficos de la década de 1960), con reconocido valor simbólico y patrimonial para la comunidad saragurense.")
P("Mediante Memorando Nro. UNL-R-2026-2083-M del 28 de mayo de 2026, el Rector de la Universidad Nacional de Loja, Dr. Nikolay Aguirre Mendoza, autorizó la participación del Ph.D. Darwin Alexander Pucha Cofrep, Responsable del Laboratorio de Dendrocronología, para el desarrollo de las actividades requeridas, en coordinación directa con el Municipio.")
P("El presente informe responde a esa solicitud. Su finalidad es entregar un criterio técnico OBJETIVO y verificable que sustente la decisión de conservar o derribar cada árbol, atendiendo especialmente al riesgo de caída y a los cipreses señalados por el Municipio. Se deja constancia de que la recomendación de derribo se emite con carácter restrictivo: solo se propone cuando la evidencia de campo demuestra un riesgo inaceptable para las personas o los bienes.")

# ---------------- 2. OBJETIVOS ----------------
H("2. Objetivos", 1)
bullet("Realizar el inventario y diagnóstico dendrométrico y fitosanitario del arbolado del Parque Central de Saraguro.")
bullet("Evaluar el nivel de riesgo de cada árbol, con énfasis en el riesgo de caída (fallo estructural) frente a las condiciones de viento y de sitio.")
bullet("Analizar de forma individual los cipreses del parque, por su relevancia patrimonial y por ser objeto expreso de la preocupación municipal.")
bullet("Emitir un veredicto técnico por árbol —conservación o derribo— con su respectivo fundamento, como base para la toma de decisiones del GAD Municipal.")

# ---------------- 3. METODOLOGÍA ----------------
H("3. Metodología", 1)
P("El levantamiento se realizó mediante inspección visual en campo (metodología tipo VTA, Visual Tree Assessment) y registro georreferenciado con GPS, sistematizado en la plataforma institucional ArboLEC de la Universidad Nacional de Loja (https://arbolec.unl.edu.ec/ec/Saraguro), donde el inventario queda publicado y disponible para consulta pública.")
P("Por cada individuo se registraron: identificación taxonómica (especie, familia, nombres comunes), ubicación (coordenadas y código Plus Code), variables dendrométricas (circunferencia y diámetro del fuste, altura total y comercial, dimensiones de copa, porcentaje de follaje), estado de madurez, rectitud del fuste y espacio de crecimiento; la condición estructural y sanitaria de raíz, fuste, corteza, ramas, hojas, cima y copa; la presencia de enfermedades, plagas y daños físicos; y los factores de riesgo (caída, afectación a construcciones, interferencia con la circulación, daño a infraestructura por raíces e interferencia con redes aéreas).", space_after=6)
P("Escala de valoración del riesgo. Cada factor se califica en tres niveles —Bajo, Medio y Alto— y el sistema integra un Riesgo total (categórico) con su Valor de riesgo total (índice numérico, aprox. 1,0–3,0). Es fundamental distinguir dos conceptos que se usan a lo largo del informe:", space_after=4)
bullet("Riesgo total / Valor de riesgo: índice global que combina TODOS los factores (incluidos los daños a infraestructura por raíces). Un valor alto no implica, por sí solo, peligro de caída.")
bullet("Riesgo de caída: probabilidad de fallo estructural (volcamiento o rotura) del árbol. Es el factor determinante para la seguridad de las personas y, por tanto, el criterio rector para una eventual recomendación de derribo.")
P("Criterio de veredicto adoptado. Se recomienda DERRIBO únicamente cuando concurre un riesgo de caída Alto (o evidencia de fallo estructural irreversible) junto con exposición de personas o bienes, y cuando el defecto no es corregible mediante poda o tratamiento. En los demás casos se recomienda CONSERVACIÓN, con o sin intervención (poda, tratamiento fitosanitario, manejo de raíces, retiro de objetos extraños y monitoreo). Este criterio, deliberadamente conservador, protege tanto la seguridad ciudadana como el patrimonio arbóreo del parque.", space_after=8)

# ================= ESTADÍSTICOS =================
from docx.enum.section import WD_SECTION
n = len(trees)
species = {}
families = {}
for t in trees:
    species[t["especie"]] = species.get(t["especie"], 0) + 1
    families[t["familia"]] = families.get(t["familia"], 0) + 1
risk_counts = {"Alto":0, "Medio":0, "Bajo":0}
caida_counts = {"Alto":0, "Medio":0, "Bajo":0}
for t in trees:
    risk_counts[t["riesgo"]] = risk_counts.get(t["riesgo"], 0) + 1
    caida_counts[t["caida"]] = caida_counts.get(t["caida"], 0) + 1
cupres = [t for t in trees if t["familia"] == "Cupressaceae"]
n_derribo = sum(1 for t in trees if t["cat"] == "DERRIBO")
n_int = sum(1 for t in trees if t["cat"] == "CONSERVAR-INT")
n_cons = sum(1 for t in trees if t["cat"] == "CONSERVAR")
vals = [float(t["valriesgo"]) for t in trees if t["valriesgo"] is not None]
hts = [float(t["ht"]) for t in trees if t["ht"] is not None]
ds = [float(t["d"]) for t in trees if t["d"] is not None]

def landscape():
    s = doc.add_section(WD_SECTION.NEW_PAGE)
    s.orientation = WD_ORIENT.LANDSCAPE
    s.page_width, s.page_height = Cm(29.7), Cm(21)
    s.top_margin = Cm(1.5); s.bottom_margin = Cm(1.5)
    s.left_margin = Cm(1.5); s.right_margin = Cm(1.5)
    return s
def portrait():
    s = doc.add_section(WD_SECTION.NEW_PAGE)
    s.orientation = WD_ORIENT.PORTRAIT
    s.page_width, s.page_height = Cm(21), Cm(29.7)
    s.top_margin = Cm(2); s.bottom_margin = Cm(2)
    s.left_margin = Cm(2.2); s.right_margin = Cm(2.2)
    return s

def header_row(table, labels):
    hdr = table.rows[0].cells
    for i, lab in enumerate(labels):
        cell_text(hdr[i], lab, bold=True, white=True, size=8.5, align="center")
        set_cell_bg(hdr[i], HDR_FILL)

CAT_FILL = {"DERRIBO":ROJO_FILL, "CONSERVAR-INT":NARANJA_FILL, "CONSERVAR":VERDE_FILL}
RISK_COLOR = {"Alto":ROJO, "Medio":NARANJA, "Bajo":VERDE}

# ---------------- 4. RESULTADOS GENERALES ----------------
H("4. Resultados generales del inventario", 1)
P("Se inventariaron %d árboles en el Parque Central (Sección S1), correspondientes a %d especies y %d familias botánicas. La familia Cupressaceae (los cipreses) es la más representada, con %d individuos (%.0f%% del total), lo que confirma su carácter dominante y su valor identitario en el parque, tal como lo señala el oficio municipal." % (n, len(species), len(families), len(cupres), 100*len(cupres)/n))
P("Rango dendrométrico: altura total de %s a %s m y diámetro de fuste de %s a %s cm, lo que refleja un arbolado heterogéneo que combina ejemplares jóvenes y de pequeño porte con individuos maduros de gran tamaño —estos últimos, principalmente cipreses—. El de mayor porte es el ciprés común %s (%s, ref. de campo «%s»), con 27,1 m de altura y 118,2 cm de diámetro." % (fnum(min(hts)), fnum(max(hts)), fnum(min(ds)), fnum(max(ds)), "A06", trees[5]["codigo"], trees[5]["ref"]))

P("Composición por especie:", bold=True, space_after=2)
sp_sorted = sorted(species.items(), key=lambda x: (-x[1], x[0]))
tsp = doc.add_table(rows=1, cols=4); tsp.style = "Table Grid"; tsp.alignment = WD_TABLE_ALIGNMENT.CENTER
header_row(tsp, ["Especie", "Nombre común", "Familia", "N°"])
comun_by_sp = {}
fam_by_sp = {}
for t in trees:
    comun_by_sp.setdefault(t["especie"], t["comun"])
    fam_by_sp.setdefault(t["especie"], t["familia"])
for sp, cnt in sp_sorted:
    row = tsp.add_row().cells
    cell_text(row[0], sp, italic if False else False, size=8.5); row[0].paragraphs[0].runs[0].italic = True
    cell_text(row[1], comun_by_sp.get(sp, ""), size=8.5)
    cell_text(row[2], fam_by_sp.get(sp, ""), size=8.5)
    cell_text(row[3], str(cnt), size=8.5, align="center")
doc.add_paragraph()

P("Distribución del riesgo total:", bold=True, space_after=2)
tr = doc.add_table(rows=1, cols=4); tr.style = "Table Grid"; tr.alignment = WD_TABLE_ALIGNMENT.CENTER
header_row(tr, ["Nivel de riesgo total", "N° de árboles", "%", "Interpretación"])
interp = {"Alto":"Requieren intervención y/o seguimiento", "Medio":"Intervención preventiva / monitoreo", "Bajo":"Mantenimiento ordinario"}
for lvl in ["Alto","Medio","Bajo"]:
    row = tr.add_row().cells
    cell_text(row[0], lvl, bold=True, color=RISK_COLOR[lvl], size=9, align="center")
    cell_text(row[1], str(risk_counts[lvl]), size=9, align="center")
    cell_text(row[2], "%.0f%%" % (100*risk_counts[lvl]/n), size=9, align="center")
    cell_text(row[3], interp[lvl], size=9)
P("Nota: el riesgo total integra todos los factores, incluido el daño a infraestructura por raíces; por ello varios árboles estructuralmente sanos figuran en riesgo «Alto» sin ser candidatos a derribo (ver Sección 5).", size=9, italic=True, color=GRIS, space_after=8)

# ---------------- 5. RIESGO DE CAÍDA (ÉNFASIS) ----------------
H("5. Análisis del riesgo de caída (énfasis)", 1)
P("El riesgo de caída es el criterio central de este informe por su relación directa con la seguridad de las personas. La distribución obtenida es la siguiente:", space_after=4)
tc = doc.add_table(rows=1, cols=3); tc.style = "Table Grid"; tc.alignment = WD_TABLE_ALIGNMENT.CENTER
header_row(tc, ["Riesgo de caída", "N° de árboles", "%"])
for lvl in ["Alto","Medio","Bajo"]:
    row = tc.add_row().cells
    cell_text(row[0], lvl, bold=True, color=RISK_COLOR[lvl], size=9, align="center")
    cell_text(row[1], str(caida_counts[lvl]), size=9, align="center")
    cell_text(row[2], "%.0f%%" % (100*caida_counts[lvl]/n), size=9, align="center")
doc.add_paragraph()
P("Hallazgo principal. De los %d árboles evaluados, UN (1) solo individuo presenta riesgo de caída ALTO: el ciprés común identificado como A06 (código %s, referencia de campo «%s»). Este mismo árbol es el único con recomendación de DERRIBO en el levantamiento de campo. Diez (10) árboles presentan riesgo de caída Medio —manejables mediante poda de reequilibrio, tratamiento y vigilancia— y trece (13) presentan riesgo de caída Bajo." % (n, trees[5]["codigo"], trees[5]["ref"]), space_after=6)
P("Este resultado es determinante para el veredicto: aunque el 33% del arbolado figura en riesgo total «Alto», ese nivel está mayoritariamente asociado a fustes inclinados y a daños de raíces sobre el adoquinado y las estructuras de concreto —el problema que motivó la solicitud municipal— y NO a una probabilidad real de volcamiento. En términos de seguridad por caída, el parque presenta un único punto crítico. Debe subrayarse que la inclinación del fuste y el daño radicular, si bien no implican caída inminente, sí requieren seguimiento: se establecen como árboles de VIGILANCIA los A07 (raíz en mal estado, 17,8 m), A14 y A20 (fustes muy inclinados).", space_after=6)
P("Recomendación de método: para el árbol A06, y ante cualquier duda futura sobre ejemplares de gran porte, se aconseja confirmar el diagnóstico con evaluación instrumental (resistógrafo o tomografía sónica) antes de ejecutar el derribo, de modo que la decisión quede respaldada por medición directa del estado interno del fuste.", space_after=8)

# ---------------- 6. ANÁLISIS INDIVIDUAL DE LOS CIPRESES ----------------
H("6. Análisis individual de los cipreses", 1)
P("Atendiendo a la preocupación expresa del Municipio y al valor patrimonial de estas especies, se analiza individualmente cada uno de los %d cipreses del parque (4 Cupressus sempervirens — ciprés vela; 3 Hesperocyparis macrocarpa — ciprés común). Se precisa que ambas especies son introducidas (originarias del Mediterráneo y de California, respectivamente); su valor es cultural, histórico y paisajístico —plenamente reconocido en este informe— más que estrictamente ecológico." % len(cupres), space_after=6)
for t in cupres:
    H("Ciprés %s — %s (%s)" % (t["id"], t["especie"], t["comun"]), 2, color=(ROJO if t["cat"]=="DERRIBO" else VERDE))
    tt = doc.add_table(rows=1, cols=4); tt.style = "Table Grid"
    tt.rows[0].cells[0].width = Cm(4)
    data = [
        ("Código / Ref. campo", "%s / %s" % (t["codigo"], t["ref"] or "s/n"), "Coordenadas", "%s, %s" % (fnum(t["lat"],6), fnum(t["lon"],6))),
        ("Diámetro / Altura total", "%s cm / %s m" % (fnum(t["d"]), fnum(t["ht"])), "Copa (NS×EW)", "%s × %s m" % (fnum(t["copaNS"]), fnum(t["copaEW"]))),
        ("Rectitud / Espacio", "%s / %s" % (t["rectitud"], t["espacio"]), "Follaje", "%s%%" % t["follaje"]),
        ("Raíz / Fuste (estado)", "%s / %s" % (t["raiz"], t["fuste_est"]), "Riesgo total (valor)", "%s (%s)" % (t["riesgo"], fnum(t["valriesgo"],2))),
        ("RIESGO DE CAÍDA", t["caida"], "Afect. infraestructura (raíz)", t["afec_raiz"]),
    ]
    for a,b,c,d in data:
        row = tt.add_row().cells
        cell_text(row[0], a, bold=True, size=8.5)
        cell_text(row[1], b, size=8.5, bold=(a=="RIESGO DE CAÍDA"),
                  color=(RISK_COLOR.get(b) if a=="RIESGO DE CAÍDA" else None))
        cell_text(row[2], c, bold=True, size=8.5)
        cell_text(row[3], d, size=8.5)
    pv = doc.add_paragraph()
    rlab = pv.add_run("Veredicto: "); rlab.bold = True; rlab.font.size = Pt(10)
    rcat = pv.add_run(CAT_LABEL[t["cat"]] + ". "); rcat.bold = True; rcat.font.size = Pt(10)
    rcat.font.color.rgb = (ROJO if t["cat"]=="DERRIBO" else (NARANJA if t["cat"]=="CONSERVAR-INT" else VERDE))
    rf = pv.add_run(t["fund"]); rf.font.size = Pt(10)
    pv.alignment = WD_ALIGN_PARAGRAPH.JUSTIFY
    pv.paragraph_format.space_after = Pt(10)

P("Síntesis de los cipreses: de los %d cipreses, se recomienda el derribo de UNO (A06) por riesgo de caída alto y alta exposición; los %d restantes se CONSERVAN, con intervenciones de poda sanitaria, manejo de epífitas, tratamiento de daños mecánicos, retiro de clavos/alambres y, en el caso de A07, monitoreo prioritario del anclaje radicular. Se conserva así el conjunto patrimonial de cipreses del parque, retirando únicamente el ejemplar que representa un peligro real." % (len(cupres), len(cupres)-1), bold=False, space_after=8)

# ---------------- 7. TABLA RESUMEN: VEREDICTO ----------------
landscape()
H("7. Tabla resumen: conservación vs. derribo (todo el inventario)", 1)
P("Veredicto técnico por árbol. Código de color: rojo = Derribo; naranja = Conservar con intervención; verde = Conservar. De los %d árboles, se recomienda %d derribo, %d conservación con intervención y %d conservación con mantenimiento ordinario." % (n, n_derribo, n_int, n_cons), size=9.5, space_after=6)
tv = doc.add_table(rows=1, cols=9); tv.style = "Table Grid"; tv.alignment = WD_TABLE_ALIGNMENT.CENTER
header_row(tv, ["ID", "Especie (común)", "Ref. campo", "D (cm)", "H (m)", "Riesgo total", "Caída", "VEREDICTO", "Fundamento"])
widths = [Cm(1.0), Cm(4.2), Cm(1.7), Cm(1.3), Cm(1.3), Cm(1.7), Cm(1.4), Cm(3.0), Cm(9.8)]
for t in trees:
    row = tv.add_row().cells
    vals_row = [t["id"], "%s\n(%s)" % (t["especie"], t["comun"]), t["ref"] or "-", fnum(t["d"]), fnum(t["ht"]),
                "%s (%s)" % (t["riesgo"], fnum(t["valriesgo"],2)), t["caida"], CAT_LABEL[t["cat"]], t["fund"]]
    for i, v in enumerate(vals_row):
        cell_text(row[i], v, size=7.5, align=("left" if i in (1,8) else "center"),
                  bold=(i in (0,7)))
        set_cell_bg(row[i], CAT_FILL[t["cat"]])
    # colorear caída
    row[6].paragraphs[0].runs[0].font.color.rgb = RISK_COLOR.get(t["caida"], GRIS)
    row[6].paragraphs[0].runs[0].bold = True
for i, w in enumerate(widths):
    for r_ in tv.rows:
        r_.cells[i].width = w

# ---------------- 8. REGISTRO FOTOGRÁFICO ----------------
portrait()
H("8. Registro fotográfico por árbol", 1)
P("Las fotografías de cada árbol se encuentran publicadas en la plataforma institucional ArboLEC, junto con su ficha completa. Cada individuo puede localizarse por su código y coordenadas en https://arbolec.unl.edu.ec/ec/Saraguro. A continuación se reserva el espacio para la fotografía de cada árbol; las imágenes se insertarán en la versión final para el expediente municipal.", space_after=8)
for idx, t in enumerate(trees):
    tf = doc.add_table(rows=1, cols=2); tf.style = "Table Grid"
    tf.columns[0].width = Cm(6.5); tf.columns[1].width = Cm(10)
    c0, c1 = tf.rows[0].cells
    c0.width = Cm(6.5); c1.width = Cm(10)
    # celda de foto (placeholder)
    cell_text(c0, "", size=9)
    pph = c0.paragraphs[0]; pph.alignment = WD_ALIGN_PARAGRAPH.CENTER
    rr = pph.add_run("\n[  FOTOGRAFÍA  ]\n%s\n(insertar imagen)\n" % t["id"]); rr.font.size = Pt(9); rr.font.color.rgb = GRIS; rr.italic = True
    set_cell_bg(c0, "F2F2F2")
    # celda de datos
    c1.text = ""
    p = c1.paragraphs[0]
    r1 = p.add_run("%s — %s (%s)\n" % (t["id"], t["especie"], t["comun"])); r1.bold = True; r1.font.size = Pt(10); r1.font.color.rgb = VERDE
    r2 = p.add_run("Código: %s   |   Ref. campo: %s\n" % (t["codigo"], t["ref"] or "s/n")); r2.font.size = Pt(9)
    r3 = p.add_run("Coordenadas: %s, %s\n" % (fnum(t["lat"],6), fnum(t["lon"],6))); r3.font.size = Pt(9)
    r4 = p.add_run("D=%s cm · H=%s m · Follaje %s%%\n" % (fnum(t["d"]), fnum(t["ht"]), t["follaje"])); r4.font.size = Pt(9)
    r5 = p.add_run("Riesgo total: %s (%s) · Caída: %s\n" % (t["riesgo"], fnum(t["valriesgo"],2), t["caida"])); r5.font.size = Pt(9)
    r5.font.color.rgb = RISK_COLOR.get(t["caida"], GRIS)
    r6 = p.add_run("Veredicto: %s\n" % CAT_LABEL[t["cat"]]); r6.bold = True; r6.font.size = Pt(9.5)
    r6.font.color.rgb = (ROJO if t["cat"]=="DERRIBO" else (NARANJA if t["cat"]=="CONSERVAR-INT" else VERDE))
    r7 = p.add_run("Ficha ArboLEC: https://arbolec.unl.edu.ec/ec/Saraguro"); r7.font.size = Pt(8.5); r7.font.color.rgb = AZUL
    doc.add_paragraph()

# ---------------- 9. CONCLUSIONES Y RECOMENDACIONES ----------------
H("9. Conclusiones y recomendaciones", 1)
P("Conclusiones:", bold=True, space_after=2)
bullet("El Parque Central de Saraguro alberga %d árboles de %d especies; la familia de los cipreses (Cupressaceae) es la dominante y de mayor valor patrimonial." % (n, len(species)))
bullet("En términos de seguridad, el parque presenta UN único punto crítico de riesgo de caída Alto: el árbol A06 (ciprés común, código %s, junto a los baños), el más grande del parque y único con recomendación de derribo en campo." % trees[5]["codigo"])
bullet("El resto del arbolado (%d árboles) se CONSERVA. El riesgo total «Alto» de varios ejemplares se explica mayoritariamente por daño de raíces a la infraestructura y por inclinación del fuste, condiciones manejables sin derribo." % (n-1))
bullet("El daño al adoquinado y a las estructuras de concreto —motivo central de la solicitud municipal— se aborda con manejo de raíces (barreras físicas, reparación del adoquín, poda) y no justifica, por sí mismo, la tala de árboles estructuralmente sanos.")
P("Recomendaciones:", bold=True, space_after=2)
bullet("Ejecutar el DERRIBO técnico controlado del árbol A06, previa verificación instrumental si se dispone del equipo, con personal especializado y medidas de seguridad, dada su cercanía a los baños y zonas de tránsito.")
bullet("Programar la CONSERVACIÓN con intervención de los demás árboles: poda sanitaria y de reequilibrio, tratamiento fitosanitario (hongos, exudados), manejo de epífitas y retiro de clavos/alambres del fuste.")
bullet("Establecer MONITOREO prioritario del árbol A07 (raíz en mal estado, 17,8 m) y de los ejemplares muy inclinados (A14, A20); reevaluar si progresa la inclinación o se confirma pérdida de anclaje.")
bullet("Implementar manejo de raíces (barreras anti-raíz, reparación del adoquinado) en los árboles con afectación Alta a infraestructura (A10, A23 y otros cipreses), preservando el árbol.")
bullet("Considerar un plan de reposición y sucesión del arbolado a mediano plazo, priorizando especies nativas (nogal andino, molle, arupo) para acompañar el recambio natural sin perder cobertura ni identidad del parque.")
bullet("Mantener actualizado el inventario en la plataforma ArboLEC como herramienta de gestión y seguimiento del arbolado urbano de Saraguro.")

doc.add_paragraph(); doc.add_paragraph()
P("____________________________________", align="center", space_after=0)
P("Ph.D. Darwin Alexander Pucha Cofrep", bold=True, align="center", space_after=0)
P("Responsable del Laboratorio de Dendrocronología — Universidad Nacional de Loja", size=9.5, align="center", color=GRIS, space_after=0)
P("Loja, 11 de julio de 2026", size=9.5, align="center", color=GRIS)

doc.save(OUT_DOCX)
print("Informe técnico completo generado:", OUT_DOCX)
print("Resumen -> Derribo:", n_derribo, "| Conservar c/interv:", n_int, "| Conservar:", n_cons, "| Total:", n)
print("Caída Alto:", caida_counts["Alto"], "| Cipreses:", len(cupres))
