# -*- coding: utf-8 -*-
"""Genera el Informe Técnico en Word (python-docx)."""
import os
from docx import Document
from docx.shared import Pt, Cm, RGBColor
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.enum.table import WD_TABLE_ALIGNMENT
from docx.enum.section import WD_ORIENT, WD_SECTION
from docx.oxml.ns import qn
from docx.oxml import OxmlElement
import _data as D

OUT = os.path.join(os.path.dirname(__file__), "..", "Informe_Tecnico_Arbolado_Parque_Central_Saraguro.docx")
VERDE=RGBColor(0x1B,0x5E,0x20); VERDE_H="1B5E20"; GRIS=RGBColor(0x42,0x42,0x42)
ROJO=RGBColor(0xB7,0x1C,0x1C); NARANJA=RGBColor(0xE6,0x5A,0x00); AZUL=RGBColor(0x0D,0x47,0xA1)
ROJO_F="F4C7C3"; NAR_F="FCE5CD"; VER_F="D9EAD3"; PH_F="F2F2F2"
CATF={"DERRIBO":ROJO_F,"CONSERVAR-INT":NAR_F,"CONSERVAR":VER_F}
RISKC={"Alto":ROJO,"Medio":NARANJA,"Bajo":VERDE}
CAT=D.CAT_LABEL; fnum=D.fnum

doc=Document()
st=doc.styles["Normal"]; st.font.name="Calibri"; st.font.size=Pt(10.5)
sec=doc.sections[0]
sec.top_margin=Cm(2); sec.bottom_margin=Cm(2); sec.left_margin=Cm(2.2); sec.right_margin=Cm(2.2)

def set_bg(cell,hexc):
    tcPr=cell._tc.get_or_add_tcPr(); shd=OxmlElement("w:shd")
    shd.set(qn("w:val"),"clear"); shd.set(qn("w:fill"),hexc); tcPr.append(shd)
def ctext(cell,text,bold=False,color=None,size=9,align="left",white=False):
    cell.text=""; p=cell.paragraphs[0]
    p.alignment={"left":WD_ALIGN_PARAGRAPH.LEFT,"center":WD_ALIGN_PARAGRAPH.CENTER,"right":WD_ALIGN_PARAGRAPH.RIGHT}[align]
    r=p.add_run(str(text)); r.bold=bold; r.font.size=Pt(size)
    if white: r.font.color.rgb=RGBColor(0xFF,0xFF,0xFF)
    elif color: r.font.color.rgb=color
def H(text,level=1,color=VERDE):
    h=doc.add_heading(level=level); r=h.add_run(text); r.font.color.rgb=color
    r.font.size=Pt(15 if level==1 else 12.5); return h
def P(text,size=10.5,bold=False,italic=False,align="justify",color=None,after=6):
    p=doc.add_paragraph()
    p.alignment={"justify":WD_ALIGN_PARAGRAPH.JUSTIFY,"left":WD_ALIGN_PARAGRAPH.LEFT,"center":WD_ALIGN_PARAGRAPH.CENTER,"right":WD_ALIGN_PARAGRAPH.RIGHT}[align]
    r=p.add_run(text); r.bold=bold; r.italic=italic; r.font.size=Pt(size)
    if color: r.font.color.rgb=color
    p.paragraph_format.space_after=Pt(after); return p
def bullet(text,size=10.5):
    p=doc.add_paragraph(style="List Bullet"); r=p.add_run(text); r.font.size=Pt(size); return p
def hdr_row(table,labels):
    for i,lab in enumerate(labels):
        ctext(table.rows[0].cells[i],lab,bold=True,white=True,size=8.5,align="center"); set_bg(table.rows[0].cells[i],VERDE_H)
def photo_strip(labels,height=2.9,tid=None):
    photos=D.tree_photos(tid) if tid else [None]*len(labels)
    t=doc.add_table(rows=1,cols=len(labels)); t.alignment=WD_TABLE_ALIGNMENT.CENTER
    t.rows[0].height=Cm(height); cw=Cm(17.0/len(labels))
    for i,lab in enumerate(labels):
        c=t.rows[0].cells[i]; c.width=cw
        c.text=""; p=c.paragraphs[0]; p.alignment=WD_ALIGN_PARAGRAPH.CENTER
        ph=photos[i] if i<len(photos) else None
        if ph:
            try:
                p.add_run().add_picture(ph,width=Cm(17.0/len(labels)-0.3))
            except Exception:
                set_bg(c,PH_F); p.add_run("\n[ %s ]\n(insertar foto)\n"%lab).font.size=Pt(8.5)
        else:
            set_bg(c,PH_F); r=p.add_run("\n[ %s ]\n(insertar foto)\n"%lab); r.font.size=Pt(8.5); r.font.color.rgb=GRIS; r.italic=True
    return t
def url_par(t):
    p=doc.add_paragraph(); r=p.add_run("Ficha del árbol: "); r.bold=True; r.font.size=Pt(8.5)
    r2=p.add_run(t["url"]); r2.font.size=Pt(8.5); r2.font.color.rgb=AZUL
    p.paragraph_format.space_after=Pt(3); return p

# ---------- PORTADA ----------
logo_tbl=doc.add_table(rows=1,cols=2); logo_tbl.alignment=WD_TABLE_ALIGNMENT.CENTER
lc=logo_tbl.rows[0].cells
lc[0].paragraphs[0].alignment=WD_ALIGN_PARAGRAPH.LEFT
lc[0].paragraphs[0].add_run().add_picture(D.LOGO_UNL,width=Cm(7.2))
lc[1].paragraphs[0].alignment=WD_ALIGN_PARAGRAPH.RIGHT
lc[1].paragraphs[0].add_run().add_picture(D.LOGO_ARBOLEC,width=Cm(2.7))
P("",after=4)
P("UNIVERSIDAD NACIONAL DE LOJA",size=13,bold=True,align="center",color=VERDE,after=0)
P("Facultad Agropecuaria y de Recursos Naturales Renovables · Laboratorio de Dendrocronología",size=10.5,align="center",color=GRIS,after=18)
P("INFORME TÉCNICO DE EVALUACIÓN DEL ARBOLADO URBANO",size=18,bold=True,align="center",color=VERDE,after=2)
P("Parque Central y Avenida El Oro del cantón Saraguro",size=15,bold=True,align="center",color=GRIS,after=4)
P("Diagnóstico dendrométrico, estado fitosanitario y evaluación del riesgo de caída, con veredicto técnico de conservación o derribo",size=11,italic=True,align="center",color=GRIS,after=20)
P("Solicitado por el Gobierno Autónomo Descentralizado Municipal Intercultural de Saraguro",size=10.5,align="center",after=2)
P("Oficio Nro. 0286-A-GADMIS (19/05/2026) — Autorización Rectorado UNL-R-2026-2083-M (28/05/2026)",size=10,align="center",color=GRIS,after=2)
P("Referencia: UNL-SG-2026-0421-EX",size=10,align="center",color=GRIS,after=20)
P("Responsable técnico: Ph.D. Darwin Alexander Pucha Cofrep",size=11,bold=True,align="center",after=2)
P("Responsable del Laboratorio de Dendrocronología — Director de la Maestría en Biodiversidad y Cambio Climático",size=10,align="center",color=GRIS,after=2)
P("Equipo de campo: Darwin Pucha · Ariel Arévalo · Cristian Retete",size=10,align="center",color=GRIS,after=6)
P("Plataforma de consulta pública: %s"%D.BASE_URL,size=10,align="center",color=AZUL,after=2)
P("Loja, 11 de julio de 2026",size=10.5,bold=True,align="center",color=GRIS)
doc.add_page_break()

# ---------- 1-2 ----------
H("1. Antecedentes y justificación",1)
P("El Gobierno Autónomo Descentralizado Municipal Intercultural de Saraguro, mediante Oficio Nro. 0286-A-GADMIS del 19 de mayo de 2026, suscrito por el Lic. Segundo Abel Sarango Quizhpe, Alcalde del cantón, solicitó a la Universidad Nacional de Loja el apoyo técnico para la inspección, evaluación y emisión de un criterio especializado sobre el estado actual de los árboles del Parque Central de Saraguro.")
P("El Municipio expuso su preocupación por: (i) el crecimiento radicular de varios árboles, que ha ocasionado afectaciones en verjas, adoquinado y estructuras de concreto; y (ii) el estado fitosanitario y la estabilidad estructural frente a las fuertes corrientes de viento de la zona, situación que podría representar un riesgo para la seguridad ciudadana. El oficio destaca la presencia histórica de cipreses (registros fotográficos de la década de 1960), de reconocido valor simbólico y patrimonial.")
P("Mediante Memorando Nro. UNL-R-2026-2083-M del 28 de mayo de 2026, el Rector, Dr. Nikolay Aguirre Mendoza, autorizó la participación del Ph.D. Darwin Alexander Pucha Cofrep, Responsable del Laboratorio de Dendrocronología.")
P("El presente informe entrega un criterio técnico OBJETIVO y verificable para conservar o derribar cada árbol, con énfasis en el riesgo de caída y en los cipreses señalados. La evaluación se amplió, a solicitud del Municipio, a cuatro árboles de la isleta de la Avenida El Oro, contiguos al parque. La recomendación de derribo se emite con carácter restrictivo: solo cuando la evidencia demuestra un riesgo inaceptable para las personas o los bienes.")
H("2. Objetivos",1)
for x in ["Inventariar y diagnosticar (dendrométrica y fitosanitariamente) el arbolado del Parque Central de Saraguro y de la isleta de la Avenida El Oro.",
          "Evaluar el nivel de riesgo de cada árbol, con énfasis en el riesgo de caída frente a las condiciones de viento y de sitio.",
          "Analizar de forma individual los cipreses del parque, por su relevancia patrimonial y por ser objeto de la preocupación municipal.",
          "Emitir un veredicto técnico por árbol —conservación o derribo— con su respectivo fundamento."]:
    bullet(x)
doc.add_page_break()

# ---------- 3 METODOLOGÍA ----------
H("3. Metodología",1)
P("El levantamiento se realizó mediante inspección visual en campo (metodología tipo VTA, Visual Tree Assessment) y registro georreferenciado con GPS, sistematizado en la plataforma institucional ArboLEC (%s), donde el inventario queda publicado para consulta pública. Cada árbol posee una ficha individual accesible por su código."%D.BASE_URL)
P("Por cada individuo se registraron: identificación taxonómica, ubicación (coordenadas y código Plus Code), variables dendrométricas (circunferencia y diámetro, altura total y comercial, dimensiones de copa, follaje), estado de madurez, rectitud de fuste y espacio de crecimiento; la condición estructural y sanitaria de raíz, fuste, corteza, ramas, hojas, cima y copa; enfermedades, plagas y daños físicos; y los factores de riesgo (caída, afectación a construcciones, circulación, daño de raíces e interferencia con redes aéreas).")
P("Escala de riesgo. Cada factor se califica en Bajo, Medio y Alto; el sistema integra un Riesgo total (categórico) y un Valor de riesgo (índice ~1,0–3,0). Se distinguen dos conceptos clave:",after=4)
bullet("Riesgo total / valor de riesgo: índice global que combina TODOS los factores (incluido el daño de raíces a la infraestructura). Un valor alto no implica, por sí solo, peligro de caída.")
bullet("Riesgo de caída: probabilidad de fallo estructural (volcamiento o rotura). Criterio rector para una eventual recomendación de derribo.")
P("Criterio de veredicto. Se recomienda DERRIBO únicamente cuando concurre un riesgo de caída Alto (o fallo estructural irreversible) con exposición de personas o bienes, y cuando el defecto no es corregible por poda o tratamiento. En los demás casos, CONSERVACIÓN con o sin intervención. Criterio deliberadamente conservador que protege la seguridad ciudadana y el patrimonio arbóreo.",after=8)
H("3.1. Mapa de ubicación de los árboles",2)
pmap=doc.add_paragraph(); pmap.alignment=WD_ALIGN_PARAGRAPH.CENTER
pmap.add_run().add_picture(D.MAPA,width=Cm(17))
P("Figura 1. Ubicación georreferenciada (GPS, WGS84) de los %d árboles evaluados en el Parque Central y en la isleta de la Avenida El Oro, con su veredicto técnico. En rojo, el único árbol propuesto para derribo (A06)."%D.n,size=8.5,italic=True,align="center",color=GRIS,after=8)
H("3.2. Registro fotográfico del trabajo de campo",2)
P("Se reserva el siguiente espacio para las fotografías del levantamiento en campo (mediciones dendrométricas, inspección visual y georreferenciación):",size=10)
photo_strip(["Campo 1","Campo 2","Campo 3"],height=3.4)
P("",after=2)
photo_strip(["Campo 4","Campo 5","Campo 6"],height=3.4)
doc.add_page_break()

# ---------- 4 RESULTADOS ----------
H("4. Resultados generales del inventario",1)
P("Se inventariaron %d árboles: %d en el Parque Central y %d en la isleta de la Avenida El Oro, de %d especies y %d familias. La familia Cupressaceae (cipreses) es la más representada, con %d individuos (%.0f%%). El rango dendrométrico va de %s a %s m de altura y de %s a %s cm de diámetro; el mayor es el ciprés común A06 (código %s), con 27,1 m y 118,2 cm."%(
    D.n,D.n_parque,D.n_av,len(D.species),len(D.families),len(D.cupres),100*len(D.cupres)/D.n,
    fnum(min(float(t['ht']) for t in D.trees)),fnum(max(float(t['ht']) for t in D.trees)),
    fnum(min(float(t['d']) for t in D.trees)),fnum(max(float(t['d']) for t in D.trees)),D.DERRIBO_TREE['codigo']))
P("Composición por especie:",bold=True,after=2)
tsp=doc.add_table(rows=1,cols=4); tsp.style="Table Grid"; tsp.alignment=WD_TABLE_ALIGNMENT.CENTER
hdr_row(tsp,["Especie","Nombre común","Familia","N°"])
for sp,cnt in sorted(D.species.items(),key=lambda x:(-x[1],x[0])):
    row=tsp.add_row().cells
    ctext(row[0],sp,size=8.5); row[0].paragraphs[0].runs[0].italic=True
    ctext(row[1],D.comun_by.get(sp,""),size=8.5); ctext(row[2],D.fam_by.get(sp,""),size=8.5); ctext(row[3],str(cnt),size=8.5,align="center")
P("",after=6)
P("Distribución del riesgo total:",bold=True,after=2)
tr=doc.add_table(rows=1,cols=4); tr.style="Table Grid"; tr.alignment=WD_TABLE_ALIGNMENT.CENTER
hdr_row(tr,["Nivel de riesgo total","N° de árboles","%","Interpretación"])
interp={"Alto":"Requieren intervención y/o seguimiento","Medio":"Intervención preventiva / monitoreo","Bajo":"Mantenimiento ordinario"}
for lvl in ["Alto","Medio","Bajo"]:
    row=tr.add_row().cells
    ctext(row[0],lvl,bold=True,color=RISKC[lvl],size=9,align="center")
    ctext(row[1],str(D.risk_counts[lvl]),size=9,align="center"); ctext(row[2],"%.0f%%"%(100*D.risk_counts[lvl]/D.n),size=9,align="center"); ctext(row[3],interp[lvl],size=9)
P("Nota: el riesgo total integra todos los factores, incluido el daño de raíces a la infraestructura; por ello varios árboles estructuralmente sanos figuran en riesgo «Alto» sin ser candidatos a derribo.",size=9,italic=True,color=GRIS,after=8)

# ---------- 5 SALUD / ESTADO FITOSANITARIO ----------
def fito_fill_hex(v): return {"buena":VER_F,"regular":NAR_F,"mala":ROJO_F,"na":PH_F}[D.fito_color_key(v)]
H("5. Salud y estado fitosanitario del arbolado",1)
P("Esta sección resume la salud del arbolado a partir de cinco categorías evaluadas en campo: (i) estado fitosanitario de sus componentes (raíz, fuste, corteza, ramas, hojas, cima y copa); (ii) presencia de enfermedades; (iii) ubicación de las enfermedades; (iv) presencia de plagas; y (v) ubicación de las plagas. El detalle por árbol se presenta en la ficha de salud (5.5). La Figura 2 sintetiza, en un «árbol promedio», el estado más frecuente (moda) de cada componente.")
_pinf=doc.add_paragraph(); _pinf.alignment=WD_ALIGN_PARAGRAPH.CENTER
_pinf.add_run().add_picture(D.INFOGRAFIA,width=Cm(14.5))
P("Figura 2. «El árbol promedio de Saraguro»: cada elemento se colorea según el valor más frecuente (moda) de los 40 árboles (verde = bueno, ámbar = regular, rojo = malo, gris = no visible).",size=8.5,italic=True,align="center",color=GRIS,after=8)
H("5.1. Estado fitosanitario por componente",2)
tf=doc.add_table(rows=1,cols=5); tf.style="Table Grid"; tf.alignment=WD_TABLE_ALIGNMENT.CENTER
hdr_row(tf,["Componente","Buena/Bueno","Regular","Mala/Malo","No visible/NA"])
for comp in D.FITO_COMPONENTS:
    d=D.fito_summary[comp]; row=tf.add_row().cells
    ctext(row[0],comp,size=9)
    ctext(row[1],str(d["Buena/Bueno"]),size=9,align="center",color=VERDE,bold=True)
    ctext(row[2],str(d["Regular"]),size=9,align="center")
    ctext(row[3],str(d["Mala/Malo"]),size=9,align="center",color=ROJO,bold=True)
    ctext(row[4],str(d["No visible/NA"]),size=9,align="center")
P("Lectura: la mayoría de los componentes están en estado Bueno o Regular; los estados «Malo» se concentran en pocos árboles (ver 5.4) y orientan las intervenciones de poda sanitaria, tratamiento y —donde hay pudrición o raíz comprometida— evaluación estructural.",size=9,italic=True,color=GRIS,after=8)
H("5.2. Presencia de enfermedades",2)
te=doc.add_table(rows=1,cols=3); te.style="Table Grid"; te.alignment=WD_TABLE_ALIGNMENT.CENTER
hdr_row(te,["Enfermedad","N° de árboles","% del total"])
for k,v in (D.enf_freq.items() or []):
    row=te.add_row().cells; ctext(row[0],k,size=9); ctext(row[1],str(v),size=9,align="center"); ctext(row[2],"%.0f%%"%(100*v/D.n),size=9,align="center")
H("5.3. Presencia de plagas",2)
tp=doc.add_table(rows=1,cols=3); tp.style="Table Grid"; tp.alignment=WD_TABLE_ALIGNMENT.CENTER
hdr_row(tp,["Plaga / agente","N° de árboles","% del total"])
for k,v in (D.plaga_freq.items() or []):
    row=tp.add_row().cells; ctext(row[0],k,size=9); ctext(row[1],str(v),size=9,align="center"); ctext(row[2],"%.0f%%"%(100*v/D.n),size=9,align="center")
P("La afección más frecuente es la decoloración de hojas y, entre los agentes, las epífitas y líquenes —de bajo impacto estructural, manejables con poda sanitaria y control de epífitas—. Las enfermedades estructuralmente relevantes (pudrición) son escasas pero decisivas para la seguridad.",size=10,after=6)
H("5.4. Casos de atención estructural (prioridad de seguridad)",2)
for x in ["A06 (ciprés común): riesgo de caída Alto → DERRIBO (Sección 7). Es el único con este nivel.",
          "AV02 (molle) y A25 (arabisco): PUDRICIÓN DEL TRONCO. Requieren evaluación instrumental del fuste (resistógrafo/tomografía) y monitoreo; reevaluar derribo si el defecto progresa.",
          "A07 (ciprés vela) y AV04 (acacia): RAÍZ en mal estado → puntos críticos de anclaje. Inspección radicular y monitoreo prioritario.",
          "A04 (ciprés vela): fuste en mal estado con pudrición de ramas y afección fitosanitaria; poda sanitaria y tratamiento (su riesgo de caída es Bajo por el porte moderado)."]:
    bullet(x)
# ficha de salud por árbol (landscape)
shl=doc.add_section(WD_SECTION.NEW_PAGE); shl.orientation=WD_ORIENT.LANDSCAPE
shl.page_width,shl.page_height=Cm(29.7),Cm(21); shl.top_margin=Cm(1.5); shl.bottom_margin=Cm(1.5); shl.left_margin=Cm(1.5); shl.right_margin=Cm(1.5)
H("5.5. Ficha de salud por árbol",1)
P("Estado fitosanitario por componente (verde = bueno · naranja = regular · rojo = malo · gris = no visible/NA) y presencia/ubicación de enfermedades y plagas.",size=9.5,after=6)
th=doc.add_table(rows=1,cols=11); th.style="Table Grid"; th.alignment=WD_TABLE_ALIGNMENT.CENTER
hdr_row(th,["ID","Especie (común)","Raíz","Fuste","Corteza","Ramas","Hojas","Cima","Copa","Enfermedades (ubicación)","Plagas (ubicación)"])
hw=[Cm(1.0),Cm(3.5),Cm(1.35),Cm(1.35),Cm(1.4),Cm(1.4),Cm(1.35),Cm(1.3),Cm(1.55),Cm(6.1),Cm(4.9)]
for grp_label,grp in [("PARQUE CENTRAL",D.parque),("AVENIDA EL ORO (fuera del parque)",D.avenida)]:
    grow=th.add_row().cells; grow[0].merge(grow[10])
    ctext(grow[0],grp_label,bold=True,white=True,size=8,align="left"); set_bg(grow[0],"37474F")
    for t in grp:
        row=th.add_row().cells
        ctext(row[0],t["id"],size=7.5,align="center",bold=True)
        ctext(row[1],"%s (%s)"%(t["especie"],t["comun"]),size=7.5)
        for ci,c in enumerate(D.FITO_COMPONENTS):
            ctext(row[2+ci],t["fito"][c],size=7,align="center"); set_bg(row[2+ci],fito_fill_hex(t["fito"][c]))
        enf=", ".join(t["enfermedades"]) or "—"
        if t["ubic_enf"]: enf+="  · ubic: "+", ".join(t["ubic_enf"])
        pl=", ".join(t["plagas"]) or "—"
        if t["ubic_plaga"]: pl+="  · ubic: "+", ".join(t["ubic_plaga"])
        ctext(row[9],enf,size=7); ctext(row[10],pl,size=7)
for i,w in enumerate(hw):
    for r_ in th.rows: r_.cells[i].width=w
# volver a portrait
spr=doc.add_section(WD_SECTION.NEW_PAGE); spr.orientation=WD_ORIENT.PORTRAIT
spr.page_width,spr.page_height=Cm(21),Cm(29.7); spr.top_margin=Cm(2); spr.bottom_margin=Cm(2); spr.left_margin=Cm(2.2); spr.right_margin=Cm(2.2)

# ---------- 5 CAÍDA ----------
H("6. Análisis del riesgo de caída (énfasis)",1)
P("El riesgo de caída es el criterio central por su relación directa con la seguridad de las personas. Distribución obtenida:",after=4)
tc=doc.add_table(rows=1,cols=3); tc.style="Table Grid"; tc.alignment=WD_TABLE_ALIGNMENT.CENTER
hdr_row(tc,["Riesgo de caída","N° de árboles","%"])
for lvl in ["Alto","Medio","Bajo"]:
    row=tc.add_row().cells
    ctext(row[0],lvl,bold=True,color=RISKC[lvl],size=9,align="center"); ctext(row[1],str(D.caida_counts[lvl]),size=9,align="center"); ctext(row[2],"%.0f%%"%(100*D.caida_counts[lvl]/D.n),size=9,align="center")
P("",after=4)
P("Hallazgo principal. De los %d árboles, UN (1) solo individuo presenta riesgo de caída ALTO: el ciprés común A06 (código %s, ref. «%s»), único con recomendación de DERRIBO en campo. %d árboles presentan caída Media —manejables con poda de reequilibrio, tratamiento y vigilancia— y %d caída Baja."%(D.n,D.DERRIBO_TREE['codigo'],D.DERRIBO_TREE['ref'],D.caida_counts['Medio'],D.caida_counts['Bajo']),after=6)
P("Este resultado es determinante: aunque el %.0f%% del arbolado figura en riesgo total «Alto», ese nivel se asocia mayoritariamente a fustes inclinados y a daños de raíces sobre el adoquinado y el concreto —el problema que motivó la solicitud— y NO a una probabilidad real de volcamiento. Se establecen como árboles de VIGILANCIA por sus defectos estructurales: A07 (raíz en mal estado, 17,8 m), A14 y A20 (fustes muy inclinados) y, en la avenida, AV02 (pudrición del tronco en un molle) y AV04 (acacia vieja con raíz y fuste en mal estado)."%(100*D.risk_counts['Alto']/D.n),after=6)
P("Recomendación de método: para A06, y ante dudas sobre ejemplares de gran porte o con pudrición (AV02, AV04), se aconseja confirmar el diagnóstico con evaluación instrumental (resistógrafo o tomografía sónica) antes de cualquier decisión de derribo.",after=8)

# ---------- 6 CIPRESES ----------
H("7. Análisis individual de los cipreses",1)
P("Atendiendo a la preocupación del Municipio y al valor patrimonial de estas especies, se analiza cada uno de los %d cipreses (4 Cupressus sempervirens — ciprés vela; 3 Hesperocyparis macrocarpa — ciprés común). Ambas especies son introducidas (Mediterráneo y California); su valor es cultural, histórico y paisajístico —reconocido aquí— más que estrictamente ecológico. Bajo la ficha de cada uno se reserva espacio para tres fotografías y se indica su dirección en ArboLEC."%len(D.cupres),after=6)
for t in D.cupres:
    H("Ciprés %s — %s (%s)"%(t["id"],t["especie"],t["comun"]),2,color=(ROJO if t["cat"]=="DERRIBO" else VERDE))
    tt=doc.add_table(rows=1,cols=4); tt.style="Table Grid"
    data=[("Código / Ref. campo","%s / %s"%(t["codigo"],t["ref"] or "s/n"),"Coordenadas","%s, %s"%(fnum(t["lat"],6),fnum(t["lon"],6))),
          ("Diámetro / Altura","%s cm / %s m"%(fnum(t["d"]),fnum(t["ht"])),"Copa (NS×EW)","%s × %s m"%(fnum(t["copaNS"]),fnum(t["copaEW"]))),
          ("Rectitud / Espacio","%s / %s"%(t["rectitud"],t["espacio"]),"Follaje","%s%%"%t["follaje"]),
          ("Raíz / Fuste (estado)","%s / %s"%(t["raiz"],t["fuste_est"]),"Riesgo total (valor)","%s (%s)"%(t["riesgo"],fnum(t["valriesgo"],2))),
          ("RIESGO DE CAÍDA",t["caida"],"Afect. infraestructura (raíz)",t["afec_raiz"])]
    first=True
    for a,b,c,d in data:
        row=tt.rows[0].cells if first else tt.add_row().cells; first=False
        ctext(row[0],a,bold=True,size=8.5); ctext(row[1],b,size=8.5,bold=(a=="RIESGO DE CAÍDA"),color=(RISKC.get(b) if a=="RIESGO DE CAÍDA" else None))
        ctext(row[2],c,bold=True,size=8.5); ctext(row[3],d,size=8.5)
    pv=doc.add_paragraph(); pv.alignment=WD_ALIGN_PARAGRAPH.JUSTIFY
    rl=pv.add_run("Veredicto: "); rl.bold=True; rl.font.size=Pt(10)
    rc=pv.add_run(CAT[t["cat"]]+". "); rc.bold=True; rc.font.size=Pt(10)
    rc.font.color.rgb=(ROJO if t["cat"]=="DERRIBO" else (NARANJA if t["cat"]=="CONSERVAR-INT" else VERDE))
    rf=pv.add_run(t["fund"]); rf.font.size=Pt(10); pv.paragraph_format.space_after=Pt(3)
    url_par(t)
    photo_strip(["Foto 1 — %s"%t["id"],"Foto 2 — %s"%t["id"],"Foto 3 — %s"%t["id"]],tid=t["id"])
    P("",after=4)
P("Síntesis de los cipreses: de los %d cipreses se recomienda el derribo de UNO (A06) por riesgo de caída alto y alta exposición; los %d restantes se CONSERVAN con poda sanitaria, manejo de epífitas, tratamiento de daños mecánicos, retiro de clavos/alambres y, en A07, monitoreo prioritario del anclaje."%(len(D.cupres),len(D.cupres)-1),after=8)

# ---------- 7 TABLA VEREDICTO (landscape) ----------
s=doc.add_section(WD_SECTION.NEW_PAGE); s.orientation=WD_ORIENT.LANDSCAPE
s.page_width,s.page_height=Cm(29.7),Cm(21); s.top_margin=Cm(1.5); s.bottom_margin=Cm(1.5); s.left_margin=Cm(1.5); s.right_margin=Cm(1.5)
H("8. Tabla resumen: conservación vs. derribo (todo el inventario)",1)
P("Código de color: rojo = Derribo; naranja = Conservar con intervención; verde = Conservar. De los %d árboles: %d derribo, %d conservación con intervención y %d conservación con mantenimiento ordinario."%(D.n,D.n_derribo,D.n_int,D.n_cons),size=9.5,after=6)
tv=doc.add_table(rows=1,cols=9); tv.style="Table Grid"; tv.alignment=WD_TABLE_ALIGNMENT.CENTER
hdr_row(tv,["ID","Especie (común)","Ref.","D (cm)","H (m)","Riesgo total","Caída","VEREDICTO","Fundamento"])
widths=[Cm(1.1),Cm(4.0),Cm(1.6),Cm(1.2),Cm(1.2),Cm(1.7),Cm(1.4),Cm(3.0),Cm(9.9)]
for grp_label,grp in [("PARQUE CENTRAL",D.parque),("AVENIDA EL ORO (fuera del parque)",D.avenida)]:
    grow=tv.add_row().cells; grow[0].merge(grow[8])
    ctext(grow[0],grp_label,bold=True,white=True,size=8.5,align="left"); set_bg(grow[0],"37474F")
    for t in grp:
        row=tv.add_row().cells
        vals=[t["id"],"%s (%s)"%(t["especie"],t["comun"]),t["ref"] or "-",fnum(t["d"]),fnum(t["ht"]),
              "%s (%s)"%(t["riesgo"],fnum(t["valriesgo"],2)),t["caida"],CAT[t["cat"]],t["fund"]]
        for i,v in enumerate(vals):
            ctext(row[i],v,size=7.5,align=("left" if i in (1,8) else "center"),bold=(i in (0,7)))
            set_bg(row[i],CATF[t["cat"]])
        row[6].paragraphs[0].runs[0].font.color.rgb=RISKC.get(t["caida"],GRIS); row[6].paragraphs[0].runs[0].bold=True
for i,w in enumerate(widths):
    for r_ in tv.rows: r_.cells[i].width=w

# ---------- 8 FOTOS (portrait) ----------
s2=doc.add_section(WD_SECTION.NEW_PAGE); s2.orientation=WD_ORIENT.PORTRAIT
s2.page_width,s2.page_height=Cm(21),Cm(29.7); s2.top_margin=Cm(2); s2.bottom_margin=Cm(2); s2.left_margin=Cm(2.2); s2.right_margin=Cm(2.2)
H("9. Registro fotográfico por árbol",1)
P("Las fotografías de cada árbol están publicadas en la plataforma ArboLEC junto con su ficha completa; se indica la dirección URL individual y se reserva espacio para tres fotografías por árbol, a insertar en la versión final del expediente municipal.",after=8)
for grp_label,grp in [("PARQUE CENTRAL",D.parque),("AVENIDA EL ORO (fuera del parque)",D.avenida)]:
    P(grp_label,size=11,bold=True,color=AZUL,after=4)
    for t in grp:
        catcol=ROJO if t["cat"]=="DERRIBO" else (NARANJA if t["cat"]=="CONSERVAR-INT" else VERDE)
        p=doc.add_paragraph()
        r1=p.add_run("%s — %s (%s)"%(t["id"],t["especie"],t["comun"])); r1.bold=True; r1.font.size=Pt(10); r1.font.color.rgb=VERDE
        r2=p.add_run("   ·   Código: %s · Ref.: %s\n"%(t["codigo"],t["ref"] or "s/n")); r2.font.size=Pt(9)
        r3=p.add_run("Coord.: %s, %s · D=%s cm · H=%s m · Follaje %s%% · Riesgo total: %s (%s) · Caída: "%(fnum(t["lat"],6),fnum(t["lon"],6),fnum(t["d"]),fnum(t["ht"]),t["follaje"],t["riesgo"],fnum(t["valriesgo"],2))); r3.font.size=Pt(9)
        r4=p.add_run("%s"%t["caida"]); r4.font.size=Pt(9); r4.font.color.rgb=RISKC.get(t["caida"],GRIS)
        r5=p.add_run("   ·   Veredicto: %s\n"%CAT[t["cat"]]); r5.bold=True; r5.font.size=Pt(9.5); r5.font.color.rgb=catcol
        r6=p.add_run("Ficha del árbol: "); r6.bold=True; r6.font.size=Pt(8.5)
        r7=p.add_run(t["url"]); r7.font.size=Pt(8.5); r7.font.color.rgb=AZUL
        p.paragraph_format.space_after=Pt(2)
        photo_strip(["Foto 1 — %s"%t["id"],"Foto 2 — %s"%t["id"],"Foto 3 — %s"%t["id"]],height=2.7,tid=t["id"])
        P("",after=4)

# ---------- 9 CONCLUSIONES ----------
H("10. Conclusiones y recomendaciones",1)
P("Conclusiones:",bold=True,after=2)
for x in ["Se evaluaron %d árboles (%d en el Parque Central y %d en la Avenida El Oro) de %d especies; la familia de los cipreses es la dominante y de mayor valor patrimonial."%(D.n,D.n_parque,D.n_av,len(D.species)),
          "En seguridad existe UN único punto crítico de riesgo de caída Alto: el árbol A06 (ciprés común, código %s, junto a los baños), el más grande del parque y único con recomendación de derribo en campo."%D.DERRIBO_TREE["codigo"],
          "Los demás %d árboles se CONSERVAN; su riesgo total «Alto» se explica mayoritariamente por daño de raíces a la infraestructura y por inclinación del fuste, condiciones manejables sin derribo."%(D.n-1),
          "En la Avenida El Oro destacan dos casos que, sin ser derribo, requieren evaluación estructural y monitoreo prioritario: AV02 (molle con pudrición del tronco) y AV04 (acacia vieja con raíz y fuste en mal estado).",
          "El daño al adoquinado y al concreto por raíces —motivo central de la solicitud— se atiende con manejo de raíces (barreras, reparación) y no justifica, por sí mismo, talar árboles sanos."]:
    bullet(x)
P("Recomendaciones:",bold=True,after=2)
for x in ["Ejecutar el DERRIBO técnico controlado del árbol A06, previa verificación instrumental si se dispone del equipo, con personal especializado y medidas de seguridad por su cercanía a los baños.",
          "Programar la CONSERVACIÓN con intervención de los demás árboles: poda sanitaria y de reequilibrio, tratamiento fitosanitario, manejo de epífitas y retiro de clavos/alambres.",
          "Realizar EVALUACIÓN ESTRUCTURAL (resistógrafo/tomografía) de AV02 y AV04, y MONITOREO prioritario de A07 (raíz en mal estado), A14 y A20 (muy inclinados); reevaluar si progresa el deterioro o la inclinación.",
          "Implementar manejo de raíces (barreras anti-raíz, reparación del adoquinado) en los árboles con afectación Alta a infraestructura (A10, A23, AV03 y otros), preservando el árbol.",
          "Coordinar con la empresa eléctrica la poda de despeje donde hay interferencia Alta con redes aéreas (AV04).",
          "Considerar un plan de reposición y sucesión a mediano plazo, priorizando especies nativas (nogal andino, molle, arupo), y mantener actualizado el inventario en ArboLEC."]:
    bullet(x)
P("",after=8)
P("____________________________________",align="center",after=0)
P("Ph.D. Darwin Alexander Pucha Cofrep",bold=True,align="center",after=0)
P("Responsable del Laboratorio de Dendrocronología — Universidad Nacional de Loja",size=9.5,align="center",color=GRIS,after=0)
P("Loja, 11 de julio de 2026",size=9.5,align="center",color=GRIS)

doc.save(OUT)
print("Word informe:",OUT)
