# -*- coding: utf-8 -*-
"""Genera el PDF del informe técnico (ReportLab)."""
import os
from PIL import Image as PILImage
from reportlab.lib import colors
from reportlab.lib.pagesizes import A4, landscape
from reportlab.lib.units import cm
from reportlab.lib.styles import ParagraphStyle
from reportlab.lib.enums import TA_JUSTIFY, TA_CENTER, TA_LEFT, TA_RIGHT
from reportlab.platypus import (BaseDocTemplate, PageTemplate, Frame, Paragraph, Spacer,
    Table, TableStyle, PageBreak, NextPageTemplate, KeepTogether, Image, ListFlowable, ListItem)
import _data as D

OUTDIR = os.path.join(os.path.dirname(__file__), "..")
VERDE=colors.HexColor("#1B5E20"); GRIS=colors.HexColor("#424242"); ROJO=colors.HexColor("#B71C1C")
NARANJA=colors.HexColor("#E65A00"); AZUL=colors.HexColor("#0D47A1")
HDR=colors.HexColor("#1B5E20"); ROJO_F=colors.HexColor("#F4C7C3"); NAR_F=colors.HexColor("#FCE5CD"); VER_F=colors.HexColor("#D9EAD3")
PH_BG=colors.HexColor("#F2F2F2")
CAT=D.CAT_LABEL; CATF={"DERRIBO":ROJO_F,"CONSERVAR-INT":NAR_F,"CONSERVAR":VER_F}
RC={"Alto":ROJO,"Medio":NARANJA,"Bajo":VERDE}
fnum=D.fnum

def S(name,**kw):
    base=dict(fontName="Helvetica",fontSize=10.5,leading=14,alignment=TA_JUSTIFY); base.update(kw)
    return ParagraphStyle(name,**base)
body=S("body"); bodyc=S("bodyc",alignment=TA_CENTER)
h1=S("h1",fontName="Helvetica-Bold",fontSize=15,textColor=VERDE,spaceBefore=10,spaceAfter=6,alignment=TA_LEFT,leading=18)
small=S("small",fontSize=8.5,leading=11); smallc=S("smallc",fontSize=8.5,leading=11,alignment=TA_CENTER)
cellst=S("cell",fontSize=7.6,leading=9); cellc=S("cellc",fontSize=7.6,leading=9,alignment=TA_CENTER)
def P(t,st=body): return Paragraph(t,st)
def bullets(items,st=body):
    return ListFlowable([ListItem(P(x,st),leftIndent=10) for x in items],bulletType="bullet",start="•",leftIndent=14)
def img_scaled(path,width):
    iw,ih=PILImage.open(path).size
    return Image(path,width=width,height=width*ih/iw)
def cell_image(path, w, h):
    iw, ih = PILImage.open(path).size
    sc = min(w/iw, h/ih)
    return Image(path, width=iw*sc, height=ih*sc)
def photo_strip(labels, h=2.9*cm, tid=None):
    photos = D.tree_photos(tid) if tid else [None]*len(labels)
    cw = (17.0/len(labels))*cm
    cells=[]
    for i, lab in enumerate(labels):
        ph = photos[i] if i < len(photos) else None
        if ph:
            cells.append(cell_image(ph, cw-0.2*cm, h-0.2*cm))
        else:
            cells.append(P("<br/>[ %s ]<br/><i>(insertar foto)</i>"%lab, S("ph",fontSize=8,alignment=TA_CENTER,textColor=GRIS,leading=11)))
    t=Table([cells],colWidths=[cw]*len(labels),rowHeights=[h])
    t.setStyle(TableStyle([("BOX",(0,0),(-1,-1),0.5,colors.grey),("INNERGRID",(0,0),(-1,-1),0.5,colors.grey),
        ("BACKGROUND",(0,0),(-1,-1),PH_BG),("VALIGN",(0,0),(-1,-1),"MIDDLE"),("ALIGN",(0,0),(-1,-1),"CENTER")]))
    return t
def header_cells(labels):
    return [P(x,S("hh",fontName="Helvetica-Bold",fontSize=8,textColor=colors.white,alignment=TA_CENTER)) for x in labels]

def build_report(path):
    doc=BaseDocTemplate(path,pagesize=A4,leftMargin=2*cm,rightMargin=2*cm,topMargin=1.8*cm,bottomMargin=1.6*cm)
    fw,fh=A4
    pf=Frame(2*cm,1.6*cm,fw-4*cm,fh-3.4*cm,id="p")
    lw,lh=landscape(A4); lf=Frame(1.3*cm,1.3*cm,lw-2.6*cm,lh-2.6*cm,id="l")
    def foot(canvas,d):
        canvas.saveState(); canvas.setFont("Helvetica",7.5); canvas.setFillColor(GRIS)
        canvas.drawString(2*cm,1*cm,"UNL · Laboratorio de Dendrocronología — Informe Arbolado, Parque Central y Avenida El Oro (Saraguro)")
        canvas.drawRightString(d.pagesize[0]-2*cm,1*cm,"Pág. %d"%canvas.getPageNumber()); canvas.restoreState()
    doc.addPageTemplates([
        PageTemplate(id="portrait",frames=[pf],pagesize=A4,onPage=foot),
        PageTemplate(id="land",frames=[lf],pagesize=landscape(A4),onPage=foot)])
    E=[]
    # PORTADA
    logos=Table([[img_scaled(D.LOGO_UNL,7.2*cm), img_scaled(D.LOGO_ARBOLEC,2.7*cm)]],colWidths=[9*cm,4*cm])
    logos.setStyle(TableStyle([("VALIGN",(0,0),(-1,-1),"MIDDLE"),("ALIGN",(0,0),(0,0),"LEFT"),("ALIGN",(1,0),(1,0),"RIGHT")]))
    E+=[logos,Spacer(1,0.5*cm),
        P("UNIVERSIDAD NACIONAL DE LOJA",S("x",fontName="Helvetica-Bold",fontSize=14,textColor=VERDE,alignment=TA_CENTER)),
        P("Facultad Agropecuaria y de Recursos Naturales Renovables · Laboratorio de Dendrocronología",smallc),
        Spacer(1,1.0*cm),
        P("INFORME TÉCNICO DE EVALUACIÓN DEL ARBOLADO URBANO",S("x",fontName="Helvetica-Bold",fontSize=19,textColor=VERDE,alignment=TA_CENTER,leading=23)),
        P("Parque Central y Avenida El Oro del cantón Saraguro",S("x",fontName="Helvetica-Bold",fontSize=15,textColor=GRIS,alignment=TA_CENTER)),
        Spacer(1,0.3*cm),
        P("Diagnóstico dendrométrico, estado fitosanitario y evaluación del riesgo de caída, con veredicto técnico de conservación o derribo",S("x",fontSize=11,textColor=GRIS,alignment=TA_CENTER,leading=15)),
        Spacer(1,1.0*cm),
        P("Solicitado por el Gobierno Autónomo Descentralizado Municipal Intercultural de Saraguro",smallc),
        P("Oficio Nro. 0286-A-GADMIS (19/05/2026) — Autorización Rectorado UNL-R-2026-2083-M (28/05/2026)",smallc),
        P("Referencia: UNL-SG-2026-0421-EX",smallc),
        Spacer(1,1.1*cm),
        P("Responsable técnico: Ph.D. Darwin Alexander Pucha Cofrep",S("x",fontSize=11,fontName="Helvetica-Bold",alignment=TA_CENTER)),
        P("Responsable del Laboratorio de Dendrocronología — Director de la Maestría en Biodiversidad y Cambio Climático",smallc),
        P("Equipo de campo: Darwin Pucha · Ariel Arévalo · Cristian Retete",smallc),
        Spacer(1,0.3*cm),
        P('Plataforma de consulta pública: <a href="%s" color="blue">%s</a>'%(D.BASE_URL,D.BASE_URL),smallc),
        P("Loja, 11 de julio de 2026",S("x",fontSize=10.5,fontName="Helvetica-Bold",textColor=GRIS,alignment=TA_CENTER)),
        PageBreak()]
    # 1-2
    E+=[P("1. Antecedentes y justificación",h1),
        P("El Gobierno Autónomo Descentralizado Municipal Intercultural de Saraguro, mediante Oficio Nro. 0286-A-GADMIS del 19 de mayo de 2026, suscrito por el Lic. Segundo Abel Sarango Quizhpe, Alcalde del cantón, solicitó a la Universidad Nacional de Loja el apoyo técnico para la inspección, evaluación y emisión de un criterio especializado sobre el estado actual de los árboles del Parque Central de Saraguro."),
        P("El Municipio expuso su preocupación por: (i) el crecimiento radicular de varios árboles, que ha ocasionado afectaciones en verjas, adoquinado y estructuras de concreto; y (ii) el estado fitosanitario y la estabilidad estructural frente a las fuertes corrientes de viento de la zona, situación que podría representar un riesgo para la seguridad ciudadana. El oficio destaca la presencia histórica de cipreses (registros fotográficos de la década de 1960), de reconocido valor simbólico y patrimonial."),
        P("Mediante Memorando Nro. UNL-R-2026-2083-M del 28 de mayo de 2026, el Rector, Dr. Nikolay Aguirre Mendoza, autorizó la participación del Ph.D. Darwin Alexander Pucha Cofrep, Responsable del Laboratorio de Dendrocronología."),
        P("El presente informe entrega un criterio técnico OBJETIVO y verificable para conservar o derribar cada árbol, con énfasis en el riesgo de caída y en los cipreses señalados. La evaluación se amplió, a solicitud del Municipio, a cuatro árboles de la isleta de la Avenida El Oro, contiguos al parque. La recomendación de derribo se emite con carácter restrictivo: solo cuando la evidencia demuestra un riesgo inaceptable para las personas o los bienes."),
        P("2. Objetivos",h1),
        bullets([
            "Inventariar y diagnosticar (dendrométrica y fitosanitariamente) el arbolado del Parque Central de Saraguro y de la isleta de la Avenida El Oro.",
            "Evaluar el nivel de riesgo de cada árbol, con énfasis en el riesgo de caída frente a las condiciones de viento y de sitio.",
            "Analizar de forma individual los cipreses del parque, por su relevancia patrimonial y por ser objeto de la preocupación municipal.",
            "Emitir un veredicto técnico por árbol —conservación o derribo— con su respectivo fundamento."]),
        PageBreak()]
    # 3 METODOLOGÍA
    E+=[P("3. Metodología",h1),
        P('El levantamiento se realizó mediante inspección visual en campo (metodología tipo VTA, <i>Visual Tree Assessment</i>) y registro georreferenciado con GPS, sistematizado en la plataforma institucional ArboLEC (<a href="%s" color="blue">%s</a>), donde el inventario queda publicado para consulta pública. Cada árbol posee una ficha individual accesible por su código.'%(D.BASE_URL,D.BASE_URL)),
        P("Por cada individuo se registraron: identificación taxonómica, ubicación (coordenadas y código Plus Code), variables dendrométricas (circunferencia y diámetro, altura total y comercial, dimensiones de copa, follaje), estado de madurez, rectitud de fuste y espacio de crecimiento; la condición estructural y sanitaria de raíz, fuste, corteza, ramas, hojas, cima y copa; enfermedades, plagas y daños físicos; y los factores de riesgo (caída, afectación a construcciones, circulación, daño de raíces e interferencia con redes aéreas)."),
        P("<b>Escala de riesgo.</b> Cada factor se califica en Bajo, Medio y Alto; el sistema integra un Riesgo total (categórico) y un Valor de riesgo (índice ~1,0–3,0). Se distinguen dos conceptos clave:"),
        bullets([
            "<b>Riesgo total / valor de riesgo:</b> índice global que combina TODOS los factores (incluido el daño de raíces a la infraestructura). Un valor alto no implica, por sí solo, peligro de caída.",
            "<b>Riesgo de caída:</b> probabilidad de fallo estructural (volcamiento o rotura). Criterio rector para una eventual recomendación de derribo."]),
        P("<b>Criterio de veredicto.</b> Se recomienda DERRIBO únicamente cuando concurre un riesgo de caída Alto (o fallo estructural irreversible) con exposición de personas o bienes, y cuando el defecto no es corregible por poda o tratamiento. En los demás casos, CONSERVACIÓN con o sin intervención. Criterio deliberadamente conservador que protege la seguridad ciudadana y el patrimonio arbóreo."),
        Spacer(1,6),
        P("3.1. Mapa de ubicación de los árboles",S("h2",fontName="Helvetica-Bold",fontSize=12,textColor=VERDE,spaceAfter=4)),
        img_scaled(D.MAPA,17*cm),
        P("Figura 1. Ubicación georreferenciada (GPS, WGS84) de los %d árboles evaluados en el Parque Central y en la isleta de la Avenida El Oro, con su veredicto técnico. En rojo, el único árbol propuesto para derribo (A06)."%D.n,S("cap",fontSize=8.5,textColor=GRIS,alignment=TA_CENTER,leading=11)),
        Spacer(1,8),
        P("3.2. Registro fotográfico del trabajo de campo",S("h2",fontName="Helvetica-Bold",fontSize=12,textColor=VERDE,spaceAfter=4)),
        P("Se reserva el siguiente espacio para las fotografías del levantamiento en campo (mediciones dendrométricas, inspección visual y georreferenciación):",small),
        Spacer(1,3),photo_strip(["Campo 1","Campo 2","Campo 3"],h=3.4*cm),
        Spacer(1,3),photo_strip(["Campo 4","Campo 5","Campo 6"],h=3.4*cm),
        PageBreak()]
    # 4 RESULTADOS
    E+=[P("4. Resultados generales del inventario",h1),
        P("Se inventariaron %d árboles: %d en el Parque Central y %d en la isleta de la Avenida El Oro, de %d especies y %d familias. La familia Cupressaceae (cipreses) es la más representada, con %d individuos (%.0f%%). El rango dendrométrico va de %s a %s m de altura y de %s a %s cm de diámetro; el mayor es el ciprés común A06 (código %s), con 27,1 m y 118,2 cm."%(
            D.n,D.n_parque,D.n_av,len(D.species),len(D.families),len(D.cupres),100*len(D.cupres)/D.n,
            fnum(min(float(t['ht']) for t in D.trees)),fnum(max(float(t['ht']) for t in D.trees)),
            fnum(min(float(t['d']) for t in D.trees)),fnum(max(float(t['d']) for t in D.trees)),D.DERRIBO_TREE['codigo']))]
    sp_sorted=sorted(D.species.items(),key=lambda x:(-x[1],x[0]))
    data=[header_cells(["Especie","Nombre común","Familia","N°"])]
    for sp,cnt in sp_sorted: data.append([P(sp,cellst),P(D.comun_by.get(sp,""),cellst),P(D.fam_by.get(sp,""),cellst),P(str(cnt),cellc)])
    tsp=Table(data,colWidths=[5.5*cm,4.5*cm,4*cm,1.3*cm],repeatRows=1)
    tsp.setStyle(TableStyle([("BACKGROUND",(0,0),(-1,0),HDR),("GRID",(0,0),(-1,-1),0.4,colors.grey),
        ("VALIGN",(0,0),(-1,-1),"MIDDLE"),("ROWBACKGROUNDS",(0,1),(-1,-1),[colors.white,colors.HexColor("#F3F7F3")])]))
    E+=[Spacer(1,4),P("Composición por especie:",S("b",fontName="Helvetica-Bold",fontSize=10)),tsp,Spacer(1,8)]
    interp={"Alto":"Requieren intervención y/o seguimiento","Medio":"Intervención preventiva / monitoreo","Bajo":"Mantenimiento ordinario"}
    d2=[header_cells(["Nivel de riesgo total","N°","%","Interpretación"])]
    for lvl in ["Alto","Medio","Bajo"]: d2.append([P(lvl,cellc),P(str(D.risk_counts[lvl]),cellc),P("%.0f%%"%(100*D.risk_counts[lvl]/D.n),cellc),P(interp[lvl],cellst)])
    t2=Table(d2,colWidths=[4.5*cm,1.5*cm,1.5*cm,7.8*cm])
    stl=[("BACKGROUND",(0,0),(-1,0),HDR),("GRID",(0,0),(-1,-1),0.4,colors.grey),("VALIGN",(0,0),(-1,-1),"MIDDLE")]
    for i,lvl in enumerate(["Alto","Medio","Bajo"],1): stl.append(("TEXTCOLOR",(0,i),(0,i),RC[lvl]))
    t2.setStyle(TableStyle(stl))
    E+=[P("Distribución del riesgo total:",S("b",fontName="Helvetica-Bold",fontSize=10)),t2,
        P("Nota: el riesgo total integra todos los factores, incluido el daño de raíces a la infraestructura; por ello varios árboles estructuralmente sanos figuran en riesgo «Alto» sin ser candidatos a derribo.",S("i",fontSize=8.5,textColor=GRIS,leading=11)),
        PageBreak()]
    # 5 SALUD / ESTADO FITOSANITARIO
    h2s=S("h2",fontName="Helvetica-Bold",fontSize=12,textColor=VERDE,spaceBefore=8,spaceAfter=4)
    FITO_FILL={"buena":VER_F,"regular":NAR_F,"mala":ROJO_F,"na":PH_BG}
    def fito_fill(v): return FITO_FILL[D.fito_color_key(v)]
    _inf=img_scaled(D.INFOGRAFIA,14.5*cm); _inf.hAlign="CENTER"
    E+=[P("5. Salud y estado fitosanitario del arbolado",h1),
        P("Esta sección resume la salud del arbolado a partir de cinco categorías evaluadas en campo: (i) <b>estado fitosanitario</b> de sus componentes (raíz, fuste, corteza, ramas, hojas, cima y copa); (ii) <b>presencia de enfermedades</b>; (iii) <b>ubicación de las enfermedades</b>; (iv) <b>presencia de plagas</b>; y (v) <b>ubicación de las plagas</b>. El detalle por árbol se presenta en la ficha de salud (5.5). La Figura 2 sintetiza, en un «árbol promedio», el estado más frecuente (moda) de cada componente."),
        _inf,
        P("Figura 2. «El árbol promedio de Saraguro»: cada elemento se colorea según el valor más frecuente (moda) de los 40 árboles (verde = bueno, ámbar = regular, rojo = malo, gris = no visible).",S("cap",fontSize=8.5,textColor=GRIS,alignment=TA_CENTER,leading=11)),
        P("5.1. Estado fitosanitario por componente",h2s)]
    fh=[header_cells(["Componente","Buena/Bueno","Regular","Mala/Malo","No visible/NA"])]
    for comp in D.FITO_COMPONENTS:
        d=D.fito_summary[comp]
        fh.append([P(comp,cellst),P(str(d["Buena/Bueno"]),cellc),P(str(d["Regular"]),cellc),P(str(d["Mala/Malo"]),cellc),P(str(d["No visible/NA"]),cellc)])
    tfito=Table(fh,colWidths=[3.5*cm,3*cm,2.6*cm,2.6*cm,3*cm])
    tfito.setStyle(TableStyle([("BACKGROUND",(0,0),(-1,0),HDR),("GRID",(0,0),(-1,-1),0.4,colors.grey),
        ("VALIGN",(0,0),(-1,-1),"MIDDLE"),("TEXTCOLOR",(1,1),(1,-1),VERDE),("TEXTCOLOR",(3,1),(3,-1),ROJO),
        ("FONTNAME",(1,1),(1,-1),"Helvetica-Bold"),("FONTNAME",(3,1),(3,-1),"Helvetica-Bold")]))
    E+=[tfito,
        P("Lectura: la mayoría de los componentes están en estado Bueno o Regular; los estados «Malo» se concentran en pocos árboles (ver 5.4) y orientan las intervenciones de poda sanitaria, tratamiento y —donde hay pudrición o raíz comprometida— evaluación estructural.",S("i",fontSize=8.5,textColor=GRIS,leading=11)),
        P("5.2. Presencia de enfermedades",h2s)]
    eh=[header_cells(["Enfermedad","N° de árboles","% del total"])]
    for k,v in D.enf_freq.items(): eh.append([P(k,cellst),P(str(v),cellc),P("%.0f%%"%(100*v/D.n),cellc)])
    if len(eh)==1: eh.append([P("Sin registros",cellst),P("0",cellc),P("0%",cellc)])
    tenf=Table(eh,colWidths=[7*cm,3*cm,3*cm]); tenf.setStyle(TableStyle([("BACKGROUND",(0,0),(-1,0),HDR),
        ("GRID",(0,0),(-1,-1),0.4,colors.grey),("VALIGN",(0,0),(-1,-1),"MIDDLE"),
        ("ROWBACKGROUNDS",(0,1),(-1,-1),[colors.white,colors.HexColor("#F3F7F3")])]))
    E+=[tenf, P("5.3. Presencia de plagas",h2s)]
    ph=[header_cells(["Plaga / agente","N° de árboles","% del total"])]
    for k,v in D.plaga_freq.items(): ph.append([P(k,cellst),P(str(v),cellc),P("%.0f%%"%(100*v/D.n),cellc)])
    if len(ph)==1: ph.append([P("Sin registros",cellst),P("0",cellc),P("0%",cellc)])
    tpl=Table(ph,colWidths=[7*cm,3*cm,3*cm]); tpl.setStyle(TableStyle([("BACKGROUND",(0,0),(-1,0),HDR),
        ("GRID",(0,0),(-1,-1),0.4,colors.grey),("VALIGN",(0,0),(-1,-1),"MIDDLE"),
        ("ROWBACKGROUNDS",(0,1),(-1,-1),[colors.white,colors.HexColor("#F3F7F3")])]))
    E+=[tpl,
        P("La afección más frecuente es la decoloración de hojas y, entre los agentes, las epífitas y líquenes —de bajo impacto estructural, manejables con poda sanitaria y control de epífitas—. Las enfermedades estructuralmente relevantes (pudrición) son escasas pero decisivas para la seguridad (ver abajo).",S("cap2",fontSize=9,leading=12)),
        P("5.4. Casos de atención estructural (prioridad de seguridad)",h2s),
        bullets([
            "<b>A06</b> (ciprés común): riesgo de caída Alto → DERRIBO (Sección 7). Es el único con este nivel.",
            "<b>AV02</b> (molle) y <b>A25</b> (arabisco): PUDRICIÓN DEL TRONCO. Requieren evaluación instrumental del fuste (resistógrafo/tomografía) y monitoreo; reevaluar derribo si el defecto progresa.",
            "<b>A07</b> (ciprés vela) y <b>AV04</b> (acacia): RAÍZ en mal estado → puntos críticos de anclaje. Inspección radicular y monitoreo prioritario.",
            "<b>A04</b> (ciprés vela): fuste en mal estado con pudrición de ramas y afección fitosanitaria; poda sanitaria y tratamiento (su riesgo de caída es Bajo por el porte moderado).",]),
        NextPageTemplate("land"),PageBreak()]
    # 5.4 tabla de salud por árbol (landscape)
    E+=[P("5.5. Ficha de salud por árbol",h1),
        P("Estado fitosanitario por componente (verde = bueno · naranja = regular · rojo = malo · gris = no visible/NA) y presencia/ubicación de enfermedades y plagas.",small)]
    hh=["ID","Especie (común)","Raíz","Fuste","Corteza","Ramas","Hojas","Cima","Copa","Enfermedades (ubicación)","Plagas (ubicación)"]
    dd=[header_cells(hh)]
    hstyl=[("BACKGROUND",(0,0),(-1,0),HDR),("GRID",(0,0),(-1,-1),0.4,colors.grey),("VALIGN",(0,0),(-1,-1),"MIDDLE"),
           ("TOPPADDING",(0,0),(-1,-1),1.5),("BOTTOMPADDING",(0,0),(-1,-1),1.5)]
    ri=1
    for grp_label,grp in [("PARQUE CENTRAL",D.parque),("AVENIDA EL ORO (fuera del parque)",D.avenida)]:
        dd.append([P("<b>%s</b>"%grp_label,S("g",fontName="Helvetica-Bold",fontSize=7.5,textColor=colors.white))]+[P("")]*10)
        hstyl.append(("BACKGROUND",(0,ri),(-1,ri),colors.HexColor("#37474F"))); hstyl.append(("SPAN",(0,ri),(-1,ri))); ri+=1
        for t in grp:
            enf=", ".join(t["enfermedades"]) or "—"
            if t["ubic_enf"]: enf+="  · ubic: "+", ".join(t["ubic_enf"])
            pl=", ".join(t["plagas"]) or "—"
            if t["ubic_plaga"]: pl+="  · ubic: "+", ".join(t["ubic_plaga"])
            row=[P(t["id"],cellc),P("%s (%s)"%(t["especie"],t["comun"]),cellst)]
            row+=[P(t["fito"][c],cellc) for c in D.FITO_COMPONENTS]
            row+=[P(enf,cellst),P(pl,cellst)]
            dd.append(row)
            for ci,c in enumerate(D.FITO_COMPONENTS):
                hstyl.append(("BACKGROUND",(2+ci,ri),(2+ci,ri),fito_fill(t["fito"][c])))
            ri+=1
    thealth=Table(dd,colWidths=[1.0*cm,3.5*cm,1.35*cm,1.35*cm,1.4*cm,1.4*cm,1.35*cm,1.3*cm,1.55*cm,6.1*cm,4.9*cm],repeatRows=1)
    thealth.setStyle(TableStyle(hstyl))
    E+=[Spacer(1,4),thealth,NextPageTemplate("portrait"),PageBreak()]
    # 6 CAÍDA
    d3=[header_cells(["Riesgo de caída","N°","%"])]
    for lvl in ["Alto","Medio","Bajo"]: d3.append([P(lvl,cellc),P(str(D.caida_counts[lvl]),cellc),P("%.0f%%"%(100*D.caida_counts[lvl]/D.n),cellc)])
    t3=Table(d3,colWidths=[4.5*cm,2*cm,2*cm])
    stl3=[("BACKGROUND",(0,0),(-1,0),HDR),("GRID",(0,0),(-1,-1),0.4,colors.grey),("VALIGN",(0,0),(-1,-1),"MIDDLE")]
    for i,lvl in enumerate(["Alto","Medio","Bajo"],1): stl3.append(("TEXTCOLOR",(0,i),(0,i),RC[lvl]))
    t3.setStyle(TableStyle(stl3))
    E+=[P("6. Análisis del riesgo de caída (énfasis)",h1),
        P("El riesgo de caída es el criterio central por su relación directa con la seguridad de las personas. Distribución obtenida:"),
        t3,Spacer(1,6),
        P("<b>Hallazgo principal.</b> De los %d árboles, UN (1) solo individuo presenta riesgo de caída ALTO: el ciprés común A06 (código %s, ref. «%s»), único con recomendación de DERRIBO en campo. %d árboles presentan caída Media —manejables con poda de reequilibrio, tratamiento y vigilancia— y %d caída Baja."%(D.n,D.DERRIBO_TREE['codigo'],D.DERRIBO_TREE['ref'],D.caida_counts['Medio'],D.caida_counts['Bajo'])),
        P("Este resultado es determinante: aunque el %.0f%% del arbolado figura en riesgo total «Alto», ese nivel se asocia mayoritariamente a fustes inclinados y a daños de raíces sobre el adoquinado y el concreto —el problema que motivó la solicitud— y NO a una probabilidad real de volcamiento. Se establecen como árboles de VIGILANCIA por sus defectos estructurales: A07 (raíz en mal estado, 17,8 m), A14 y A20 (fustes muy inclinados) y, en la avenida, AV02 (pudrición del tronco en un molle) y AV04 (acacia vieja con raíz y fuste en mal estado)."%(100*D.risk_counts['Alto']/D.n)),
        P("<b>Recomendación de método:</b> para A06, y ante dudas sobre ejemplares de gran porte o con pudrición (AV02, AV04), se aconseja confirmar el diagnóstico con evaluación instrumental (resistógrafo o tomografía sónica) antes de cualquier decisión de derribo."),
        PageBreak()]
    # 6 CIPRESES
    E+=[P("7. Análisis individual de los cipreses",h1),
        P("Atendiendo a la preocupación del Municipio y al valor patrimonial de estas especies, se analiza cada uno de los %d cipreses (4 <i>Cupressus sempervirens</i> — ciprés vela; 3 <i>Hesperocyparis macrocarpa</i> — ciprés común). Ambas especies son introducidas (Mediterráneo y California); su valor es cultural, histórico y paisajístico —reconocido aquí— más que estrictamente ecológico. Bajo la ficha de cada uno se reserva espacio para tres fotografías y se indica su dirección en ArboLEC."%len(D.cupres))]
    for t in D.cupres:
        rows=[
            ["Código / Ref. campo","%s / %s"%(t["codigo"],t["ref"] or "s/n"),"Coordenadas","%s, %s"%(fnum(t["lat"],6),fnum(t["lon"],6))],
            ["Diámetro / Altura","%s cm / %s m"%(fnum(t["d"]),fnum(t["ht"])),"Copa (NS×EW)","%s × %s m"%(fnum(t["copaNS"]),fnum(t["copaEW"]))],
            ["Rectitud / Espacio","%s / %s"%(t["rectitud"],t["espacio"]),"Follaje","%s%%"%t["follaje"]],
            ["Raíz / Fuste (estado)","%s / %s"%(t["raiz"],t["fuste_est"]),"Riesgo total (valor)","%s (%s)"%(t["riesgo"],fnum(t["valriesgo"],2))],
            ["RIESGO DE CAÍDA",t["caida"],"Afect. infraestructura (raíz)",t["afec_raiz"]]]
        tb=Table([[P(a,S("k",fontName="Helvetica-Bold",fontSize=8.3,leading=10)),P(b,small),
                   P(c,S("k",fontName="Helvetica-Bold",fontSize=8.3,leading=10)),P(d,small)] for a,b,c,d in rows],
                 colWidths=[4*cm,4.5*cm,4.3*cm,4*cm])
        tb.setStyle(TableStyle([("GRID",(0,0),(-1,-1),0.4,colors.grey),("VALIGN",(0,0),(-1,-1),"MIDDLE"),
            ("BACKGROUND",(0,0),(0,-1),colors.HexColor("#EEF4EE")),("BACKGROUND",(2,0),(2,-1),colors.HexColor("#EEF4EE")),
            ("TEXTCOLOR",(1,4),(1,4),RC.get(t["caida"],GRIS)),("FONTNAME",(1,4),(1,4),"Helvetica-Bold")]))
        catcol=ROJO if t["cat"]=="DERRIBO" else (NARANJA if t["cat"]=="CONSERVAR-INT" else VERDE)
        block=[P("Ciprés %s — %s (%s)"%(t["id"],t["especie"],t["comun"]),
                 S("hc",fontName="Helvetica-Bold",fontSize=11.5,textColor=catcol,spaceBefore=6,spaceAfter=3)),
               tb,Spacer(1,3),
               P('<b>Veredicto: <font color="%s">%s.</font></b> %s'%(catcol.hexval(),CAT[t["cat"]],t["fund"]),body),Spacer(1,3),
               P('<b>Ficha del árbol:</b> <a href="%s" color="blue">%s</a>'%(t["url"],t["url"]),S("u",fontSize=8.5,textColor=AZUL,leading=11)),Spacer(1,3),
               photo_strip(["Foto 1 — %s"%t["id"],"Foto 2 — %s"%t["id"],"Foto 3 — %s"%t["id"]],tid=t["id"]),Spacer(1,8)]
        E.append(KeepTogether(block))
    E+=[P("<b>Síntesis de los cipreses:</b> de los %d cipreses se recomienda el derribo de UNO (A06) por riesgo de caída alto y alta exposición; los %d restantes se CONSERVAN con poda sanitaria, manejo de epífitas, tratamiento de daños mecánicos, retiro de clavos/alambres y, en A07, monitoreo prioritario del anclaje."%(len(D.cupres),len(D.cupres)-1)),
        NextPageTemplate("land"),PageBreak()]
    # 7 VEREDICTO (landscape)
    E+=[P("8. Tabla resumen: conservación vs. derribo (todo el inventario)",h1),
        P("Código de color: rojo = Derribo; naranja = Conservar con intervención; verde = Conservar. De los %d árboles: %d derribo, %d conservación con intervención y %d conservación con mantenimiento ordinario."%(D.n,D.n_derribo,D.n_int,D.n_cons),small)]
    dd=[header_cells(["ID","Especie (común)","Sitio","Ref.","D (cm)","H (m)","Riesgo total","Caída","VEREDICTO","Fundamento"])]
    styl=[("BACKGROUND",(0,0),(-1,0),HDR),("GRID",(0,0),(-1,-1),0.4,colors.grey),("VALIGN",(0,0),(-1,-1),"MIDDLE"),
          ("TOPPADDING",(0,0),(-1,-1),2),("BOTTOMPADDING",(0,0),(-1,-1),2)]
    ri=1
    for grp_label,grp in [("PARQUE CENTRAL",D.parque),("AVENIDA EL ORO (fuera del parque)",D.avenida)]:
        dd.append([P("<b>%s</b>"%grp_label,S("g",fontName="Helvetica-Bold",fontSize=8,textColor=colors.white))]+[P("",cellc)]*9)
        styl.append(("BACKGROUND",(0,ri),(-1,ri),colors.HexColor("#37474F"))); styl.append(("SPAN",(0,ri),(-1,ri))); ri+=1
        for t in grp:
            dd.append([P(t["id"],cellc),P("%s (%s)"%(t["especie"],t["comun"]),cellst),P(t["sitio"].split("(")[0].strip(),cellc),
                       P(t["ref"] or "-",cellc),P(fnum(t["d"]),cellc),P(fnum(t["ht"]),cellc),
                       P("%s (%s)"%(t["riesgo"],fnum(t["valriesgo"],2)),cellc),
                       P('<font color="%s">%s</font>'%(RC.get(t["caida"],GRIS).hexval(),t["caida"]),cellc),
                       P(CAT[t["cat"]],cellc),P(t["fund"],cellst)])
            styl.append(("BACKGROUND",(0,ri),(-1,ri),CATF[t["cat"]])); ri+=1
    tv=Table(dd,colWidths=[1.3*cm,3.5*cm,1.9*cm,1.2*cm,1.1*cm,1.1*cm,1.9*cm,1.2*cm,2.6*cm,10.3*cm],repeatRows=1)
    tv.setStyle(TableStyle(styl))
    E+=[Spacer(1,4),tv,NextPageTemplate("portrait"),PageBreak()]
    # 8 FOTOS
    E+=[P("9. Registro fotográfico por árbol",h1),
        P('Las fotografías de cada árbol están publicadas en la plataforma ArboLEC junto con su ficha completa; se indica la dirección URL individual y se reserva espacio para tres fotografías por árbol, a insertar en la versión final del expediente municipal.'),Spacer(1,4)]
    for grp_label,grp in [("PARQUE CENTRAL",D.parque),("AVENIDA EL ORO (fuera del parque)",D.avenida)]:
        E.append(P(grp_label,S("g8",fontName="Helvetica-Bold",fontSize=11,textColor=AZUL,spaceBefore=6,spaceAfter=4)))
        for t in grp:
            catcol=ROJO if t["cat"]=="DERRIBO" else (NARANJA if t["cat"]=="CONSERVAR-INT" else VERDE)
            info=P("<b><font color='%s'>%s — %s (%s)</font></b> · Código: %s · Ref.: %s<br/>Coordenadas: %s, %s · D=%s cm · H=%s m · Follaje %s%% · Riesgo total: %s (%s) · Caída: <font color='%s'>%s</font> · <b><font color='%s'>Veredicto: %s</font></b><br/><b>Ficha del árbol:</b> <a href='%s' color='blue'>%s</a>"%(
                VERDE.hexval(),t["id"],t["especie"],t["comun"],t["codigo"],t["ref"] or "s/n",fnum(t["lat"],6),fnum(t["lon"],6),
                fnum(t["d"]),fnum(t["ht"]),t["follaje"],t["riesgo"],fnum(t["valriesgo"],2),RC.get(t["caida"],GRIS).hexval(),t["caida"],
                catcol.hexval(),CAT[t["cat"]],t["url"],t["url"]),S("info",fontSize=8.5,leading=11.5))
            E.append(KeepTogether([info,Spacer(1,2),photo_strip(["Foto 1 — %s"%t["id"],"Foto 2 — %s"%t["id"],"Foto 3 — %s"%t["id"]],h=2.7*cm,tid=t["id"]),Spacer(1,8)]))
    # 9 CONCLUSIONES
    E+=[PageBreak(),P("10. Conclusiones y recomendaciones",h1),
        P("Conclusiones:",S("b",fontName="Helvetica-Bold",fontSize=10.5)),
        bullets([
            "Se evaluaron %d árboles (%d en el Parque Central y %d en la Avenida El Oro) de %d especies; la familia de los cipreses es la dominante y de mayor valor patrimonial."%(D.n,D.n_parque,D.n_av,len(D.species)),
            "En seguridad existe UN único punto crítico de riesgo de caída Alto: el árbol A06 (ciprés común, código %s, junto a los baños), el más grande del parque y único con recomendación de derribo en campo."%D.DERRIBO_TREE["codigo"],
            "Los demás %d árboles se CONSERVAN; su riesgo total «Alto» se explica mayoritariamente por daño de raíces a la infraestructura y por inclinación del fuste, condiciones manejables sin derribo."%(D.n-1),
            "En la Avenida El Oro destacan dos casos que, sin ser derribo, requieren evaluación estructural y monitoreo prioritario: AV02 (molle con pudrición del tronco) y AV04 (acacia vieja con raíz y fuste en mal estado).",
            "El daño al adoquinado y al concreto por raíces —motivo central de la solicitud— se atiende con manejo de raíces (barreras, reparación) y no justifica, por sí mismo, talar árboles sanos."]),
        P("Recomendaciones:",S("b",fontName="Helvetica-Bold",fontSize=10.5)),
        bullets([
            "Ejecutar el DERRIBO técnico controlado del árbol A06, previa verificación instrumental si se dispone del equipo, con personal especializado y medidas de seguridad por su cercanía a los baños.",
            "Programar la CONSERVACIÓN con intervención de los demás árboles: poda sanitaria y de reequilibrio, tratamiento fitosanitario, manejo de epífitas y retiro de clavos/alambres.",
            "Realizar EVALUACIÓN ESTRUCTURAL (resistógrafo/tomografía) de AV02 y AV04, y MONITOREO prioritario de A07 (raíz en mal estado), A14 y A20 (muy inclinados); reevaluar si progresa el deterioro o la inclinación.",
            "Implementar manejo de raíces (barreras anti-raíz, reparación del adoquinado) en los árboles con afectación Alta a infraestructura (A10, A23, AV03 y otros), preservando el árbol.",
            "Coordinar con la empresa eléctrica la poda de despeje donde hay interferencia Alta con redes aéreas (AV04).",
            "Considerar un plan de reposición y sucesión a mediano plazo, priorizando especies nativas (nogal andino, molle, arupo), y mantener actualizado el inventario en ArboLEC."]),
        Spacer(1,1.0*cm),
        P("____________________________________",bodyc),
        P("Ph.D. Darwin Alexander Pucha Cofrep",S("x",fontName="Helvetica-Bold",fontSize=11,alignment=TA_CENTER)),
        P("Responsable del Laboratorio de Dendrocronología — Universidad Nacional de Loja",smallc),
        P("Loja, 11 de julio de 2026",smallc)]
    doc.build(E)
    print("PDF informe:",path)

def build_oficio(path):
    doc=BaseDocTemplate(path,pagesize=A4,leftMargin=2.5*cm,rightMargin=2.5*cm,topMargin=1.6*cm,bottomMargin=1.6*cm)
    fw,fh=A4; fr=Frame(2.5*cm,1.6*cm,fw-5*cm,fh-3.4*cm,id="f")
    doc.addPageTemplates([PageTemplate(id="p",frames=[fr])])
    logos=Table([[img_scaled(D.LOGO_UNL,6.3*cm), img_scaled(D.LOGO_ARBOLEC,2.3*cm)]],colWidths=[10*cm,4*cm])
    logos.setStyle(TableStyle([("VALIGN",(0,0),(-1,-1),"MIDDLE"),("ALIGN",(0,0),(0,0),"LEFT"),("ALIGN",(1,0),(1,0),"RIGHT")]))
    E=[logos,Spacer(1,4),
       P("UNIVERSIDAD NACIONAL DE LOJA",S("x",fontName="Helvetica-Bold",fontSize=12.5,textColor=VERDE,alignment=TA_CENTER)),
       P("Laboratorio de Dendrocronología y Anatomía de la Madera · Facultad Agropecuaria y de Recursos Naturales Renovables",smallc),Spacer(1,10),
       P("Oficio Nro. UNL-LDEND-2026-001",S("r",fontSize=10.5,alignment=TA_RIGHT)),
       P("Loja, 11 de julio de 2026",S("r",fontSize=10.5,alignment=TA_RIGHT)),Spacer(1,8),
       P("Licenciado",body),P("<b>Segundo Abel Sarango Quizhpe</b>",body),
       P("<b>ALCALDE DEL GOBIERNO AUTÓNOMO DESCENTRALIZADO MUNICIPAL INTERCULTURAL DE SARAGURO</b>",S("b",fontName="Helvetica-Bold",fontSize=10.5,leading=13)),
       P("Presente.-",body),Spacer(1,8),
       P("<b>ASUNTO:</b> Remisión del informe técnico de evaluación del arbolado del Parque Central de Saraguro y criterio de conservación/derribo.",body),
       P("<i>Referencias: Oficio Nro. 0286-A-GADMIS (19/05/2026); Memorando Nro. UNL-R-2026-2083-M (28/05/2026); Ref. UNL-SG-2026-0421-EX.</i>",S("i",fontSize=9.5,textColor=GRIS,leading=12)),Spacer(1,6),
       P("De mi consideración:",body),
       P("Reciba un cordial saludo. En atención a su Oficio Nro. 0286-A-GADMIS y a la autorización conferida por el señor Rector mediante Memorando Nro. UNL-R-2026-2083-M, me permito remitir el Informe Técnico de Evaluación del Arbolado Urbano del Parque Central del cantón Saraguro, con el criterio especializado solicitado sobre el estado actual de los árboles y su eventual conservación o derribo.",body),
       P('El estudio comprendió el inventario y diagnóstico dendrométrico, fitosanitario y de riesgo de %d árboles (%d en el Parque Central y %d en la isleta de la Avenida El Oro), sistematizado y publicado en la plataforma institucional ArboLEC (<a href="%s" color="blue">%s</a>), disponible para consulta pública. La evaluación atendió de manera particular a los cipreses del parque y al riesgo de caída frente a las condiciones de viento de la zona.'%(D.n,D.n_parque,D.n_av,D.BASE_URL,D.BASE_URL),body),
       P("<b>Principales resultados:</b>",body),
       bullets([
         "Se recomienda el DERRIBO de UN (1) solo árbol: el ciprés común (<i>Hesperocyparis macrocarpa</i>) ubicado junto a los baños, identificado como A06 (código %s). Es el de mayor porte del parque (27,1 m; 118,2 cm de diámetro), el único con riesgo de caída ALTO, con exudados de resina y copa amplia y poco simétrica que, ante los fuertes vientos y su cercanía a una zona de alta concurrencia, representa un riesgo inaceptable para las personas."%D.DERRIBO_TREE["codigo"],
         "Se recomienda CONSERVAR los %d árboles restantes, incluidos los demás cipreses patrimoniales, mediante manejo: poda sanitaria y de reequilibrio, tratamiento fitosanitario, manejo de raíces y monitoreo de los ejemplares inclinados o con anclaje comprometido."%(D.n-1),
         "En la isleta de la Avenida El Oro, dos árboles requieren evaluación estructural y monitoreo prioritario (sin ser derribo): un molle con pudrición del tronco (AV02) y una acacia vieja con raíz y fuste en mal estado (AV04).",
         "El daño al adoquinado y al concreto por raíces —que motivó su solicitud— se atiende con manejo de raíces (barreras y reparación) sin necesidad de talar árboles estructuralmente sanos."]),
       P("Debo señalar, con la objetividad que exige este pronunciamiento, que la recomendación de derribo se ha emitido con criterio restrictivo: únicamente cuando la evidencia técnica demuestra un riesgo real e irreversible para la seguridad ciudadana, protegiendo así tanto a la población como el patrimonio arbóreo del Parque Central. Para el árbol A06 se sugiere, de ser posible, una verificación instrumental previa (resistógrafo o tomografía sónica) y su derribo por personal especializado con las debidas medidas de seguridad.",body),
       P("El detalle por árbol, el fundamento de cada veredicto, el mapa de ubicación, la tabla resumen de conservación y derribo y el registro fotográfico constan en el informe adjunto. Quedo a disposición del Municipio para socializar los resultados y acompañar técnicamente las acciones que se deriven.",body),
       P("Con sentimientos de distinguida consideración.",body),Spacer(1,10),
       P("Atentamente,",body),Spacer(1,18),
       P("____________________________________",body),
       P("<b>Ph.D. Darwin Alexander Pucha Cofrep</b>",body),
       P("Responsable del Laboratorio de Dendrocronología",S("g",fontSize=10,textColor=GRIS)),
       P("Universidad Nacional de Loja",S("g",fontSize=10,textColor=GRIS)),Spacer(1,10),
       P("<i>Adjunto: Informe Técnico de Evaluación del Arbolado del Parque Central y Avenida El Oro de Saraguro.</i>",S("i",fontSize=9.5,textColor=GRIS)),
       P("<i>Copia: Dr. Nikolay Aguirre Mendoza, Rector de la Universidad Nacional de Loja.</i>",S("i",fontSize=9.5,textColor=GRIS))]
    doc.build(E); print("PDF oficio:",path)

build_report(os.path.join(OUTDIR,"Informe_Tecnico_Arbolado_Parque_Central_Saraguro.pdf"))
build_oficio(os.path.join(OUTDIR,"Oficio_Respuesta_GAD_Saraguro.pdf"))
