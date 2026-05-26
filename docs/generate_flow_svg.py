import xml.etree.ElementTree as ET

def create_svg():
    W, H = 1800, 1400
    
    svg = ET.Element('svg')
    svg.set('xmlns', 'http://www.w3.org/2000/svg')
    svg.set('width', str(W))
    svg.set('height', str(H))
    svg.set('viewBox', f'0 0 {W} {H}')
    svg.set('style', 'font-family:Segoe UI,Arial,sans-serif; background:#f8f9fa;')
    
    # Background
    bg = ET.SubElement(svg, 'rect')
    bg.set('width', str(W))
    bg.set('height', str(H))
    bg.set('fill', '#f8f9fa')
    
    defs = ET.SubElement(svg, 'defs')
    
    # Arrow marker
    marker = ET.SubElement(defs, 'marker')
    marker.set('id', 'arrow')
    marker.set('markerWidth', '10')
    marker.set('markerHeight', '7')
    marker.set('refX', '9')
    marker.set('refY', '3.5')
    marker.set('orient', 'auto')
    mp = ET.SubElement(marker, 'polygon')
    mp.set('points', '0 0, 10 3.5, 0 7')
    mp.set('fill', '#666')
    
    # Dashed arrow
    marker2 = ET.SubElement(defs, 'marker')
    marker2.set('id', 'arrow-red')
    marker2.set('markerWidth', '10')
    marker2.set('markerHeight', '7')
    marker2.set('refX', '9')
    marker2.set('refY', '3.5')
    marker2.set('orient', 'auto')
    mp2 = ET.SubElement(marker2, 'polygon')
    mp2.set('points', '0 0, 10 3.5, 0 7')
    mp2.set('fill', '#d32f2f')
    
    def box(x, y, w, h, text, fill='#e3f2fd', stroke='#2196f3', font_size=13, subtext=None):
        r = ET.SubElement(svg, 'rect')
        r.set('x', str(x))
        r.set('y', str(y))
        r.set('width', str(w))
        r.set('height', str(h))
        r.set('rx', '8')
        r.set('fill', fill)
        r.set('stroke', stroke)
        r.set('stroke-width', '2')
        
        t = ET.SubElement(svg, 'text')
        t.set('x', str(x + w/2))
        t.set('y', str(y + h/2 - (8 if subtext else 0)))
        t.set('text-anchor', 'middle')
        t.set('dominant-baseline', 'middle')
        t.set('font-size', str(font_size))
        t.set('font-weight', 'bold')
        t.set('fill', '#1a1a1a')
        t.text = text
        
        if subtext:
            t2 = ET.SubElement(svg, 'text')
            t2.set('x', str(x + w/2))
            t2.set('y', str(y + h/2 + 14))
            t2.set('text-anchor', 'middle')
            t2.set('dominant-baseline', 'middle')
            t2.set('font-size', '11')
            t2.set('fill', '#555')
            t2.text = subtext
        return (x, y, w, h)
    
    def arrow(x1, y1, x2, y2, color='#666', dash=False, label=None):
        line = ET.SubElement(svg, 'line')
        line.set('x1', str(x1))
        line.set('y1', str(y1))
        line.set('x2', str(x2))
        line.set('y2', str(y2))
        line.set('stroke', color)
        line.set('stroke-width', '2')
        line.set('marker-end', 'url(#arrow)' if color != '#d32f2f' else 'url(#arrow-red)')
        if dash:
            line.set('stroke-dasharray', '5,3')
        if label:
            mid_x = (x1 + x2) / 2
            mid_y = (y1 + y2) / 2
            lt = ET.SubElement(svg, 'text')
            lt.set('x', str(mid_x + 8))
            lt.set('y', str(mid_y - 4))
            lt.set('font-size', '11')
            lt.set('fill', color)
            lt.text = label
    
    def label_bg(x, y, w, h, text, fill='#fff3e0', stroke='#ff9800'):
        r = ET.SubElement(svg, 'rect')
        r.set('x', str(x))
        r.set('y', str(y))
        r.set('width', str(w))
        r.set('height', str(h))
        r.set('rx', '6')
        r.set('fill', fill)
        r.set('stroke', stroke)
        r.set('stroke-width', '1.5')
        t = ET.SubElement(svg, 'text')
        t.set('x', str(x + w/2))
        t.set('y', str(y + h/2))
        t.set('text-anchor', 'middle')
        t.set('dominant-baseline', 'middle')
        t.set('font-size', '12')
        t.set('font-weight', 'bold')
        t.set('fill', '#333')
        t.text = text
    
    # Title
    title = ET.SubElement(svg, 'text')
    title.set('x', str(W/2))
    title.set('y', '35')
    title.set('text-anchor', 'middle')
    title.set('font-size', '22')
    title.set('font-weight', 'bold')
    title.set('fill', '#212529')
    title.text = 'slFTP Release Flow — Von der Erkennung bis zum Complete'
    
    # Phase 0: Release-Erkennung
    label_bg(20, 60, 220, 28, 'Phase 0: Release-Erkennung', '#e3f2fd', '#2196f3')
    
    b_irc = box(30, 100, 180, 50, 'IRC Announce', '#e3f2fd', '#2196f3', 13, 'precatcher.pas:572')
    b_pre = box(230, 100, 180, 50, 'Pre-DB / !addpre', '#e3f2fd', '#2196f3', 13, 'dbaddpre.pas:323')
    b_auto = box(430, 100, 180, 50, 'Auto-Dirlist', '#e3f2fd', '#2196f3', 13, 'taskautodirlist.pas:428')
    b_req = box(630, 100, 180, 50, 'Request-Fill', '#e3f2fd', '#2196f3', 13, 'taskautodirlist.pas:79')
    
    # Phase 1: Kern-Objekte
    label_bg(20, 180, 200, 28, 'Phase 1: Kern-Objekte', '#fff3e0', '#ff9800')
    
    b_kb = box(130, 220, 200, 55, 'kb_AddB', '#fff3e0', '#ff9800', 14, 'kb.pas:181')
    b_pazo = box(370, 220, 180, 55, 'TPazo.Create', '#fff3e0', '#ff9800', 14, 'pazo.pas:808')
    b_psite = box(590, 220, 200, 55, 'TPazoSite.Create', '#fff3e0', '#ff9800', 14, 'pazo.pas:1335')
    b_dest = box(830, 220, 200, 55, 'AddDestination', '#fff3e0', '#ff9800', 14, 'pazo.pas:1266')
    
    arrow(230, 150, 230, 220)  # IRC -> kb_AddB
    arrow(320, 150, 320, 220)  # Pre -> kb_AddB
    arrow(520, 150, 430, 220)  # Auto -> kb_AddB
    arrow(720, 150, 430, 220)  # Req -> kb_AddB
    arrow(330, 247, 370, 247)  # kb_AddB -> TPazo
    arrow(550, 247, 590, 247)  # TPazo -> TPazoSite
    arrow(790, 247, 830, 247)  # TPazoSite -> AddDestination
    
    # Phase 2: Queue-System
    label_bg(20, 300, 220, 28, 'Phase 2: Queue-System', '#e8f5e9', '#4caf50')
    
    b_addtask = box(80, 340, 160, 50, 'AddTask', '#e8f5e9', '#4caf50', 13, 'sitesunit.pas:1104')
    b_qsize = box(280, 340, 160, 50, 'QueueSort + Fire', '#e8f5e9', '#4caf50', 13, 'queueunit.pas')
    b_qexec = box(480, 340, 180, 50, 'TQueueThread.Execute', '#e8f5e9', '#4caf50', 13, 'queueunit.pas:1596')
    b_assign = box(700, 340, 180, 50, 'TryToAssignSlots', '#e8f5e9', '#4caf50', 13, 'queueunit.pas:710')
    
    arrow(230, 275, 160, 340)
    arrow(160, 365, 280, 365)
    arrow(440, 365, 480, 365)
    arrow(660, 365, 700, 365)
    
    # Phase 3: Task-Ausführung (3 columns)
    label_bg(20, 420, 250, 28, 'Phase 3: Task-Ausführung', '#fce4ec', '#e91e63')
    
    # Dirlist Task column
    b_dirlist = box(60, 470, 200, 55, 'TPazoDirlistTask', '#fce4ec', '#e91e63', 14, 'taskrace.pas:181')
    b_parsedir = box(60, 560, 200, 55, 'ParseDirlist', '#fce4ec', '#e91e63', 14, 'pazo.pas:1479')
    b_tuzelj = box(60, 650, 200, 55, 'Tuzelj', '#fff3e0', '#ff9800', 16, 'pazo.pas:423')
    
    arrow(790, 365, 160, 470, dash=True)
    arrow(160, 525, 160, 560)
    arrow(160, 615, 160, 650)
    
    # Race Task column
    b_race = box(520, 470, 200, 55, 'TPazoRaceTask', '#e8f5e9', '#4caf50', 14, 'taskrace.pas:1204')
    b_parsedupe = box(520, 560, 200, 55, 'ParseDupe', '#ffebee', '#d32f2f', 14, 'pazo.pas:1639')
    
    arrow(790, 365, 620, 470, dash=True)
    arrow(620, 525, 620, 560)
    arrow(620, 615, 160, 675, color='#d32f2f', dash=True, label='ruft Tuzelj')
    
    # Mkdir Task column
    b_mkdir = box(980, 470, 180, 55, 'TPazoMkdirTask', '#e3f2fd', '#2196f3', 14, 'pazo.pas:539')
    
    arrow(790, 365, 1070, 470, dash=True)
    
    # Tuzelj outputs
    arrow(260, 677, 520, 677, label='Race-Task')
    arrow(260, 677, 1070, 525, label='Mkdir-Task')
    arrow(260, 677, 160, 760, label='Dirlist-Task')
    
    # Self-Requeue (Problem 1)
    b_selfrequeue = box(60, 760, 200, 55, 'Self-Requeue', '#ffebee', '#d32f2f', 13, 'taskrace.pas:593')
    arrow(160, 705, 160, 760)
    arrow(160, 815, 160, 860, color='#d32f2f', dash=True)
    
    # Subdir explosion (Problem 3)
    b_subdir = box(60, 860, 200, 55, 'Subdir-Explosion', '#ffebee', '#d32f2f', 13, 'taskrace.pas:446')
    arrow(160, 815, 160, 860, color='#d32f2f')
    
    # Phase 4: Complete
    label_bg(20, 940, 220, 28, 'Phase 4: Complete', '#f3e5f5', '#9c27b0')
    
    b_complete = box(80, 990, 200, 55, 'TDirList.Complete', '#f3e5f5', '#9c27b0', 14, 'dirlist.pas:301')
    b_maincomp = box(340, 990, 200, 55, 'Main-Dir Complete', '#f3e5f5', '#9c27b0', 14, 'dirlist.pas:351')
    
    arrow(160, 915, 180, 990)
    arrow(280, 1017, 340, 1017)
    
    # Problem area (right side)
    label_bg(1250, 420, 500, 28, 'Warum die Queue nicht sauber aufteilt', '#ffebee', '#d32f2f')
    
    problems = [
        ('1. Self-Requeue', 'Jeder unvollständige Dirlist-Lauf\nerzeugt sofort einen neuen Task\nfür dieselbe Source-Site.', 470),
        ('2. Break-After-First', 'Die Destination-Driven-Schleife\nbricht nach der ERSTEN Destination ab.\nDie Source bekommt trotzdem einen Task.', 580),
        ('3. Subdir-Explosion', 'Jedes entdeckte Sample/Subs/Proof/Covers\nerzeugt einen neuen Dirlist-Task\nfür die Source-Site.', 690),
    ]
    
    for title, desc, y in problems:
        r = ET.SubElement(svg, 'rect')
        r.set('x', '1260')
        r.set('y', str(y))
        r.set('width', '480')
        r.set('height', '90')
        r.set('rx', '8')
        r.set('fill', '#ffebee')
        r.set('stroke', '#d32f2f')
        r.set('stroke-width', '2')
        
        t = ET.SubElement(svg, 'text')
        t.set('x', '1275')
        t.set('y', str(y + 22))
        t.set('font-size', '13')
        t.set('font-weight', 'bold')
        t.set('fill', '#b71c1c')
        t.text = title
        
        lines = desc.split('\\n')
        for i, line in enumerate(lines):
            t2 = ET.SubElement(svg, 'text')
            t2.set('x', '1275')
            t2.set('y', str(y + 42 + i * 16))
            t2.set('font-size', '11')
            t2.set('fill', '#333')
            t2.text = line
    
    # Lock info box
    label_bg(1250, 810, 500, 28, 'Lock-Konformität (AGENTS.md)', '#fff8e1', '#ff9800')
    
    locks = [
        ('dirlist_lock', 'TSlCriticalSection2 ✅', '#4caf50'),
        ('main_lock (Queue)', 'TSlCriticalSection2 ✅', '#4caf50'),
        ('destinations_cs', 'TCriticalSection ⚠️ VERSTOSS', '#f44336'),
        ('FActiveTransfersCS', 'TCriticalSection ⚠️ VERSTOSS', '#f44336'),
    ]
    
    for i, (name, status, color) in enumerate(locks):
        y = 855 + i * 28
        t = ET.SubElement(svg, 'text')
        t.set('x', '1275')
        t.set('y', str(y))
        t.set('font-size', '12')
        t.set('font-weight', 'bold')
        t.set('fill', '#333')
        t.text = name
        
        t2 = ET.SubElement(svg, 'text')
        t2.set('x', '1410')
        t2.set('y', str(y))
        t2.set('font-size', '12')
        t2.set('fill', color)
        t2.text = status
    
    # Queue routing note
    r = ET.SubElement(svg, 'rect')
    r.set('x', '1250')
    r.set('y', '980')
    r.set('width', '500')
    r.set('height', '90')
    r.set('rx', '8')
    r.set('fill', '#e3f2fd')
    r.set('stroke', '#2196f3')
    r.set('stroke-width', '2')
    
    note_lines = [
        'Queue-Routing:',
        '• Race-Tasks → Source-Site Queue',
        '• Dirlist-/Mkdir-Tasks → Destination-Site Queue',
        '• Sortierung nach lastTouch → Clustering desselben Releases'
    ]
    for i, line in enumerate(note_lines):
        t = ET.SubElement(svg, 'text')
        t.set('x', '1265')
        t.set('y', str(1000 + i * 18))
        t.set('font-size', '12')
        t.set('font-weight', 'bold' if i == 0 else 'normal')
        t.set('fill', '#1565c0' if i == 0 else '#333')
        t.text = line
    
    # Footer
    footer = ET.SubElement(svg, 'text')
    footer.set('x', str(W/2))
    footer.set('y', str(H - 15))
    footer.set('text-anchor', 'middle')
    footer.set('font-size', '11')
    footer.set('fill', '#999')
    footer.text = 'Stand: dev_racing_time_debugging (0eafd612) | slFTP Queue & Lock Analyse'
    
    return ET.tostring(svg, encoding='unicode')

svg_content = create_svg()
with open('docs/slftp_flow.svg', 'w', encoding='utf-8') as f:
    f.write(svg_content)

print('SVG erstellt: docs/slftp_flow.svg')
print(f'Grösse: {len(svg_content)} Zeichen')
