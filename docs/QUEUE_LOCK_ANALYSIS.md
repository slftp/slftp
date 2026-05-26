# slFTP: Queue- & Lock-Zusammenhänge — Visualisierung & Problemanalyse

> Stand: `dev_racing_time_debugging` (`0eafd612`)  
> Scope: Release-Erkennung → Queue → Dirlist → Tuzelj → Race → Complete  
> Ziel: Bild der Zusammenhänge, Identifikation von Lock-/Queue-Problemen  

---

## 1. Gesamtarchitektur — Von der Erkennung zum Complete

```mermaid
flowchart TD
    subgraph Eingang["Phase 0: Release-Erkennung"]
        IRC["IRC Announce<br/>precatcher.pas:572"]
        PRE["Pre-DB / !addpre<br/>dbaddpre.pas:323"]
        AUTO["Auto-Dirlist<br/>taskautodirlist.pas:428"]
        REQ["Request-Fill<br/>taskautodirlist.pas:79"]
    end

    subgraph Kern["Phase 1: Kern-Objekte"]
        KB["kb_AddB<br/>kb.pas:181"]
        PAZO["TPazo.Create<br/>pazo.pas:808"]
        PSITE["TPazoSite.Create<br/>pazo.pas:1335"]
        DEST["AddDestination<br/>pazo.pas:1266"]
    end

    subgraph QueueSystem["Phase 2: Queue-System"]
        ADD["AddTask<br/>sitesunit.pas:1104"]
        QSORT["QueueSort<br/>queueunit.pas:390"]
        QFIRE["QueueFire<br/>queueunit.pas:116"]
        EXEC["TQueueThread.Execute<br/>queueunit.pas:1596"]
        ASSIGN["TryToAssignSlots<br/>queueunit.pas:710"]
    end

    subgraph Tasks["Phase 3: Task-Ausführung"]
        DIRLIST["TPazoDirlistTask.Execute<br/>taskrace.pas:181"]
        TUZELJ["Tuzelj<br/>pazo.pas:423"]
        RACE["TPazoRaceTask.Execute<br/>taskrace.pas:1204"]
        PARSEDIR["ParseDirlist<br/>pazo.pas:1479"]
        PARSEDUPE["ParseDupe<br/>pazo.pas:1639"]
        MKDIR["TPazoMkdirTask"]
    end

    subgraph Complete["Phase 4: Complete"]
        DCOMP["TDirList.Complete<br/>dirlist.pas:301"]
        MAINCOMP["Main-Dir Complete<br/>dirlist.pas:351"]
    end

    IRC --> KB
    PRE --> KB
    AUTO --> KB
    REQ --> PAZO
    KB --> PAZO
    PAZO --> PSITE
    PSITE --> DEST
    PSITE --> ADD
    ADD --> QSORT
    QSORT --> QFIRE
    QFIRE --> EXEC
    EXEC --> ASSIGN
    ASSIGN --> DIRLIST
    ASSIGN --> RACE
    ASSIGN --> MKDIR
    DIRLIST --> PARSEDIR
    PARSEDIR --> TUZELJ
    TUZELJ --> RACE
    TUZELJ --> MKDIR
    TUZELJ --> DIRLIST
    RACE --> PARSEDUPE
    PARSEDUPE --> TUZELJ
    DIRLIST --> DCOMP
    DCOMP --> MAINCOMP
```

---

## 2. Queue-Routing — Wer landet wo?

```mermaid
flowchart LR
    subgraph SourceSite["Source-Site Queue<br/>(z.B. '1984')"]
        direction TB
        Q_SRC["TQueueThread<br/>queueunit.pas:30"]
        RACE_TASKS["TPazoRaceTask<br/>(→ jede Destination)"]
        DIR_SRC["TPazoDirlistTask<br/>(Self-Requeue)"]
    end

    subgraph DestSiteA["Dest-Site A Queue<br/>(z.B. 'SITE_A')"]
        direction TB
        Q_DST_A["TQueueThread"]
        DIR_A["TPazoDirlistTask<br/>(von Tuzelj)"]
        MKDIR_A["TPazoMkdirTask"]
    end

    subgraph DestSiteB["Dest-Site B Queue<br/>(z.B. 'SITE_B')"]
        direction TB
        Q_DST_B["TQueueThread"]
        DIR_B["TPazoDirlistTask"]
        MKDIR_B["TPazoMkdirTask"]
    end

    TUZELJ["Tuzelj<br/>pazo.pas:423"]

    TUZELJ -->|"AddTask(site1=Source)"| RACE_TASKS
    TUZELJ -->|"AddTask(site1=Source)"| DIR_SRC
    TUZELJ -->|"AddTask(site1=DestA)"| DIR_A
    TUZELJ -->|"AddTask(site1=DestA)"| MKDIR_A
    TUZELJ -->|"AddTask(site1=DestB)"| DIR_B
    TUZELJ -->|"AddTask(site1=DestB)"| MKDIR_B
```

**Kritisch:** Race-Tasks und Source-Dirlist-Tasks teilen sich **dieselbe** Queue auf der Source-Site. Das bedeutet: Wenn viele Race-Tasks oben in der Queue sitzen, werden Dirlist-Tasks der Source verzögert.

---

## 3. Der Dirlist-Loop — Sequenzdiagramm mit Locks

```mermaid
sequenceDiagram
    participant Slot as TSiteSlot
    participant DTask as TPazoDirlistTask
    participant PDir as ParseDirlist
    participant DirL as TDirList
    participant Tuz as Tuzelj
    participant Queue as TQueueThread
    participant DestSlot as TSiteSlot (Dest)

    rect rgb(230, 245, 255)
        Note over Slot,DTask: 1. LIST ausführen
        DTask->>Slot: CWD + LIST
        Slot-->>DTask: lastResponse
    end

    rect rgb(255, 245, 230)
        Note over DTask,DirL: 2. Parsing unter dirlist_lock
        DTask->>PDir: ParseDirlist(dir, liststring)
        PDir->>DirL: dirlist_lock.Enter()
        DirL-->>PDir: Lock acquired
        PDir->>DirL: d.ParseDirlist(liststring)
        PDir->>DirL: dirlist_lock.Leave()
    end

    rect rgb(230, 255, 230)
        Note over Tuz,Queue: 3. Tuzelj erzeugt Tasks<br/>(KEIN Lock gehalten!)
        PDir->>Tuz: Tuzelj(dir, fFoundDirListEntries)
        Tuz->>Queue: AddTask(TPazoRaceTask) → Source Queue
        Tuz->>Queue: AddTask(TPazoDirlistTask) → Dest Queue
        Tuz->>Queue: AddTask(TPazoMkdirTask) → Dest Queue
    end

    rect rgb(255, 230, 245)
        Note over DTask,Queue: 4. Self-Requeue<br/>(unvollständige Source)
        DTask->>Queue: AddTask(TPazoDirlistTask für Source)
    end

    rect rgb(245, 245, 245)
        Note over Queue,DestSlot: 5. Queue-Ausführung<br/>Dest-Site dirlisted
        Queue->>DestSlot: TryToAssignSlots(TPazoDirlistTask)
        DestSlot-->>Queue: Slot zugewiesen
    end
```

---

## 4. Race-Task Execute — Sequenzdiagramm mit Dupe-Erkennung

```mermaid
sequenceDiagram
    participant Race as TPazoRaceTask
    participant Src as Source Slot
    participant Dst as Dest Slot
    participant PD as ParseDupe
    participant PX as ProcessXDupeResponse
    participant Tuz as Tuzelj

    rect rgb(230, 245, 255)
        Note over Race,Dst: PRET Phase
        Race->>Src: PRET RETR
        Race->>Dst: PRET STOR
    end

    alt Dupe erkannt (550/553)
        Dst-->>Race: 550/553 + Dupe-Keyword
        Race->>PD: ParseDupe(filename, False, Complete?)
        Race->>PX: ProcessXDupeResponse(lastResponse)
        PX->>PD: ParseDupe([array], False, False)
        PD->>Tuz: Tuzelj(fFilesToRace)
        Note over Race: Task beendet
    else Transfer gestartet
        Race->>Src: RETR
        Race->>Dst: STOR
        Race->>PD: ParseDupe(filename, False, False)
        Note over Src,Dst: Datenübertragung...
        Src-->>Race: 226 Complete
        Dst-->>Race: 226 Complete
        Race->>PD: ParseDupe(filename, True, True)
        PD->>Tuz: Tuzelj(fFilesToRace)
    end
```

---

## 5. Lock-Landschaft

### 5.1 Lock-Typen im System

| Lock-Name | Typ | Datei | Zweck | AGENTS.md-konform? |
|-----------|-----|-------|-------|-------------------|
| `dirlist_lock` | **TSlCriticalSection2** | `dirlist.pas:133` | Schützt `TDirList.entries` | ✅ Ja |
| `main_lock` | **TSlCriticalSection2** | `queueunit.pas:23` | Schützt `TQueueThread.tasks` | ✅ Ja |
| `destinations_cs` | **TCriticalSection** | `pazo.pas:68` | Schützt `TPazoSite.destinations` | ❌ **Nein** |
| `FActiveTransfersCS` | **TCriticalSection** | `pazo.pas:56` | Schützt `FActiveTransfers` | ❌ **Nein** |
| `uid_lock` | **TSlCriticalSection2** | `tasksunit.pas` | Task-UID-Generierung | ✅ Ja |
| `queueevent` | **TEvent** | `queueunit.pas:32` | Wake-up Signal für Queue-Thread | ⚠️ OK (kein Mutex) |

### 5.2 Lock-Hierarchie & Interaktionen

```mermaid
flowchart TD
    subgraph PazoLocks["TPazoSite Locks"]
        DEST_CS["destinations_cs<br/>TCriticalSection ⚠️"]
        ACT_CS["FActiveTransfersCS<br/>TCriticalSection ⚠️"]
    end

    subgraph DirlistLocks["TDirList Locks"]
        DL_LOCK["dirlist_lock<br/>TSlCriticalSection2 ✅"]
    end

    subgraph QueueLocks["Queue Locks"]
        MAIN_LOCK["main_lock<br/>TSlCriticalSection2 ✅"]
        SLOT_LOCK["TSite.AcquireSlotsAssignmentLock<br/>(implizit)"]
    end

    subgraph Caller["Aufrufer"]
        TUZ["Tuzelj"]
        PARSE["ParseDirlist"]
        PD["ParseDupe"]
        QUEUE["TQueueThread.Execute"]
    end

    TUZ -->|"Liest destinations"| DEST_CS
    TUZ -->|"Liest/Schreibt entries"| DL_LOCK
    PARSE -->|"Liest/Schreibt entries"| DL_LOCK
    PD -->|"Liest/Schreibt entries"| DL_LOCK
    QUEUE -->|"Liest/Schreibt tasks"| MAIN_LOCK
    QUEUE -->|"Weist Slots zu"| SLOT_LOCK

    style DEST_CS fill:#ffcccc
    style ACT_CS fill:#ffcccc
    style DL_LOCK fill:#ccffcc
    style MAIN_LOCK fill:#ccffcc
```

---

## 6. Identifizierte Probleme

### 6.1 🟡 Queue-Clustering (Design-Feature mit Nebenwirkungen)

**Symptom:** Eine Site bekommt 2-3 Dirlist-Tasks bevor eine andere Site startet.

**Ursachen:**
1. **Self-Requeue** (`taskrace.pas:593`): Jeder unvollständige Dirlist-Lauf erzeugt sofort einen neuen Task für dieselbe Site.
2. **Break-After-First** (`taskrace.pas:611`): Die Destination-Driven-Requeue-Schleife bricht nach der **ersten** Destination ab — die Source bekommt aber trotzdem einen neuen Task.
3. **Subdir-Explosion** (`taskrace.pas:446`): Jedes entdeckte Unterverzeichnis erzeugt einen neuen Dirlist-Task für die **Source**.
4. **Sortierung nach `lastTouch`** (`queueunit.pas:380`): Alle Tasks desselben Releases clustern am Queue-Anfang.

**Konsequenz:** Eine vielbeschäftigte Source-Site monopolisiert ihre eigenen Slots mit Dirlist-Tasks, während Destinations warten.

**Mögliche Lösungsansätze (nur zur Planung):**
- `dirlistadded`-Prüfung für Source-Requeue einführen (verhindert Duplikate)
- Break-After-First aufheben oder round-robin über Destinations
- Per-site Cap auf Dirlist-Tasks pro Release
- Sort-Key erweitern, um Dirlist-Tasks verschiedener Releases zu interleaven

---

### 6.2 🔴 Lock-Verstösse gegen AGENTS.md

**Befund:** `pazo.pas` verwendet `TCriticalSection` statt `TSlCriticalSection2`:

```pascal
destinations_cs: TCriticalSection;      // pazo.pas:68
FActiveTransfersCS: TCriticalSection;    // pazo.pas:56
```

**AGENTS.md sagt:**
> Thread safety: Use `TSlCriticalSection2` from `slcriticalsection2.pas` exclusively. Never standard Pascal sync objects.

**Risiko:** `TCriticalSection` bietet kein Timeout-Monitoring. Bei einem Deadlock friert der Thread ewig ein. `TSlCriticalSection2` hat Timeout-Logging und Deadlock-Erkennung.

**Empfohlener Fix:** `TCriticalSection` → `TSlCriticalSection2` ersetzen, Constructor/Destructor anpassen.

---

### 6.3 🟡 Race Condition: `fBusyDestinations`

**Befund:** `fBusyDestinations` (ein `TDictionary<TObject, integer>`) wird in `TQueueThread.Execute` **pro Loop-Iteration neu erzeugt** (`queueunit.pas:1627`).

**Konsequenz:** Es verhindert nur, dass **in derselben Iteration** zwei Races zur selben Destination gestartet werden. Aber:
- Wenn die Queue kurz darauf erneut feuert, ist das Dictionary leer.
- Es gibt **keine** langfristige Buchhaltung, welche Destinationen bereits beschäftigt sind.
- Das kann zu übermässigen parallelen Uploads zur selben Destination führen.

---

### 6.4 🟡 Lock-Ordnung: Potential für Inversion

**Befund:** `ParseDupe` hält `dirlist_lock`, gibt ihn frei, und ruft dann `Tuzelj` auf. `Tuzelj` greift auf `destinations_cs` zu. Das ist OK (kein Nested-Lock-Problem).

Aber: `Tuzelj` kann `AddTask` aufrufen, was `main_lock` der Queue hält. `main_lock` und `dirlist_lock` sind in **verschiedenen** Threads/Sites — kein klassischer Deadlock.

**Potenzielles Problem:** Wenn `TQueueThread.Execute` gleichzeitig `TryToAssignSlots` für einen Race-Task ausführt, der `dirlist_lock` benötigt, während `ParseDirlist` gerade `dirlist_lock` hält und auf `AddTask` wartet (welches `main_lock` braucht), könnte es zu einer **lock-order inversion** kommen, wenn `main_lock` und `dirlist_lock` im selben Kontext gebraucht werden.

**Aktuelle Analyse:** Kein direkter Deadlock nachweisbar, weil `AddTask` nicht versucht, `dirlist_lock` zu erwerben. Aber die Komplexität ist hoch.

---

### 6.5 🟢 Positiv: Dirlist-Task-Limit

**Befund:** `TryToAssignSlots` limitiert Dirlist-Tasks auf `slots.Count div 2` (`queueunit.pas:774-799`).

**Das ist gut:** Es verhindert, dass eine Site komplett von Dirlist-Tasks überflutet wird und nie Races ausführt.

---

## 7. Zusammenfassung

| Problem | Schwere | Ort | Kurzbeschreibung |
|---------|---------|-----|------------------|
| Queue-Clustering | 🟡 Mittel | `taskrace.pas:593/611`, `queueunit.pas:380` | Source-Site monopolisiert eigene Queue |
| `TCriticalSection` statt `TSlCriticalSection2` | 🔴 Hoch | `pazo.pas:56/68` | Verstoss gegen Thread-Safety-Policy |
| `fBusyDestinations` nicht persistent | 🟡 Mittel | `queueunit.pas:1627` | Keine langfristige Destination-Buchhaltung |
| Lock-Order-Komplexität | 🟡 Niedrig-Mittel | `pazo.pas`/`queueunit.pas` | Hohe Anzahl verschachtelter Locks |
| Dirlist-Task-Limit | 🟢 Gut | `queueunit.pas:774` | Schützt vor Total-Überflutung |

---

*Dieses Dokument ist eine Planungs- und Analyse-Grundlage. Keine Code-Änderungen wurden vorgenommen.*
