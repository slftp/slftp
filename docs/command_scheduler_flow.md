# slFTP Command Scheduler – Flow Diagram

> Branch: `feature/command-scheduler`  
> Stand: Commit `1844dfae`

---

## 1. High-Level Architektur

```mermaid
flowchart TB
    subgraph PRE["📥 PRE-Catcher"]
        A[Release announced<br/>on IRC]
    end

    subgraph KB["🧠 KB-Thread (kb.pas)"]
        B[kb_AddB / kb_Add]<-->C[FindPazoById<br/>Create TPazo]
        C-->D[Tuzelj]
    end

    subgraph ROUTING["🗺️ Race Routing (pazo.pas)"]
        D-->E{Destination<br/>needs mkdir?}
        E-->|ja|F[TPazoMkdirTask]
        E-->|nein|G[TPazoDirlistTask]
        D-->H[TPazoRaceTask]
    end

    subgraph SCHEDULER["📋 Command Scheduler<br/>(commandscheduler.pas)"]
        I[fMkdirRequests<br/>TList&lt;TCommandRequest&gt;]
        J[fDirlistRequests<br/>TList&lt;TCommandRequest&gt;]
        K[fOtherRequests<br/>SFV/NFO/CWD/Raw/Login]
        L[InternalGetNextRequest<br/>Priority + FIFO]
    end

    subgraph QUEUE["📦 Task Queue<br/>(queueunit.pas)"]
        M[tasks: TList&lt;TTask&gt;]<-->N[TryToAssignSlots]
        N-->O[TryToAssignRaceSlots]
    end

    subgraph SITES["🖥️ Site Slots<br/>(sitesunit.pas)"]
        P[TSiteSlot.Execute<br/>Main Loop]
        Q[TryExecuteCommand]
        R[SchedulerFire]
        S[QueueFire]
    end

    F-->I
    G-->J
    H-->M

    I-->L
    J-->L
    K-->L

    O-->P
    R-->P
    S-->N

    style PRE fill:#e1f5fe
    style KB fill:#fff3e0
    style ROUTING fill:#f3e5f5
    style SCHEDULER fill:#e8f5e9
    style QUEUE fill:#fff8e1
    style SITES fill:#fce4ec
```

---

## 2. Task Creation Flow (`Tuzelj` in `pazo.pas`)

```mermaid
flowchart TD
    A[Tuzelj called<br/>with dirlist entries]-->B{Destination dirlist<br/>entries.Count = 0?}

    B-->|ja|C{need_mkdir = True<br/>AND<br/>dependency_mkdir = ''?}
    C-->|ja|D[Create TPazoMkdirTask]
    D-->E[SchedulePazoMkdir]
    E-->F[Convert to TCommandRequest]
    F-->G[Add to fMkdirRequests]
    G-->H[SchedulerFire]<-->I[Wake idle slot]

    B-->|nein|J[Skip mkdir]
    C-->|nein|J

    A-->K{Dirlist not yet added<br/>AND not gave up?}
    K-->|ja|L[Create TPazoDirlistTask]
    L-->M[SchedulePazoDirlist]
    M-->N[Convert to TCommandRequest]
    N-->O[Add to fDirlistRequests]
    O-->H

    K-->|nein|P[Skip dirlist]

    A-->Q{Files to race?}
    Q-->|ja|R[Create TPazoRaceTask]
    R-->S[AddTask to Queue]
    S-->T[QueueFire]

    Q-->|nein|U[No race tasks]

    style H fill:#ffccbc
    style T fill:#ffccbc
```

### Wichtige Details
- `TPazoMkdirTask` / `TPazoDirlistTask` Objekte werden **sofort nach dem Scheduling freigegeben** (`aTask.Free`).
- Der Scheduler speichert nur leichtgewichtige `TCommandRequest` Records.
- Race-Tasks (`TPazoRaceTask`) gehen **nicht** in den Scheduler, sondern in die traditionelle Queue.

---

## 3. Slot Execution Loop (`TSiteSlot.Execute` in `sitesunit.pas`)

```mermaid
flowchart TD
    A[TSiteSlot.Execute<br/>Thread Loop]-->B{todotask <> nil?}

    B-->|JA|C[Execute todotask<br/>race / idle / login]
    C-->D[todotask := nil]
    D-->E[site.QueueFire]
    E-->A

    B-->|NEIN|F[TryExecuteCommand]
    F-->G{Command<br/>executed?}

    G-->|JA|H[site.QueueFire]
    H-->I[site.SchedulerFire]
    I-->A

    G-->|NEIN|J[event.WaitFor<br/>15 Min Timeout]
    J-->K{Event signaled?}
    K-->|JA|A
    K-->|Timeout|L[Force Leave / Recycle]
    L-->A

    style C fill:#c8e6c9
    style F fill:#bbdefb
    style I fill:#ffccbc
    style E fill:#ffccbc
```

### Schlüsselerkenntnis
- **Race-Tasks haben immer Vorrang** (`todotask` wird zuerst geprüft).
- Der Scheduler bekommt nur "Rest-Slots" (idle Slots ohne `todotask`).
- Nach **jedem** Arbeitsschritt (egal ob Race oder Scheduler) wird `QueueFire` aufgerufen, damit die Queue neue Race-Tasks zuweisen kann.
- Nach einem Scheduler-Command wird zusätzlich `SchedulerFire` aufgerufen, damit der nächste idle Slot den nächsten Command abarbeitet.

---

## 4. Scheduler Command Execution (`TryExecuteCommand` in `sitesunit.pas`)

```mermaid
flowchart TD
    A[TryExecuteCommand]-->B{ActiveCommandCount<br/>>= MaxCommandSlots?}
    B-->|JA|C[Exit - no slot available]
    B-->|NEIN|D[GetNextMkdir]
    D-->|found|E[Create TPazoMkdirTask<br/>temporary]
    E-->F[Execute MKD<br/>s.Mkdir]
    F-->G{Success?}
    G-->|JA|H[ps1.MkdirReady<br/>need_mkdir := False]
    G-->|550 File exists|H
    G-->|NEIN|I[readyerror := True]
    H-->J[Free temp task]
    I-->J
    J-->K[CompleteMkdir<br/>remove from scheduler]
    K-->L[Result := True]

    D-->|not found|M[GetNextLogin]
    M-->|found|N[Execute Login]
    N-->O[CompleteCommand]
    O-->L

    M-->|not found|P[GetNextDirlist]
    P-->|found|Q[Create TPazoDirlistTask<br/>temporary]
    Q-->R[Execute LIST/STAT<br/>s.Dirlist]
    R-->S[Parse response<br/>ps1.ParseDirlist]
    S-->T[Schedule subdir dirlists<br/>priority 5]
    T-->U[Re-schedule self<br/>if incomplete]
    U-->V[Free temp task]
    V-->W[CompleteDirlist<br/>remove from scheduler]
    W-->L

    P-->|not found|X[GetNext SFV/NFO/CWD/Raw]
    X-->|found|Y[Execute command]
    Y-->Z[CompleteCommand]
    Z-->L

    X-->|not found|AA[Result := False]

    style E fill:#fff9c4
    style Q fill:#fff9c4
    style H fill:#c8e6c9
    style L fill:#ffccbc
```

### Prioritäten-Reihenfolge
1. **Mkdir** (höchste Priorität – muss vor Dirlist laufen)
2. **Login**
3. **Dirlist**
4. **SFV Download**
5. **NFO Download**
6. **CWD**
7. **Raw** (niedrigste Priorität)

---

## 5. Scheduler Internals (`commandscheduler.pas`)

### Request Selection (`InternalGetNextRequest`)

```mermaid
flowchart TD
    A[fArr := aList.ToArray<br/>Snapshot]-->B[for each request]
    B-->C{startat > Now?}
    C-->|JA|D[Skip - delayed]
    C-->|NEIN|E{command_type = ctDirlist<br/>AND<br/>depending_on_dirlist <> nil?}
    E-->|JA|F{need_mkdir = True<br/>AND<br/>error = False?}
    F-->|JA|G[Skip - wait for mkdir]
    F-->|NEIN|H[Eligible]
    E-->|NEIN|H
    H-->I{Better priority<br/>or older?}
    I-->|JA|J[Select as best]
    I-->|NEIN|B
    G-->B
    D-->B

    J-->K[Return best request]
    B-->|end|L{Any selected?}
    L-->|JA|K
    L-->|NEIN|M[Return False]
```

### Deduplication & Cap (`InternalAddRequest`)

```mermaid
flowchart TD
    A[Schedule Request]-->B{Duplicate?<br/>same pazo_id + dir + site}
    B-->|JA|C[Reject - already scheduled]
    B-->|NEIN|D{Dirlist AND<br/>PazoDirCount >= 50?}
    D-->|JA|E[Reject - cap reached]
    D-->|NEIN|F[Add to TList]
    F-->G{Dirlist?}
    G-->|JA|H[IncrementPazoDirCount]
    G-->|NEIN|I[Done]
    H-->I

    style C fill:#ffcdd2
    style E fill:#ffcdd2
    style F fill:#c8e6c9
```

---

## 6. Race Task Assignment (`TryToAssignRaceSlots` in `queueunit.pas`)

```mermaid
flowchart TD
    A[TryToAssignRaceSlots]-->B{Source freeslots > 0?}
    B-->|NEIN|C[Exit]
    B-->|JA|D{Dest freeslots > 0?}
    D-->|NEIN|C
    D-->|JA|E{MaxSim cooldowns?}
    E-->|JA|C
    E-->|NEIN|F{Busy destination?}
    F-->|JA|C
    F-->|NEIN|G{Active transfer<br/>same file to dest?}
    G-->|JA|C
    G-->|NEIN|H{Dest upload slots<br/>maxed out?}
    H-->|JA|C
    H-->|NEIN|I{Source download slots<br/>maxed out?}
    I-->|JA|C
    I-->|NEIN|J[Find free online<br/>source slot]
    J-->K[Acquire destination<br/>SlotsAssignmentLock]
    K-->|timeout|C
    K-->|acquired|L[Find free online<br/>dest slot]
    L-->M{Both slots found?}
    M-->|NEIN|N[Release lock<br/>Exit]
    M-->|JA|O[Assign tasks:<br/>ss1.todotask := race<br/>ss2.todotask := wait]
    O-->P[Fire both slots]
    P-->Q[Release lock]

    style O fill:#c8e6c9
    style P fill:#ffccbc
```

---

## 7. Komplettes Sequenzdiagramm: Release → Transfer

```mermaid
sequenceDiagram
    participant IRC as IRC Pre
    participant KB as KB Thread
    participant PZ as TPazoSite.Tuzelj
    participant SCH as TCommandScheduler
    participant Q as TQueueThread
    participant S1 as TSiteSlot STARWARS/0
    participant S2 as TSiteSlot CZ/0
    participant FTP as FTP Server

    IRC->>KB: NEW release announced
    KB->>PZ: kb_AddB creates pazo

    PZ->>SCH: SchedulePazoMkdir<br/>dir=""
    SCH->>SCH: Add to fMkdirRequests
    SCH->>S1: SchedulerFire<br/>wake slot

    S1->>SCH: TryExecuteCommand<br/>GetNextMkdir
    S1->>S1: Create TPazoMkdirTask
    S1->>FTP: MKD /release
    FTP-->>S1: 257 created
    S1->>S1: MkdirReady("")<br/>need_mkdir := False
    S1->>SCH: CompleteMkdir
    S1->>Q: QueueFire
    S1->>SCH: SchedulerFire<br/>chain next slot

    PZ->>SCH: SchedulePazoDirlist<br/>dir=""
    SCH->>S1: SchedulerFire<br/>wake slot

    S1->>SCH: TryExecuteCommand<br/>GetNextDirlist
    S1->>S1: Create TPazoDirlistTask
    S1->>FTP: LIST /release
    FTP-->>S1: file list
    S1->>PZ: ParseDirlist<br/>discover files
    S1->>SCH: CompleteDirlist

    PZ->>Q: AddTask TPazoRaceTask<br/>file.nfo STARWARS→CZ
    Q->>Q: TryToAssignRaceSlots
    Q->>S1: ss1.todotask := race
    Q->>S2: ss2.todotask := wait
    Q->>S1: Fire
    Q->>S2: Fire

    S1->>S1: todotask <> nil
    S1->>FTP: RETR file.nfo
    FTP-->>S1: 150 Opening

    S2->>S2: todotask <> nil
    S2->>FTP: STOR file.nfo
    FTP-->>S2: 150 Opening

    S1->>S2: Data transfer<br/>STARWARS → CZ
    S2->>S2: Transfer complete
    S1->>S1: Transfer complete
    S1->>Q: QueueFire
    S2->>Q: QueueFire
```

---

## 8. Zustandsdiagramm: Ein Release durchlaufen

```mermaid
stateDiagram-v2
    [*] --> Routing: Release announced

    Routing --> MkdirPending: Destination empty
    Routing --> DirlistPending: Destination allowed
    Routing --> RaceQueued: Files found on source

    MkdirPending --> MkdirScheduled: Tuzelj creates mkdir
    MkdirScheduled --> MkdirExecuting: SchedulerFire wakes slot
    MkdirExecuting --> DirlistPending: MkdirReady()<br/>need_mkdir := False
    MkdirExecuting --> MkdirError: 550 / timeout

    DirlistPending --> DirlistScheduled: Tuzelj creates dirlist
    DirlistScheduled --> DirlistExecuting: SchedulerFire wakes slot
    DirlistExecuting --> RaceQueued: ParseDirlist finds files
    DirlistExecuting --> DirlistScheduled: Incomplete, re-schedule
    DirlistExecuting --> DirlistGaveUp: Too many retries

    RaceQueued --> RaceAssigned: TryToAssignRaceSlots<br/>finds free slots
    RaceQueued --> RaceQueued: No free slots / not ready

    RaceAssigned --> Downloading: Source slot RETR
    RaceAssigned --> Uploading: Dest slot STOR

    Downloading --> TransferComplete: RETR done
    Uploading --> TransferComplete: STOR done

    TransferComplete --> RaceQueued: More files to race
    TransferComplete --> Complete: All files done

    Complete --> [*]
    MkdirError --> [*]: Mark as failed
    DirlistGaveUp --> [*]: Mark as incomplete
```

---

## 9. Kritische Locks & Synchronisation

| Lock | Ort | Schützt |
|------|-----|---------|
| `fLock` | `TCommandScheduler` | Alle Request-Listen (Mkdir/Dirlist/Other) |
| `fSlotsAssignmentLock` | `TSite` | Slot-Iteration in `SchedulerFire` und `TryToAssignRaceSlots` |
| `dirlist_lock` | `TDirList` | `need_mkdir`, `entries`, `dirlistadded` |
| `kb_lock` | `kb.pas` (global) | `kb_list`, `kb_freeze` |
| `queue_lock` | `queueunit.pas` | `tasks`-Liste |
| `fActiveTransfersCS` | `TPazoSite` | `FActiveTransfers` Dictionary |

### Wake-Chain nach Command-Ausführung

```
Slot A (idle) → TryExecuteCommand → Execute Mkdir
    → CompleteMkdir
    → QueueFire  ──────┐
    → SchedulerFire ───┼──→ Slot B (idle) → TryExecuteCommand → Execute Dirlist
                        │       → CompleteDirlist
                        │       → QueueFire  ──────┐
                        │       → SchedulerFire ───┼──→ Slot C (idle) → ...
                        │                            │
                        └──→ Queue Thread wacht auf  ├──→ TryToAssignRaceSlots
                                                     │       → findet Race-Task ready
                                                     │       → weist Slot D + Slot E zu
                                                     │       → Fire D, Fire E
                                                     │
                                                     └──→ Slot D (todotask := race)
                                                              → Execute RETR
                                                              → QueueFire
```

---

## 10. Bekannte Bugs (fixed in `1844dfae`)

### Vor dem Fix
```mermaid
flowchart LR
    A[SchedulePazoMkdir]-->B[aTask.Free]
    B-->C[Debug log reads<br/>aTask.dir]
    C-->D[💥 Use-After-Free]
    B-->E[SchedulerFire throws]
    E-->F[except block]
    F-->G[aTask.Free again]
    G-->H[💥 Double-Free<br/>corrupted fastbins]
```

### Nach dem Fix
```mermaid
flowchart LR
    A[SchedulePazoMkdir]-->B[Debug log]
    B-->C[aTask.Free]
    C-->D[fFreed := True]
    D-->E[SchedulerFire]
    E-->F[throws?]
    F-->|JA|G[except block]
    G-->H{if not fFreed}
    H-->|nein|I[Skip Free ✅]
```

---

*Diagramme erstellt mit Mermaid-Syntax. In GitLab, GitHub oder VS Code mit Mermaid-Plugin renderbar.*
