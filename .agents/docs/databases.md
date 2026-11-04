# Databases

## SQLite

All internal databases are SQLite via mORMot2. Files live in `databases/` next to the binary
(`globals.DATABASEFOLDERNAME`). `dbhandler.pas` opens them in WAL mode with exclusive locking, so only
slFtp itself can access a database file while it is running.

Two access patterns exist. Follow the one the database you touch already uses:

- **ORM** — `TSQLRestClientDB` with `TOrm` record classes, created by `dbhandler.CreateORMSQLite3DB`.
  Example: `statsunit.pas`.
- **SynDB** — `TSQLDBSQLite3ConnectionProperties`, created by `dbhandler.CreateSQLite3DbConn`.
  Examples: `indexer.pas`, `dbtvinfo.pas`.

## MySQL / MariaDB

ZeosLib through mORMot2 (`mormot.db.sql.zeos`), e.g. in `dbaddpre.pas`.

## Pitfall: mORMot2 parameter binding

In `FormatSql`-style APIs, `%` placeholders are filled from the *params* array, but `?` placeholders are
bound from the *bounds* array. When an overload takes both arrays, bounds is the **second** one:

```pascal
// overload with one array: that array is the bounds -> OK
TSQLSitesRecord.CreateAndFillPrepare(ORMStatsDB.Client, 'Name = ?', [fSite], 'ID');

// overload with params AND bounds: the '?' values belong in the second array
TSQLStatsRecord.CreateAndFillPrepare(ORMStatsDB.Client, 'SrcSiteRec = ? AND DstSiteRec = ?', [], [0, 0]);
TSQLStatsRecord.CreateAndFillPrepareJoined(ORMStatsDB.Client, 'DstSiteRec.Name = ?', [], [fSite]);
```

Passing the values in the params array leaves `?` unbound, and the query silently returns 0 rows.
