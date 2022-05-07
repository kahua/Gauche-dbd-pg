# PostgreSQL binding of Gauche-dbd

## Requirements

- Gauche 0.9.8 or later.
- pg_config

`./configure` requires `pg_config` program.  Make sure it is in the PATH,
or you can give `--with-pg-config=PATH` to the `configure` program.


## Default database

make test requires PostgreSQL's default database.

if it's not exist, you may make it like that:

```
% sudo -u postgres createuser <your-login-name>
% sudo -u postgres createdb <your-login-name>
```

## Build and Install

```
% ./configure
% make
% make check
% sudo make install
```
