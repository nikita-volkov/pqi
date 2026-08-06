# v1.1.0.0

## Breaking

- Renamed `Notify` fields to match `postgresql-libpq` exactly: `relname` → `notifyRelname`, `bePid` → `notifyBePid`, `extra` → `notifyExtra`

# v1.0.0.3

Doc corrections.

# v1.0.0.2

Release process fixes.

# v1.0.0.1

Documentation corrections.

# v1.0.0.0

## Breaking

- Add the `resStatus` field to `Adapter`

# v0.1.0.2

Polish the docs.

# v0.1.0.1

Corrected the readme.

# v0.1.0.0

## Breaking

- Replace IsConnection/IsResult/IsCancel type classes with concrete records
- Move unescapeBytea into Adapter

## Non-breaking

- Add the Adapter type
