export PGHOST=localhost
export PGPORT=5432
export PGUSER=trailio
export PGDATABASE=trailio
export PGPASS=trailio

env | grep PG

trails-exe --log-dir /tmp --dev
