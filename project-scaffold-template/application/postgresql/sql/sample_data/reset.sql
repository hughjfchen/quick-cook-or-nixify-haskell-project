BEGIN;
\set QUIET on
\set ON_ERROR_STOP on
set client_min_messages to warning;
truncate :data_schema.user restart identity cascade;
\ir data.sql
COMMIT;
