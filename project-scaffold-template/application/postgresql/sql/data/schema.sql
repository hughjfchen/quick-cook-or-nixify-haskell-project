drop schema if exists :data_schema cascade;
create schema :data_schema;
set search_path = :data_schema, public;

-- import our application models
\ir user.sql
\ir jobs.sql
\ir user_jobs_rel.sql
