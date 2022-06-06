drop schema if exists :api_schema cascade;
create schema :api_schema;
set search_path = :api_schema, public;

-- this role will be used as the owner of the views in the api schema
-- it is needed for the definition of the RLS policies
drop role if exists api;
create role api;
grant api to current_user; -- this is a workaround for RDS where the master user does not have SUPERUSER priviliges  


-- our endpoints
\ir login.sql
\ir refresh_token.sql
\ir signup.sql
\ir me.sql
\ir jobs.sql
\ir submit_parsedump_job.sql
\ir update_parse_report.sql
