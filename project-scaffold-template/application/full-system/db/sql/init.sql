drop database if exists $DB_NAME;
create database $DB_NAME;
\c $DB_NAME

-- some setting to make the output less verbose
\set QUIET on
\set ON_ERROR_STOP on
set client_min_messages to warning;

-- load some variables from the env
\setenv base_dir :DIR
\set base_dir `if [ $base_dir != ":"DIR ]; then echo $base_dir; else echo "/docker-entrypoint-initdb.d"; fi`
\set anonymous `echo $DB_ANON_ROLE`
\set data_schema `echo $DB_DATA_SCHEMA`
\set api_schema `echo $DB_API_SCHEMA`
\set api_authenticator `echo $DB_API_USER`
\set api_authenticator_pass `echo $DB_API_PASS`
\set data_authenticator `echo $DB_DATA_USER`
\set data_authenticator_pass `echo $DB_DATA_PASS`
\set jwt_secret `echo $JWT_SECRET`
\set quoted_jwt_secret '\'' :jwt_secret '\''
\set jwt_lifetime `echo $JWT_LIFETIME`
\set quoted_jwt_lifetime '\'' :jwt_lifetime '\''


\echo # Loading database definition
begin;
create extension if not exists pgcrypto;

\echo # Loading dependencies

-- functions for storing different settins in a table
\ir libs/settings.sql

-- functions for reading different http request properties exposed by PostgREST
\ir libs/request.sql

-- functions for sending messages to RabbitMQ entities
\ir libs/rabbitmq.sql

-- functions for JWT token generation in the database context
\ir libs/pgjwt.sql

-- save app settings (they are storred in the settings.secrets table)
select settings.set('jwt_secret', :quoted_jwt_secret);
select settings.set('jwt_lifetime', :quoted_jwt_lifetime);


\echo # Loading application definitions

-- private schema where all tables will be defined
-- you can use othere names besides "data" or even spread the tables
-- between different schemas. The schema name "data" is just a convention
\ir data/schema.sql

-- entities inside this schema (which should be only views and stored procedures) will be 
-- exposed as API endpoints. Access to them however is still governed by the 
-- privileges defined for the current PostgreSQL role making the requests
\ir api/schema.sql


\echo # Loading roles and privilege settings
\ir authorization/roles.sql
\ir authorization/privileges.sql

\echo # Loading sample data
\ir sample_data/data.sql

commit;
\echo # ==========================================
