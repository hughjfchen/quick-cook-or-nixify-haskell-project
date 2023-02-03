-- This file contains the definition of the applications specific roles
-- the roles defined here should not be made owners of database entities (tables/views/...)

\echo # Loading roles

-- first create the normal role to access to the data schema
drop role if exists :data_authenticator;
create role :"data_authenticator" with login password :'data_authenticator_pass';

-- the role used by postgrest to connect to the database
-- notice how this role does not have any privileges attached specifically to it
-- it can only switch to other roles
drop role if exists :api_authenticator;
create role :"api_authenticator" with login password :'api_authenticator_pass';

-- this is an application level role
-- requests that are not authenticated will be executed with this role's privileges
drop role if exists :"anonymous";
create role :"anonymous";
grant :"anonymous" to :"api_authenticator";

-- role for the main application user accessing the api
drop role if exists webuser;
create role webuser;
grant webuser to :"api_authenticator";
