\echo # Loading roles privilege

-- following is for normal data base access to the data schema
grant usage on schema :data_schema to :"data_authenticator";
grant select, insert, update, delete on :data_schema.jobs to :"data_authenticator";

-- this file contains the privileges of all aplications roles to each database entity
-- if it gets too long, you can split it one file per entity ore move the permissions
-- to the file where you defined the entity

-- specify which application roles can access this api (you'll probably list them all)
grant usage on schema :api_schema to anonymous, webuser;

-- set privileges to all the auth flow functions
grant execute on function :api_schema.login(text,text) to anonymous;
grant execute on function :api_schema.signup(text,text,text) to anonymous;
grant execute on function :api_schema.me() to webuser;
grant execute on function :api_schema.login(text,text) to webuser;
grant execute on function :api_schema.refresh_token() to webuser;
grant execute on function :api_schema.submit_parsedump_job(text,json) to webuser;
grant execute on function :api_schema.update_parse_report(integer,json) to webuser;


-- give access to the view owner to this table
grant select, insert, update, delete on :data_schema.jobs to api;
grant usage on :data_schema.jobs_id_seq to webuser;


-- While grants to the view owner and the RLS policy on the underlying table
-- takes care of what rows the view can see, we still need to define what
-- are the rights of our application user in regard to this api view.

-- authenticated users can request/change all the columns for this view
grant select, insert, update, delete on :api_schema.jobs to webuser;

-- anonymous users can only request specific columns from this view
-- grant select (id, jobs) on api.jobs to anonymous;

-- give access to the view owner to this table
grant select, insert, update, delete on :data_schema.user_jobs_rel to api;
grant usage on :data_schema.user_jobs_rel_id_seq to webuser;


-- While grants to the view owner and the RLS policy on the underlying table
-- takes care of what rows the view can see, we still need to define what
-- are the rights of our application user in regard to this api view.

-- authenticated users can request/change all the columns for this view
-- grant select, insert, update, delete on api.user_jobs_rel to webuser;

-- anonymous users can only request specific columns from this view
-- grant select (id, jobs) on api.jobs to anonymous;
