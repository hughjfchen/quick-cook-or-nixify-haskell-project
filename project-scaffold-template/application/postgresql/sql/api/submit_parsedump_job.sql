
create or replace function submit_parsedump_job(status text, payload json) returns json as $$
declare
    job record;
    user_jobs_rel1 record;
begin
    insert into $DB_DATA_SCHEMA.jobs as j
    (status, payload) values ($1, $2)
    returning *
   	into job;

    insert into $DB_DATA_SCHEMA.user_jobs_rel as ujr
    (job_id, user_id) values (job.id, request.user_id())
    returning *
   	into user_jobs_rel1;

    return json_build_object(
        'id', job.id
    );
end
$$ security definer language plpgsql;
-- by default all functions are accessible to the public, we need to remove that and define our specific access rules
revoke all privileges on function submit_parsedump_job(text, json) from public;
