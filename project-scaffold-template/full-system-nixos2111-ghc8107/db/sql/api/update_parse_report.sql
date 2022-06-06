
create or replace function update_parse_report(job_id integer, last_update json) returns json as $$
declare
    job record;
begin
    update $DB_DATA_SCHEMA.jobs as j
    set last_error = $2
    where j.id = $1
    returning *
   	into job;

    return json_build_object(
        'id', job.id
    );
end
$$ security definer language plpgsql;
-- by default all functions are accessible to the public, we need to remove that and define our specific access rules
revoke all privileges on function update_parse_report(integer, json) from public;
