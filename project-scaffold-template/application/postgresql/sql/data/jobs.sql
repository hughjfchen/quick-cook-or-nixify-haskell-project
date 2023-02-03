\echo # Loading the jobs definition

-- the jobs entity definition
CREATE TABLE IF NOT EXISTS jobs
( id serial primary key
, created_at timestamp with time zone default now() not null
, updated_at timestamp with time zone default now() not null
, run_at timestamp with time zone default now() not null
, status text not null
, payload jsonb not null
, last_error jsonb null
, attempts int not null default 0
, locked_at timestamp with time zone null
, locked_by text null
, constraint incorrect_locking_info CHECK ((status <> 'locked' and locked_at is null and locked_by is null) or (status = 'locked' and locked_at is not null and locked_by is not null))
);

create index if not exists idx_jobs_created_at on jobs(created_at);
create index if not exists idx_jobs_updated_at on jobs(updated_at);
create index if not exists idx_jobs_locked_at on jobs(locked_at);
create index if not exists idx_jobs_locked_by on jobs(locked_by);
create index if not exists idx_jobs_status on jobs(status);
create index if not exists idx_jobs_run_at on jobs(run_at);

create or replace function notify_job_monitor_for_jobs() returns trigger as $$
begin
  perform pg_notify('job_created_jobs', row_to_json(new)::text);
  return new;
end;
$$ language plpgsql;
drop trigger if exists trg_notify_job_monitor_for_jobs on jobs;
