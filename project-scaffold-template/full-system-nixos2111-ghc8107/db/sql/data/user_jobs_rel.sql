\echo # Loading the user_jobs_rel definition

-- the user jobs relationship definition
CREATE TABLE IF NOT EXISTS user_jobs_rel
( id serial primary key
, job_id int references jobs(id) on delete cascade
, user_id int references "user"(id) on delete cascade
);

create index if not exists idx_user_jobs_rel_job_id on user_jobs_rel(job_id);
create index if not exists idx_user_jobs_rel_user_id on user_jobs_rel(user_id);
