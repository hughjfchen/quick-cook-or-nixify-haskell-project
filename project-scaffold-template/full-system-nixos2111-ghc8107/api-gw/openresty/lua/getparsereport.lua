local cjson = require('cjson')

ngx.req.read_body()
local req_json = ngx.req.get_body_data()

if req_json then
    local req_query_job = cjson.decode(req_json)
    if req_query_job then
        local job_id = req_query_job['job_id']
	ngx.req.set_header("Content-Type", "application/json")
	ngx.req.set_header("Accept", "application/json")
        local job_query_res, parsereport_query_res = ngx.location.capture_multi({
		{"/rest/jobs?id=eq."..job_id}
		, { "/rest/parsereports?id=eq."..job_id}
	})
	ngx.log(ngx.INFO, job_query_res.body)
	ngx.log(ngx.INFO, parsereport_query_res.body)
	if job_query_res.body ~= "[]" then
            ngx.status = job_query_res.status
            ngx.header["Content-Type"] = job_query_res.header["Content-Type"]
            ngx.say(job_query_res.body)
	elseif parsereport_query_res.body ~= "[]" then
            ngx.status = parsereport_query_res.status
            ngx.header["Content-Type"] = parsereport_query_res.header["Content-Type"]
            ngx.say(parsereport_query_res.body)
	else
            ngx.status = ngx.HTTP_NOT_FOUND
            ngx.header["Content-Type"] = "application/json"
            ngx.say(cjson.encode({ id = job_id, message = "Cannot find the Job with the specified Id"}))
        end
    end
end
