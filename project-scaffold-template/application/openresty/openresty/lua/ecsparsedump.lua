local cjson = require('cjson')

if ngx.req.get_method() == "POST" then

  ngx.req.read_body() 

  local body_data = ngx.req.get_body_data()

  local body_json = cjson.decode(body_data)
  if body_json then
    local job_create_payload = {tag=body_json["parsetype"], contents={tag='HttpUrl', contents=body_json["dumpurl"]}}
    local job_create_req = {status='queued', payload=job_create_payload}
    ngx.log(ngx.INFO, cjson.encode(job_create_req))
    ngx.req.set_header("Content-Type", "application/json")
    ngx.req.set_header("Accept", "application/json")
    -- ngx.req.set_header("Prefer", "return=headers-only")
    local job_create_res = ngx.location.capture("/rest/rpc/submit_parsedump_job", {method = ngx.HTTP_POST, body = cjson.encode(job_create_req)})
    if job_create_res then
      ngx.status = job_create_res.status
      ngx.header["Content-Type"] = job_create_res.header["Content-Type"]
      -- local id_location = ngx.re.match(job_create_res.header["Location"], '(.+)=eq\\.(.+)')
      -- local theId
      -- if id_location then
      --    theId = id_location[2]
      -- end
      -- ngx.header = job_create_res.header
      -- ngx.say(cjson.encode({ id = theId }))
      ngx.say(job_create_res.body)
    end
  else
    ngx.say(cjson.encode({error="Error parsing post json", original_data=body_data}))
  end

else
  ngx.say(cjson.encode({error="Only POST allowed", original_data=""}))
end
